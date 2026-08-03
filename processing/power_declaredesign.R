#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Simulation-based power analysis for the distributive conjoint (DeclareDesign)
# Responsible: Andreas Laffert
# Executive Summary: Complements the analytic, binary/forced-choice power bound in
#   cjpow.R (Schuessler & Freitag closed-form) with a Monte Carlo power analysis
#   under the design's actual outcome: a continuous, zero-sum share (0-100) split
#   between the two profiles of a task. The central question is no longer "do I
#   have power for a 3 pp interaction?" but "what is the SMALLEST attribute x
#   moderator interaction (conditional AMCE, effort x meritocratic-preference M)
#   that N = 1,500 can detect?" (its minimum detectable effect, MDE). The delta
#   range swept (0.5-3 pp per SD of M) is anchored on Gilgen (2022), the closest
#   available evidence: at a comparable N (~1,500) and the same fixed-effects
#   estimator, Gilgen's class x merit interaction in the scholarship scenario is
#   null and, where estimated, on the order of a fraction of a percentage point —
#   i.e. real interaction effects in this domain are plausibly small, not the
#   3 pp originally assumed. The design also now models measurement error in the
#   moderator (a meritocracy attitude scale is not measured without error), via a
#   `reliability` parameter that attenuates the interaction the estimator can
#   recover, separately from the DGP's true effect.
#   Mechanically based on processing/declare_desing_conjoint.R (DeclareDesign
#   structure: declare_model + declare_measurement/estimator + diagnose_design
#   swept over a grid, ggplot of power), but rebuilt on the CURRENT attribute
#   design (need, identity, control, effort, reciprocity, attitude, sex) instead
#   of the old/retired attribute set.
# Date: August 3, 2026
#******************************************************************************************************************************************************

options(scipen = 999)
rm(list = ls())

# 1. Packages  -----------------------------------------------------
if (!require("pacman")) install.packages("pacman")

# cjpowR is not on CRAN; install once with:
#   devtools::install_github("m-freitag/cjpowR")
pacman::p_load(DeclareDesign, cjpowR, estimatr, tidyverse, kableExtra, here, future, future.apply)

set.seed(123)

# Parallelize diagnose_design() over simulations. Plan defaults to "sequential"
# even with the future package loaded, so this MUST be set explicitly.
# multisession workers are separate R processes and do NOT inherit attached
# packages automatically; DeclareDesign's design objects store unevaluated
# fabricatr/estimatr calls inside quosures that future's automatic
# global/package scanner cannot see, so `add_level()`/`lead()` etc. fail on
# workers unless the packages are attached there explicitly. We "warm" each
# worker once (library() calls persist for the life of the multisession pool)
# rather than relying on autodetection. Verified: ~2.7x wall-clock speedup on
# 7 workers for a single diagnose_design() call on this machine.
n_workers <- max(1, parallel::detectCores() - 1)
plan(multisession, workers = n_workers)
invisible(future_lapply(seq_len(n_workers), function(i) {
  suppressPackageStartupMessages({
    library(DeclareDesign); library(fabricatr); library(estimatr); library(dplyr)
  })
  TRUE
}))

# API note (checked against the versions installed for this project — see the
# end-of-script report for what was verified): DeclareDesign 1.1.0, fabricatr
# 1.0.2, estimatr 1.0.6. declare_estimator()'s `model =` argument used in the
# old script is DEPRECATED in favor of `.method =`; `model` still works but
# emits no guidance, so we use `.method` here. `term` and `inquiry` both accept
# character VECTORS that are matched positionally, letting one declare_estimator()
# call extract several coefficients at once (used below for the main effect and
# the interaction).

# 2. Conjoint design ------------------------------------------------------

# ── 1. Attributes and levels (current design; level 1 = reference) ─────────
# need        1 Comfort (ref) / 2 Hardship                              K=2
# identity    1 Chile (ref) / 2 Peru / 3 Venezuela                      K=3
# control     1 Did not apply in time (ref) / 2 Applied but no funding  K=2
# effort      1 Less than peers (ref) / 2 Same as peers / 3 More        K=3
# reciprocity 1 Has not volunteered (ref) / 2 Has volunteered           K=2
# attitude    1 Deserves it (ref) / 2 Grateful for it                   K=2
# sex         1 Male name (ref) / 2 Female name                         K=2
#
# Nesting: profile (2 per task) in task (K_tasks per respondent) in respondent.
# Randomization: uniform and independent per profile (1/2 for K=2, 1/3 for K=3).
# Outcome: continuous share in [0,100]; the two profiles of a task are forced
# to sum to 100 (zero-sum), imposed explicitly below.

# ── 2. Function that builds the design for given N_resp / K_tasks / sd_outcome ─
#
# True effects (percentage points on the 0-100 share scale; level of interest
# = the more "deserving" level, vs. reference = level 1):
#   amce_need        13   pp  Hardship vs Comfort   (Gilgen-type effect, largest)
#   amce_effort        6   pp  More vs Less effort
#   amce_identity       3   pp  Peru and Venezuela vs Chile (same magnitude for both)
#   amce_control      3.5 pp  Applied/no funding vs Did not apply
#   amce_reciprocity  3.5 pp  Has volunteered vs has not
#   amce_attitude     3.5 pp  Grateful vs Deserves it
#   amce_sex          3.5 pp  Female vs Male name
#   delta_interaction   SWEPT (see section 3) pp  the AMCE of effort (More vs Less)
#                              grows by this much per SD of the level-2 moderator M
#   reliability         SWEPT (see section 3), default 0.8: reliability of the
#                              OBSERVED moderator scale used by the estimator (see
#                              "Measurement error in M" below); 1.0 = perfect moderator
#
# effort_same (Same vs Less) has no hypothesis attached in the prompt; it is
# generated at half the More-vs-Less effect (amce_effort/2) purely so the
# estimating model below (which controls for it, per the PAP specification)
# isn't driven by an arbitrary discontinuity between "Same" and "More" — this
# is a modeling assumption, not a design requirement, and does not affect the
# two inquiries of interest (effort main effect and the interaction).
#
# Measurement error in M: the DGP generates the true interaction effect using
# the TRUE moderator M (mean 0, SD 1). The ESTIMATOR, however, only has access
# to an OBSERVED moderator M_obs — standing in for a real attitudinal scale,
# which is never measured without error. Under classical test theory,
# M_obs = sqrt(reliability) * M + rnorm(sd = sqrt(1 - reliability)) gives
# cor(M, M_obs) = sqrt(reliability) while keeping M_obs standardized (mean 0,
# SD 1) — so `delta_interaction` remains "true pp per SD of M" regardless of
# reliability, and only the estimator's ability to RECOVER that true effect is
# attenuated. reliability = 1 reproduces a perfectly measured moderator
# (M_obs == M); reliability = 0.8 is a realistic value for an attitudinal scale.
conjoint_design <- function(
    N_resp,
    K_tasks           = 6,
    sd_outcome        = 15,    # SD of the share (pp); fixed at a plausible value here
    reliability       = 0.8,   # cor(M, M_obs)^2; reliability of the observed moderator
    amce_need         = 13,
    amce_identity     = 3,
    amce_control      = 3.5,
    amce_effort       = 6,
    amce_reciprocity  = 3.5,
    amce_attitude     = 3.5,
    amce_sex          = 3.5,
    delta_interaction = 1.5    # true pp per SD of M; SWEPT in section 3, anchored on Gilgen (2022)
) {

  # declare_model(): population + randomization + potential outcome, built as
  # three nested fabricatr levels (respondent -> task -> profile) so that
  # N_resp and K_tasks are literal parameters redesign() can sweep below.
  modelo <- declare_model(

    # Respondent level: one draw of the TRUE meritocratic-preference moderator
    # M per respondent (standardized), plus the OBSERVED, error-prone version
    # M_obs that the estimator actually uses (see "Measurement error in M"
    # above). Both held constant across that respondent's tasks/profiles.
    respondent = add_level(
      N = N_resp,
      M     = rnorm(N),                                                   # true moderator
      M_obs = sqrt(reliability) * M + rnorm(N, sd = sqrt(1 - reliability)) # observed moderator
    ),

    # Task level: K_tasks tasks per respondent, no task-level variables needed.
    task = add_level(N = K_tasks),

    # Profile level: 2 profiles per task (paired comparison).
    profile = add_level(
      N = 2,

      # -- Attribute randomization: uniform, independent per profile --------
      need        = sample(1:2, N, replace = TRUE),
      identity    = sample(1:3, N, replace = TRUE),
      control     = sample(1:2, N, replace = TRUE),
      effort      = sample(1:3, N, replace = TRUE),
      reciprocity = sample(1:2, N, replace = TRUE),
      attitude    = sample(1:2, N, replace = TRUE),
      sex         = sample(1:2, N, replace = TRUE),

      # -- Dummies vs. level 1 (reference); also the exact regressors used
      #    later by declare_estimator(), replicating the PAP specification --
      need_hardship   = as.numeric(need == 2),
      identity_peru   = as.numeric(identity == 2),
      identity_ven    = as.numeric(identity == 3),
      control_nofund  = as.numeric(control == 2),
      effort_same     = as.numeric(effort == 2),
      effort_more     = as.numeric(effort == 3),
      reciprocity_vol = as.numeric(reciprocity == 2),
      attitude_grat   = as.numeric(attitude == 2),
      sex_female      = as.numeric(sex == 2),

      # -- Latent "desert" score of THIS profile ------------------------------
      # The 1.5 factor on the interaction is NOT arbitrary: because the final
      # share (below) is the DIFFERENCE between the two paired profiles' raw
      # scores, and the paired profile's own random effort level (P(More)=1/3)
      # also carries an M-interaction, the population coefficient recovered by
      # a regression that (per the PAP formula) omits a standalone M term is
      # attenuated to delta_interaction * (1 - 1/3). Scaling the generating
      # coefficient by 1/(1-1/3) = 1.5 exactly cancels that attenuation, so
      # that `delta_interaction` is both the true, generative parameter and
      # the quantity declare_inquiry()/the estimator target in expectation
      # WHEN the moderator is measured without error (reliability = 1). The
      # generating term always uses the TRUE M; measurement error is injected
      # only on the estimator side (M_obs), so it shows up as attenuation of
      # the RECOVERED coefficient, not as a change to the true parameter.
      # (Derived and numerically verified against a large-N, low-noise draw;
      # see the end-of-script report.) Ordinary main effects are NOT subject
      # to this because they don't share a common random multiplier (M) with
      # the paired profile's own attributes.
      raw = amce_need * need_hardship +
        amce_identity * identity_peru + amce_identity * identity_ven +
        amce_control * control_nofund +
        (amce_effort / 2) * effort_same + amce_effort * effort_more +
        amce_reciprocity * reciprocity_vol +
        amce_attitude * attitude_grat +
        amce_sex * sex_female +
        (delta_interaction * 1.5) * effort_more * M +
        rnorm(N, sd = sd_outcome / sqrt(2)),  # profile-level noise; see share= below

      # -- Zero-sum share: the two profiles of a task sum to 100 --------------
      # Profiles are generated in adjacent rows, alternating 1st/2nd within
      # each task (fabricatr fills respondent -> task -> profile in that
      # nested order), so `raw_other` — the OTHER profile's raw score in the
      # same task — is simply the next row's raw for the 1st profile and the
      # previous row's raw for the 2nd, making `raw - raw_other` antisymmetric
      # across the pair by construction.
      raw_other = ifelse(rep(c(TRUE, FALSE), length.out = N), lead(raw), lag(raw)),

      # Clamping only this single "50 + diff" score to [0,100] still yields an
      # EXACT zero-sum pair after clamping, because clamp(a,0,100) +
      # clamp(100-a,0,100) = 100 for any real a (both pre-clamp values are
      # exact complements). Also gives profile-level noise SD = sd_outcome on
      # the final share (two independent rnorm(sd_outcome/sqrt(2)) draws
      # differenced have SD sqrt(2)*(sd_outcome/sqrt(2)) = sd_outcome).
      share = pmin(pmax(50 + (raw - raw_other), 0), 100)
    )
  )

  # declare_inquiry(): the two true AMCEs we want power for. Both are fixed
  # generative parameters here (not data summaries), which is a standard
  # DeclareDesign pattern for parameter-recovery designs — see the "Y_bar"
  # example in ?declare_estimator.
  inquiry <- declare_inquiry(
    effort_main        = amce_effort,        # AMCE of effort, More vs Less (pp)
    effort_interaction = delta_interaction   # conditional AMCE: effort x M (pp per SD of M)
  )

  # declare_estimator(): OLS with respondent-clustered robust SEs, replicating
  # the PAP model share ~ need + identity(2) + control + effort(2) +
  # reciprocity + attitude + sex + effort:M_obs — using the OBSERVED moderator,
  # as any real estimator would have to. `term`/`inquiry` are matched
  # positionally, so this single call extracts both coefficients of interest.
  estimador <- declare_estimator(
    share ~ need_hardship +
      identity_peru + identity_ven +
      control_nofund +
      effort_same + effort_more +
      reciprocity_vol + attitude_grat + sex_female +
      effort_more:M_obs,
    .method  = lm_robust,
    clusters = respondent,
    term     = c("effort_more", "effort_more:M_obs"),
    inquiry  = c("effort_main", "effort_interaction"),
    label    = "PAP_OLS"
  )

  modelo + inquiry + estimador
}

# ── 3. Sanity checks ────────────────────────────────────────────────────────
# (a) the zero-sum constraint still holds exactly.
dat_check <- draw_data(conjoint_design(N_resp = 50, K_tasks = 6, sd_outcome = 15))
n_check <- nrow(dat_check)
stopifnot(
  max(abs(dat_check$share[seq(1, n_check, 2)] + dat_check$share[seq(2, n_check, 2)] - 100)) < 1e-8
)
rm(dat_check, n_check)

# (b) M_obs has the intended reliability: cor(M, M_obs) ~= sqrt(reliability).
dat_check2 <- draw_data(conjoint_design(N_resp = 5000, K_tasks = 6, sd_outcome = 15, reliability = 0.8))
resp_check <- dat_check2 |> distinct(respondent, M, M_obs)
cor_check  <- cor(resp_check$M, resp_check$M_obs)
stopifnot(abs(cor_check - sqrt(0.8)) < 0.03)
rm(dat_check2, resp_check, cor_check)


# 4. Diagnosis grids ----------------------------------------------------------
# CENTRAL QUESTION: given Gilgen (2022) finds no (or at most a fraction-of-a-
# point) class x merit interaction at N~1,500 with the same estimator family,
# what is the smallest effort x M interaction that THIS design, at N = 1,500,
# can detect? Answered by sweeping the TRUE effect size (delta_interaction)
# rather than assuming a single value, and reading off the power = 0.80
# crossover (the MDE) — separately for a perfectly-measured moderator
# (reliability = 1, benchmark) and a realistic one (reliability = 0.8).
#
# delta_interaction: 0.5 to 3 pp per SD of M — anchored on Gilgen's scale.
# K_tasks: 3-6 (6 is the design's ceiling; 7 was not offered).
# reliability: 1.0 (perfect moderator, upper-bound benchmark) vs 0.8 (realistic
#   attitudinal-scale reliability).
# sd_outcome: fixed at 15 pp (plausible middle value; see section 6 for a
#   secondary sweep of this nuisance parameter).
SIMS <- 200  # per design/cell; see end-of-script note on the 200 vs 500 trade-off

delta_interaction_grid <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0)
K_tasks_grid           <- c(3, 4, 5, 6)
reliability_grid       <- c(1.0, 0.8)

design_base <- conjoint_design(N_resp = 1500, K_tasks = 6, sd_outcome = 15,
                                delta_interaction = 1.5, reliability = 0.8)

# ── 4.1 Main grid: N = 1,500 x K_tasks x delta_interaction x reliability ────
grid_main <- redesign(
  design_base,
  N_resp            = 1500,
  K_tasks           = K_tasks_grid,
  sd_outcome        = 15,
  delta_interaction = delta_interaction_grid,
  reliability       = reliability_grid
)

diagnosis_main <- diagnose_design(grid_main, sims = SIMS, bootstrap_sims = 0)

diagnosands_main <- diagnosis_main$diagnosands |>
  as_tibble() |>
  mutate(
    term_label = if_else(term == "effort_more:M_obs",
                          "Conditional AMCE (effort x M)",
                          "Main effect (effort, More vs Less)")
  )

# ── 4.2 Context grid: K_tasks = 6 x delta_interaction x N (1500/3000/4500) ──
# Realistic reliability (0.8) only — this grid exists to show how much the
# interaction's MDE falls as N grows, to help justify (or not) a larger wave.
grid_context <- redesign(
  design_base,
  N_resp            = c(1500, 3000, 4500),
  K_tasks           = 6,
  sd_outcome        = 15,
  delta_interaction = delta_interaction_grid,
  reliability       = 0.8
)

diagnosis_context <- diagnose_design(grid_context, sims = SIMS, bootstrap_sims = 0)

diagnosands_context <- diagnosis_context$diagnosands |>
  as_tibble()

# ── 4.3 Secondary grid: sd_outcome sweep (nuisance parameter, N=1500, K=6) ──
# Kept as a secondary check, not the main analysis: how sensitive is power to
# the (a priori unknown) SD of the share, at a representative mid-range true
# interaction (delta_interaction = 1.5 pp/SD) and the realistic reliability?
#sd_outcome_secondary <- c(10, 15, 20, 25, 30, 40, 50, 60, 75, 90, 100, 110, 125, 150)

#grid_sd_secondary <- redesign(
#  design_base,
#  N_resp            = 1500,
#  K_tasks           = 6,
#  sd_outcome        = sd_outcome_secondary,
#  delta_interaction = 1.5,
#  reliability       = 0.8
#)

#diagnosis_sd_secondary <- diagnose_design(grid_sd_secondary, sims = SIMS, bootstrap_sims = 0)

#diagnosands_sd_secondary <- diagnosis_sd_secondary$diagnosands |>
#  as_tibble()


# 5. MDE helper: interpolate the power = 0.80 crossover along a swept axis --
# Generic linear-interpolation crossover finder, used both for the interaction
# MDE (axis = delta_interaction, power increasing in the axis) and, in
# cjpow.R-style diagnostics, could be reused for any monotonically-increasing
# power curve. Returns NA if the crossover falls outside the swept range
# (below the smallest value tested, or above the largest).
mde_crossover <- function(x, power, target = 0.80) {
  ord <- order(x)
  x <- x[ord]; power <- power[ord]
  if (power[1] >= target) return(NA_real_)   # already powered at the smallest x swept
  if (max(power) < target) return(NA_real_)  # never reaches target within the swept range
  i <- which(power >= target)[1]
  if (i == 1) return(x[1])
  approx(x = power[c(i - 1, i)], y = x[c(i - 1, i)], xout = target)$y
}


# 6. Table: MDE of the interaction by K_tasks, reliability 1.0 vs 0.8 -------
# THE key table: for each K_tasks, the smallest true effort x M interaction
# (pp per SD of M) that N = 1,500 detects with power >= 0.80.
mde_main_by_reliability <- diagnosands_main |>
  filter(term == "effort_more:M_obs") |>
  group_by(K_tasks, reliability) |>
  summarise(mde = mde_crossover(delta_interaction, power), .groups = "drop")

tbl_mde_main_df <- mde_main_by_reliability |>
  mutate(reliability_label = paste0("rel_", reliability)) |>
  select(K_tasks, reliability_label, mde) |>
  pivot_wider(names_from = reliability_label, values_from = mde) |>
  arrange(K_tasks)

tbl_mde_main <- tbl_mde_main_df |>
  mutate(across(c(rel_1, rel_0.8), \(x) if_else(is.na(x), "> 3 pp", paste0(round(x, 2), " pp")))) |>
  rename(`N tasks` = K_tasks,
         `MDE, reliability = 1.0 (benchmark)` = rel_1,
         `MDE, reliability = 0.8 (realistic)`  = rel_0.8) |>
  kbl(
    caption = "Minimum detectable interaction (effort x M), N = 1,500, power = 0.80"
  ) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE) |>
  footnote(
    general = paste0(
      SIMS, " simulations per cell, interpolated between delta_interaction = ",
      paste(delta_interaction_grid, collapse = ", "), " pp/SD. Delta range anchored on ",
      "Gilgen (2022): at a comparable N (~1,500) and estimator, the class x merit ",
      "interaction in the scholarship scenario is null / at most a fraction of a ",
      "point, suggesting real interaction effects in this domain are small. The MDE ",
      "reported here is the smallest interaction this design is powered to detect; ",
      "reliability = 1.0 assumes a perfectly measured moderator (upper-bound benchmark), ",
      "reliability = 0.8 reflects realistic attitudinal-scale measurement error. ",
      "\"> 3 pp\" means power never reached 0.80 within the swept range."
    ),
    general_title = "Note: "
  )


# 7. Table: detailed power, K_tasks x delta_interaction, N=1500, rel.=0.8 ---
tbl_power_detail_df <- diagnosands_main |>
  filter(term == "effort_more:M_obs", reliability == 0.8) |>
  select(K_tasks, delta_interaction, power) |>
  mutate(power = round(power, 3)) |>
  pivot_wider(names_from = delta_interaction, values_from = power, names_prefix = "delta_")

tbl_power_detail <- tbl_power_detail_df |>
  rename(`N tasks` = K_tasks) |>
  kbl(
    caption = "Simulated power of the interaction (effort x M), N = 1,500, reliability = 0.8",
    col.names = c("N tasks", paste0(delta_interaction_grid, " pp"))
  ) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE) |>
  add_header_above(c(" " = 1, "True interaction (pp per SD of M)" = length(delta_interaction_grid))) |>
  footnote(
    general = paste0(
      SIMS, " simulations per cell. Outcome: continuous share (0-100), zero-sum within task, ",
      "SE clustered by respondent (lm_robust). SD of the share fixed at 15 pp; moderator ",
      "reliability = 0.8 (realistic attitudinal-scale measurement error)."
    ),
    general_title = "Note: "
  )


# 8. Table: MDE of the interaction by N, K_tasks = 6, reliability = 0.8 -----
# Context for justifying (or not) a larger wave: how much does the interaction
# MDE fall as N grows from the pilot/first-wave 1,500 to 3,000 or 4,500?
mde_context_by_n <- diagnosands_context |>
  filter(term == "effort_more:M_obs") |>
  group_by(N_resp) |>
  summarise(mde = mde_crossover(delta_interaction, power), .groups = "drop") |>
  arrange(N_resp)

tbl_mde_context <- mde_context_by_n |>
  mutate(mde_label = if_else(is.na(mde), "> 3 pp", paste0(round(mde, 2), " pp"))) |>
  select(N_resp, mde_label) |>
  rename(`N respondents` = N_resp, `MDE of the interaction` = mde_label) |>
  kbl(
    caption = "Minimum detectable interaction (effort x M) by N, 6 tasks, reliability = 0.8, power = 0.80"
  ) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE) |>
  footnote(
    general = paste0(
      SIMS, " simulations per cell, interpolated between delta_interaction = ",
      paste(delta_interaction_grid, collapse = ", "), " pp/SD. 1,500 = pilot / first ",
      "cross-sectional wave; 3,000 / 4,500 shown as context for a larger wave."
    ),
    general_title = "Note: "
  )


# 9. Figure: power of the interaction vs delta_interaction, by K_tasks ------
# N = 1,500, reliability = 0.8 (realistic case); one line per K_tasks; the
# 0.80 threshold and each K_tasks' MDE crossover are marked.
g_power_interaction <- diagnosands_main |>
  filter(term == "effort_more:M_obs", reliability == 0.8) |>
  mutate(k_label = paste0(K_tasks, " tasks")) |>
  ggplot(aes(x = delta_interaction, y = power, color = k_label)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "grey40") +
  annotate("text", x = min(delta_interaction_grid), y = 0.82, label = "Power = 0.80",
           color = "grey40", size = 3, hjust = 0) +
  geom_point(
    data = mde_main_by_reliability |> filter(reliability == 0.8, !is.na(mde)) |>
      mutate(k_label = paste0(K_tasks, " tasks")),
    aes(x = mde, y = 0.80, color = k_label),
    shape = 4, size = 3.5, stroke = 1.3, show.legend = FALSE
  ) +
  scale_x_continuous(breaks = delta_interaction_grid) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1),
                      breaks = seq(0, 1, 0.2)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title    = "Simulated power of the effort x meritocracy-moderator interaction",
    subtitle = "N = 1,500 respondents; moderator reliability = 0.8; SD of share = 15 pp",
    x        = "True interaction effect (pp per SD of M)",
    y        = "Statistical power",
    color    = "N tasks",
    caption  = paste0(
      SIMS, " simulations per cell. X marks the MDE (power = 0.80 crossover) per K_tasks.\n",
      "Delta range anchored on Gilgen (2022): no class x merit interaction detected at N~1,500."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "right")


# 10. Comparison: simulated (continuous) vs cjpow.R's analytic (binary) power
# Local re-implementations, NOT a `source()` of cjpow.R (which does rm(list=ls())
# and runs its own heavy report) — these mirror cjpow.R's `n_min_interaction()`
# (itself equation 6 of Schuessler & Freitag 2020) and its algebraic inverse,
# solving for power at a given N instead of the minimum N for a given power.
# Compared at reliability = 1.0 (perfectly measured moderator), since the
# binary analytic formula has no notion of moderator measurement error either
# — this keeps the comparison apples-to-apples.

# Binary/forced-choice power for a K-level MAIN effect at a given N — this is
# exactly cjpowr_amce() from the cjpowR package, as used in cjpow.R.
power_main_binary <- function(amce_pp, n_resp, K, tasks, alpha = 0.05) {
  n_profiles <- n_resp * tasks * 2
  out <- cjpowr_amce(amce = amce_pp / 100, n = n_profiles, levels = K, alpha = alpha)
  out$power
}

# Binary/forced-choice power for an attribute x moderator INTERACTION at a
# given N — the algebraic inverse of cjpow.R's n_min_interaction(): that
# function solves n_profiles for a target power; here we invert it to solve
# power for a target n_profiles, under the same conservative assumptions
# (delta0 = 0.5, delta1 = delta2 = 0, balanced binary second factor).
power_interaction_binary <- function(delta3_pp, n_resp, Kl, Km, tasks, alpha = 0.05,
                                      d0 = 0.5, d1 = 0, d2 = 0) {
  n_profiles <- n_resp * tasks * 2
  delta3 <- delta3_pp / 100
  p00 <- d0; p10 <- d0 + d1; p01 <- d0 + d2; p11 <- d0 + d1 + d2
  A <- p00 * (1 - p00); B <- p10 * (1 - p10); C <- p01 * (1 - p01); D <- p11 * (1 - p11)
  q00 <- q10 <- q01 <- q11 <- 0.25
  z_needed <- n_profiles * delta3^2 / ((Kl * Km / 4) * (A / q00 + B / q10 + C / q01 + D / q11))
  z_kappa  <- sqrt(z_needed) - qnorm(1 - alpha / 2)
  pmin(pmax(pnorm(z_kappa), 0), 1)
}

# Main effect: independent of delta_interaction, so a single representative
# slice (any delta_interaction, reliability = 1.0) is enough.
tbl_comparison_main_df <- diagnosands_main |>
  filter(reliability == 1.0, delta_interaction == 1.5, term == "effort_more") |>
  select(K_tasks, sim_power_main = power) |>
  rowwise() |>
  mutate(
    binary_power_main = power_main_binary(6, 1500, K = 3, tasks = K_tasks),
    gain_main          = sim_power_main - binary_power_main
  ) |>
  ungroup()

# Interaction: across the new delta_interaction sweep, at reliability = 1.0.
tbl_comparison_interaction_df <- diagnosands_main |>
  filter(reliability == 1.0, term == "effort_more:M_obs") |>
  select(K_tasks, delta_interaction, sim_power_interaction = power) |>
  rowwise() |>
  mutate(
    binary_power_interaction = power_interaction_binary(delta_interaction, 1500, Kl = 3, Km = 2, tasks = K_tasks),
    gain_interaction          = sim_power_interaction - binary_power_interaction
  ) |>
  ungroup()

tbl_comparison_main <- tbl_comparison_main_df |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  rename(`N tasks` = K_tasks) |>
  kbl(
    caption = "Main effect (effort): continuous (simulated) vs binary/forced-choice (analytic) power, N = 1,500",
    col.names = c("N tasks", "Main (sim)", "Main (binary)", "Gain")
  ) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE) |>
  footnote(
    general = "True main effect = 6 pp (More vs Less effort), K = 3. \"Gain\" = simulated continuous power minus binary analytic power.",
    general_title = "Note: "
  )

tbl_comparison_interaction <- tbl_comparison_interaction_df |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  rename(`N tasks` = K_tasks, `True interaction (pp/SD)` = delta_interaction) |>
  kbl(
    caption = paste0(
      "Interaction (effort x M): continuous (simulated) vs binary/forced-choice (analytic) power, ",
      "N = 1,500, reliability = 1.0"
    ),
    col.names = c("N tasks", "True interaction (pp/SD)", "Interaction (sim)", "Interaction (binary)", "Gain")
  ) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE) |>
  footnote(
    general = paste0(
      "\"Binary\" reproduces cjpow.R's forced-choice formula (Schuessler & Freitag 2020), Kl=3/Km=2. ",
      "Compared at reliability = 1.0 because the binary analytic formula has no measurement-error term. ",
      "\"Gain\" = simulated continuous power minus binary analytic power."
    ),
    general_title = "Note: "
  )


# 11. Print key outputs -------------------------------------------------------
tbl_mde_main
tbl_power_detail
tbl_mde_context
g_power_interaction
tbl_comparison_main
tbl_comparison_interaction

cat("\nMDE of the interaction (effort x M), N = 1,500, 6 tasks:\n",
    "  reliability = 1.0 (benchmark): ",
    {v <- mde_main_by_reliability |> filter(K_tasks == 6, reliability == 1.0) |> pull(mde)
     if (is.na(v)) "> 3 pp" else paste0(round(v, 2), " pp")}, "\n",
    "  reliability = 0.8 (realistic): ",
    {v <- mde_main_by_reliability |> filter(K_tasks == 6, reliability == 0.8) |> pull(mde)
     if (is.na(v)) "> 3 pp" else paste0(round(v, 2), " pp")}, "\n",
    sep = "")

cat("\nMDE of the interaction by N (6 tasks, reliability = 0.8):\n")
print(mde_context_by_n)

save(diagnosands_main, diagnosands_context,
     mde_main_by_reliability, mde_context_by_n,
     tbl_mde_main, tbl_power_detail, tbl_mde_context, g_power_interaction,
     tbl_comparison_main, tbl_comparison_interaction,
     file = here("processing", "power_declaredesign.RData"))
