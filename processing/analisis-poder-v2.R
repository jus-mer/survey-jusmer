#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: cjpowR for conjoint survey experiment about deservingness and educational inequality
# Responsible: Andreas Laffert
# Executive Summary: This script contains the code to perform cjpowR
# Date: May 12, 2026
#******************************************************************************************************************************************************
#
# HOW TO READ THESE NUMBERS ------------------------------------------------
# Every N and MDE below is a CONSERVATIVE UPPER BOUND, not a point estimate of
# what this design can detect. Three assumptions drive that, and each one is
# violated in a direction that makes the true requirement SMALLER (1) or
# LARGER (2, 3) than what is printed:
#
# 1. BINARY OUTCOME (violated; makes these numbers pessimistic).
#    cjpowR and Schuessler & Freitag assume a forced-choice binary outcome, so
#    the variance term is Bernoulli: (d0+d1)(1-(d0+d1)). This study's outcome
#    is a continuous fixed-sum share (0-100), not a choice. A binary outcome
#    carries the MAXIMUM variance a [0,100] variable with mean 50 can have
#    (SD = 50 pp); any realistic spread of allocated shares is lower, and lower
#    outcome variance means more power for the same N. Simulation of the real
#    design confirms the two agree only at SD = 50 pp and that power is far
#    higher at realistic dispersion — see processing/power_declaredesign.R and
#    the "Power analysis" section of documentation/03-conjoint-desing.qmd.
#    Treat this script as the upper bound and the simulation as the lower one.
#
# 2. BALANCED BINARY MODERATOR (assumed; inflates N if false).
#    The interaction rows use p00 = p10 = p01 = p11 = 0.25, which holds only
#    for a moderator split 50/50. Schuessler & Freitag are explicit that for
#    conditional AMCEs the covariate's own marginal distribution must be used.
#    Sensitivity at Kl=3, Km=2, 3 pp, 6 tasks: 50/50 -> 4,361 respondents;
#    30/70 -> 5,192 (+19%); 20/80 -> 6,814 (+56%). Pass p00/p10/p01/p11
#    explicitly to n_min_interaction() once the real split is known.
#
# 3. DICHOTOMIZED MODERATOR (a different estimand than the analysis plan).
#    Km = 2 treats the moderator as binary, but the confirmatory hypotheses
#    (H8-H10) interact the attribute with a STANDARDIZED CONTINUOUS moderator
#    (delta = change in the AMCE per 1 SD). Dichotomizing discards information,
#    so this is a conservative stand-in, not the model that will be fitted.
#
# Formulae verified against equations (4) and (6) of Schuessler & Freitag
# (2020) and against cjpowR itself: agreement is within 0.25%, always on the
# conservative side.
#
# WHICH EFFECT SIZE, AND WHY 3 pp ------------------------------------------
# "pp" = percentage points of the ALLOCATED SHARE (the outcome is a split of a
# fixed CLP 2,000,000 fund, so a 3 pp AMCE moves an applicant from 50% to 53%
# of the fund, about CLP 60,000). This is NOT the binary "change in choice
# probability" that the published-AMCE benchmarks below are measured on.
#
# This script uses 3 pp everywhere. That is not a generic placeholder: it is
# the BINDING attribute of the design. Schuessler & Freitag advise powering for
# the attribute with the MOST LEVELS and the SMALLEST expected effect. Of the
# two K=3 attributes, the assumed effects (see processing/power_declaredesign.R,
# section 2) are Effort = 6 pp and Identity = 3 pp, so:
#
#   3 pp + K=3  ->  IDENTITY  (worst case; what this script sizes for)
#   6 pp + K=3  ->  EFFORT    (what power_declaredesign.R simulates, because
#                              Effort is the attribute the moderation
#                              hypotheses H8-H10 interact with the moderator)
#
# The two scripts therefore answer different questions and their tables are NOT
# directly comparable: this one asks "what N does the most demanding attribute
# require?", the simulation asks "what power do we have for the hypothesis we
# will actually test?". Neither number is wrong; do not reconcile them by
# changing one to match the other.
#
# On choosing the target at all, the paper (section 4) surveys 258 AMCEs from
# 15 highly cited conjoints: median ~5 pp, 25% below 2 pp, 75% below 8.7 pp —
# but it explicitly advises AGAINST sizing from that distribution (different
# research questions are not comparable, and publication bias inflates it) and
# recommends subject-specific priors instead. This study follows that advice:
# the effect vector is anchored on Gilgen (2022), a comparable scholarship
# allocation experiment, not on the meta-median. Note also that the paper warns
# Cohen's d is misleading here — a "small" d = 0.2 is a 10 pp AMCE, which fewer
# than a quarter of published estimates exceed.
#******************************************************************************************************************************************************

# PROCESSING ONLY — this script computes and returns objects, it prints nothing.
# Tables and figures are rendered in documentation/03b-power-analysis.qmd, which
# source()s this file. Do NOT add rm(list = ls()) here: it would wipe the
# calling environment when the document sources this script.

options(scipen=999)

# 1. Packages  -----------------------------------------------------
#if (!require(devtools)) install.packages("devtools")
#devtools::install_github("m-freitag/cjpowR")

library(cjpowR)
library(tidyverse)

set.seed(123)

# ── 1. Point calculation ───────────────────────────────────────────
# For a specific AMCE and a given N (main study N=1,500, 6 tasks, 2 profiles)
n_study          <- 1500
n_efectivo_study <- n_study * 6 * 2  # = 18000 total profiles

# K=2 attributes (Need, Control, Reciprocity, Attitude, Sex).
# 3 pp is below every assumed K=2 effect (Need 13 pp, the rest 3.5 pp), so this
# row is slack — the K=3 row below is what binds.
point_k2 <- cjpowr_amce(amce = 0.03, n = n_efectivo_study, levels = 2, alpha = 0.05)

# K=3 attributes (Effort, Identity). 3 pp = IDENTITY, the smallest assumed
# effect among the attributes with the most levels: the binding case.
# (Effort, the other K=3 attribute, is assumed at 6 pp and so is easier.)
point_k3 <- cjpowr_amce(amce = 0.03, n = n_efectivo_study, levels = 3, alpha = 0.05)

# Minimum N for a target power, again at the binding case (Identity, K=3, 3 pp)
point_nmin_k3 <- cjpowr_amce(amce = 0.03, power = 0.80, levels = 3, alpha = 0.05)

# Assembled for the document: power at the study N for both attribute families.
tbl_point_df <- tibble(
  `Attribute family`  = c("K = 2 (Need, Control, Reciprocity, Attitude, Sex)",
                          "K = 3 (Effort, Identity)"),
  `Levels`            = c(2, 3),
  `Profiles`          = n_efectivo_study,
  `Power at 3 pp`     = c(point_k2$power, point_k3$power),
  `Type M`            = c(point_k2$exp_typeM, point_k3$exp_typeM),
  `Type S`            = c(point_k2$type_s, point_k3$type_s)
)

# ── 2. Power curves vs effective N ─────────────────────────────────
# Grid: different AMCEs × effective N (range covers the study N = 1,500)
d <- expand.grid(
  amce      = c(0.02, 0.03, 0.05, 0.07),
  n_resp    = seq(200, 5000, by = 100),   # whole respondents
  levels    = c(2, 3)
) |>
  mutate(n_efectivo = n_resp * 6 * 2)

null_na <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else x

res <- mapply(
  function(amce, n, levels) {
    out <- cjpowr_amce(amce = amce, n = n, levels = levels, alpha = 0.05)
    data.frame(
      amce   = amce,
      n      = n,
      levels = levels,
      power  = null_na(out$power),
      typeM  = null_na(out$exp_typeM),
      typeS  = null_na(out$type_s)   # cjpowr_amce() returns `type_s`, not `type_s_error`
    )
  },
  d$amce, d$n_efectivo, d$levels,
  SIMPLIFY = FALSE
) |> bind_rows()

# Add N in respondents (6 tasks × 2 profiles)
res <- res |> mutate(
  n_resp = n / 12,
  amce_label = paste0("AMCE = ", amce * 100, " pp"),
  k_label = paste0("K = ", levels, " levels")
)

# ── 3. Figure 1: Power curves — 6 vs 4 tasks ────────────────────────
library(ggdist)

# Grid: effect size × N respondents × levels × tasks (500–5,000 respondents)
d_power_tasks <- expand.grid(
  amce   = c(0.02, 0.03, 0.05),
  n_resp = seq(500, 5000, by = 100),
  levels = c(2, 3),
  tasks  = c(6, 5, 4, 3)
)

res_power_tasks <- mapply(
  function(amce, n_resp, levels, tasks) {
    out <- cjpowr_amce(amce = amce, n = n_resp * tasks * 2, levels = levels, alpha = 0.05)
    data.frame(
      amce   = amce,
      n_resp = n_resp,
      levels = levels,
      tasks  = tasks,
      power  = null_na(out$power)
    )
  },
  d_power_tasks$amce, d_power_tasks$n_resp, d_power_tasks$levels, d_power_tasks$tasks,
  SIMPLIFY = FALSE
) |> bind_rows() |>
  mutate(
    amce_label  = paste0("AMCE = ", amce * 100, " pp"),
    k_label     = paste0("K = ", levels, " levels"),
    tasks_label = paste0(tasks, " tasks per respondent")
  )

g1 <- res_power_tasks |>
  ggplot(aes(x = n_resp, y = power,
             color = amce_label, linetype = k_label)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0.90, linetype = "dotted", color = "grey40") +
  # Mark the study's target N
  geom_vline(xintercept = n_study, color = "#2c3e50", linetype = "dashed", alpha = 0.6) +
  # The study-N label goes in ONE panel only. Repeating it in all four added no
  # information and collided with the curves; a single short, one-line label
  # cannot clip against the panel edge the way the old two-line version did.
  geom_text(
    data = tibble(tasks_label = "6 tasks per respondent"),
    aes(x = n_study, y = 0.06,
        label = paste0("N = ", format(n_study, big.mark = ","))),
    inherit.aes = FALSE, color = "#2c3e50", size = 2.9, hjust = -0.12
  ) +
  facet_wrap(~ tasks_label) +
  # Breaks every 20% / every 1,000 respondents. The previous 10% and 500-unit
  # breaks produced ~10 x-labels per panel, which ran together into an
  # unreadable strip once the plot was split into four facets.
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, 0.2),
                     limits = c(0, 1)) +
  scale_x_continuous(labels = scales::label_number(scale = 1e-3, suffix = "k",
                                                   accuracy = 1),
                     breaks = seq(1000, 5000, 1000),
                     expand = expansion(mult = 0.02)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title    = "Statistical power by N and number of tasks",
    subtitle = "Distributive conjoint on deservingness (α = 0.05); panels compare 3 to 6 tasks per respondent",
    x        = "N respondents (thousands)",
    y        = "Statistical power",
    color    = "Effect size",
    linetype = "Attribute levels",
    caption  = paste0("Grey lines: power thresholds 0.80 and 0.90. ",
                      "Vertical line: study N = ", format(n_study, big.mark = ","), ".")
  ) +
  ggdist::theme_ggdist() +
  # Legend along the bottom returns its width to the panels, which is what the
  # x-axis needs most in a 2x2 layout.
  theme(
    legend.position  = "bottom",
    legend.box       = "horizontal",
    panel.spacing    = unit(1.1, "lines"),
    strip.text       = element_text(size = 10),
    axis.text        = element_text(size = 9)
  ) +
  guides(color    = guide_legend(order = 1, nrow = 1),
         linetype = guide_legend(order = 2, nrow = 1))


# ── 4. Figure 2: MDE vs N respondents ──────────────────────────────
mde_df <- expand.grid(
  n_resp = seq(300, 5000, by = 50),
  levels = c(2, 3)
) |>
  mutate(
    n_ef  = n_resp * 12,
    mde   = sqrt((levels / 2) * (qnorm(0.975) + qnorm(0.80))^2 / n_ef),
    k_label = paste0("K = ", levels, " levels")
  )

g2 <- mde_df |>
  ggplot(aes(x = n_resp, y = mde * 100, color = k_label)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 5, linetype = "dotted", color = "grey40") +
  geom_vline(xintercept = n_study, color = "#2c3e50", linetype = "dashed", alpha = 0.6) +
  annotate("text", x = n_study + 30, y = 9.5,
           label = paste0("Main study (N=", format(n_study, big.mark = ","), ")"),
           color = "#2c3e50", size = 3, hjust = 0) +
  annotate("text", x = 4900, y = 3.3, label = "3 pp", size = 3, color = "grey40") +
  annotate("text", x = 4900, y = 5.3, label = "5 pp", size = 3, color = "grey40") +
  scale_color_manual(values = c("#2c3e50", "#e74c3c")) +
  scale_x_continuous(labels = scales::comma_format()) +
  labs(
    title    = "Minimum detectable effect (MDE) by N",
    subtitle = "Power = 0.80, α = 0.05, tasks = 6, 2 profiles per task",
    x        = "N respondents",
    y        = "MDE (percentage points)",
    color    = "Attribute levels",
    caption  = "Grey lines: MDE references at 3 pp and 5 pp."
  ) +
  ggdist::theme_ggdist()+
  theme(legend.position = "right")


# ── 5. Figure 3: Type M error (exaggeration ratio) ─────────────────
g3 <- res |>
  filter(levels == 3) |>
  ggplot(aes(x = n_resp, y = typeM, color = amce_label)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = n_study, color = "#2c3e50", linetype = "dashed", alpha = 0.6) +
  coord_cartesian(ylim = c(1, 6)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title    = "Type M error (exaggeration ratio)",
    subtitle = "K=3 attributes — how much the effect is overestimated if N is insufficient",
    x        = "N respondents",
    y        = "Exaggeration ratio",
    color    = "True AMCE",
    caption  = "Ratio = 1 indicates no magnitude bias."
  ) +
  ggdist::theme_ggdist()+
  theme(legend.position = "right")


# ── 6. Helper functions: MDE and minimum N ─────────────────────────
library(kableExtra)

mde_at <- function(n_resp, levels, tasks = 6, power = 0.80, alpha = 0.05) {
  n_prof <- n_resp * tasks * 2
  sqrt((levels / 2) * (qnorm(1 - alpha / 2) + qnorm(power))^2 / n_prof) * 100
}

# Inverse of mde_at(): the minimum N of respondents required to detect an
# effect of size `delta` at the given power. With the study N fixed at 1,500,
# mde_at() answers "what can this N detect?" and n_min() answers "what N would
# each target require?" — both are used below (sections 7 and 8).
n_min <- function(levels, delta, tasks = 6, power = 0.80, alpha = 0.05) {
  z <- (qnorm(1 - alpha / 2) + qnorm(power))^2
  n_prof <- (levels / 2) * z / delta^2
  ceiling(n_prof / (tasks * 2))
}


# ── 7. MDE comparison: 6 vs 4 tasks (N = n_study) ──────────────────

# Point comparison: MDE at the study N for 6 vs 4 tasks, K=2 vs K=3 levels
mde_tasks_df <- expand.grid(
  tasks  = c(6, 4),
  levels = c(2, 3),
  n_resp = n_study
) |>
  mutate(
    n_profiles = n_resp * tasks * 2,
    mde        = mde_at(n_resp, levels, tasks = tasks)
  )

# Table: MDE for the main study — 6 vs 4 tasks, N = n_study fixed
tbl_power_main_2 <- mde_tasks_df |>
  mutate(mde_label = paste0(round(mde, 1), " pp")) |>
  select(tasks, n_profiles, levels, mde_label) |>
  pivot_wider(names_from = levels, values_from = mde_label, names_prefix = "mde_k") |>
  arrange(desc(tasks)) |>
  rename(
    `N tasks`            = tasks,
    `N profiles`         = n_profiles,
    `MDE, K=2 attribute` = mde_k2,
    `MDE, K=3 attribute` = mde_k3
  ) |>
  kbl(
    format.args = list(big.mark = ",")
  ) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)

# Figure: MDE vs N respondents, 6 vs 4 tasks
mde_tasks_curve_df <- expand.grid(
  n_resp = seq(500, 5000, by = 50),
  tasks  = c(6, 4),
  levels = c(2, 3)
) |>
  mutate(
    mde         = mde_at(n_resp, levels, tasks = tasks),
    tasks_label = paste0(tasks, " tasks"),
    k_label     = paste0("K = ", levels, " levels")
  )

g_tasks <- mde_tasks_curve_df |>
  ggplot(aes(x = n_resp, y = mde, color = tasks_label, linetype = k_label)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 2, linetype = "dotted", color = "grey40") +
  geom_vline(xintercept = n_study, color = "#2c3e50", linetype = "dashed", alpha = 0.6) +
  annotate("text", x = n_study + 30, y = max(mde_tasks_curve_df$mde) * 0.95,
           label = paste0("Main study (N=", format(n_study, big.mark = ","), ")"),
           color = "#2c3e50", size = 3, hjust = 0) +
  annotate("text", x = 4950, y = 3.3, label = "3 pp", size = 3, color = "grey40") +
  annotate("text", x = 4950, y = 2.3, label = "2 pp", size = 3, color = "grey40") +
  scale_color_manual(values = c("#2c3e50", "#e74c3c")) +
  scale_x_continuous(labels = scales::comma_format(), breaks = seq(500, 5000, 500)) +
  labs(
    title    = "Minimum detectable effect: 6 vs 4 tasks",
    subtitle = "Power = 0.80, α = 0.05, 2 profiles per task",
    x        = "N respondents",
    y        = "MDE (percentage points)",
    color    = "N tasks",
    linetype = "Attribute levels",
    caption  = "Grey lines: MDE references at 3 pp and 2 pp."
  ) +
  ggdist::theme_ggdist() +
  theme(legend.position = "right")

# ── Table 3.2: Minimum N for the main study, 6 vs 4 tasks ───────────
# Reutiliza n_min() y mde_at() ya definidas arriba.

make_main_row <- function(target, delta_pp, K, power = 0.80) {
  tibble(
    Target                    = target,
    `Minimum AMCE`            = paste0(delta_pp, " pp"),
    `Most demanding attribute`= if (K == 2) "Binary" else "Effort / Identity",
    `Min N (6 tasks)`         = n_min(K, delta_pp/100, tasks = 6, power = power),
    `Min N (4 tasks)`         = n_min(K, delta_pp/100, tasks = 4, power = power)
  )
}

tbl_power_main_df <- bind_rows(
  make_main_row("Main effects, K=2",              3, 2, 0.80),
  make_main_row("Main effects, K=3",              3, 3, 0.80),
  make_main_row("Main effects, K=3, power = 0.90",3, 3, 0.90)
)
# NOTE: a previous "Heterogeneity (binary moderator)" row using the shortcut
# "2 * n_min(...)" was removed here. That fixed factor of 2 is INCORRECT —
# see section 8 below (n_min_interaction) for the correct inflation factor,
# which comes from Kl*Km/4 (equation 6, Schuessler & Freitag), not a constant.

tbl_power_main <- tbl_power_main_df |>
  kbl(
    format.args = list(big.mark = ",")
  ) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)


# ── 8. Minimum N by number of tasks: main effects vs interactions ──────────
# CENTRAL QUESTION: what is the minimum N of respondents for a pilot or first
# cross-sectional survey, as a function of the number of tasks per respondent
# (3, 4, 5, 6), for both main effects and interactions?

# n_min_interaction() implements equation (6) in Schuessler & Freitag: the
# minimum N for an interaction coefficient delta3. The SAME formula applies to
# attribute x attribute interactions and to attribute x level-2-moderator
# interactions (conditional AMCEs) — Kl and Km are just the number of levels
# of the two interacting elements.
#   n_profiles = (Kl*Km/4) * (z_{1-alpha/2} + z_kappa)^2 / delta3^2 *
#                (A/p00 + B/p10 + C/p01 + D/p11)
# with the RESPONSE-probability terms defined cumulatively (eq. 6):
#   A = d0(1-d0)
#   B = (d0+d1)(1-(d0+d1))
#   C = (d0+d1+d2)(1-(d0+d1+d2))
#   D = (d0+d1+d2+delta3)(1-(d0+d1+d2+delta3))
# and p00, p10, p01, p11 the JOINT TREATMENT probabilities of the two
# interacting elements (they must sum to 1).
#
# NOTE ON NOTATION: an earlier version of this function reused the names
# p00/p10/p01/p11 for the cumulative response probabilities and called the
# treatment probabilities q00/... — i.e. exactly inverted with respect to the
# paper. That inversion hid two departures from eq. (6): C omitted d1, and D
# omitted delta3. At the default d1 = d2 = 0 the error was ~0.1% (harmless),
# but it reached ~18% for non-default d1/d2. Both are corrected here and the
# paper's notation is restored.
#
# Under the conservative assumption d0 = 0.5, d1 = d2 = 0 and uniform
# randomization with a balanced binary second factor (p00 = p10 = p01 = p11 =
# 0.25), the parenthetical term is ~4, so n_profiles reduces to approximately
# Kl * Km * (z_{1-alpha/2} + z_kappa)^2 / delta3^2.
# Respondents = ceiling(n_profiles / (tasks * 2)).
#
# IMPORTANT: the minimum N always INCREASES for interactions relative to main
# effects, for BOTH interaction types — there is no shortcut fixed factor
# (the old "2 * n_min(...)" row removed from tbl_power_main above was wrong).
n_min_interaction <- function(Kl, Km, delta3, tasks, power = 0.80, alpha = 0.05,
                               d0 = 0.5, d1 = 0, d2 = 0,
                               p00 = 0.25, p10 = 0.25, p01 = 0.25, p11 = 0.25) {
  stopifnot(isTRUE(all.equal(p00 + p10 + p01 + p11, 1)))
  z <- (qnorm(1 - alpha / 2) + qnorm(power))^2
  # Cumulative response probabilities, exactly as in eq. (6).
  A <- d0 * (1 - d0)
  B <- (d0 + d1) * (1 - (d0 + d1))
  C <- (d0 + d1 + d2) * (1 - (d0 + d1 + d2))
  D <- (d0 + d1 + d2 + delta3) * (1 - (d0 + d1 + d2 + delta3))
  n_profiles <- (Kl * Km / 4) * z / delta3^2 * (A / p00 + B / p10 + C / p01 + D / p11)
  ceiling(n_profiles / (tasks * 2))
}

# 3 pp = the binding attribute (Identity, K=3) — see "WHICH EFFECT SIZE" in the
# header. Applied to the interaction rows too, so those read as "the N needed
# for a 3 pp DIFFERENCE in the conditional AMCE", not for a 3 pp AMCE.
delta_pilot <- 0.03
n_ref       <- n_study  # reference N (main study) used to flag "covered" targets

# ── Table: minimum N of respondents by N tasks x estimand ──────────────────
tbl_by_tasks_df <- tibble(tasks = c(3, 4, 5, 6)) |>
  mutate(
    `Main effect, K=2` =
      map_dbl(tasks, ~ n_min(2, delta_pilot, tasks = .x)),
    `Main effect, K=3` =
      map_dbl(tasks, ~ n_min(3, delta_pilot, tasks = .x)),
    # delta3 here is the DIFFERENCE in the conditional AMCE across moderator
    # groups (the heterogeneity test), not a conditional AMCE itself. Assumes a
    # balanced 50/50 binary moderator — see assumption 2 in the header and the
    # sensitivity table below.
    `Heterogeneity: attribute × moderator (N2, binary 50/50)` =
      map_dbl(tasks, ~ n_min_interaction(Kl = 3, Km = 2, delta3 = delta_pilot, tasks = .x)),
    `Attribute × attribute interaction (reference)` =
      map_dbl(tasks, ~ n_min_interaction(Kl = 3, Km = 3, delta3 = delta_pilot, tasks = .x))
  )

tbl_by_tasks_long <- tbl_by_tasks_df |>
  pivot_longer(-tasks, names_to = "target", values_to = "n_min") |>
  mutate(covered = n_min <= n_ref)


# ── Sensitivity: how the moderator's split changes the requirement ─────────
# Assumption 2 in the header. The 50/50 row is what the table above reports;
# the others are what the same target costs once the moderator is unbalanced.
# The treatment is still randomized 50/50, so the joint probabilities are the
# product of the treatment and moderator marginals.
mod_split_df <- tibble(share = c(0.50, 0.40, 0.30, 0.20)) |>
  mutate(
    n_min = map_dbl(share, ~ n_min_interaction(
      Kl = 3, Km = 2, delta3 = delta_pilot, tasks = 6,
      p00 = 0.5 * .x,       p10 = 0.5 * .x,
      p01 = 0.5 * (1 - .x), p11 = 0.5 * (1 - .x)
    )),
    inflation = n_min / n_min[1] - 1,
    `Moderator split` = paste0(share * 100, "/", (1 - share) * 100)
  )

tbl_mod_split <- mod_split_df |>
  transmute(
    `Moderator split`,
    `Min N (6 tasks)` = n_min,
    `vs balanced`     = ifelse(inflation == 0, "—",
                               paste0("+", round(inflation * 100), "%"))
  ) |>
  kbl(
    format.args = list(big.mark = ",")
  ) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)


tbl_power_by_tasks <- tbl_by_tasks_df |>
  rename(`N tasks` = tasks) |>
  kbl(
    escape = FALSE
  ) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)

# ── Figure: minimum N of respondents by N tasks, one line per estimand ─────
g_min_n_by_tasks <- tbl_by_tasks_long |>
  ggplot(aes(x = tasks, y = n_min, color = target)) +
  geom_line(linewidth = 1) +
  geom_point(aes(shape = covered), size = 2.5) +
  geom_hline(yintercept = n_ref, linetype = "dashed", color = "grey30") +
  annotate("text", x = 3, y = n_ref, label = paste0("N = ", format(n_ref, big.mark = ",")),
           vjust = -0.6, hjust = 0, size = 3, color = "grey30") +
  scale_x_continuous(breaks = 3:6) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_shape_manual(
    values = c(`TRUE` = 16, `FALSE` = 4),
    labels = c(
      `TRUE`  = paste0("Covered (<= ", format(n_ref, big.mark = ","), ")"),
      `FALSE` = "Not covered"
    )
  ) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title    = "Minimum N of respondents by number of tasks per respondent",
    subtitle = paste0("Detecting a ", delta_pilot * 100, " pp effect, power = 0.80, ",
                      "α = 0.05 — main effects vs interactions"),
    x        = "N tasks per respondent",
    y        = "Minimum N of respondents",
    color    = "Target",
    shape    = paste0("Reference N = ", format(n_ref, big.mark = ",")),
    caption  = paste0("Dashed line: reference N = ", format(n_ref, big.mark = ","), ".")
  ) +
  ggdist::theme_ggdist() +
  theme(legend.position = "right")


# ── 9. Design check: D-efficiency of the realized design ───────────────────
# Everything above is about SAMPLE SIZE. This section is about the DESIGN
# MATRIX: given the randomization actually implemented, how close does the
# realized design come to the balanced-orthogonal optimum?
#
# The attribute structure and the pair restriction are transcribed from
# build_default_conjoint_design() in surveys/pre-piloto/sin_comentarios/app.R.
# Sex is drawn separately because it is signalled through the applicant's first
# name and does not enter the pair-difference check.
#
# D-efficiency is reported RELATIVE to the full 288-cell factorial, which is
# balanced and orthogonal by construction and therefore the optimum for these
# models: D_rel = (det(M_realized) / det(M_ideal))^(1/p), with M = X'X / n and
# effects (sum-to-zero) coding. 1.000 means the realized design extracts as
# much information per observation as the optimal design can.

conj_levels <- list(
  need = 1:2, identity = 1:3, control = 1:2, effort = 1:3,
  reciprocity = 1:2, attitude = 1:2, sex = 1:2
)
conj_vars <- names(conj_levels)

f_main <- as.formula(paste("~", paste(conj_vars, collapse = " + ")))
f_int  <- as.formula(paste("~ (", paste(conj_vars, collapse = " + "), ")^2"))

# Effects coding keeps the information matrix interpretable across factors.
conj_mm <- function(d, f) {
  model.matrix(f, data = d,
               contrasts.arg = lapply(d[conj_vars],
                                      function(x) contr.sum(nlevels(x))))
}

# The ideal: every cell of the factorial exactly once.
conj_ideal <- expand.grid(conj_levels) |> mutate(across(everything(), factor))

# One task = a pair of profiles differing on at least `min_diff` of the six
# CARIN/NICER attributes (sex excluded from the check, as in app.R).
conj_gen_pair <- function(min_diff = 2) {
  core <- conj_levels[1:6]
  repeat {
    a1 <- sapply(core, function(l) sample(l, 1))
    a2 <- sapply(core, function(l) sample(l, 1))
    if (sum(a1 != a2) >= min_diff) return(rbind(a1, a2))
  }
}

conj_build <- function(n_resp, tasks = 6, min_diff = 2) {
  m <- do.call(rbind, replicate(n_resp * tasks,
                                conj_gen_pair(min_diff), simplify = FALSE))
  d <- as.data.frame(m)
  names(d) <- conj_vars[1:6]
  d$sex <- sample(1:2, nrow(d), TRUE)
  d |> mutate(across(everything(), factor))
}

conj_d_eff <- function(d, f) {
  Xi <- conj_mm(conj_ideal, f)
  p  <- ncol(Xi)
  Mi <- crossprod(Xi) / nrow(conj_ideal)
  M  <- crossprod(conj_mm(d, f)) / nrow(d)
  (det(M) / det(Mi))^(1 / p)
}

set.seed(123)
conj_realized     <- conj_build(n_study)                  # as fielded
conj_unrestricted <- conj_build(n_study, min_diff = 0)    # restriction removed

conj_deff_df <- tibble(
  Model = c("Main effects", "All two-way interactions"),
  f     = list(f_main, f_int)
) |>
  mutate(
    Parameters       = map_dbl(f, ~ ncol(conj_mm(conj_ideal, .x))),
    identifiable     = map_lgl(f, ~ qr(conj_mm(conj_ideal, .x))$rank ==
                                     ncol(conj_mm(conj_ideal, .x))),
    `D-eff (design)` = map_dbl(f, ~ conj_d_eff(conj_realized, .x)),
    `D-eff (no restriction)` = map_dbl(f, ~ conj_d_eff(conj_unrestricted, .x)),
    `Obs. per parameter`     = nrow(conj_realized) / Parameters
  ) |>
  select(-f)

# Largest absolute correlation between indicators of DIFFERENT attributes.
# Within-attribute dummies are collinear by construction, so they are excluded.
conj_max_cor <- local({
  X   <- conj_mm(conj_realized, f_main)[, -1, drop = FALSE]
  grp <- rep(conj_vars, times = lengths(conj_levels) - 1)
  cc  <- cor(X)
  max(abs(cc[outer(grp, grp, "!=")]))
})

conj_n_cells <- nrow(conj_ideal)

tbl_deff <- conj_deff_df |>
  transmute(
    Model,
    Parameters,
    `Identifiable in the 288-cell factorial` = ifelse(identifiable, "Yes", "No"),
    `D-efficiency`            = sprintf("%.4f", `D-eff (design)`),
    `Without pair restriction` = sprintf("%.4f", `D-eff (no restriction)`),
    `Obs. per parameter`      = round(`Obs. per parameter`)
  ) |>
  kbl(
    format.args = list(big.mark = ",")
  ) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)


# ── 9b. Verification with AlgDesign (linear-model D criterion) ─────────────
# AlgDesign implements optimal design for LINEAR models: its D criterion is
# |M|^(1/k) with M = Z'Z/n, which is exactly the criterion section 9 computes by
# hand and, more importantly, exactly the criterion implied by this study's
# estimator — a profile-level linear model of a continuous share. It needs no
# parameter priors, because linear D-efficiency does not depend on the betas.
#
# eval.design() returns `determinant` = det(M/N)^(1/k). Dividing that for the
# realized design by the same quantity for the full factorial gives relative
# D-efficiency on the same scale as section 9, so the two are directly
# comparable. optFederov() additionally confirms that nothing in the candidate
# set beats the balanced factorial for this model.

library(AlgDesign)

alg_f     <- f_main
alg_ideal <- conj_ideal
alg_ev_ideal <- eval.design(alg_f, alg_ideal)

alg_realized_deff <- eval.design(alg_f, conj_realized)$determinant /
                     alg_ev_ideal$determinant

# Is the balanced factorial itself D-optimal? optFederov searches the candidate
# set for anything better; a ratio of 1 means the factorial cannot be improved.
set.seed(123)
alg_opt <- optFederov(alg_f, alg_ideal, nTrials = nrow(alg_ideal),
                      criterion = "D", nRepeats = 5)
alg_opt_ratio <- alg_opt$D / alg_ev_ideal$determinant

# ── 9c. Cross-check with idefix (MNL / choice-based D-error) ───────────────
# idefix evaluates designs for DISCRETE CHOICE experiments under a multinomial
# logit model. Its D(B)-error is built on the MNL information matrix, which for
# a two-alternative set and null priors reduces to
#     I  ∝  sum over sets of (x_1 - x_2)(x_1 - x_2)'
# i.e. ALL information lives in the CONTRAST between the two profiles of a task.
# Under that criterion, any attribute that takes the same level in both profiles
# ("level overlap") contributes nothing to that task.
#
# This matters because the two criteria disagree sharply for this design, and
# the disagreement is substantive rather than numerical:
#   - Section 9 (profile-level linear model): D-efficiency ~ 1.00
#   - idefix (contrast/MNL model):            D-efficiency ~ 0.58
# Our randomizer draws each profile independently, so levels overlap often
# (probability 1/2 for a binary attribute, 1/3 for a three-level one). A
# D-optimal choice design instead forces the alternatives to differ on every
# attribute, which is why idefix rates it much higher.
#
# WHICH CRITERION APPLIES is decided by the estimator, not by the package. The
# analysis plan fits a PROFILE-LEVEL linear model of the allocated share with
# respondent fixed effects (see documentation/03-conjoint-desing.qmd), not a
# conditional/contrast model. Under that estimator the competing profile's
# contribution goes into the error term and is orthogonal to the regressor by
# randomization, so overlap costs almost nothing. sim_estimator_df below
# verifies this by simulating the real outcome and comparing the two.
#
# idefix is therefore reported as a design diagnostic and as the answer to
# "would a choice-based D-optimal design have been better?" — not as a
# correction to section 9.

library(idefix)

conj_lv_vec <- c(need = 2, identity = 3, control = 2, effort = 3,
                 reciprocity = 2, attitude = 2, sex = 2)

# Candidate set = the full factorial, effects-coded (idefix's "E").
idefix_cand <- Profiles(lvls = as.numeric(conj_lv_vec),
                        coding = rep("E", length(conj_lv_vec)))
idefix_p    <- ncol(idefix_cand)
idefix_zero <- matrix(rep(0, idefix_p), nrow = 1)   # null priors
idefix_key  <- apply(expand.grid(lapply(conj_lv_vec, seq_len)), 1,
                     paste, collapse = "-")

# Our randomizer, expressed as an idefix design matrix (rows = alternatives).
idefix_build <- function(n_sets) {
  rows <- vector("list", n_sets * 2)
  for (s in seq_len(n_sets)) {
    pr <- conj_gen_pair()
    for (a in 1:2) {
      v <- c(pr[a, ], sample.int(2, 1))
      rows[[(s - 1) * 2 + a]] <-
        idefix_cand[match(paste(v, collapse = "-"), idefix_key), , drop = FALSE]
    }
  }
  do.call(rbind, rows)
}
idefix_db <- function(d) {
  EvaluateDesign(d, par.draws = idefix_zero, n.alts = 2)$DB.error
}

set.seed(123)
idefix_n_sets <- 72   # 144 profiles: large enough to be stable, small enough for Modfed

idefix_opt  <- Modfed(cand.set = idefix_cand, n.sets = idefix_n_sets, n.alts = 2,
                      par.draws = idefix_zero, n.start = 3, max.iter = 50,
                      parallel = FALSE)
idefix_eval_opt  <- EvaluateDesign(idefix_opt$BestDesign$design,
                                   par.draws = idefix_zero, n.alts = 2)
idefix_des_rand  <- idefix_build(idefix_n_sets)
idefix_eval_rand <- EvaluateDesign(idefix_des_rand,
                                   par.draws = idefix_zero, n.alts = 2)
idefix_db_rand   <- median(replicate(20, idefix_db(idefix_build(idefix_n_sets))))

# Mean number of attributes overlapping per choice set (0 = always differ).
# level.overlap is a data.frame with columns `set` and `overlap.count`; take the
# count column, not a rowSum, which would add the set index.
idefix_overlap     <- mean(idefix_eval_rand$level.overlap$overlap.count)
idefix_overlap_opt <- mean(idefix_eval_opt$level.overlap$overlap.count)

idefix_deff <- idefix_eval_opt$DB.error / idefix_db_rand

idefix_df <- tibble(
  Design = c("Randomized (as fielded)", "D-optimal (idefix Modfed)"),
  `DB-error`      = c(idefix_db_rand, idefix_eval_opt$DB.error),
  Orthogonality   = c(idefix_eval_rand$Orthogonality, idefix_eval_opt$Orthogonality),
  `Overlapping attributes per task` = c(idefix_overlap, idefix_overlap_opt),
  `D-efficiency vs optimal` = c(idefix_deff, 1)
)

tbl_idefix <- idefix_df |>
  mutate(across(where(is.numeric), ~ sprintf("%.3f", .x))) |>
  kbl(format.args = list(big.mark = ",")) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)


# ── 9d. Head-to-head: the same design judged by three criteria ─────────────
# The point of this table is that the spread across rows is NOT disagreement
# about arithmetic. AlgDesign and the hand calculation agree to four decimals
# because they implement the same linear criterion; idefix differs because it
# implements a different one, appropriate to a different estimator.
criteria_df <- tibble(
  Approach = c("Hand calculation",
               "AlgDesign `eval.design`",
               "idefix `EvaluateDesign`"),
  Criterion = c("Linear: |Z'Z/n|^(1/k)",
                "Linear: |Z'Z/n|^(1/k)",
                "MNL: contrast information"),
  `Model assumed` = c("Profile-level linear",
                      "Profile-level linear",
                      "Forced choice (conditional logit)"),
  `Priors required` = c("No", "No", "Yes"),
  `D-efficiency` = c(conj_deff_df[["D-eff (design)"]][1],
                     alg_realized_deff,
                     idefix_deff)
)

tbl_criteria <- criteria_df |>
  mutate(`D-efficiency` = sprintf("%.4f", `D-efficiency`)) |>
  kbl() |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)


# ── 9e. Which criterion applies? Simulation of the real outcome ────────────
# Fits the AMCE of Effort under two designs (our randomizer vs a minimum-overlap
# design) and under two estimators (the profile-level model of the analysis plan
# vs a contrast model). Relative D-efficiency is reported as the squared ratio
# of standard errors, which is the sample-size equivalent.
sim_se_once <- function(gen, n_resp = 400, tasks = 6, amce = 6, sd_resid = 25) {
  m <- do.call(rbind, replicate(n_resp * tasks, gen(), simplify = FALSE))
  d <- as.data.frame(m); names(d) <- names(conj_lv_vec)
  u   <- ifelse(d$effort == 3, amce, ifelse(d$effort == 2, amce / 2, 0))
  odd <- seq(1, nrow(d), 2); ev <- seq(2, nrow(d), 2)
  sA  <- 50 + (u[odd] - u[ev]) + rnorm(length(odd), 0, sd_resid)
  d$share <- as.vector(rbind(sA, 100 - sA))          # fixed-sum by construction
  d <- d |> mutate(across(all_of(names(conj_lv_vec)), factor))
  f <- share ~ need + identity + control + effort + reciprocity + attitude + sex
  se_profile <- summary(lm(f, data = d))$coefficients["effort3", "Std. Error"]
  X  <- model.matrix(f, d)[, -1, drop = FALSE]
  dd <- as.data.frame(X[odd, , drop = FALSE] - X[ev, , drop = FALSE])
  dd$y <- sA - (100 - sA)
  # The contrast regression recovers 2*beta, not beta: y = 2*sA - 100 and the
  # regressor is x_A - x_B, so both sides carry a factor of 2. Halving puts the
  # SE on the same AMCE scale as the profile-level model, without which the two
  # rows of the table would not be comparable.
  se_contrast <- summary(lm(y ~ . - 1, data = dd))$coefficients["effort3", "Std. Error"] / 2
  c(profile = se_profile, contrast = se_contrast)
}
# Minimum-overlap design: every attribute takes a different level in each profile.
sim_gen_minoverlap <- function() {
  a1 <- sapply(conj_lv_vec, function(k) sample.int(k, 1))
  a2 <- sapply(seq_along(conj_lv_vec),
               function(i) sample(setdiff(seq_len(conj_lv_vec[i]), a1[i]), 1))
  rbind(a1, a2)
}
sim_gen_fielded <- function() {
  pr <- conj_gen_pair()
  rbind(c(pr[1, ], sample.int(2, 1)), c(pr[2, ], sample.int(2, 1)))
}

set.seed(99)
sim_A <- replicate(30, sim_se_once(sim_gen_fielded))
sim_B <- replicate(30, sim_se_once(sim_gen_minoverlap))

sim_estimator_df <- tibble(
  Estimator = c("Profile-level linear model (analysis plan)",
                "Contrast model (what idefix assumes)"),
  `SE, as fielded`      = c(mean(sim_A["profile", ]), mean(sim_A["contrast", ])),
  `SE, minimum overlap` = c(mean(sim_B["profile", ]), mean(sim_B["contrast", ]))
) |>
  mutate(`Relative D-efficiency` = (`SE, minimum overlap` / `SE, as fielded`)^2)

tbl_estimator <- sim_estimator_df |>
  mutate(across(where(is.numeric), ~ sprintf("%.3f", .x))) |>
  kbl(format.args = list(big.mark = ",")) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)


# ── 10. Objects exported to the document ───────────────────────────────────
# Nothing is printed here on purpose: documentation/03b-power-analysis.qmd
# source()s this script and renders each object in its own output chunk, so
# that processing and presentation stay separate.
#
# Tables (kable objects)
#   tbl_power_main      min N for main effects, 6 vs 4 tasks
#   tbl_power_main_2    MDE at the study N, 6 vs 4 tasks
#   tbl_power_by_tasks  min N by tasks x estimand (main effects vs interactions)
#   tbl_mod_split       sensitivity to the moderator's marginal distribution
#   tbl_deff            D-efficiency of the realized design (design check)
#   tbl_criteria        the same design judged by three D criteria
#   tbl_idefix          idefix MNL D-error: randomized vs D-optimal design
#   tbl_estimator       which criterion applies, by estimator (simulation)
#
# Figures (ggplot objects)
#   g1                  power curves by N, faceted by number of tasks
#   g2                  MDE vs N respondents
#   g3                  type M error (exaggeration ratio)
#   g_tasks             MDE vs N respondents, 6 vs 4 tasks
#   g_min_n_by_tasks    min N by tasks, one line per estimand
#
# Data frames (for inline values in the prose)
#   res, mde_df, mde_tasks_df, tbl_by_tasks_df, tbl_by_tasks_long, mod_split_df
#   conj_deff_df, conj_max_cor, conj_n_cells
#   idefix_df, idefix_overlap, idefix_overlap_opt, idefix_deff, idefix_n_sets
#   criteria_df, alg_realized_deff, alg_opt_ratio
#   sim_estimator_df
#
# Helpers
#   mde_at(), n_min(), n_min_interaction(), conj_d_eff(), conj_build()
