#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Simulation-based power analysis for the distributive conjoint, solved for N (JC variant)
# Responsible: Juan Castillo
# Executive Summary: Companion to processing/power_declaredesign.R (Andreas
#   Laffert's script, which fixes N = 1,500 and asks "what interaction can I
#   detect?"), this script asks the inverse question directly on the sample-
#   size axis: "how many respondents does this design need for 80% power?" —
#   under the design's actual outcome (a continuous, zero-sum share), not the
#   binary worst case the analytic formula assumes. Tasks per respondent are
#   capped at 5 (this design's ceiling), not 6. Same 7-attribute structure
#   and assumed AMCEs as the rest of the project (need, identity, control,
#   effort, reciprocity, attitude, sex), reused unchanged from
#   power_declaredesign.R so the two documents' simulations stay comparable.
# Date: September 14, 2026
#******************************************************************************************************************************************************

options(scipen = 999)
rm(list = ls())

# 1. Packages  -----------------------------------------------------
if (!require("pacman")) install.packages("pacman")

# cjpowR is not on CRAN; install once with:
#   devtools::install_github("m-freitag/cjpowR")
pacman::p_load(DeclareDesign, cjpowR, estimatr, tidyverse, kableExtra, here, future, future.apply)

set.seed(123)

n_workers <- max(1, parallel::detectCores() - 1)
plan(multisession, workers = n_workers)
invisible(future_lapply(seq_len(n_workers), function(i) {
  suppressPackageStartupMessages({
    library(DeclareDesign); library(fabricatr); library(estimatr); library(dplyr)
  })
  TRUE
}))

# 2. Conjoint design ------------------------------------------------------
# Attributes, levels and assumed AMCEs are IDENTICAL to
# processing/power_declaredesign.R (see that script for the full rationale,
# including the measurement-error treatment of the moderator). The only
# structural change here is that N_resp is the object of interest — this
# script sweeps it directly and solves for the crossover, instead of fixing
# N_resp = 1500 and sweeping the effect size.
conjoint_design <- function(
    N_resp,
    K_tasks           = 5,
    sd_outcome        = 15,
    reliability       = 0.8,
    amce_need         = 13,
    amce_identity     = 3,
    amce_control      = 3.5,
    amce_effort       = 6,
    amce_reciprocity  = 3.5,
    amce_attitude     = 3.5,
    amce_sex          = 3.5,
    delta_interaction = 1.5    # true pp per SD of M; anchored on Gilgen (2022)
) {

  modelo <- declare_model(

    respondent = add_level(
      N = N_resp,
      M     = rnorm(N),
      M_obs = sqrt(reliability) * M + rnorm(N, sd = sqrt(1 - reliability))
    ),

    task = add_level(N = K_tasks),

    profile = add_level(
      N = 2,

      need        = sample(1:2, N, replace = TRUE),
      identity    = sample(1:3, N, replace = TRUE),
      control     = sample(1:2, N, replace = TRUE),
      effort      = sample(1:3, N, replace = TRUE),
      reciprocity = sample(1:2, N, replace = TRUE),
      attitude    = sample(1:2, N, replace = TRUE),
      sex         = sample(1:2, N, replace = TRUE),

      need_hardship   = as.numeric(need == 2),
      identity_peru   = as.numeric(identity == 2),
      identity_ven    = as.numeric(identity == 3),
      control_nofund  = as.numeric(control == 2),
      effort_same     = as.numeric(effort == 2),
      effort_more     = as.numeric(effort == 3),
      reciprocity_vol = as.numeric(reciprocity == 2),
      attitude_grat   = as.numeric(attitude == 2),
      sex_female      = as.numeric(sex == 2),

      raw = amce_need * need_hardship +
        amce_identity * identity_peru + amce_identity * identity_ven +
        amce_control * control_nofund +
        (amce_effort / 2) * effort_same + amce_effort * effort_more +
        amce_reciprocity * reciprocity_vol +
        amce_attitude * attitude_grat +
        amce_sex * sex_female +
        (delta_interaction * 1.5) * effort_more * M +
        rnorm(N, sd = sd_outcome / sqrt(2)),

      raw_other = ifelse(rep(c(TRUE, FALSE), length.out = N), lead(raw), lag(raw)),

      share = pmin(pmax(50 + (raw - raw_other), 0), 100)
    )
  )

  inquiry <- declare_inquiry(
    effort_main        = amce_effort,
    effort_interaction = delta_interaction
  )

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

# ── Sanity checks (same as power_declaredesign.R) ──────────────────────────
dat_check <- draw_data(conjoint_design(N_resp = 50, K_tasks = 5, sd_outcome = 15))
n_check <- nrow(dat_check)
stopifnot(
  max(abs(dat_check$share[seq(1, n_check, 2)] + dat_check$share[seq(2, n_check, 2)] - 100)) < 1e-8
)
rm(dat_check, n_check)

# 3. Diagnosis grid: sweep N_resp directly, find the power = 0.80 crossover -
# CENTRAL QUESTION: at each K_tasks (1 to 5, this design's ceiling), what is
# the smallest N_resp for which power >= 0.80, for (a) the effort main effect
# (6 pp, the design's easier K=3 target) and (b) the effort x M interaction
# at delta_interaction = 1.5 pp/SD (the Gilgen-anchored central estimate) and
# realistic moderator reliability (0.8)?
#
# N_resp grid is coarser than a fully continuous search — diagnose_design()
# is expensive — and the true crossover is recovered by linear interpolation
# (mde_crossover(), reused on the N axis instead of the effect axis).
SIMS <- 150  # simulations per design/cell

N_resp_grid  <- c(100, 200, 300, 500, 750, 1000, 1500, 2000, 3000, 4500, 6000)
K_tasks_grid <- 1:5

design_base <- conjoint_design(N_resp = 1500, K_tasks = 5, sd_outcome = 15,
                                delta_interaction = 1.5, reliability = 0.8)

grid_n <- redesign(
  design_base,
  N_resp            = N_resp_grid,
  K_tasks           = K_tasks_grid,
  sd_outcome        = 15,
  delta_interaction = 1.5,
  reliability       = 0.8
)

diagnosis_n <- diagnose_design(grid_n, sims = SIMS, bootstrap_sims = 0)

diagnosands_n <- diagnosis_n$diagnosands |>
  as_tibble() |>
  mutate(
    term_label = if_else(term == "effort_more:M_obs",
                          "Conditional AMCE (effort x M)",
                          "Main effect (effort, More vs Less)")
  )

# 4. Crossover finder (power = 0.80), on the N axis -------------------------
n_crossover <- function(n_resp, power, target = 0.80) {
  ord <- order(n_resp)
  n_resp <- n_resp[ord]; power <- power[ord]
  if (power[1] >= target) return(n_resp[1])    # already powered at the smallest N swept
  if (max(power) < target) return(NA_real_)    # never reaches target within the swept range
  i <- which(power >= target)[1]
  approx(x = power[c(i - 1, i)], y = n_resp[c(i - 1, i)], xout = target)$y
}

# 5. Table: minimum N by K_tasks, main effect vs interaction ----------------
min_n_by_term <- diagnosands_n |>
  group_by(K_tasks, term_label) |>
  summarise(min_n = n_crossover(N_resp, power), .groups = "drop")

tbl_min_n_sim_df <- min_n_by_term |>
  pivot_wider(names_from = term_label, values_from = min_n) |>
  arrange(K_tasks)

tbl_min_n_sim <- tbl_min_n_sim_df |>
  mutate(across(-K_tasks, \(x) ceiling(x))) |>
  rename(`Tasks per respondent` = K_tasks) |>
  kbl(
    caption = "Simulated minimum N of respondents for 80% power (continuous outcome), by number of tasks"
  ) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE) |>
  add_header_above(c(" " = 1, "Minimum N of respondents (not profile evaluations)" = 2)) |>
  footnote(
    general = paste0(
      SIMS, " simulations per cell, N interpolated between ", paste(N_resp_grid, collapse = ", "),
      " respondents. Main effect: effort, More vs Less, true AMCE = 6 pp. Interaction: effort x M, ",
      "true effect = 1.5 pp per SD of M (anchored on Gilgen 2022), moderator reliability = 0.8."
    ),
    general_title = "Note: "
  )

# 6. Figure: simulated power vs N_resp, by K_tasks, for the interaction -----
g_power_vs_n <- diagnosands_n |>
  filter(term == "effort_more:M_obs") |>
  mutate(k_label = paste0(K_tasks, " tasks")) |>
  ggplot(aes(x = N_resp, y = power, color = k_label)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "grey40") +
  annotate("text", x = min(N_resp_grid), y = 0.82, label = "Power = 0.80",
           color = "grey40", size = 3, hjust = 0) +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1),
                      breaks = seq(0, 1, 0.2)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title    = "Simulated power of the effort x meritocracy-moderator interaction, by N",
    subtitle = "True interaction = 1.5 pp/SD, moderator reliability = 0.8, 1 to 5 tasks per respondent",
    x        = "N respondents",
    y        = "Statistical power",
    color    = "N tasks",
    caption  = paste0(SIMS, " simulations per cell.")
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "right")

# 7. Comparison: simulated (continuous) vs cjpow.R-style analytic (binary) --
# Local re-implementations of n_min()/n_min_interaction() (equations 4 and 6,
# Schuessler & Freitag 2020), evaluated at the SAME N_resp grid so the
# simulated and analytic minimum N can be read side by side.
power_main_binary <- function(amce_pp, n_resp, K, tasks, alpha = 0.05) {
  n_profiles <- n_resp * tasks * 2
  out <- cjpowr_amce(amce = amce_pp / 100, n = n_profiles, levels = K, alpha = alpha)
  out$power
}

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

tbl_comparison_df <- diagnosands_n |>
  filter(K_tasks == 5) |>
  select(term_label, N_resp, sim_power = power) |>
  rowwise() |>
  mutate(
    binary_power = if_else(
      term_label == "Main effect (effort, More vs Less)",
      power_main_binary(6, N_resp, K = 3, tasks = 5),
      power_interaction_binary(1.5, N_resp, Kl = 3, Km = 2, tasks = 5)
    ),
    gain = sim_power - binary_power
  ) |>
  ungroup()

tbl_comparison <- tbl_comparison_df |>
  mutate(across(where(is.numeric) & !N_resp, \(x) round(x, 3))) |>
  rename(`N respondents` = N_resp, Target = term_label,
         `Power (simulated, continuous)` = sim_power,
         `Power (analytic, binary)` = binary_power, Gain = gain) |>
  kbl() |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE) |>
  footnote(
    general = "5 tasks per respondent. Binary reproduces the Schuessler & Freitag (2020) forced-choice formula. Gain = simulated continuous power minus binary analytic power.",
    general_title = "Note: "
  )

# 8. Save -------------------------------------------------------------------
save(diagnosands_n, min_n_by_term, tbl_min_n_sim_df, tbl_min_n_sim,
     g_power_vs_n, tbl_comparison_df, tbl_comparison,
     N_resp_grid, K_tasks_grid, SIMS,
     file = here("processing", "power_declaredesign_jc.RData"))
