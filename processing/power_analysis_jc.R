#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Analytic power analysis for the distributive conjoint, solved for N (JC variant)
# Responsible: Juan Castillo
# Executive Summary: Companion to processing/analisis-poder-v2.R (which asks
#   "what power does N = 1,500 give me?"), this script asks the inverse
#   question: "how many respondents does this design NEED for 80% power?" —
#   using the same closed-form bound from Schuessler & Freitag (2020) /
#   cjpowR, but solved for N rather than evaluated at a fixed N. It also
#   caps tasks per respondent at 5 (this design's ceiling), rather than 6.
#   Same attribute structure and assumed AMCEs as the rest of the project
#   (need, identity, control, effort, reciprocity, attitude, sex).
# Date: September 14, 2026
#******************************************************************************************************************************************************
#
# HOW TO READ THESE NUMBERS ------------------------------------------------
# Every minimum N below is a CONSERVATIVE UPPER BOUND, not the smallest N
# that could possibly work. The formula assumes a binary forced-choice
# outcome (Bernoulli variance, the maximum a bounded outcome can have); this
# design's outcome is a continuous fixed-sum share, whose variance is lower
# in practice, so the true minimum N is smaller than reported here. Treat
# this script as the upper bound; a DeclareDesign simulation of the actual
# continuous outcome (processing/power_declaredesign_jc.R) gives the
# realistic, lower bound, exactly as in the sibling documents.
#
# WHICH EFFECT SIZE, AND WHY 3 pp ------------------------------------------
# "pp" = percentage points of the allocated share. 3 pp is the binding case
# for this design: of the two three-level attributes, the assumed effects
# are Effort = 6 pp and Identity = 3 pp, so Identity — most levels, smallest
# assumed effect — is what the design must be sized for. The same 3 pp
# target is applied to the two-level attributes and to the interaction rows
# as a single, conservative design target (every assumed K=2 effect, 3.5 to
# 13 pp, is easier than 3 pp; see processing/power_declaredesign_jc.R,
# section 2, for the full effect vector).
#******************************************************************************************************************************************************

# PROCESSING ONLY — this script computes and returns objects, it prints
# nothing. Tables and figures are rendered in
# documentation/03_power_analysis_jc.qmd, which source()s this file.

options(scipen = 999)

# 1. Packages  -----------------------------------------------------
library(tidyverse)
library(kableExtra)
library(ggdist)

set.seed(123)

# 2. Helper functions: MDE, minimum N, minimum N for interactions ----------

# What can a given N detect? (power = 0.80 by default)
mde_at <- function(n_resp, levels, tasks, power = 0.80, alpha = 0.05) {
  n_prof <- n_resp * tasks * 2
  sqrt((levels / 2) * (qnorm(1 - alpha / 2) + qnorm(power))^2 / n_prof) * 100
}

# Inverse: minimum N of respondents to detect a main effect `delta` (in
# proportion units, e.g. 0.03 for 3 pp) at a given power, for an attribute
# with `levels` levels and `tasks` tasks per respondent.
n_min <- function(levels, delta, tasks, power = 0.80, alpha = 0.05) {
  z <- (qnorm(1 - alpha / 2) + qnorm(power))^2
  n_prof <- (levels / 2) * z / delta^2
  ceiling(n_prof / (tasks * 2))
}

# Minimum N for an interaction coefficient delta3 (equation 6, Schuessler &
# Freitag 2020). Kl, Km = levels of the two interacting elements (attribute
# and moderator, or attribute and attribute). d0/d1/d2 = conservative
# cumulative response probabilities (0.5/0/0 = maximal-variance default).
# p00..p11 = joint treatment probabilities of the two elements (default:
# uniform randomization x a balanced 50/50 moderator).
n_min_interaction <- function(Kl, Km, delta3, tasks, power = 0.80, alpha = 0.05,
                               d0 = 0.5, d1 = 0, d2 = 0,
                               p00 = 0.25, p10 = 0.25, p01 = 0.25, p11 = 0.25) {
  stopifnot(isTRUE(all.equal(p00 + p10 + p01 + p11, 1)))
  z <- (qnorm(1 - alpha / 2) + qnorm(power))^2
  A <- d0 * (1 - d0)
  B <- (d0 + d1) * (1 - (d0 + d1))
  C <- (d0 + d1 + d2) * (1 - (d0 + d1 + d2))
  D <- (d0 + d1 + d2 + delta3) * (1 - (d0 + d1 + d2 + delta3))
  n_profiles <- (Kl * Km / 4) * z / delta3^2 * (A / p00 + B / p10 + C / p01 + D / p11)
  ceiling(n_profiles / (tasks * 2))
}

# 3. Design constants -------------------------------------------------------
delta_target <- 0.03      # 3 pp — the binding target effect (see header)
tasks_max    <- 5         # this design's ceiling (max 5 tasks per respondent)
tasks_grid   <- 1:tasks_max

# 4. Table: minimum N of respondents by number of tasks x estimand ----------
# Mirrors tbl_by_tasks_df in analisis-poder-v2.R, but tasks capped at 5 and
# framed as the primary question ("what N do I need?") rather than a
# secondary check against a pre-fixed N.
tbl_by_tasks_df <- tibble(tasks = tasks_grid) |>
  mutate(
    `Main effect, K=2` =
      map_dbl(tasks, ~ n_min(2, delta_target, tasks = .x)),
    `Main effect, K=3` =
      map_dbl(tasks, ~ n_min(3, delta_target, tasks = .x)),
    `Heterogeneity: attribute x moderator (K=3 x binary, 50/50)` =
      map_dbl(tasks, ~ n_min_interaction(Kl = 3, Km = 2, delta3 = delta_target, tasks = .x)),
    `Attribute x attribute interaction (reference)` =
      map_dbl(tasks, ~ n_min_interaction(Kl = 3, Km = 3, delta3 = delta_target, tasks = .x))
  )

tbl_by_tasks_long <- tbl_by_tasks_df |>
  pivot_longer(-tasks, names_to = "target", values_to = "n_min")

# Every number in this table is a count of RESPONDENTS, not profile
# evaluations (a respondent contributes tasks * 2 profiles). The value
# columns are labelled by estimand, not by unit, so an `add_header_above()`
# band states the unit once, over all four value columns, rather than
# leaving it to the caption alone.
tbl_power_by_tasks <- tbl_by_tasks_df |>
  rename(`Tasks per respondent` = tasks) |>
  kbl(escape = FALSE, format.args = list(big.mark = ",")) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE) |>
  add_header_above(c(" " = 1, "Minimum N of respondents (not profile evaluations)" = 4))

# ── Figure: minimum N by number of tasks, one line per estimand ────────────
g_min_n_by_tasks <- tbl_by_tasks_long |>
  ggplot(aes(x = tasks, y = n_min, color = target)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = tasks_grid) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title    = "Minimum N of respondents by number of tasks per respondent",
    subtitle = paste0("Detecting a ", delta_target * 100,
                      " pp effect, power = 0.80, alpha = 0.05 - main effects vs interactions"),
    x        = "N tasks per respondent (design ceiling: 5)",
    y        = "Minimum N of respondents",
    color    = "Target"
  ) +
  ggdist::theme_ggdist() +
  theme(legend.position = "bottom", legend.direction = "vertical")

# 5. Table: minimum N at 5, 4 and 3 tasks, power 0.80 vs 0.90 ----------------
# Long format, one row per (target x tasks), with BOTH units shown explicitly
# side by side: N respondents (what you recruit) and N profile evaluations
# (respondents x tasks x 2, what the power formula actually operates on).
# Keeping both columns in the same table is the direct fix for the ambiguity
# a reader hits when a bare "Min N (5 tasks)" column could mean either.
make_main_rows <- function(target, delta_pp, K, power = 0.80,
                            tasks_vals = c(5, 4, 3)) {
  tibble(
    Target        = target,
    `Target AMCE` = paste0(delta_pp, " pp"),
    Power         = power,
    Tasks         = tasks_vals
  ) |>
    mutate(
      `N respondents (minimum)`  = map_dbl(Tasks, ~ n_min(K, delta_pp / 100, tasks = .x, power = power)),
      `N profile evaluations`    = `N respondents (minimum)` * Tasks * 2
    )
}

tbl_power_main_df <- bind_rows(
  make_main_rows("Main effect, K=2",               3, 2, 0.80),
  make_main_rows("Main effect, K=3",                3, 3, 0.80),
  make_main_rows("Main effect, K=3, power = 0.90",  3, 3, 0.90)
)

tbl_power_main <- tbl_power_main_df |>
  kbl(format.args = list(big.mark = ",")) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE) |>
  footnote(
    general = paste0(
      "\"N respondents\" is who you recruit; \"N profile evaluations\" = N respondents x tasks x 2 ",
      "is what the power formula operates on. Every other table in this document reports N respondents ",
      "unless its own caption says otherwise."
    ),
    general_title = "Note: "
  )

# 6. Figure: minimum N required, by assumed effect size, at 5 tasks ---------
# Complementary view: given the design ceiling of 5 tasks, how does the
# required N change with the size of the effect one wants to be powered for?
d_effect_curve <- expand.grid(
  amce_pp = seq(1, 8, by = 0.25),
  levels  = c(2, 3)
) |>
  mutate(
    n_min_resp = map2_dbl(amce_pp, levels, ~ n_min(.y, .x / 100, tasks = tasks_max)),
    k_label    = paste0("K = ", levels, " levels")
  )

g_effect_curve <- d_effect_curve |>
  ggplot(aes(x = amce_pp, y = n_min_resp, color = k_label)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = delta_target * 100, linetype = "dashed", color = "grey40") +
  annotate("text", x = delta_target * 100 + 0.1, y = max(d_effect_curve$n_min_resp) * 0.95,
           label = paste0(delta_target * 100, " pp target"), hjust = 0, size = 3, color = "grey40") +
  scale_color_manual(values = c("#2c3e50", "#e74c3c")) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title    = "Minimum N of respondents required, by target effect size",
    subtitle = "5 tasks per respondent (design ceiling), power = 0.80, alpha = 0.05",
    x        = "Target AMCE (percentage points)",
    y        = "Minimum N of respondents",
    color    = "Attribute levels"
  ) +
  ggdist::theme_ggdist() +
  theme(legend.position = "right")

# 7. Sensitivity: how the moderator's split changes the interaction N -------
# At the design ceiling of 5 tasks. Mirrors tbl_mod_split in
# analisis-poder-v2.R.
mod_split_df <- tibble(share = c(0.50, 0.40, 0.30, 0.20)) |>
  mutate(
    n_min_int = map_dbl(share, ~ n_min_interaction(
      Kl = 3, Km = 2, delta3 = delta_target, tasks = tasks_max,
      p00 = 0.5 * .x,       p10 = 0.5 * .x,
      p01 = 0.5 * (1 - .x), p11 = 0.5 * (1 - .x)
    )),
    inflation = n_min_int / n_min_int[1] - 1,
    `Moderator split` = paste0(share * 100, "/", (1 - share) * 100)
  )

tbl_mod_split <- mod_split_df |>
  transmute(
    `Moderator split`,
    `Min N respondents (5 tasks)` = n_min_int,
    `vs balanced`      = ifelse(inflation == 0, "-", paste0("+", round(inflation * 100), "%"))
  ) |>
  kbl(format.args = list(big.mark = ",")) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)

# ── 8. Objects exported to the document ─────────────────────────────────────
# Tables (kable objects): tbl_power_by_tasks, tbl_power_main, tbl_mod_split
# Figures (ggplot objects): g_min_n_by_tasks, g_effect_curve
# Data frames: tbl_by_tasks_df, tbl_by_tasks_long, tbl_power_main_df,
#   d_effect_curve, mod_split_df
# Helpers: mde_at(), n_min(), n_min_interaction()
