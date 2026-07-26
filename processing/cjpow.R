#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: cjpowR for conjoint survey experiment about deservingness and educational inequality
# Responsible: Andreas Laffert
# Executive Summary: This script contains the code to perform cjpowR
# Date: May 12, 2026
#******************************************************************************************************************************************************

options(scipen=999)
rm(list = ls())

# 1. Packages  -----------------------------------------------------
#if (!require(devtools)) install.packages("devtools")
#devtools::install_github("m-freitag/cjpowR")

library(cjpowR)
library(tidyverse)

set.seed(123)

# ── 1. Point calculation ───────────────────────────────────────────
# For a specific AMCE and a given N (main study N=4,500, 6 tasks, 2 profiles)
n_study          <- 4500
n_efectivo_study <- n_study * 6 * 2  # = 54,000 total profiles

# K=2 attribute (Need, Control, Reciprocity, Attitude, Sex)
cjpowr_amce(amce = 0.03, n = n_efectivo_study, levels = 2, alpha = 0.05)

# K=3 attribute (Effort, Identity)
cjpowr_amce(amce = 0.03, n = n_efectivo_study, levels = 3, alpha = 0.05)

# Minimum N for a target power
cjpowr_amce(amce = 0.03, power = 0.80, levels = 3, alpha = 0.05)

# ── 2. Power curves vs effective N ─────────────────────────────────
# Vectorize over parameter ranges
cjpowr_amce_vec <- Vectorize(cjpowr_amce)

# Grid: different AMCEs × effective N (range extended to cover N = 4,500)
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
      typeS  = null_na(out$type_s_error)
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
  tasks  = c(6, 4)
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
  annotate("text", x = n_study - 30, y = 0.15, label = "Main study\n(N=4,500)",
           color = "#2c3e50", size = 3, hjust = 1) +
  facet_wrap(~ tasks_label) +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(labels = scales::comma_format(),
                     breaks = seq(0, 5000, 1000)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    subtitle = "Distributive conjoint on deservingness (α = 0.05); panels compare 6 vs 4 tasks per respondent",
    x        = "N respondents",
    y        = "Statistical power",
    color    = "Effect size",
    linetype = "Attribute levels",
    caption  = "Grey lines: power thresholds 0.80 and 0.90. Vertical line: study N = 4,500."
  ) +
  ggdist::theme_ggdist()+
  theme(legend.position = "right")


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
  annotate("text", x = n_study + 30, y = 9.5, label = "Main study (N=4,500)",
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

# Kept for reference (minimum N for a target power/effect); not called below
# because the study N is now fixed at 4,500 — the question is what that N
# can detect (mde_at), not what N would be required.
n_min <- function(levels, delta, tasks = 6, power = 0.80, alpha = 0.05) {
  z <- (qnorm(1 - alpha / 2) + qnorm(power))^2
  n_prof <- (levels / 2) * z / delta^2
  ceiling(n_prof / (tasks * 2))
}


# ── 7. MDE comparison: 6 vs 4 tasks (N = 4,500) ────────────────────

# Point comparison: MDE at N = 4,500 for 6 vs 4 tasks, K=2 vs K=3 levels
mde_tasks_df <- expand.grid(
  tasks  = c(6, 4),
  levels = c(2, 3),
  n_resp = n_study
) |>
  mutate(
    n_profiles = n_resp * tasks * 2,
    mde        = mde_at(n_resp, levels, tasks = tasks)
  )

# Table: MDE for the main study — 6 vs 4 tasks, N = 4,500 fixed
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
    caption = paste0(
      "Minimum detectable effect for the main study (N = ",
      format(n_study, big.mark = ","), ", power = 0.80, α = 0.05), 6 vs 4 tasks"
    ),
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
           label = "Main study (N=4,500)", color = "#2c3e50", size = 3, hjust = 0) +
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
) |>
  # heterogeneidad: N mínimo se duplica respecto a efectos principales K=3
  bind_rows(
    tibble(
      Target                     = "Heterogeneity (binary moderator)",
      `Minimum AMCE`             = "3 pp",
      `Most demanding attribute` = "K=3 + subgroup",
      `Min N (6 tasks)`          = 2 * n_min(3, 0.03, tasks = 6),
      `Min N (4 tasks)`          = 2 * n_min(3, 0.03, tasks = 4)
    )
  )

tbl_power_main <- tbl_power_main_df |>
  kbl(
    caption = "Minimum N for the main study by number of tasks (power = 0.80 unless noted, α = 0.05)",
    format.args = list(big.mark = ",")
  ) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)

tbl_power_main_2
