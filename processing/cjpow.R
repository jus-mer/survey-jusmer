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
# For a specific AMCE and a given N (pilot N=600, 6 tasks, 2 profiles)
n_efectivo_piloto <- 600 * 6 * 2  # = 7200 total profiles

# K=2 attribute (Need, Control, Reciprocity, Attitude, Sex)
cjpowr_amce(amce = 0.03, n = n_efectivo_piloto, levels = 2, alpha = 0.05)

# K=3 attribute (Effort, Identity)
cjpowr_amce(amce = 0.03, n = n_efectivo_piloto, levels = 3, alpha = 0.05)

# Minimum N for a target power
cjpowr_amce(amce = 0.03, power = 0.80, levels = 3, alpha = 0.05)

# ── 2. Power curves vs effective N ─────────────────────────────────
# Vectorize over parameter ranges
cjpowr_amce_vec <- Vectorize(cjpowr_amce)

# Grid: different AMCEs × effective N
d <- expand.grid(
  amce      = c(0.02, 0.03, 0.05, 0.07),
  n_resp    = seq(200, 2500, by = 50),   # whole respondents
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

# ── 3. Figure 1: Power curves ──────────────────────────────────────
library(ggdist)

g1 <- res |>
  ggplot(aes(x = n_resp, y = power,
             color = amce_label, linetype = k_label)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0.90, linetype = "dotted", color = "grey40") +
  # Mark pilot and main study
  geom_vline(xintercept = 600,  color = "#e74c3c", linetype = "dashed", alpha = 0.6) +
  geom_vline(xintercept = 1500, color = "#2c3e50", linetype = "dashed", alpha = 0.6) +
  annotate("text", x = 630,  y = 0.15, label = "Pilot\n(N=600)",
           color = "#e74c3c", size = 3, hjust = 0) +
  annotate("text", x = 1530, y = 0.15, label = "Main study\n(N=1,500)",
           color = "#2c3e50", size = 3, hjust = 0) +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(labels = scales::comma_format(),
                     breaks = seq(0, 2500, 500)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    subtitle = "Distributive conjoint on deservingness (α = 0.05, tasks = 6)",
    x        = "N respondents",
    y        = "Statistical power",
    color    = "Effect size",
    linetype = "Attribute levels",
    caption  = "Grey lines: power thresholds 0.80 and 0.90."
  ) +
  ggdist::theme_ggdist()+
  theme(legend.position = "right")


# ── 4. Figure 2: MDE vs N respondents ──────────────────────────────
mde_df <- expand.grid(
  n_resp = seq(300, 2500, by = 50),
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
  geom_vline(xintercept = 600,  color = "#e74c3c", linetype = "dashed", alpha = 0.6) +
  geom_vline(xintercept = 1500, color = "#2c3e50", linetype = "dashed", alpha = 0.6) +
  annotate("text", x = 630,  y = 9.5, label = "Pilot (N=600)",
           color = "#e74c3c", size = 3, hjust = 0) +
  annotate("text", x = 1530, y = 9.5, label = "Main study (N=1,500)",
           color = "#2c3e50", size = 3, hjust = 0) +
  annotate("text", x = 2400, y = 3.3, label = "3 pp", size = 3, color = "grey40") +
  annotate("text", x = 2400, y = 5.3, label = "5 pp", size = 3, color = "grey40") +
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
  geom_vline(xintercept = 600,  color = "#e74c3c", linetype = "dashed", alpha = 0.6) +
  geom_vline(xintercept = 1500, color = "#2c3e50", linetype = "dashed", alpha = 0.6) +
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


# ── 6. Table 1: MDE by pilot size ──────────────────────────────────
library(kableExtra)

mde_at <- function(n_resp, levels, tasks = 6, power = 0.80, alpha = 0.05) {
  n_prof <- n_resp * tasks * 2
  sqrt((levels / 2) * (qnorm(1 - alpha / 2) + qnorm(power))^2 / n_prof) * 100
}

pilot_sizes <- c(500, 600, 700)

tbl_power_pilot <- tibble(
  `N respondents`      = pilot_sizes,
  `N profiles`         = pilot_sizes * 6 * 2,
  `MDE, K=2 attribute` = paste0(round(mde_at(pilot_sizes, 2), 1), " pp"),
  `MDE, K=3 attribute` = paste0(round(mde_at(pilot_sizes, 3), 1), " pp")
) |>
  kbl(
    caption = "Minimum detectable effect by pilot size (power = 0.80, α = 0.05)",
    format.args = list(big.mark = ",")
  ) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)


# ── 7. Table 2: Minimum N for the main study ───────────────────────
n_min <- function(levels, delta, tasks = 6, power = 0.80, alpha = 0.05) {
  z <- (qnorm(1 - alpha / 2) + qnorm(power))^2
  n_prof <- (levels / 2) * z / delta^2
  ceiling(n_prof / (tasks * 2))
}

tbl_power_main <- tibble(
  Target = c(
    "Main effects, K=2",
    "Main effects, K=3",
    "Heterogeneity (binary moderator)",
    "Main effects, K=3, power = 0.90"
  ),
  `Minimum AMCE` = c("3 pp", "3 pp", "3 pp", "3 pp"),
  `Most demanding attribute` = c(
    "Binary",
    "Effort / Identity",
    "K=3 + subgroup",
    "Effort / Identity"
  ),
  `Minimum N` = c(
    n_min(2, 0.03),
    n_min(3, 0.03),
    2 * n_min(3, 0.03),
    n_min(3, 0.03, power = 0.90)
  )
) |>
  kbl(
    caption = "Minimum N for the main study (power = 0.80, α = 0.05, 6 tasks)",
    format.args = list(big.mark = ",")
  ) |>
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)