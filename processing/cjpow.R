#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: cjpowR for conjoint survey experiment abour deservingness and educational inequality
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

# ── 1. Cálculo puntual ──────────────────────────────────────────────
# Para un AMCE específico y un N dado (piloto N=600, 6 tareas, 2 perfiles)
n_efectivo_piloto <- 600 * 6 * 2  # = 7200 perfiles totales

# Atributo K=2 (situación financiera, nacionalidad, etc.)
cjpowr_amce(amce = 0.03, n = n_efectivo_piloto, levels = 2, alpha = 0.05)

# Atributo K=3 (rendimiento escolar, tipo de colegio)
cjpowr_amce(amce = 0.03, n = n_efectivo_piloto, levels = 3, alpha = 0.05)

# Para obtener N mínimo dado un poder objetivo
cjpowr_amce(amce = 0.03, power = 0.80, levels = 3, alpha = 0.05)

# ── 2. Curvas de poder vs N efectivo ───────────────────────────────
# Vectorizar para rangos de parámetros
cjpowr_amce_vec <- Vectorize(cjpowr_amce)

# Grid: distintos AMCEs × N efectivos
d <- expand.grid(
  amce      = c(0.02, 0.03, 0.05, 0.07),
  n_resp    = seq(200, 2500, by = 50),   # respondentes enteros
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

# Añadir N en respondentes (6 tareas × 2 perfiles)
res <- res |> mutate(
  n_resp = n / 12,
  amce_label = paste0("AMCE = ", amce * 100, " pp"),
  k_label = paste0("K = ", levels, " niveles")
)

# ── 3. Gráfico 1: Curvas de poder ──────────────────────────────────
library(ggdist)

g1 <- res |>
  ggplot(aes(x = n_resp, y = power,
             color = amce_label, linetype = k_label)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0.90, linetype = "dotted", color = "grey40") +
  # Marcar piloto y estudio principal
  geom_vline(xintercept = 600,  color = "#e74c3c", linetype = "dashed", alpha = 0.6) +
  geom_vline(xintercept = 1500, color = "#2c3e50", linetype = "dashed", alpha = 0.6) +
  annotate("text", x = 630,  y = 0.15, label = "Piloto\n(N=600)",
           color = "#e74c3c", size = 3, hjust = 0) +
  annotate("text", x = 1530, y = 0.15, label = "Estudio\n(N=1.500)",
           color = "#2c3e50", size = 3, hjust = 0) +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(labels = scales::comma_format(),
                     breaks = seq(0, 2500, 500)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    subtitle = "Conjoint distributivo sobre merecimiento (α = 0.05, K tareas = 6)",
    x        = "N respondentes",
    y        = "Poder estadístico",
    color    = "Tamaño de efecto",
    linetype = "Niveles del atributo",
    caption  = "Líneas grises: umbrales de poder 0.80 y 0.90."
  ) +
  ggdist::theme_ggdist()+
  theme(legend.position = "right")


# ── 4. Gráfico 2: MDE vs N respondentes ────────────────────────────
mde_df <- expand.grid(
  n_resp = seq(300, 2500, by = 50),
  levels = c(2, 3)
) |>
  mutate(
    n_ef  = n_resp * 12,
    mde   = sqrt((levels / 2) * (qnorm(0.975) + qnorm(0.80))^2 / n_ef),
    k_label = paste0("K = ", levels, " niveles")
  )

g2 <- mde_df |>
  ggplot(aes(x = n_resp, y = mde * 100, color = k_label)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 5, linetype = "dotted", color = "grey40") +
  geom_vline(xintercept = 600,  color = "#e74c3c", linetype = "dashed", alpha = 0.6) +
  geom_vline(xintercept = 1500, color = "#2c3e50", linetype = "dashed", alpha = 0.6) +
  annotate("text", x = 630,  y = 9.5, label = "Piloto (N=600)",
           color = "#e74c3c", size = 3, hjust = 0) +
  annotate("text", x = 1530, y = 9.5, label = "Estudio (N=1.500)",
           color = "#2c3e50", size = 3, hjust = 0) +
  annotate("text", x = 2400, y = 3.3, label = "3 pp", size = 3, color = "grey40") +
  annotate("text", x = 2400, y = 5.3, label = "5 pp", size = 3, color = "grey40") +
  scale_color_manual(values = c("#2c3e50", "#e74c3c")) +
  scale_x_continuous(labels = scales::comma_format()) +
  labs(
    title    = "Efecto mínimo detectable (MDE) según N",
    subtitle = "Poder = 0.80, α = 0.05, K tareas = 6, 2 perfiles por tarea",
    x        = "N respondentes",
    y        = "MDE (puntos porcentuales)",
    color    = "Niveles del atributo",
    caption  = "Líneas grises: MDE de 3 pp y 5 pp como referencias."
  ) +
  ggdist::theme_ggdist()+
  theme(legend.position = "right")


# ── 5. Gráfico 3: Type M error (exaggeration ratio) ────────────────
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
    subtitle = "Atributos K=3 — cuánto se sobreestima el efecto si N es insuficiente",
    x        = "N respondentes",
    y        = "Ratio de exageración",
    color    = "AMCE verdadero",
    caption  = "Ratio = 1 indica estimación sin sesgo de magnitud."
  ) +
  ggdist::theme_ggdist()+
  theme(legend.position = "right")
