#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Declare desing for conjoint survey experiment abour deservingness and educational inequality
# Responsable: Research assistant
# Executive Summary: This script contains the code to perform declare desing analysis (Blair et al., 2020)
# Date: January 19, 2026
#******************************************************************************************************************************************************

options(scipen=999)
rm(list = ls())

# 1. Packages  -----------------------------------------------------
if (! require("pacman")) install.packages("pacman")

pacman::p_load(DeclareDesign,
               rdss,
               cjoint,
               estimatr,
               tidyverse,
               here)
set.seed(123)

# 2. Conjoint desing ------------------------------------------------------

# ── 1. Función que define el diseño para un N dado ──────────────────

conjoint_design <- function(
    N_resp,
    K_tasks    = 6,
    amce_need  = 0.13,   # situación financiera — efecto Gilgen (2022)
    amce_merit = 0.06,   # rendimiento escolar
    amce_id    = 0.03,   # identidad (nacionalidad / origen indígena)
    sd_outcome = 18      # SD del % asignado (estimación conservadora)
) {

  N_profiles <- N_resp * K_tasks * 2

  # Modelo poblacional
  modelo <- declare_model(
    N = N_profiles,

    # Atributos K=2 (0/1 con prob uniforme)
    sit_financiera  = rbinom(N, 1, 0.5),  # 1 = dificultad económica
    educ_parental   = rbinom(N, 1, 0.5),  # 1 = sin educ. superior
    nacionalidad    = rbinom(N, 1, 0.5),  # 1 = extranjero/a
    origen_indigena = rbinom(N, 1, 0.5),  # 1 = origen indígena
    sexo            = rbinom(N, 1, 0.5),  # 1 = mujer

    # Atributos K=3 (dummy: nivel alto vs. nivel bajo)
    rendimiento  = sample(c(-1L, 0L, 1L), N, replace = TRUE),
    tipo_colegio = sample(c(-1L, 0L, 1L), N, replace = TRUE),

    # IDs anidados
    respondente_id = rep(seq_len(N_resp), each = K_tasks * 2),
    tarea_id       = rep(rep(seq_len(K_tasks), each = 2), N_resp),

    # Outcome potencial: share base 50 pp + efectos + ruido
    Y_latente = 50 +
      (amce_need  * 100) * sit_financiera +
      (amce_merit * 100) * (rendimiento == 1L) +
      (amce_id    * 100) * origen_indigena +
      rnorm(N, mean = 0, sd = sd_outcome)
  )

  # Medición: clampear entre 0 y 100
  medicion <- declare_measurement(
    Y = pmin(pmax(Y_latente, 0), 100)
  )

  # Estimador: OLS con SE cluster por respondente
  # Atributo focal: sit_financiera (efecto más grande, referencia principal)
  estimador <- declare_estimator(
    Y ~ sit_financiera + educ_parental + nacionalidad +
        origen_indigena + sexo +
        I(rendimiento == 1L) + I(rendimiento == -1L) +
        I(tipo_colegio == 1L) + I(tipo_colegio == -1L),
    model    = lm_robust,
    clusters = respondente_id,
    term     = "sit_financiera",
    label    = "AMCE sit. financiera"
  )

  modelo + medicion + estimador
}


# ── 2. Diagnosticar para un rango de N respondentes ────────────────

ns <- c(500, 600, 700, 1000, 1200, 1500, 2000)

diagnosticos <- map_dfr(ns, function(n) {
  d    <- conjoint_design(N_resp = n)
  diag <- diagnose_design(d, sims = 500, bootstrap_sims = 0)
  diag$diagnosands |>
    mutate(N_resp = n)
}, .progress = TRUE)


# ── 3. Gráfico: poder simulado vs N respondentes ───────────────────

diagnosticos |>
  ggplot(aes(x = N_resp, y = power)) +
  geom_line(color = "#2c3e50", linewidth = 1) +
  geom_point(color = "#2c3e50", size = 3) +
  geom_ribbon(
    aes(ymin = power - 1.96 * se_power,
        ymax = power + 1.96 * se_power),
    alpha = 0.15, fill = "#2c3e50"
  ) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0.90, linetype = "dotted", color = "grey40") +
  geom_vline(xintercept = 600,  color = "#e74c3c", linetype = "dashed", alpha = 0.7) +
  geom_vline(xintercept = 1500, color = "#2980b9", linetype = "dashed", alpha = 0.7) +
  annotate("text", x = 620,  y = 0.08, label = "Piloto\n(N=600)",
           color = "#e74c3c", size = 3, hjust = 0) +
  annotate("text", x = 1520, y = 0.08, label = "Estudio\n(N=1.500)",
           color = "#2980b9", size = 3, hjust = 0) +
  annotate("text", x = 2000, y = 0.82, label = "Poder = 0.80",
           color = "grey40", size = 3, hjust = 1) +
  scale_y_continuous(
    labels = scales::percent_format(),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1)
  ) +
  scale_x_continuous(breaks = ns) +
  labs(
    title    = "Poder estadístico simulado (DeclareDesign)",
    subtitle = "Outcome continuo (% beca asignado) — AMCE sit. financiera = 13 pp — SD = 18 pp\n6 tareas × 2 perfiles — SE cluster por respondente",
    x        = "N respondentes",
    y        = "Poder estadístico",
    caption  = "500 simulaciones por N. Banda = IC 95%.\nEstimador: OLS con SE cluster por respondente (lm_robust)."
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())


# ── 4. Tabla resumen de diagnósticos ───────────────────────────────

diagnosticos |>
  select(N_resp, power, se_power, bias, rmse) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print()

