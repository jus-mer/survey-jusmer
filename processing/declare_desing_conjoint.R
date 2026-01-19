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
               tidyverse,
               here)
set.seed(123)

# 2. Conjoint desing ------------------------------------------------------

# -----------------------------------------------------------------------------
# 1. Design features
# -----------------------------------------------------------------------------

N_subjects <- 1000
N_tasks    <- 8


# -----------------------------------------------------------------------------
# 2. Attributes and levels (10 attributes; 1 ternary, rest binary)
# -----------------------------------------------------------------------------

levels_list <- list(
  # Demographics / background
  gender      = c("Man", "Woman"),
  nationality = c("Chilean", "Non-Chilean"),
  ethnicity   = c("Indigenous", "Non-Indigenous"),
  
  # School background
  school_type = c("Municipal", "Subsidized private", "Private paid"),
  top100      = c("Yes", "No"),
  
  # CARIN (one attribute per component)
  control_academic  = c("Above average", "Average"),               # Control proxy (performance)
  attitude_study    = c("High dedication", "Irregular dedication"),# Attitude
  reciprocity_extra = c("Sustained involvement", "No involvement"),# Reciprocity
  identity_firstgen = c("First-generation", "Not first-generation"),# Identity
  need_income       = c("Low-income household", "Middle/upper-income household") # Need
)

# Conjectured utility function (additive DGP; adjust magnitudes as needed): this is the estimated effect size
conjoint_utility <- function(data) {
  data |>
    mutate(
      U =
        0.20  * (gender == "Woman") +
        (-0.25) * (nationality == "Non-Chilean") +
        (-0.15) * (ethnicity == "Indigenous") +
        (-0.30) * (need_income == "Low-income household") +
        
        0.35  * (control_academic == "Above average") +
        0.30  * (attitude_study == "High dedication") +
        0.15  * (reciprocity_extra == "Sustained involvement") +
        0.20  * (identity_firstgen == "First-generation") +
        
        # school_type: base = Municipal
        0.10  * (school_type == "Subsidized private") +
        0.25  * (school_type == "Private paid") +
        
        0.25  * (top100 == "Yes") +
        
        # idiosyncratic noise at profile level
        uij
    )
}

# -----------------------------------------------------------------------------
# 3. Measurement handler for YOUR REAL TASK: allocation 0-100 (sum to 100)
# -----------------------------------------------------------------------------
# The task: allocate 100% between 2 profiles based on their utilities.
# We map utilities to shares via softmax:
# share_ij = exp(scale*U_ij) / sum_j exp(scale*U_ij)
# alloc_ij = 100 * share_ij
#
# scale controls how "deterministic" allocations are (higher -> more extreme 0/100).
# For power diagnostics, start with scale = 1 and then test sensitivity.

conjoint_measurement_allocation <- function(data, utility_fn, scale = 1) {
  data2 <- utility_fn(data)
  
  data2 |>
    group_by(subject, task) |>
    mutate(
      expU  = exp(scale * U),
      share = expU / sum(expU),
      alloc = 100 * share
    ) |>
    ungroup() |>
    select(-expU) # keep U, share, alloc if you want
}

# -----------------------------------------------------------------------------
# 4. Model declaration (common backbone)
# -----------------------------------------------------------------------------

base_model <- declare_model(
  subject = add_level(N = N_subjects),
  task    = add_level(N = N_tasks, task = 1:N_tasks),
  profile = add_level(
    N       = 2,
    profile = 1:2,
    uij     = rnorm(N, sd = 1)
  )
)

# Helper: formula including all attributes
f_all <- choice ~ gender + nationality + ethnicity +
  school_type + top100 +
  control_academic + attitude_study + reciprocity_extra + identity_firstgen + need_income

f_all_alloc <- alloc ~ gender + nationality + ethnicity +
  school_type + top100 +
  control_academic + attitude_study + reciprocity_extra + identity_firstgen + need_income

# -----------------------------------------------------------------------------
# 5A. Declaration A: STANDARD conjoint (binary choice) using rdss measurement
# -----------------------------------------------------------------------------
declaration_choice <-
  base_model +
  declare_inquiry(
    handler    = conjoint_inquiries,
    levels_list = levels_list,
    utility_fn = conjoint_utility
  ) +
  declare_assignment(
    handler    = conjoint_assignment,
    levels_list = levels_list
    # fully randomized: no weights, no restrictions
  ) +
  declare_measurement(
    handler    = conjoint_measurement,
    utility_fn = conjoint_utility
  ) +
  declare_estimator(
    f_all,
    respondent.id = "subject",
    .method = amce
  )

# Diagnose
diagnosis_choice <- diagnose_design(declaration_choice, future.seed = FALSE)
diagnosis_choice

# -----------------------------------------------------------------------------
# 5B. Declaration B: YOUR TASK (allocation 0-100) with custom measurement
# -----------------------------------------------------------------------------
declaration_alloc <-
  base_model +
  declare_inquiry(
    handler     = conjoint_inquiries,
    levels_list = levels_list,
    utility_fn  = conjoint_utility
  ) +
  declare_assignment(
    handler     = conjoint_assignment,
    levels_list = levels_list
    # fully randomized: no weights, no restrictions
  ) +
  declare_measurement(
    handler     = conjoint_measurement_allocation,
    utility_fn  = conjoint_utility,
    scale       = 1
  ) +
  declare_estimator(
    f_all_alloc,
    respondent.id = "subject",
    .method = amce
  )

diagnosis_alloc <- diagnose_design(declaration_alloc, future.seed = FALSE)
diagnosis_alloc

# -----------------------------------------------------------------------------
# 6. Optional: Sensitivity to determinism in allocations (scale)
# -----------------------------------------------------------------------------
# If you suspect respondents will allocate more extremely, test larger scales:
scale_grid <- c(0.5, 1, 1.5, 2)

diag_alloc_by_scale <- map(
  scale_grid,
  ~ diagnose_design(
    base_model +
      declare_inquiry(handler = conjoint_inquiries, levels_list = levels_list, utility_fn = conjoint_utility) +
      declare_assignment(handler = conjoint_assignment, levels_list = levels_list) +
      declare_measurement(handler = conjoint_measurement_allocation, utility_fn = conjoint_utility, scale = .x) +
      declare_estimator(f_all_alloc, respondent.id = "subject", .method = amce),
    future.seed = FALSE
  )
)

names(diag_alloc_by_scale) <- paste0("scale_", scale_grid)
diag_alloc_by_scale
