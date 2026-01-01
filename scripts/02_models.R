# Author: Research Team
# Purpose: Define and fit Linear, GAM, and NLS models

library(tidyverse)
library(mgcv)
#library(minpack.lm) # For nlsLM robustness if needed

# Load processed data
# Determine data directory
if (file.exists("C:/Users/Acer/OneDrive/Desktop/Research/bg_gam_paper/data/processed_data.RData")) {
  data_path <- "C:/Users/Acer/OneDrive/Desktop/Research/bg_gam_paper/data/processed_data.RData"
} else if (file.exists("data/processed_data.RData")) {
  data_path <- "data/processed_data.RData"
} else if (file.exists("../data/processed_data.RData")) {
  data_path <- "../data/processed_data.RData"
} else {
  stop("Processed data file not found! Please run 'scripts/01_preprocessing.R' first.")
}

load(data_path)

# --- Model 1: Linear Regression (Baseline) ---
cat("Fitting Linear Regression...\n")
lm_model <- lm(
  bg.1.00 ~ bg_mean + hr_mean + hr_missing +
            insulin_sum + carbs_log + steps_log + cals_log,
  data = train_data
)

# --- Model 2: Generalized Additive Model (GAM) ---
cat("Fitting GAM...\n")
gam_model <- gam(
  bg.1.00 ~
    s(bg_mean, k = 10) +
    s(hr_mean, k = 10) +
    hr_missing +
    s(insulin_sum, k = 10) +
    s(carbs_log, k = 10) +
    s(steps_log, k = 10) +
    s(cals_log, k = 10),
  data = train_data,
  method = "REML"
)

# --- Model 3: Nonlinear Regression (NLS) ---
cat("Fitting NLS...\n")

# Initial parameter guesses based on Linear Model coefficients
start_vals <- list(
  b0 = mean(train_data$bg.1.00, na.rm = TRUE),
  b1 = 1,                                      
  b2 = 0.1,                                    
  b3 = coef(lm_model)["carbs_log"],          
  b4 = abs(coef(lm_model)["insulin_sum"]),   
  b5 = coef(lm_model)["hr_mean"],            
  b6 = coef(lm_model)["hr_missing"],         
  b7 = coef(lm_model)["steps_log"]            
)

nls_model <- tryCatch({
  nls(
    bg.1.00 ~
      b0 +
      b1 * exp(b2 * bg_mean_sc) +
      b3 * carbs_log -
      b4 * insulin_sum +
      b5 * hr_mean +
      b6 * hr_missing -
      b7 * steps_log,
    data = train_data,
    start = start_vals,
    algorithm = "port",
    control = nls.control(maxiter = 300, warnOnly = TRUE)
  )
}, error = function(e) {
  message("NLS fit failed: ", e$message)
  return(NULL)
})


# Save models
data_dir <- dirname(data_path)
output_path <- file.path(data_dir, "models.RData")
save(lm_model, gam_model, nls_model, file = output_path)
cat("Models saved to:", normalizePath(output_path), "\n")
