# Author: Research Team
# Purpose: Evaluate models on test set and compare performance

library(tidyverse)
library(mgcv)
library(knitr)
library(ggplot2)

# Load data and models
# Determine data data and figures directories
if (file.exists("data/processed_data.RData")) {
  root_dir <- "."
} else if (file.exists("../data/processed_data.RData")) {
  root_dir <- ".."
} else {
  stop("Required data/models missing. Please run scripts 01 and 02 first.")
}

data_dir <- file.path(root_dir, "data")
tables_dir <- file.path(root_dir, "tables")
figures_dir <- file.path(root_dir, "figures")

load(file.path(data_dir, "processed_data.RData"))
load(file.path(data_dir, "models.RData"))

# --- Helper Function for Evaluation ---
calc_metrics <- function(actual, predicted) {
  rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
  mae  <- mean(abs(actual - predicted), na.rm = TRUE)
  return(c(RMSE = rmse, MAE = mae))
}

# --- 1. Linear Regression Evaluation ---
pred_lm <- predict(lm_model, newdata = test_data)
metrics_lm <- calc_metrics(test_data$bg.1.00, pred_lm)

# --- 2. GAM Evaluation ---
pred_gam <- predict(gam_model, newdata = test_data)
metrics_gam <- calc_metrics(test_data$bg.1.00, pred_gam)

# --- 3. NLS Evaluation ---
if (!is.null(nls_model)) {
  pred_nls <- tryCatch({
    predict(nls_model, newdata = test_data)
  }, error = function(e) {
    warning("NLS Prediction failed")
    return(rep(NA, nrow(test_data)))
  })
  metrics_nls <- calc_metrics(test_data$bg.1.00, pred_nls)
} else {
  pred_nls <- rep(NA, nrow(test_data))
  metrics_nls <- c(RMSE = NA, MAE = NA)
}

# --- Comparison Table ---
results <- data.frame(
  Model = c("Linear Regression", "GAM", "Nonlinear Regression"),
  RMSE = c(metrics_lm["RMSE"], metrics_gam["RMSE"], metrics_nls["RMSE"]),
  MAE  = c(metrics_lm["MAE"],  metrics_gam["MAE"],  metrics_nls["MAE"])
)

# Print Text Table
print(kable(results, digits = 3, caption = "Test Set Performance"))

# Save Results to CSV
# Save Results to CSV
write.csv(results, file.path(tables_dir, "model_comparison.csv"), row.names = FALSE)

# --- Visualization ---

# 1. Bar Chart of Errors
results_long <- results %>%
  pivot_longer(cols = c("RMSE", "MAE"), names_to = "Metric", values_to = "Value") %>%
  filter(!is.na(Value))

p1 <- ggplot(results_long, aes(x = Model, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  facet_wrap(~Metric, scales = "free_y") +
  theme_minimal() +
  labs(title = "Model Performance Metric Comparison", y = "Error Value") +
  theme(legend.position = "none")

ggsave(file.path(figures_dir, "model_comparison_bar.png"), p1, width = 8, height = 6)

# 2. Observed vs Predicted Plot (For best performing model, or all)
# Let's plot GAM vs Observed as it is often the most visually interesting
comparison_df <- data.frame(
  Time = 1:nrow(test_data),
  Observed = test_data$bg.1.00,
  Predicted_GAM = pred_gam,
  Predicted_LM = pred_lm
)

p2 <- ggplot(comparison_df, aes(x = Observed)) +
  geom_point(aes(y = Predicted_LM, color = "Linear"), alpha = 0.3) +
  geom_point(aes(y = Predicted_GAM, color = "GAM"), alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "Observed vs Predicted", y = "Predicted BG", color = "Model")

ggsave(file.path(figures_dir, "obs_vs_pred.png"), p2, width = 8, height = 6)

cat("Evaluation complete. Results saved to tables/ and figures/.\n")
