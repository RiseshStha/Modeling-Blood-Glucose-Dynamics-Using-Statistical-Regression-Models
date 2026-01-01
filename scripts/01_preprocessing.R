# Author: Research Team
# Purpose: Preprocessing blood glucose data for statistical modeling
# 
# Steps:
# 1. Load data
# 2. Impute missing heart rate
# 3. Create log-transformed features
# 4. Save processed data (optional, or return object)

library(tidyverse)
library(zoo)

# 1. Load Data
# Using relative path assuming script is run from project root or scripts/ folder
# 1. Load Data
# Try loading from project root
input_path <- "C:/Users/Acer/OneDrive/Desktop/Research/bg_gam_paper/data/bg_data.csv" 


if(!file.exists(input_path)) {
  stop("Could not find bg_data.csv in 'data/' or '../data/'")
}

data <- read.csv(input_path)

# 2. Handling Missing Data (Heart Rate)
# Median imputation due to skewness
hr_median <- median(data$hr_mean, na.rm = TRUE)

data <- data %>%
  mutate(
    hr_missing = ifelse(is.na(hr_mean), 1, 0),
    hr_mean = ifelse(is.na(hr_mean), hr_median, hr_mean)
  )

# 3. Log Transformations for Right-Skewed Variables
data <- data %>%
  mutate(
    steps_log = log1p(steps_sum),
    carbs_log = log1p(carbs_sum),
    cals_log  = log1p(cals_sum)
  )

# 4. Time-Aware Train-Test Split (70/30)
n <- nrow(data)
split_idx <- floor(0.7 * n)

# Train/Test scaling for NLS stability
# We do this here to ensure consistent feature engineering
train_raw <- data[1:split_idx, ]

bg_mean_mean <- mean(train_raw$bg_mean, na.rm=TRUE)
bg_mean_sd   <- sd(train_raw$bg_mean, na.rm=TRUE)

data <- data %>%
  mutate(bg_mean_sc = (bg_mean - bg_mean_mean) / bg_mean_sd)

train_data <- data[1:split_idx, ]
test_data  <- data[(split_idx + 1):n, ]

cat("Preprocessing Complete.\n")
cat("Training Samples:", nrow(train_data), "\n")
cat("Testing Samples: ", nrow(test_data), "\n")

# Save processed objects for other scripts to use
# Save to the same data directory where input was found
data_dir <- dirname(input_path)
output_path <- file.path(data_dir, "processed_data.RData")
save(data, train_data, test_data, file = output_path)
cat("Processed data saved to:", normalizePath(output_path), "\n")
