# 01_data_cleaning.R
# Student Success Predictive Analytics
# Loads the raw dataset, checks data quality, handles sleep-hour outliers,
# and saves a cleaned dataset for downstream analysis.

library(tidyverse)
library(dplyr)

data <- read.csv("data/student_success_data.csv")

# -------------------------
# Data quality checks
# -------------------------
summary(data)
missing_values <- sapply(data, function(x) sum(is.na(x)))
print(missing_values)

# -------------------------
# Outlier detection using IQR
# -------------------------
outlier_cols <- c(
  "final_score", "previous_score", "daily_study_hours",
  "homework_completion_rate", "physical_activity_minutes",
  "motivation_score", "attendance_percentage", "exam_anxiety_score",
  "science_prev_score", "math_prev_score", "language_prev_score"
)

outlier_summary <- lapply(outlier_cols, function(col) {
  x <- data[[col]]
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_value <- IQR(x, na.rm = TRUE)
  lower <- q1 - 1.5 * iqr_value
  upper <- q3 + 1.5 * iqr_value
  outliers <- x[x < lower | x > upper]
  data.frame(variable = col, n_outliers = length(outliers))
}) |> bind_rows()

print(outlier_summary)

# Sleep hours was the variable for which the original analysis
# removed observations outside the IQR bounds.
sleep_q1 <- quantile(data$sleep_hours, 0.25, na.rm = TRUE)
sleep_q3 <- quantile(data$sleep_hours, 0.75, na.rm = TRUE)
sleep_iqr <- IQR(data$sleep_hours, na.rm = TRUE)
sleep_lower <- sleep_q1 - 1.5 * sleep_iqr
sleep_upper <- sleep_q3 + 1.5 * sleep_iqr

data_clean <- data |>
  filter(sleep_hours >= sleep_lower, sleep_hours <= sleep_upper)

# Save cleaned data
write.csv(data_clean, "data/student_success_clean.csv", row.names = FALSE)

cat("Rows before cleaning:", nrow(data), "\n")
cat("Rows after cleaning:", nrow(data_clean), "\n")
