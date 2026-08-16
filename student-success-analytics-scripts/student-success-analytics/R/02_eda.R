# 02_eda.R
# Exploratory Data Analysis
# Examines distributions, categorical variables, and correlations.

library(tidyverse)
library(dplyr)

data <- read.csv("data/student_success_clean.csv")

# -------------------------
# Numeric distributions
# -------------------------
p <- ggplot(data, aes(final_score)) +
  geom_histogram(binwidth = 2, fill = "grey70", color = "black") +
  labs(title = "Distribution of Final Scores",
       x = "Final Score", y = "Count") +
  theme_minimal()
print(p)
ggsave("figures/distributions/final_score_distribution.png", p, width = 8, height = 5)

numeric_vars <- c(
  "daily_study_hours", "attendance_percentage",
  "homework_completion_rate", "sleep_hours",
  "screen_time_hours", "physical_activity_minutes",
  "motivation_score", "exam_anxiety_score"
)

for (var in numeric_vars) {
  p <- ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(bins = 30, fill = "grey70", color = "black") +
    labs(title = paste("Distribution of", var),
         x = var, y = "Count") +
    theme_minimal()
  print(p)
}

# -------------------------
# Grade distribution
# -------------------------
grade_distribution <- data |>
  mutate(grade_numeric = recode(
    grade, "A" = 4, "B" = 3, "C" = 2, "D" = 1, "F" = 0
  ))

p <- ggplot(grade_distribution, aes(x = grade_numeric)) +
  geom_histogram(binwidth = 1, fill = "grey70", color = "black") +
  labs(title = "Grade Distribution", x = "Grade (4.0 scale)", y = "Count") +
  theme_minimal()
print(p)
ggsave("figures/distributions/grade_distribution.png", p, width = 8, height = 5)

# -------------------------
# Parent education
# -------------------------
education_distribution <- data |>
  mutate(parent_education_numeric = recode(
    parent_education_level,
    "Bachelor" = 3, "Master" = 2, "High School" = 1
  ))

p <- ggplot(education_distribution, aes(x = parent_education_numeric)) +
  geom_histogram(binwidth = 1, fill = "grey70", color = "black") +
  labs(title = "Parent Education Level",
       x = "Education Level", y = "Count") +
  theme_minimal()
print(p)

# -------------------------
# Study environment
# -------------------------
study_environment_distribution <- data |>
  mutate(study_environment_numeric = recode(
    study_environment,
    "Noisy" = 3, "Moderate" = 2, "Quiet" = 1
  ))

p <- ggplot(study_environment_distribution, aes(x = study_environment_numeric)) +
  geom_histogram(binwidth = 1, fill = "grey70", color = "black") +
  labs(title = "Study Environment Distribution",
       x = "Noise Level", y = "Count") +
  theme_minimal()
print(p)

# -------------------------
# Correlation analysis
# -------------------------
categories <- data[, c(
  "final_score", "previous_score", "math_prev_score",
  "science_prev_score", "language_prev_score", "daily_study_hours",
  "attendance_percentage", "homework_completion_rate", "sleep_hours",
  "screen_time_hours", "physical_activity_minutes",
  "motivation_score", "exam_anxiety_score"
)]

cor_matrix <- cor(categories, use = "complete.obs")
print(cor_matrix)

png("figures/correlations/correlation_heatmap.png",
    width = 1200, height = 1000)
heatmap(
  cor_matrix,
  main = "Correlation Matrix",
  scale = "none",
  cexRow = 0.6,
  cexCol = 0.6,
  margins = c(8, 8),
  col = colorRampPalette(c("blue", "white", "red"))(100)
)
dev.off()
