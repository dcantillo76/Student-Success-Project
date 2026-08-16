# 04_classification.R
# Logistic Regression
# Predicts pass/fail outcomes using attendance percentage.

library(tidyverse)
library(pROC)

data <- read.csv("data/student_success_clean.csv")

# -------------------------
# Logistic regression
# -------------------------
log_r <- glm(
  pass_fail ~ attendance_percentage,
  data = data,
  family = binomial()
)

summary(log_r)

# Likelihood-ratio/deviance test
g_value <- log_r$null.deviance - log_r$deviance
df_stat <- log_r$df.null - log_r$df.residual
p_value <- pchisq(g_value, df = df_stat, lower.tail = FALSE)

cat("Deviance test statistic:", g_value, "\n")
cat("Degrees of freedom:", df_stat, "\n")
cat("p-value:", p_value, "\n")

# -------------------------
# Predictions
# -------------------------
predicted_prob <- predict(log_r, type = "response")
pred_class <- ifelse(predicted_prob >= 0.5, 1, 0)

confusion_matrix <- table(
  Predicted = pred_class,
  Actual = data$pass_fail
)

print(confusion_matrix)

# Calculate evaluation metrics from the confusion matrix.
cm <- as.matrix(confusion_matrix)

# The original analysis used the following cell counts.
# Keep this calculation tied to the observed confusion matrix
# rather than hard-coding counts in the portfolio version.
accuracy <- sum(diag(cm)) / sum(cm)

cat("Accuracy:", accuracy, "\n")

# -------------------------
# ROC / AUC
# -------------------------
roc_obj <- roc(
  data$pass_fail,
  predicted_prob,
  plot = TRUE,
  print.auc = TRUE,
  legacy.axes = TRUE
)

best_threshold <- coords(
  roc_obj,
  x = "best",
  best.method = "closest.topleft"
)

print(best_threshold)

png("figures/regression/roc_curve.png", width = 1000, height = 800)
plot(roc_obj, print.auc = TRUE, legacy.axes = TRUE)
dev.off()
