# 03_regression.R
# Linear Regression
# Includes a from-scratch simple regression implementation,
# goodness-of-fit calculations, and multiple linear regression.

library(tidyverse)
library(matlib)

data <- read.csv("data/student_success_clean.csv")

# -------------------------
# Simple linear regression from scratch
# -------------------------
simple_regression_plot <- function(x, y, xlab, ylab) {
  plot(
    x, y,
    xlab = xlab, ylab = ylab,
    pch = 16,
    main = paste(xlab, "vs", ylab)
  )

  n <- length(x)
  x_avg <- sum(x) / n
  y_avg <- sum(y) / n
  Sxy <- sum((x - x_avg) * (y - y_avg))
  Sxx <- sum((x - x_avg)^2)

  beta1_hat <- Sxy / Sxx
  beta0_hat <- y_avg - beta1_hat * x_avg

  lines(
    c(min(x), max(x)),
    beta0_hat + beta1_hat * c(min(x), max(x)),
    col = "blue"
  )

  return(list(beta0_hat = beta0_hat, beta1_hat = beta1_hat))
}

# Study hours -> final score
x <- data$daily_study_hours
y <- data$final_score

reg1 <- simple_regression_plot(
  x, y, "Daily Study Hours", "Final Score"
)

print(reg1)

expected_score <- reg1$beta1_hat * 3 + reg1$beta0_hat
cat("Expected score at 3 study hours:", expected_score, "\n")

# Homework completion -> final score
x <- data$homework_completion_rate
y <- data$final_score

reg2 <- simple_regression_plot(
  x, y, "Homework Completion Rate", "Final Score"
)

print(reg2)

# -------------------------
# Goodness-of-fit calculations
# -------------------------
n <- length(y)
p <- 1

X <- matrix(
  c(rep(1, n), x),
  nrow = n,
  ncol = p + 1
)

beta_hat <- solve(t(X) %*% X, t(X) %*% y)
predicted <- X %*% beta_hat
residuals <- y - predicted

r_squared <- 1 - sum(residuals^2) / sum((y - mean(y))^2)
r_squared_adj <- 1 - ((1 - r_squared) * (n - 1)) / (n - (p + 1))
rse <- sqrt(sum(residuals^2) / (n - (p + 1)))

se_beta <- rse * sqrt(diag(inv(t(X) %*% X)))

alpha <- 0.05
critical_value <- qt(
  p = alpha / 2,
  df = n - (p + 1),
  lower.tail = FALSE
)

ci <- c(
  beta_hat[2] - critical_value * se_beta[2],
  beta_hat[2] + critical_value * se_beta[2]
)

t_stat <- beta_hat[2] / se_beta[2]

cat("R-squared:", r_squared, "\n")
cat("Adjusted R-squared:", r_squared_adj, "\n")
cat("Residual standard error:", rse, "\n")
cat("95% CI for slope:", ci, "\n")
cat("t-statistic:", t_stat, "\n")

# -------------------------
# Multiple linear regression
# -------------------------
x1 <- data$daily_study_hours
x2 <- data$exam_anxiety_score
x3 <- data$motivation_score
y <- data$final_score

n <- length(y)

X <- matrix(
  c(rep(1, n), x1, x2, x3),
  nrow = n,
  ncol = 4
)

beta_hat <- solve(t(X) %*% X, t(X) %*% y)
print(beta_hat)

model <- lm(
  final_score ~ daily_study_hours + motivation_score + exam_anxiety_score,
  data = data
)

summary(model)
