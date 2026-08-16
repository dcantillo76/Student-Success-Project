# 05_pca_clustering.R
# PCA, K-Means clustering, and correspondence analysis.

library(tidyverse)
library(FactoMineR)
library(factoextra)

data <- read.csv("data/student_success_clean.csv")

# Preserve categorical variables before selecting PCA features.
grades <- as.factor(data$grade)
study_environment <- data$study_environment

# -------------------------
# PCA
# -------------------------
pca_data <- data[, c(
  "final_score", "previous_score",
  "math_prev_score", "science_prev_score",
  "language_prev_score", "attendance_percentage",
  "daily_study_hours", "homework_completion_rate"
)]

X <- scale(pca_data)
S <- cor(X)
E <- eigen(S)

loadings <- E$vectors
rownames(loadings) <- colnames(pca_data)

pcs <- X %*% loadings
variance_explained <- 100 * E$values / sum(E$values)
cumulative_variance <- cumsum(variance_explained)

print(loadings)
print(variance_explained)

png("figures/clustering/pca_variance.png", width = 1000, height = 800)
plot(
  cumulative_variance,
  type = "b",
  col = "blue",
  ylim = c(0, 100),
  xlab = "Component",
  ylab = "Percentage of Variance",
  main = "PCA Variance Explained"
)
lines(variance_explained, type = "b", col = "red")
dev.off()

# PC1 vs PC2
png("figures/clustering/pca_pc1_pc2.png", width = 1000, height = 800)
plot(
  pcs[, 1], pcs[, 2],
  xlab = "PC1",
  ylab = "PC2",
  main = "Principal Component Scores"
)
dev.off()

# -------------------------
# K-Means clustering
# -------------------------
pca_model <- PCA(pca_data, scale.unit = TRUE, graph = FALSE)

km <- kmeans(
  pca_model$ind$coord[, 1:2],
  centers = 4,
  nstart = 20
)

print(km)

png("figures/clustering/kmeans_pca.png", width = 1000, height = 800)
print(
  fviz_pca_ind(
    pca_model,
    habillage = as.factor(km$cluster)
  )
)
dev.off()

# Compare candidate cluster counts.
wss_plot <- fviz_nbclust(
  pca_model$ind$coord[, 1:2],
  FUNcluster = kmeans,
  nstart = 20,
  method = "wss",
  k.max = 10
)
ggsave(
  "figures/clustering/kmeans_wss.png",
  wss_plot,
  width = 8,
  height = 6
)

silhouette_plot <- fviz_nbclust(
  pca_model$ind$coord[, 1:2],
  FUNcluster = kmeans,
  nstart = 20,
  method = "silhouette",
  k.max = 10
)
ggsave(
  "figures/clustering/kmeans_silhouette.png",
  silhouette_plot,
  width = 8,
  height = 6
)

# -------------------------
# Correspondence analysis
# -------------------------
CA1 <- grades
CA2 <- study_environment

grade_environment_table <- table(CA1, CA2)
print(grade_environment_table)

print(chisq.test(grade_environment_table))

res.ca <- CA(grade_environment_table, graph = TRUE)

print(res.ca$row$coord)
print(res.ca$col$coord)
