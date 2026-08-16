# Student Success Predictive Analytics

Exploratory and predictive analysis of ~1,000 student records to identify which
academic, behavioral, and demographic factors relate to student performance,
and to test whether those factors can predict pass/fail outcomes.

## Question

What combination of study habits, attendance, prior academic performance, and
behavioral factors (sleep, motivation, exam anxiety, etc.) best explains and
predicts student success?

## Dataset

~1,000 student records with 15+ variables spanning:
- **Academic:** final score, previous score, subject-level prior scores (math, science, language), grade, pass/fail
- **Behavioral:** daily study hours, attendance %, homework completion rate, sleep hours, screen time, physical activity
- **Psychological:** motivation score, exam anxiety score
- **Demographic:** parent education level, study environment

## Methods

- **Data cleaning:** missing value audit, IQR-based outlier detection and removal
- **EDA:** distribution analysis across all key variables, correlation heatmap to check for multicollinearity
- **Regression (built from scratch):** simple and multiple linear regression via the normal equations (matrix algebra), with hypothesis testing (t-tests) on coefficient significance, cross-validated against R's `lm()`
- **Classification:** logistic regression predicting pass/fail from attendance, evaluated with a confusion matrix, ROC curve, and AUC
- **Dimensionality reduction:** Principal Component Analysis via manual eigendecomposition of the correlation matrix
- **Clustering:** K-Means (k=4) on PCA-reduced data, with optimal k validated via the elbow (WSS) and silhouette methods
- **Association testing:** chi-square test and correspondence analysis on grade vs. study environment

## Key Results

| Model | Result |
|---|---|
| Logistic regression (pass/fail ~ attendance) | 92.2% accuracy, 98.6% precision, 93.4% recall |
| Linear regression (homework completion → final score) | Statistically significant relationship (t-test, α = 0.05) |
| PCA | Reduced 8 academic/behavioral features into principal components; students visually separate by grade along PC1/PC2 |
| K-Means clustering | 4 clusters, validated by elbow and silhouette methods |
| Correspondence analysis | Tested association between grade and study environment via chi-square |

## Tools

R, tidyverse, ggplot2, matlib, pROC, factoextra, FactoMineR

## How to Run

1. Clone this repo
2. Place `student_success_data.csv` in the project directory (or update the file path in the script)
3. Install required packages:
   ```r
   install.packages(c("tidyverse", "dplyr", "ggplot2", "matlib", "pROC", "factoextra", "FactoMineR"))
   ```
4. Run `student_success_analysis.R` top to bottom in RStudio or via `Rscript`

## Notes

This was originally built as a coursework project and has been cleaned up for
portfolio use — the outlier-detection logic (originally repeated per-variable)
was consolidated into a single reusable function, hardcoded local file paths
were removed, and sections were organized with clear headers.
