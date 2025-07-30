# -------------------------------
# 📦 Step 1: Install packages (once)
# -------------------------------
install.packages("tidyverse")
install.packages("caret")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("dplyr")
install.packages("reshape2")
install.packages("Metrics")
install.packages("rpart")
install.packages("randomForest")

# -------------------------------
# 📚 Step 2: Load libraries
# -------------------------------
library(tidyverse)
library(caret)
library(ggplot2)
library(corrplot)
library(dplyr)
library(reshape2)
library(Metrics)
library(rpart)
library(randomForest)

# -------------------------------
# 📂 Step 3: Load data
# -------------------------------
data <- read.csv("StudentsPerformance.csv")

cat("Number of rows: ", nrow(data), "\n")
cat("Column names:\n")
print(colnames(data))
cat("Preview:\n")
head(data)

# Convert categorical columns
data$gender <- as.factor(data$gender)
data$race.ethnicity <- as.factor(data$race.ethnicity)
data$parental.level.of.education <- as.factor(data$parental.level.of.education)
data$lunch <- as.factor(data$lunch)
data$test.preparation.course <- as.factor(data$test.preparation.course)

# -------------------------------
# 📊 Step 4: Correlation plot
# -------------------------------
numeric_data <- data[, c("math.score", "reading.score", "writing.score")]
correlation <- cor(numeric_data)
corrplot(correlation, method = "circle")

# -------------------------------
# ✂️ Step 5: Train/test split (80/20)
# -------------------------------
set.seed(123)
sample_index <- sample(1:nrow(data), 0.8 * nrow(data))
trainData <- data[sample_index, ]
testData <- data[-sample_index, ]

cat("Training rows:", nrow(trainData), "\n")
cat("Testing rows:", nrow(testData), "\n")

# -------------------------------
# 🔁 Step 6: Linear Regression Model
# -------------------------------
model <- lm(math.score ~ reading.score + writing.score + gender + lunch + test.preparation.course, data = trainData)

# Predict on test data (Linear Regression)
predictions <- predict(model, newdata = testData)
testData$predicted_math_score_lr <- round(predictions)

# -------------------------------
# 📉 Step 7: Residuals and Performance
# -------------------------------
testData$residual <- testData$math.score - testData$predicted_math_score_lr

testData$performance_category <- cut(
  testData$math.score,
  breaks = c(-Inf, 60, 85, Inf),
  labels = c("Needs Improvement", "Average", "High Performer")
)

testData$underperforming <- testData$residual < 0

underperforming_needs_help <- subset(
  testData,
  performance_category == "Needs Improvement" & residual < 0
)

# -------------------------------
# 📈 Step 8: Visualizations
# -------------------------------

# Bar plot - performance category
ggplot(testData, aes(x = performance_category)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Performance Category Count", x = "Category", y = "Number of Students")

# Histogram - residuals
ggplot(testData, aes(x = residual)) +
  geom_histogram(binwidth = 1, fill = "pink", color = "black") +
  labs(title = "Residual Distribution", x = "Residual", y = "Frequency")

# Scatter plot - actual vs predicted (linear regression)
data_to_plot <- data.frame(actual = testData$math.score, predicted = testData$predicted_math_score_lr)
plot(data_to_plot$actual, data_to_plot$predicted,
     col = "blue", pch = 19,
     xlab = "Actual Math Score",
     ylab = "Predicted Math Score",
     main = "Actual vs Predicted Math Scores")
abline(0, 1, col = "red", lwd = 2)

# Pie chart - gender distribution
gender_counts <- table(testData$gender)
pie(gender_counts,
    labels = paste(names(gender_counts), "\n", gender_counts),
    main = "Gender Distribution",
    col = c("lightblue", "pink"))

# Stacked bar - lunch vs performance
ggplot(testData, aes(x = lunch, fill = performance_category)) +
  geom_bar(position = "dodge") +
  labs(title = "Lunch Type vs Performance Category", x = "Lunch Type", y = "Count") +
  theme_minimal()

# Violin plot - test prep vs math score
ggplot(testData, aes(x = test.preparation.course, y = math.score, fill = test.preparation.course)) +
  geom_violin(trim = FALSE) +
  labs(title = "Math Score by Test Prep", x = "Test Prep", y = "Math Score") +
  theme_minimal()

# Density plot - subject scores
scores_melted <- melt(testData[, c("math.score", "reading.score", "writing.score")])
ggplot(scores_melted, aes(x = value, color = variable)) +
  geom_density(size = 1.2) +
  labs(title = "Density Plot of Scores", x = "Score", y = "Density") +
  theme_minimal()

# Horizontal bar - race vs performance
ggplot(testData, aes(x = race.ethnicity, fill = performance_category)) +
  geom_bar() +
  labs(title = "Performance by Race", x = "Race", y = "Count") +
  coord_flip() +
  theme_minimal()

# Scatter plot - reading vs writing by gender
ggplot(testData, aes(x = reading.score, y = writing.score)) +
  geom_point(aes(color = gender), alpha = 0.6) +
  facet_wrap(~gender) +
  labs(title = "Reading vs Writing Score by Gender", x = "Reading", y = "Writing") +
  theme_minimal()

# -------------------------------
# 👁️ Step 9: View Environment DataFrames
# -------------------------------

# Decision Tree
tree_model <- rpart(math.score ~ ., data = trainData, method = "anova")
pred_tree <- predict(tree_model, testData)
testData$predicted_math_score_tree <- round(pred_tree)

# Random Forest
rf_model <- randomForest(math.score ~ ., data = trainData)
pred_rf <- predict(rf_model, testData)
testData$predicted_math_score_rf <- round(pred_rf)

# View testData with all predictions in Environment panel
View(testData)

# -------------------------------
# 📊 Step 10: Linear Model Accuracy
# -------------------------------
rmse_lr <- rmse(testData$math.score, predictions)
r2_lr <- summary(model)$r.squared

cat("Linear Regression RMSE:", rmse_lr, "\n")
cat("Linear Regression R-squared:", r2_lr, "\n")

# -------------------------------
# 🌳 Step 11: Decision Tree & Random Forest Metrics
# -------------------------------
rmse_tree <- rmse(testData$math.score, pred_tree)
r2_tree <- 1 - sum((testData$math.score - pred_tree)^2) / sum((testData$math.score - mean(testData$math.score))^2)

rmse_rf <- rmse(testData$math.score, pred_rf)
r2_rf <- 1 - sum((testData$math.score - pred_rf)^2) / sum((testData$math.score - mean(testData$math.score))^2)

cat("Decision Tree RMSE:", rmse_tree, "\n")
cat("Decision Tree R-squared:", r2_tree, "\n")
cat("Random Forest RMSE:", rmse_rf, "\n")
cat("Random Forest R-squared:", r2_rf, "\n")

# -------------------------------
# 📊 Step 12: Compare Models - Bar Charts
# -------------------------------
metrics <- data.frame(
  Model = c("Linear Regression", "Decision Tree", "Random Forest"),
  RMSE = c(round(rmse_lr, 2), round(rmse_tree, 2), round(rmse_rf, 2)),
  R2 = c(round(r2_lr, 3), round(r2_tree, 3), round(r2_rf, 3))
)

# RMSE Comparison Plot
ggplot(metrics, aes(x = Model, y = RMSE, fill = Model)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ggtitle("RMSE Comparison of Models")

# R² Comparison Plot
ggplot(metrics, aes(x = Model, y = R2, fill = Model)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ggtitle("R² Comparison of Models")


