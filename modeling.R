# Install necessary packages (if not already installed)
install.packages("tidyverse")
install.packages("caret")
install.packages("ggplot2")
install.packages("corrplot")

# Load libraries
library(tidyverse)
library(caret)
library(corrplot)
library(ggplot2)

# ✅ Step 1: Load the data
data <- read.csv("C:/Users/Dell/Downloads/StudentsPerformance.csv")

# ✅ Step 2: Check structure and contents
cat("Number of rows: ", nrow(data), "\n")
cat("Column names:\n")
print(colnames(data))
cat("Preview:\n")
head(data)

# ✅ Step 3: Convert categorical variables to factors (use exact column names)
data$gender <- as.factor(data$gender)
data$race.ethnicity <- as.factor(data$race.ethnicity)
data$parental.level.of.education <- as.factor(data$parental.level.of.education)
data$lunch <- as.factor(data$lunch)
data$test.preparation.course <- as.factor(data$test.preparation.course)

# ✅ Step 4: Correlation plot of numeric columns (use exact names)
numeric_data <- data[, c("math.score", "reading.score", "writing.score")]
correlation <- cor(numeric_data)
corrplot(correlation, method = "circle")

# ✅ Step 5: Boxplot of Math Score by Gender
ggplot(data, aes(x = gender, y = math.score)) + 
  geom_boxplot(fill = "skyblue") + 
  labs(title = "Math Score by Gender")

# ✅ Step 6: Split into training (80%) and testing (20%) sets
set.seed(123)
sample_index <- sample(1:nrow(data), 0.8 * nrow(data))
trainData <- data[sample_index, ]
testData <- data[-sample_index, ]

cat("Training rows:", nrow(trainData), "\n")
cat("Testing rows:", nrow(testData), "\n")

# ✅ Step 7: Train linear regression model (fix variable names)
model <- lm(math.score ~ reading.score + writing.score + gender + lunch + test.preparation.course, data = trainData)

# ✅ Step 8: Predictions
predictions <- predict(model, newdata = testData)
actual <- testData$math.score

# ✅ Step 9: Ensure predictions and actual values are valid
if (length(predictions) == 0 || length(actual) == 0) {
  cat("❌ Error: Predictions or actual values are empty.\n")
} else {
  cat("✅ Predictions and actual values are populated correctly.\n")
  print(head(predictions))
  print(head(actual))
}

# ✅ Step 10: Calculate RMSE
rmse <- sqrt(mean((predictions - actual)^2))
cat("📊 Root Mean Squared Error (RMSE):", rmse, "\n")

# ✅ Step 11: Plot actual vs predicted math scores
data_to_plot <- data.frame(actual = actual, predicted = predictions)
plot(data_to_plot$actual, data_to_plot$predicted, 
     col = "blue", pch = 19,
     xlab = "Actual Math Score", 
     ylab = "Predicted Math Score",
     main = "Actual vs Predicted Math Scores")
abline(0, 1, col = "red", lwd = 2)

# Make sure actual and predicted scores are numeric
data$math.score <- as.numeric(as.character(data$math.score))
data$predicted.math.score <- as.numeric(as.character(data$predicted.math.score))

# Create performance categories based on actual scores
data$performance_category <- cut(
  data$math.score,
  breaks = c(-Inf, 60, 85, Inf),
  labels = c("Needs Improvement", "Average", "High Performer")
)

# Calculate residuals (actual - predicted)
data$residual <- data$math.score - data$predicted.math.score

# Flag underperforming students (actual < predicted)
data$underperforming <- data$residual < 0

# Summary of performance categories
print(table(data$performance_category))

# View students needing improvement
needs_help <- subset(data, performance_category == "Needs Improvement")
head(needs_help)

# View students underperforming compared to prediction
underperformers <- subset(data, underperforming == TRUE)
head(underperformers)

# Optional visualization
library(ggplot2)

ggplot(data, aes(x = performance_category)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Number of Students by Performance Category", x = "Category", y = "Count")

ggplot(data, aes(x = residual)) +
  geom_histogram(binwidth = 5, fill = "salmon", color = "black") +
  labs(title = "Distribution of Residuals (Actual - Predicted)", x = "Residual", y = "Count")



length(data$predicted.math.score)
data$predicted.math.score <- predict(model, newdata = data)
data$predicted.math.score <- predict(model, newdata = data)
length(data$predicted.math.score)