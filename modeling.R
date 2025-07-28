# Install necessary packages (only run once)
install.packages("tidyverse")
install.packages("caret")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("dplyr")
install.packages("reshape2")


# Load libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(corrplot)
library(dplyr)
library(reshape2)

# Step 2: Read the dataset
data <- read.csv("C:/Users/Dell/Downloads/StudentsPerformance.csv")



# Check structure and contents
cat("Number of rows: ", nrow(data), "\n")
cat("Column names:\n")
print(colnames(data))
cat("Preview:\n")
head(data)

# Convert categorical variables to factors
data$gender <- as.factor(data$gender)
data$race.ethnicity <- as.factor(data$race.ethnicity)
data$parental.level.of.education <- as.factor(data$parental.level.of.education)
data$lunch <- as.factor(data$lunch)
data$test.preparation.course <- as.factor(data$test.preparation.course)

# Correlation plot of numeric columns
numeric_data <- data[, c("math.score", "reading.score", "writing.score")]
correlation <- cor(numeric_data)
corrplot(correlation, method = "circle")



# Step 3: Split into training and testing sets (80/20)
sample_index <- sample(1:nrow(data), 0.8 * nrow(data))
trainData <- data[sample_index, ]
testData <- data[-sample_index, ]

cat("Training rows:", nrow(trainData), "\n")
cat("Testing rows:", nrow(testData), "\n")

# Train linear regression model
model <- lm(math.score ~ reading.score + writing.score + gender + lunch + test.preparation.course, data = trainData)


# Step 4: Build model on training data
model <- lm(math.score ~ reading.score + writing.score, data = trainData)

# Step 5: Predict math scores on test data
predictions <- predict(model, newdata = testData)
testData$predicted_math_score <- round(predictions)

# Step 6: Calculate residuals
testData$residual <- testData$math.score - testData$predicted_math_score

# Step 7: Categorize performance
testData$performance_category <- cut(
  testData$math.score,
  breaks = c(-Inf, 60, 85, Inf),
  labels = c("Needs Improvement", "Average", "High Performer")
)

# Step 8: Identify underperforming students
testData$underperforming <- testData$residual < 0

# Step 9: Environmental panel - students needing help
underperforming_needs_help <- subset(
  testData,
  performance_category == "Needs Improvement" & residual < 0
)

# Step 10: Bar plot - performance category
ggplot(testData, aes(x = performance_category)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Performance Category Count", x = "Category", y = "Number of Students")

# Step 11: Histogram - residuals
ggplot(testData, aes(x = residual)) +
  geom_histogram(binwidth = 1, fill = "pink", color = "black") +
  labs(title = "Residual Distribution", x = "Residual", y = "Frequency")

# Step 12: Scatter plot - actual vs predicted
data_to_plot <- data.frame(actual = testData$math.score, predicted = testData$predicted_math_score)
plot(data_to_plot$actual, data_to_plot$predicted,
     col = "blue", pch = 19,
     xlab = "Actual Math Score",
     ylab = "Predicted Math Score",
     main = "Actual vs Predicted Math Scores")
abline(0, 1, col = "red", lwd = 2)

# Step 13: Pie chart - gender distribution
gender_counts <- table(testData$gender)
pie(gender_counts,
    labels = paste(names(gender_counts), "\n", gender_counts),
    main = "Gender Distribution",
    col = c("lightblue", "pink"))

# Step 14: Stacked bar - lunch vs performance
ggplot(testData, aes(x = lunch, fill = performance_category)) +
  geom_bar(position = "dodge") +
  labs(title = "Lunch Type vs Performance Category", x = "Lunch Type", y = "Count") +
  theme_minimal()

# Step 15: Violin plot - test prep vs math score
ggplot(testData, aes(x = test.preparation.course, y = math.score, fill = test.preparation.course)) +
  geom_violin(trim = FALSE) +
  labs(title = "Math Score by Test Prep", x = "Test Prep", y = "Math Score") +
  theme_minimal()

# Step 16: Density plot - subject scores
scores_melted <- melt(testData[, c("math.score", "reading.score", "writing.score")])
ggplot(scores_melted, aes(x = value, color = variable)) +
  geom_density(size = 1.2) +
  labs(title = "Density Plot of Scores", x = "Score", y = "Density") +
  theme_minimal()

# Step 17: Horizontal bar - race vs performance
ggplot(testData, aes(x = race.ethnicity, fill = performance_category)) +
  geom_bar() +
  labs(title = "Performance by Race", x = "Race", y = "Count") +
  coord_flip() +
  theme_minimal()

# Step 18: Scatter plot - reading vs writing by gender
ggplot(testData, aes(x = reading.score, y = writing.score)) +
  geom_point(aes(color = gender), alpha = 0.6) +
  facet_wrap(~gender) +
  labs(title = "Reading vs Writing Score by Gender", x = "Reading", y = "Writing") +
  theme_minimal()

# Step 19: View in Environment Panel
View(trainData)
View(testData)
View(underperforming_needs_help)


# Test commit - check push
