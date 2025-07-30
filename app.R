install.packages("shiny")

library(shiny)
library(randomForest)
library(rpart)
library(caret)
library(ggplot2)

# Load data
data <- read.csv("StudentsPerformance.csv")

# Data preprocessing
data$parental.level.of.education <- as.factor(data$parental.level.of.education)
data$gender <- as.factor(data$gender)
data$race.ethnicity <- as.factor(data$race.ethnicity)
data$lunch <- as.factor(data$lunch)
data$test.preparation.course <- as.factor(data$test.preparation.course)

# Train-test split
set.seed(123)
trainIndex <- createDataPartition(data$math.score, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train models
lr_model <- lm(math.score ~ gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course + reading.score + writing.score, data = trainData)
dt_model <- rpart(math.score ~ gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course + reading.score + writing.score, data = trainData)
rf_model <- randomForest(math.score ~ gender + race.ethnicity + parental.level.of.education + lunch + test.preparation.course + reading.score + writing.score, data = trainData)

# Define UI
ui <- fluidPage(
  titlePanel("Student Performance Prediction"),
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Gender:", choices = unique(data$gender)),
      selectInput("race", "Race/Ethnicity:", choices = unique(data$race.ethnicity)),
      selectInput("parent_edu", "Parental Level of Education:", choices = unique(data$parental.level.of.education)),
      selectInput("lunch", "Lunch Type:", choices = unique(data$lunch)),
      selectInput("prep", "Test Preparation Course:", choices = unique(data$test.preparation.course)),
      numericInput("reading", "Reading Score:", value = 70, min = 0, max = 100),
      numericInput("writing", "Writing Score:", value = 70, min = 0, max = 100)
    ),
    mainPanel(
      h4("Predicted Math Score (by Model):"),
      verbatimTextOutput("prediction_text"),
      
      h4("Performance Category:"),
      verbatimTextOutput("performance"),
      
      h4("Model Metrics (RMSE & R²):"),
      tableOutput("model_metrics"),
      
      h4("Predicted Score Comparison Plot:"),
      plotOutput("prediction_plot"),
      
      h4("Help Message:"),
      textOutput("help_message"),
      
      h4("Export:"),
      downloadButton("downloadData", "Download Prediction Report")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive user input
  user_input <- reactive({
    data.frame(
      gender = factor(input$gender, levels = levels(data$gender)),
      race.ethnicity = factor(input$race, levels = levels(data$race.ethnicity)),
      parental.level.of.education = factor(input$parent_edu, levels = levels(data$parental.level.of.education)),
      lunch = factor(input$lunch, levels = levels(data$lunch)),
      test.preparation.course = factor(input$prep, levels = levels(data$test.preparation.course)),
      reading.score = input$reading,
      writing.score = input$writing
    )
  })
  
  # Prediction output
  output$prediction_text <- renderPrint({
    new_data <- user_input()
    pred_lr <- predict(lr_model, newdata = new_data)
    pred_dt <- predict(dt_model, newdata = new_data)
    pred_rf <- predict(rf_model, newdata = new_data)
    
    cat("Linear Regression:", round(pred_lr, 2), "\n")
    cat("Decision Tree:", round(pred_dt, 2), "\n")
    cat("Random Forest:", round(pred_rf, 2), "\n")
  })
  
  # Performance category
  output$performance <- renderPrint({
    new_data <- user_input()
    avg_score <- mean(c(
      predict(lr_model, newdata = new_data),
      predict(dt_model, newdata = new_data),
      predict(rf_model, newdata = new_data)
    ))
    
    if (avg_score < 50) {
      cat("Underperforming")
    } else {
      cat("Performing Well")
    }
  })
  
  # Model evaluation metrics
  output$model_metrics <- renderTable({
    pred_lr <- predict(lr_model, newdata = testData)
    pred_dt <- predict(dt_model, newdata = testData)
    pred_rf <- predict(rf_model, newdata = testData)
    
    data.frame(
      Model = c("Linear Regression", "Decision Tree", "Random Forest"),
      RMSE = c(RMSE(pred_lr, testData$math.score),
               RMSE(pred_dt, testData$math.score),
               RMSE(pred_rf, testData$math.score)),
      R2 = c(R2(pred_lr, testData$math.score),
             R2(pred_dt, testData$math.score),
             R2(pred_rf, testData$math.score))
    )
  })
  
  # Comparison plot
  output$prediction_plot <- renderPlot({
    new_data <- user_input()
    predictions <- data.frame(
      Model = c("Linear Regression", "Decision Tree", "Random Forest"),
      Score = c(
        predict(lr_model, newdata = new_data),
        predict(dt_model, newdata = new_data),
        predict(rf_model, newdata = new_data)
      )
    )
    
    ggplot(predictions, aes(x = Model, y = Score, fill = Model)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Predicted Math Scores by Model", y = "Score", x = "")
  })
  
  # Help message
  output$help_message <- renderText({
    " Click 'Download' to export your results."
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("prediction_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      new_data <- user_input()
      results <- data.frame(
        Gender = input$gender,
        Race = input$race,
        Parental_Education = input$parent_edu,
        Lunch = input$lunch,
        Test_Prep = input$prep,
        Reading_Score = input$reading,
        Writing_Score = input$writing,
        Pred_LR = predict(lr_model, newdata = new_data),
        Pred_DT = predict(dt_model, newdata = new_data),
        Pred_RF = predict(rf_model, newdata = new_data)
      )
      write.csv(results, file, row.names = FALSE)
    }
  )
}

# Run app
shinyApp(ui = ui, server = server)
