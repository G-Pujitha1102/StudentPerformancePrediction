


library(shiny)
library(randomForest)
library(rpart)
library(caret)
library(ggplot2)

# Load data  
data <- read.csv("StudentsPerformance.csv")

# Convert relevant columns to factors
data$gender <- as.factor(data$gender)
data$race.ethnicity <- as.factor(data$race.ethnicity)
data$parental.level.of.education <- as.factor(data$parental.level.of.education)
data$lunch <- as.factor(data$lunch)
data$test.preparation.course <- as.factor(data$test.preparation.course)

# Train models
set.seed(123)
model_lm <- train(math.score ~ ., data = data, method = "lm")
model_tree <- rpart(math.score ~ ., data = data)
model_rf <- randomForest(math.score ~ ., data = data)

# Model metrics
rmse_lm <- RMSE(predict(model_lm), data$math.score)
rmse_tree <- RMSE(predict(model_tree), data$math.score)
rmse_rf <- RMSE(predict(model_rf), data$math.score)

r2_lm <- R2(predict(model_lm), data$math.score)
r2_tree <- R2(predict(model_tree), data$math.score)
r2_rf <- R2(predict(model_rf), data$math.score)

# Create a dataframe to store model metrics
model_metrics <- data.frame(
  Model = c("Linear Regression", "Decision Tree", "Random Forest"),
  RMSE = c(rmse_lm, rmse_tree, rmse_rf),
  R2 = c(r2_lm, r2_tree, r2_rf)
)

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      h4 { font-weight: bold; color: #1a1a1a; }
      .accuracy-highlight {
        color: green;
        font-weight: bold;
        font-size: 16px;
        padding-top: 10px;
        display: block;
      }
    "))
  ),
  
  titlePanel("📊 Student Performance Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Gender:", choices = levels(data$gender)),
      selectInput("race", "Race/Ethnicity:", choices = levels(data$race.ethnicity)),
      selectInput("parent_edu", "Parental Level of Education:", choices = levels(data$parental.level.of.education)),
      selectInput("lunch", "Lunch Type:", choices = levels(data$lunch)),
      selectInput("prep_course", "Test Preparation Course:", choices = levels(data$test.preparation.course)),
      numericInput("reading", "Reading Score:", value = 50, min = 0, max = 100),
      numericInput("writing", "Writing Score:", value = 50, min = 0, max = 100)
    ),
    
    mainPanel(
      h4(strong("📈 Predicted Math Score (by Model):")),
      verbatimTextOutput("prediction_output"),
      
      h4(strong("📌 Performance Category:")),
      verbatimTextOutput("performance_category"),
      
      h4(strong("📊 Model Metrics (RMSE & R²):")),
      tableOutput("model_metrics"),
      
      span(textOutput("modelAccuracyMessage"), class = "accuracy-highlight"),
      
      h4(strong("📉 Predicted Score Comparison Plot:")),
      plotOutput("score_plot"),
      
      h4(strong("❓ Help Message:")),
      textOutput("help_message"),
      
      downloadButton("downloadData", "📥 Download Prediction Report")
    )
  )
)

# Server
server <- function(input, output) {
  user_input <- reactive({
    data.frame(
      gender = factor(input$gender, levels = levels(data$gender)),
      race.ethnicity = factor(input$race, levels = levels(data$race.ethnicity)),
      parental.level.of.education = factor(input$parent_edu, levels = levels(data$parental.level.of.education)),
      lunch = factor(input$lunch, levels = levels(data$lunch)),
      test.preparation.course = factor(input$prep_course, levels = levels(data$test.preparation.course)),
      reading.score = input$reading,
      writing.score = input$writing
    )
  })
  
  prediction <- reactive({
    df <- user_input()
    pred_lm <- predict(model_lm, df)
    pred_tree <- predict(model_tree, df)
    pred_rf <- predict(model_rf, df)
    data.frame(
      Model = c("Linear Regression", "Decision Tree", "Random Forest"),
      PredictedScore = c(pred_lm, pred_tree, pred_rf)
    )
  })
  
  output$prediction_output <- renderPrint({
    print(prediction())
  })
  
  output$performance_category <- renderText({
    avg_score <- mean(prediction()$PredictedScore)
    if (avg_score < 40) {
      "Underperforming"
    } else if (avg_score < 70) {
      "Average"
    } else {
      "High Performing"
    }
  })
  
  output$model_metrics <- renderTable({
    model_metrics
  })
  
  output$modelAccuracyMessage <- renderText({
    best_model <- model_metrics$Model[which.min(model_metrics$RMSE)]
    paste("✅ Based on input, the most accurate model is:", best_model)
  })
  
  output$score_plot <- renderPlot({
    ggplot(prediction(), aes(x = Model, y = PredictedScore, fill = Model)) +
      geom_bar(stat = "identity", width = 0.6) +
      labs(title = "Predicted Math Scores by Model", y = "Score") +
      theme_minimal()
  })
  
  output$help_message <- renderText({
    avg_score <- mean(prediction()$PredictedScore)
    if (avg_score < 40) {
      "Needs Improvement. Please work hard to improve your scores."
    } else if (avg_score < 70) {
      "Average performance. Keep pushing to improve!"
    } else {
      "Well performed! Keep it up! 🎉"
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("prediction_result", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(prediction(), file, row.names = FALSE)
    }
  )
}

# Run app
shinyApp(ui = ui, server = server)


