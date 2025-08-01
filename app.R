


library(shiny)
library(shinydashboard)
library(randomForest)
library(rpart)
library(caret)
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)

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

model_metrics <- data.frame(
  Model = c("Linear Regression", "Decision Tree", "Random Forest"),
  RMSE = c(rmse_lm, rmse_tree, rmse_rf),
  R2 = c(r2_lm, r2_tree, r2_rf)
)

# UI
ui <- dashboardPage(
  
  dashboardHeader(
    titleWidth = 400,  # Widen title area
    title = tags$div(
      style = "font-size: 18px; white-space: normal; overflow: visible;",
      tags$strong("📊 Student Performance Prediction Dashboard")
    )
  ),
  
  dashboardSidebar(
    # Sidebar font styling + mobile responsiveness
    tags$head(tags$style(HTML('
      .main-sidebar .sidebar-menu a {
        font-size: 16px !important;
      }
    '))),
    tags$head(tags$script(HTML('
      $(document).on("shiny:connected", function() {
        if ($(window).width() < 768) {
          $("body").addClass("sidebar-collapse");
        }
      });
    '))),
    
    sidebarMenu(
      menuItem(" Student Score Prediction", tabName = "predict", icon = icon("calculator")),
      menuItem(" Compare Prediction Models", tabName = "comparison", icon = icon("chart-bar")),
      menuItem(" Explore Student Data", tabName = "eda", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "predict",
              fluidRow(
                box(title = "Input Student Data", status = "primary", solidHeader = TRUE, width = 4,
                    selectInput("gender", "Gender:", choices = levels(data$gender)),
                    selectInput("race", "Race/Ethnicity:", choices = levels(data$race.ethnicity)),
                    selectInput("parent_edu", "Parental Level of Education:", choices = levels(data$parental.level.of.education)),
                    selectInput("lunch", "Lunch Type:", choices = levels(data$lunch)),
                    selectInput("prep_course", "Test Preparation Course:", choices = levels(data$test.preparation.course)),
                    numericInput("reading", "Reading Score:", value = 50, min = 0, max = 100),
                    numericInput("writing", "Writing Score:", value = 50, min = 0, max = 100)
                ),
                box(title = "Prediction Result", status = "success", solidHeader = TRUE, width = 8,
                    verbatimTextOutput("prediction_output"),
                    h4("Performance Category:"),
                    verbatimTextOutput("performance_category"),
                    textOutput("modelAccuracyMessage"),
                    h3("📊 Model Performance Overview"),
                    plotOutput("model_bar_chart"),
                    h4("Help Message:"),
                    textOutput("help_message"),
                    tags$br(),
                    tags$div(style = "text-align: left;", downloadButton("downloadData", "Download", class = "btn btn-success"))
                )
              )
      ),
      tabItem(tabName = "comparison",
              fluidRow(
                box(title = "Model Metrics Table", status = "info", solidHeader = TRUE, width = 12,
                    tableOutput("model_metrics")
                )
              ),
              fluidRow(
                box(title = "RMSE Comparison", status = "info", solidHeader = TRUE, width = 6,
                    plotOutput("rmse_plot")
                ),
                box(title = "R² Comparison", status = "info", solidHeader = TRUE, width = 6,
                    plotOutput("r2_plot")
                )
              )
      ),
      tabItem(tabName = "eda",
              fluidRow(
                box(title = "Score Distributions", status = "info", solidHeader = TRUE, width = 6,
                    plotOutput("score_dist")
                ),
                box(title = "Math Score by Gender", status = "info", solidHeader = TRUE, width = 6,
                    plotOutput("boxplot_gender")
                )
              ),
              fluidRow(
                box(title = "Math Score by Parental Education", status = "info", solidHeader = TRUE, width = 6,
                    plotOutput("bar_parent_edu")
                ),
                box(title = "Average Scores by Test Preparation", status = "info", solidHeader = TRUE, width = 6,
                    plotOutput("avg_score_prep")
                )
              )
      )
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
  
  output$prediction_output <- renderPrint({ prediction() })
  
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
  
  output$modelAccuracyMessage <- renderText({
    best_model <- model_metrics$Model[which.min(model_metrics$RMSE)]
    paste("✅ Based on input, the most accurate model is:", best_model)
  })
  
  output$model_bar_chart <- renderPlot({
    ggplot(prediction(), aes(x = Model, y = PredictedScore, fill = Model)) +
      geom_bar(stat = "identity", width = 0.6) +
      labs(title = "Predicted Scores by Model", y = "Predicted Math Score") +
      theme_minimal() +
      theme(legend.position = "none")
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
    filename = function() { paste("prediction_result", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(prediction(), file, row.names = FALSE)
    }
  )
  
  output$model_metrics <- renderTable({ model_metrics })
  
  output$rmse_plot <- renderPlot({
    ggplot(model_metrics, aes(x = Model, y = RMSE, fill = Model)) +
      geom_bar(stat = "identity", width = 0.6) +
      labs(title = "Model RMSE Comparison", y = "RMSE") +
      theme_minimal()
  })
  
  output$r2_plot <- renderPlot({
    ggplot(model_metrics, aes(x = Model, y = R2, fill = Model)) +
      geom_bar(stat = "identity", width = 0.6) +
      labs(title = "Model R² Comparison", y = "R² Score") +
      theme_minimal()
  })
  
  output$score_dist <- renderPlot({
    scores <- data.frame(
      Reading = data$reading.score,
      Writing = data$writing.score,
      Math = data$math.score
    )
    scores_melt <- melt(scores)
    ggplot(scores_melt, aes(x = value, fill = variable)) +
      geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
      theme_minimal() +
      labs(title = "Score Distributions", x = "Score", y = "Frequency")
  })
  
  output$boxplot_gender <- renderPlot({
    ggplot(data, aes(x = gender, y = math.score, fill = gender)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Math Score by Gender", x = "Gender", y = "Math Score")
  })
  
  output$bar_parent_edu <- renderPlot({
    ggplot(data, aes(x = parental.level.of.education, y = math.score, fill = parental.level.of.education)) +
      geom_bar(stat = "summary", fun = mean) +
      theme_minimal() +
      labs(title = "Average Math Score by Parental Education", x = "Parental Education", y = "Average Math Score") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$avg_score_prep <- renderPlot({
    avg_scores <- data %>%
      group_by(test.preparation.course) %>%
      summarise(
        Math = mean(math.score),
        Reading = mean(reading.score),
        Writing = mean(writing.score)
      ) %>%
      pivot_longer(cols = c(Math, Reading, Writing), names_to = "Subject", values_to = "Score")
    
    ggplot(avg_scores, aes(x = test.preparation.course, y = Score, fill = Subject)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Average Scores by Test Preparation", x = "Test Preparation Course", y = "Score") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

