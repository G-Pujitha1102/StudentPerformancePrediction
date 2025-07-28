# 🎓 Student Performance Prediction 📊

This project analyzes and predicts student performance using R. It uses the *Students Performance Dataset* to explore various factors influencing student scores, visualize trends, build predictive models, and identify students who may need help.

---

## 📁 Files

- StudentsPerformance.csv — Dataset from Kaggle.
- modeling.R — Complete R script for data analysis, visualization, and modeling.
- README.md — Project documentation.

---

## 🎯 Project Objectives

- Load and preprocess the dataset.
- Perform Exploratory Data Analysis (EDA).
- Visualize patterns and insights across gender, parental education, and preparation.
- Build predictive models:
  - **Linear Regression**
  - **Decision Tree**
  - **Random Forest**
- Evaluate models using RMSE and R-squared metrics.
- Identify underperforming students and those who may need help.
- Plot actual vs predicted scores and analyze residuals.

---

## 📌 Technologies Used

- *Language:* R
- *Libraries:*  
  tidyverse, caret, ggplot2, corrplot, dplyr, reshape2, Metrics, rpart, randomForest

---

## 📊 Model Performance Comparison

| Model            | RMSE   | R-squared |
|------------------|--------|-----------|
| Linear Regression| 5.88   | 0.87      |
| Decision Tree     | 7.78   | 0.74      |
| Random Forest     | 6.46   | 0.82      |

---

📊 Sample Visualizations
Performance Category - Bar Plot<img width="491" height="470" alt="Screenshot 2025-07-27 222735" src="https://github.com/user-attachments/assets/c61f876b-2b38-449d-a1bb-a2c4a4f4438b" />
Scatter Plot - Actual vs Predicted Math Scores<img width="501" height="471" alt="actualvsprediction" src="https://github.com/user-attachments/assets/6e1f5e76-4d6d-4b3b-ab79-9d7245282c21" />
Pie Chart - Gender Distribution<img width="884" height="720" alt="genderdistribution" src="https://github.com/user-attachments/assets/2e0b91ec-e671-4e62-8fa0-137f159f05fb" />
Histogram - Residuals<img width="502" height="475" alt="Screenshot " src="https://github.com/user-attachments/assets/b7382ffb-fb28-478e-bb57-7311dabb7c3b" />
Scatter Plot - Reading vs Writing by Gender<img width="840" height="721" alt="scatterplot" src="https://github.com/user-attachments/assets/228cbcd4-74b4-482b-98d6-7581d201ed74" />
Stacked Bar Plot - Lunch Type vs Performance<img width="883" height="711" alt="lunchvsperformance" src="https://github.com/user-attachments/assets/b52eb36f-171f-45dd-bacc-a80f92fcd40d" />
Horizontal Bar Plot - Race vs Performance<img width="884" height="720" alt="horizontalbargraph" src="https://github.com/user-attachments/assets/c2a5e8f9-153e-4c8e-ad1a-514f0be6bca9" />
RMSE Comparison - Bar Plot<img width="898" height="715" alt="RMSE" src="https://github.com/user-attachments/assets/da62e37a-7f3b-4573-8cfe-cf4dabbd7ad1" />
R² Comparison - Bar Plot<img width="891" height="714" alt="R2" src="https://github.com/user-attachments/assets/310b8445-2ccd-43c7-9095-582b04ced935" />


## 🚀 How to Run

1. Clone the repo:  
   `git clone https://github.com/G-Pujitha1102/StudentPerformancePrediction.git`

2. Open RStudio and load `modeling.R`.

3. Run the script to perform EDA, modeling, and visualizations step-by-step.

---

## 📎 Dataset Source

[Students Performance Dataset on Kaggle](https://www.kaggle.com/datasets/spscientist/students-performance-in-exams)

---

---

## 💡 Challenges Faced & What I Learned

- Handling missing or inconsistent data in R.
- Understanding how different factors (gender, lunch, education) impact scores.
- Learned how to visualize data effectively using `ggplot2`.
- Faced difficulty resolving GitHub merge conflicts and learned version control through practice.
- Understood the importance of model evaluation metrics like RMSE and R².


## 🙋‍♀️ Author

G. Pujitha  
🎓 B.Tech - Computer Science Engineering  
[GitHub Profile](https://github.com/G-Pujitha1102)

---

## 🔮 Future Improvements

- Add interactive dashboard using R Shiny.
- Explore classification models (Pass/Fail).
- Add more advanced models like Gradient Boosting.
- Expand analysis to writing and reading score predictions.
