# 🎓 Student Performance Prediction (R Project)

This project predicts students' **math scores** using various demographic and educational factors such as gender, parental education, lunch type, and test preparation course. It also identifies which students are performing well and which ones may need academic support.

---

## 📁 Project Structure

```
StudentPerformancePrediction/
├── StudentsPerformance.csv      # Dataset used
├── modeling.R                   # Main R script (data analysis & modeling)
├── README.md                    # This file
```

---

## 🧠 Objectives

- Predict student **math scores** using linear regression.
- Calculate prediction error (RMSE).
- Compare **actual vs predicted** scores visually.
- Classify students into:
  - High Performers
  - Average
  - Needs Improvement
- Identify students **underperforming** compared to model expectations.

---

## 🛠️ Technologies Used

- **R Language**
- **Libraries**:  
  - `tidyverse`  
  - `caret`  
  - `ggplot2`  
  - `corrplot`

---

## 🚀 How to Run the Project

1. Clone or download this repository.
2. Open the `modeling.R` file in RStudio.
3. Make sure you have the following packages installed:

```r
install.packages("tidyverse")
install.packages("caret")
install.packages("ggplot2")
install.packages("corrplot")
```

4. Load the dataset `StudentsPerformance.csv`.
5. Run the script step-by-step in RStudio.

---

## 📊 Results

- **RMSE** (Root Mean Squared Error): ~5.88  
- Most students fall in the **"Average"** performance category.
- Students are flagged if they **underperform compared to predicted scores**.

**Visualizations include:**

- Scatter plot of **Actual vs Predicted** math scores  
- Histogram of **Residuals (Actual - Predicted)**  
- Bar plot of **Performance Categories**

---

## 📌 Insights

- Students with **no test preparation** and **lower parental education** tend to score lower.
- **Lunch type** (standard vs free/reduced) has a noticeable impact.
- This model can help teachers **identify students needing support early**.

---

## 📈 Future Improvements

- Try advanced models like Random Forest, Ridge, or Lasso Regression.
- Build an interactive Shiny app to explore predictions dynamically.
- Include reading and writing scores as features for better accuracy.

---

## 👤 Author

- **Name:** G.Pujitha  
- **Note:** This is my first data science project using R and GitHub!  
- Thank you for checking it out 😊

---

## 🌟 Feedback

If you found this helpful or have suggestions, feel free to open an issue or fork the project.  
Let’s make education data work better for everyone!
