# HR Analytics: Employee Attrition Prediction

This project, developed as part of a university assignment, aims to predict employee attrition using the HR Analytics dataset. The primary goal is to identify key factors influencing employee turnover and build predictive models to help organizations retain their workforce.

## Table of Contents
- [Dataset Overview](#dataset-overview)
- [Key Steps](#key-steps)
- [Tools and Libraries](#tools-and-libraries)
- [Conclusion](#conclusion)
- [Mentor](#Mentor)
- [Authors](#authors)

## Dataset Overview
The HR Analytics dataset contains 38 columns and 1480 rows, providing insights into both professional and personal aspects of employee lives, such as:

- **Professional**: Job satisfaction, training participation, promotions, etc.

- **Personal**: Marital status, work-life balance, commute distance, etc.

Key predictors include **Age**, **YearsInCurrentRole**, and **OverTime status**, among others.

## Key Steps
1. **Data Preparation**:
   - Removal of duplicates and irrelevant columns.
   - Correction of structural errors in categorical data.
   - Handling missing values through group-specific averages.
   - Creation of new features, such as `DistanceFromHomeGroup` and others.

2. **Exploratory Data Analysis**:
   - Insights into factors like attrition rates across departments, satisfaction levels, and the impact of commute distance.
   - Visualizations created for numerical and categorical variables using custom functions like `plot_histogram`, `plot_bar`, and `plot_boxplot`.

3. **Model Development**:
   - **Logistic Regression**: Selected predictors: `Age`, `OverTime`, and `YearsInCurrentRole`.
   - **Generalized Linear Model (GLM)**: Focused on `OverTime`, `MonthlyIncome`, and `JobInvolvement`.
   - **Random Forest**: Leveraged for its ability to handle complex interactions.
   - All models were fine-tuned using cross-validation and up-sampling techniques.

4. **Results**:
   - Logistic Regression emerged as the most effective model based on metrics like accuracy, sensitivity, and Kappa value.
   - Results comparison:
     | Metric            | Logistic Regression | GLM Model | Random Forest |
     |-------------------|---------------------|-----------|---------------|
     | Accuracy          | 73.13%              | 68.71%    | 72.45%        |
     | Balanced Accuracy | 69.36%              | 64.15%    | 62.93%        |
     | Sensitivity       | 74.9%               | 70.85%    | 76.92%        |
     | Specificity       | 63.83%              | 57.45%    | 48.94%        |
     | Kappa Value       | 0.2791              | 0.1954    | 0.2014        |

## Conclusion
The analysis revealed that logistic regression performed best for predicting employee attrition, indicating a predominantly linear relationship between variables. This project demonstrates how machine learning can guide HR strategies to improve employee satisfaction and reduce turnover.

## Tools and Libraries
- **R Programming**: Data analysis and modeling
- **Libraries**: `caret`, `ggplot2`, `stats`

## Mentor
- Dr. Branko Arsić

## Authors
- Jovan Radovanović (85/2018)
- Nemanja Trakić (130/2018)
