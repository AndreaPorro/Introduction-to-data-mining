# 📊 HR Analytics: Data Mining & Insights
This last project contains a data mining project focused on **Human Resources**.  The project explores employee data to uncover patterns in performance, compensation, and retention.

## 🎯 Project Objectives
The main goal of this study is to extract actionable insights from HR data through a structured statistical approach: 

- **Exploratory Data Analysis (EDA)**: Understanding variables, handling missing values, and answering key business questions (e.g., gender pay gap, impact of special projects). 
- **Dimensionality Reduction (PCA)**: Reducing complex features into principal components to visualize variable correlations. 
- **Cluster Analysis**: Segmenting employees into distinct groups based on performance, engagement, and salary profiles. 

## 🛠️ Tech Stack
- **Language**: R
- **Libraries**: `ggplot2` (Visualization), `DataExplorer` (Automated EDA), `corrplot` (Correlation analysis), `psych` (Statistical analysis), `factoextra` (PCA & Clustering). 

## 📈 Key Findings

- **Salary Drivers**: A positive correlation was found between the number of Special Projects and salary, with certain departments (ID 3 and 4) being more involved in these projects. 
- **Gender Equality**: The analysis suggests no significant discrimination in salary based on gender within the analyzed dataset. 
- **Employee Segmentation**: Through K-Means Clustering, 3 distinct employee profiles were identified:
  -   Cluster 1: Average-performing employees with moderate salaries.
  -   Cluster 2: High-performing, highly engaged employees with significant project involvement.
  -   Cluster 3: Underperforming/disengaged employees with punctuality and performance issues.
- **Predicting Employee Termination**: The analysis successfully created a predictive model with a 97.78% accuracy rate to determine whether an employee will be terminated. The most significant predictor in this model is the employee's status (EmpStatusID).


## 📂 Repository Structure
- **Human rss.R**: The R script containing all the code for cleaning, PCA, and Clustering.
- **Human rss.Rmd**: The complete R Markdown script with code, comments, and analysis
- **Human-rss.pdf**: The final comprehensive report of the analysis.
- **HRDataset_v14.csv**: The dataset used for the study

---
*Developed by Andrea Porro as an individual project.*
