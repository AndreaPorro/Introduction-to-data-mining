# 📉 Multivariate Analysis: PCA on Global Health & Consumer Behavior

This repository features a comprehensive study on **Principal Component Analysis (PCA)**. 

The project evaluates two distinct datasets to identify underlying structures and reduce dimensionality while preserving maximum variance.

## 🎯 Project Objectives
The analysis is divided into two main parts to test PCA effectiveness across different domains:
* **Part A: Life Expectancy Analysis:** Exploring global health indicators (WHO data) to identify the "latent" factors that drive life expectancy across developing and developed countries.
* **Part B: Starbucks Customer Survey:** Analyzing consumer behavior and satisfaction levels to segment customers based on their engagement and spending habits.

## 🛠️ Tech Stack & Skills
* **Language:** `R`
* **Reporting:** `R Markdown`
* **Key Libraries:** `FactoMineR`, `factoextra`, `corrplot`, `ggplot2`.
* **Methods:** Principal Component Analysis (PCA), Biplot Analysis, Scree Plots, Dimensionality Reduction (Kaiser Criterion & Elbow Method).

## 📈 Key Insights & Results
**Part A, Life Expectancy Analysis:** 
* The PCA identified **9 Principal Components** in total. The first **5 components** alone account for approximately **91% of the total variance**, allowing for significant dimensionality reduction while preserving most of the dataset's information.
* The variables can be categorized into three distinct groups of variables and health outcomes:
  1. **Cluster 1 (General Health Metrics):** Associated with positive indicators such as Life Expectancy, Polio, and Diphtheria immunization. This cluster reflects states with moderate to good health outcomes.
  2. **Cluster 2 (Adult Health Risks):** Driven by factors like HIV/AIDS and Adult Mortality. This group is associated with poorer health outcomes specifically in adult populations.
  3. **Cluster 3 (Child Health Risks):** Related to Infant Deaths and Under-five Deaths. Analysis shows that while fewer states fall into this category, it reflects serious health challenges regarding children.

**Part B, Starbucks Customer Survey**
* The PCA identified **20 Principal Components** in total. The first **15 components** are sufficient to explain over **91% of the total variance**. This indicates that the dataset's complexity can be effectively reduced while retaining the vast majority of the original information.
* The variables can be categorized into three main clusters based on their correlation with the first two Principal Components::
    1. **Cluster 1:** High focus on **Customer Satisfaction** and product evaluation.
    2. **Cluster 2:** Driven by **Financial and Demographic** factors.
    3. **Cluster 3:** Related to specific **Customer Behavior** and brand engagement.


## 📂 Repository Structure
* `PCA_analysis.R`: Cleaned R script for quick execution of the models.
* `PCA-Andrea-Porro.Rmd`: The complete R Markdown script with detailed comments and statistical interpretations.
* `PCA-Andrea-Porro.pdf`: The final compiled technical report.
* `Life Expectancy Data.csv`: WHO global health dataset.
* `Starbucks satisfactory survey encode cleaned.csv`: Pre-processed consumer survey data.

---
*Developed by Andrea Porro as an individual project.*
