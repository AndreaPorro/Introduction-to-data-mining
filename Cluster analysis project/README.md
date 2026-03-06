# 🏠 Real Estate Segmentation: Cluster Analysis

This repository contains a specialized data mining project focused on the **Real Estate market**.

The project applies unsupervised learning techniques to segment housing data based on structural characteristics, age, and market price.

## 🎯 Project Objectives
The analysis aims to identify distinct patterns in the housing market through a rigorous statistical workflow:
* **Exploratory Data Analysis (EDA):** Initial data screening of variables like Living Area, Bathrooms, Bedrooms, Lot Size, and Age.
* **Cluster Analysis:** Implementing and comparing multiple hierarchical clustering methods to find the most natural grouping of properties.
* **Methodological Comparison:** Evaluation of different linkage methods (Ward, Single, Average) using the **Cophenetic Correlation Coefficient**.

## 🛠️ Tech Stack & Skills
* **Language:** `R`
* **Reporting:** `R Markdown`
* **Key Libraries:** `factoextra`, `cluster`, `ggplot2`, `dendextend`
* **Methods:** Hierarchical Clustering, Cophenetic Correlation, Euclidean Distance normalization.

## 📈 Key Insights & Results
The study compared three different clustering approaches, using the Cophenetic correlation to validate the results:
1. **Average Method (Best Performer):** Achieved the highest correlation (**0.7604**), proving to be the most effective in representing the data structure.
2. **Cluster Profiles identified:**
    * **Cluster 1:** High-end market segment featuring large, recent, and expensive properties.
    * **Cluster 2:** Entry-level/Mid-market segment with smaller, older, and more affordable houses.
3. **Validation:** The Single method (0.6815) and Ward method (0.5230) were also evaluated, highlighting the sensitivity of the dataset to outliers.

## 📂 Repository Structure
* `Cluster analysis.R`: The R script containing all the code.
* `Cluster-analyze.Rmd`: The R Markdown script containing the full pipeline and methodology.
* `Cluster-analyze.pdf`: The final technical report.
* `houseprice.csv`: The dataset containing real estate features and prices.


---
*Developed by Andrea Porro as an individual project.*
