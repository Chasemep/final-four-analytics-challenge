# **Project Title**
NCAA Final Four Analytics Challenge Model

---

## **Table of Contents**
1. [Project Overview](#project-overview)
2. [Data](#data)
3. [Installation / Required Packages](#installation--required-packages)
4. [Usage](#usage)
5. [Analysis / Methods](#analysis--methods)
6. [Results](#results)
7. [Contributing](#contributing)
8. [License](#license)

---

## **Project Overview**
- **Purpose**: Develop models to predict overall seeds for NCAA Men's Basketball teams.
- **Problem Statement**: The NCAA has provided data containing basketball statistics over the last several years. Competitors are expected to use this data, as well as outside sources, to predict the seeds of NCAA Men's Basketball Teams.
- **Approach**: Make use of various statistical techniques including linear regression, machine learning (xgboost), and feature engineering to create accurate predictions.

---

## **Data**
- **Source(s)**: Data is sourced from the NCAA and provided through Kaggle.
- **Description**: Training and test data sets have been provided, information regarding columns can be found in `data/FFAC Data Dictionary.xlsx`.
- **Preprocessing**: All cleaning and preprocessing is done in the `Model.R` file. This includes separating hyphenated entries, handling invalid entries, and removing NA values.

---

## **Installation / Required Packages**
Before running the scripts, install the required packages:
- xgboost
- tools

---

## **Usage** ##
Run the code in the attached Model.R file. This file contains all code to clean data, add features, and run linear or xgboost models. Ensure that when running a model, only run the section with the model you would like use.

---

## **Analysis / Methods** ##
- **Accuracy** - Calculated using RMSE through Kaggle. 
- **Analysis** - Analysis in performed using standard metrics for linear models (R squared, F statistic, etc.) and the influcence feature of XGBoost.

---

## **Contributing** ##
- Chase Pluimer
- Gowrisree Juttiga
- Lillian Gurtatowski

---

## **License** ##
This project is licensed under the MIT License.