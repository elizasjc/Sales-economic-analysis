Economic & Consumer Sales Data Cleaning, Validation and Trend Analysis using Excel and R

# Market Research Data Cleaning, Validation & Analysis Project

## 📌 Project Overview

This project demonstrates an end-to-end data analysis workflow aligned with the market research data analyst job requirements. The focus is on data cleaning, standardisation, validation, exploratory analysis, and reporting using Excel and R.

The project simulates a real-world market research dataset involving products, brands, categories, prices, and dates, and shows how raw, inconsistent data is transformed into an analysis-ready dataset.

---

## 🎯 Objectives

* Clean and standardise messy categorical and numeric data
* Validate data quality (duplicates, missing values, incorrect prices)
* Prepare data for time-series and aggregated analysis
* Generate insights and visualisations using R
* Demonstrate reproducible, well-documented workflows

---

## 🛠 Tools & Technologies

* Excel: Initial data cleaning, category & brand mapping, validation
* R & RStudio: Data validation, feature engineering, analysis, visualisation, forecasting
* Libraries used: `readxl`, `dplyr`, `ggplot2`


---

## 🧹 Data Cleaning & Validation (Excel)

* Standardised brand and category names using mapping tables
* Cleaned numeric fields (price validation, negative value checks)
* Preserved raw data for traceability
* Created clean columns alongside raw values

Key cleaned columns:

* `clean_brand`
* `clean_category`
* `clean_price`

---

## 📊 Analysis & Feature Engineering (R)

* Imported cleaned Excel data into R
* Converted date fields and derived:

  * `year`
  * `month`
* Checked for:

  * Missing values
  * Duplicate records
  * Price consistency
* Aggregated data by brand, category, and time
* Created summary statistics and visualisations
* Built simple time-series trend and forecast models

---

## 📈 Key Insights (Example)

* Brand-level average pricing differences
* Strong upward price trend across years
* Consistent category-wise growth patterns
* Clean, validated dataset suitable for forecasting

---

## Why This Project Matters

This project demonstrates:

* Strong focus on data quality and standardisation
* Practical use of Excel and R together
* Ability to work with messy market research data
* Clear documentation and reproducible workflows
* Readiness for analyst roles involving research, validation, and reporting

---

## 👤 Author

Shilpa J Chethalen

Aspiring Data Analyst | Market & Economic Data Enthusiast
