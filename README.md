# HEI Prediction Using NHANES Data (2017–2020)

This project investigates how demographic and behavioral factors influence Healthy Eating Index (HEI) scores in U.S. adults using pre-pandemic NHANES data. Developed as part of my Master’s coursework, the entire analysis was executed in **RStudio**, encompassing everything from data wrangling to multiple linear regression modeling.

---

## 🌟 Project Objective

To evaluate predictors of HEI scores using variables such as:

* Age
* Gender
* Race/Ethnicity
* Marital status
* Education level
* Physical activity
* Shared meal preparation responsibilities
* Self-perceived diet quality

---

## 📊 Data & Tools

* **Data Source**: NHANES 2017–2020 (Demographics, Diet Behavior, Physical Activity)
* **Target Variable**: HEI2015\_TOTAL\_SCORE
* **Software**: RStudio
* **Libraries Used**: `haven`, `caret`, `dplyr`, `pROC`

### Data Cleaning & Preparation:

* Focused on adults aged 20+
* Merged multiple NHANES datasets via `SEQN`
* Filtered and transformed variables for modeling

---

## 📈 Modeling Approach

Five linear regression models were constructed using training/test splits (70/30):

| Model | Predictors Included            | MRSS         |
| ----- | ------------------------------ | ------------ |
| 1     | Age, Gender                    | 195.75       |
| 2     | + Race/Ethnicity               | 194.99       |
| 3     | + Education, Physical Activity | 178.06       |
| 4     | + Meal Planning Involvement    | 175.70       |
| 5     | + Self-rated Diet Quality      | **174.78** ✅ |

Model 5 had the best performance with the lowest MRSS, reflecting the importance of behavioral perceptions alongside demographic and activity-based predictors.

---

## 🗂️ Repository Structure

* `Midterm Report_Nuzhat.pdf`: Full report with methodology, descriptive statistics, model comparison, and references
* `Codes by Nuzhat for Midterm Report.R`: Complete R script for data loading, transformation, model building, and evaluation

---

## 📚 Highlights

* First end-to-end RStudio project involving NHANES data
* Built and compared five linear regression models with demographic and behavioral predictors
* Interpreted public health implications of predictors on diet quality

---

## 📍 Acknowledgments

With support from my professor, I was able to complete this project as part of my Data Analytics coursework. I welcome constructive feedback as I continue to grow in this space.

---

## ✨ Let's Connect!

Curious about diet quality, NCDs, or health equity in datasets? Feel free to reach out or open an issue. Always open to good conversations and thoughtful critiques.
