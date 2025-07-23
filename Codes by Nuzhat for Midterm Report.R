# Loading necessary packages
require("haven")
require("pROC")
require("caret")
require(dplyr)

##################################################

# Dependent variable: 

#HEI from week 8 folder in Blackboard
hei_prep<-as.data.frame(read_sas("~/./Downloads/hei2015r1720.sas7bdat"))[,c("SEQN","HEI2015_TOTAL_SCORE")] 

# For this report, I am considering variables which might positively contribute to a positive HEI score

# Independent variables: 

# (1) Under demographic data, I want to include the following variables: 
#P_DEMO - Demographic Variables and Sample Weights
        #--- a. RIDAGEYR - Age in years at screening
        #--- b. RIAGENDR - Gender
        #--- c. RIDRETH3 - Race/Hispanic origin w/ NH Asian
        #--- d. DMDMARTZ - Marital status
        #--- e. DMDEDUC2 - Education level - Adults 20+

# (2) Under questionnaire data, I want to include the following variables: 
#P_DBQ - Diet Behavior and Nutrition
       #--- a. DBQ935 - Shared meal planning/preparing duty 
       #--- b. DBQ700 - How healthy is the diet? 
#P_PAQ - Physical Activity 
       #--- c. PAQ610 - Number of days vigorous work 

##################################################

# Reading all the necessary data from NHANES 
#Pre-pandemic demographics data 
demo_prep <- as.data.frame(read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DEMO.XPT")) 

# #Pre-pandemic data for physical activity
physical_act_prep <- as.data.frame(read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_PAQ.XPT")) 

#Pre-pandemic data for Diet Behavior & Nutrition

P_DBQ <- as.data.frame(read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DBQ.XPT")) 
##################################################

# Calculating the descriptive statistics for continuous variables first - Adults 20+

# *** Since there are missing values for some of the key variables, 
#I am adding na.rm = TRUE in both the calculations to ignore missing values

# Mean and standard deviation for Age

demo_prep_adult <- demo_prep %>% filter(RIDAGEYR >= 20) #Filtering the data of Age for participants aged 20 and above 
age_mean <- mean(demo_prep_adult$RIDAGEYR, na.rm = TRUE)
age_sd <- sd(demo_prep_adult$RIDAGEYR, na.rm = TRUE)

# Mean and standard deviation for Number of days of vigorous work 

physical_act_prep_adult <- merge(demo_prep, physical_act_prep, by = "SEQN") %>% filter(RIDAGEYR >= 20) # Merging demo_prep and physical_act_prep by SEQN to include age in the physical activity dataset

PAQ610_numeric <- as.numeric(physical_act_prep_adult$PAQ610)    # Converting PAQ610 to numeric
PAQ610_exclude <- PAQ610_numeric[PAQ610_numeric >= 1 & PAQ610_numeric <= 7]   # Excluding values 77 ("Refused"), 99 ("Don't know"), and missing values

days_vig_mean <- mean(PAQ610_exclude, na.rm = TRUE)  #Mean for Number of days of vigorous work 
days_vig_sd <- sd(PAQ610_exclude, na.rm = TRUE) #Standard deviation for Number of days of vigorous work 

# Displaying the results
cat("Mean Days of Vigorous Work (20+):", days_vig_mean, "SD:", days_vig_sd, "\n")

##################################################

# Calculating the descriptive statistics for categorical variables: 

# For Gender - Adults 20+
# Based on the data provided for RIAGENDR: 1 = Male, 2 = Female

demo_prep_adult <- demo_prep %>% filter(RIDAGEYR >= 20) # Filtering demo_prep for participants aged 20 and above

gen_count <- table(demo_prep_adult$RIAGENDR)  # Calculating the count for each gender
gen_proportion <- prop.table(gen_count) * 100   # Calculating the proportion for each gender

# Displaying the results
cat("Proportion of Males:", gen_proportion["1"], "%\n")
cat("Proportion of Females:", gen_proportion["2"], "%\n")

# For Race/Hispanic origin w/ NH Asian - Adults 20+

race_eth_data <- demo_prep %>% filter(RIDAGEYR >= 20, !is.na(RIDRETH3)) %>% pull(RIDRETH3) # Filtering demo_prep for participants aged 20 and above, and exclude missing values for RIDRETH3
race_eth_count <- table(race_eth_data) # Calculating the count for each race/ethnicity category
race_eth_proportion <- prop.table(race_eth_count) * 100 # Calculating the proportion for each race/ethnicity category

# Displaying the results
cat("Proportion of Mexican American:", race_eth_proportion["1"], "%\n")
cat("Proportion of Other Hispanic:", race_eth_proportion["2"], "%\n")
cat("Proportion of Non-Hispanic White:", race_eth_proportion["3"], "%\n")
cat("Proportion of Non-Hispanic Black:", race_eth_proportion["4"], "%\n")
cat("Proportion of Non-Hispanic Asian:", race_eth_proportion["6"], "%\n")
cat("Proportion of Other Race (Including Multi-Racial):", race_eth_proportion["7"], "%\n")

# For Marital Status - Adults 20+

ms_data_prep <- demo_prep %>% filter(RIDAGEYR >= 20, DMDMARTZ %in% c(1, 2, 3)) %>% pull(DMDMARTZ) # Filtering for participants aged 20 and above, and exclude irrelevant codes (77 and 99) for DMDMARTZ
ms_count_prep <- table(ms_data_prep) # Calculating the count for each marital status category
ms_proportion_prep <- prop.table(ms_count_prep) * 100 # Calculating the proportion for each marital status category

# Displaying the results
cat("Proportion Married/Living with Partner:", ms_proportion_prep["1"], "%\n")
cat("Proportion Widowed/Divorced/Separated:", ms_proportion_prep["2"], "%\n")
cat("Proportion Never Married:", ms_proportion_prep["3"], "%\n")

#For Education level - Adults 20+

edu_data_prep <- demo_prep %>% filter(RIDAGEYR >= 20, DMDEDUC2 %in% c(1, 2, 3, 4, 5)) %>% pull(DMDEDUC2)
edu_count_prep <- table(edu_data_prep)
edu_proportion_prep <- prop.table(edu_count_prep) * 100

# Displaying the results
cat("Proportion Less than 9th grade:", edu_proportion_prep["1"], "%\n")
cat("Proportion 9-11th grade (No diploma):", edu_proportion_prep["2"], "%\n")
cat("Proportion High school graduate/GED:", edu_proportion_prep["3"], "%\n")
cat("Proportion Some college or AA degree:", edu_proportion_prep["4"], "%\n")
cat("Proportion College graduate or above:", edu_proportion_prep["5"], "%\n")

# For Shared meal planning/preparing duty - Adults 20+ 

meal_planning_data <- merged_data_dbq_935 %>%
  filter(RIDAGEYR >= 20, DBQ935 %in% c(1, 2)) %>%
  select(DBQ935)
meal_planning_count <- table(meal_planning_data$DBQ935)
meal_planning_proportion <- prop.table(meal_planning_count) * 100

# Displaying the results
cat("Proportion Yes (Shares meal planning):", meal_planning_proportion["1"], "%\n")
cat("Proportion No (Does not share meal planning):", meal_planning_proportion["2"], "%\n")

# For 'How healthy is the diet?' - Adults 20+

diet_health_data <- merged_data_700 %>%
  filter(RIDAGEYR >= 20, DBQ700 %in% c(1, 2, 3, 4, 5)) %>%
  select(DBQ700)
diet_health_count <- table(diet_health_data$DBQ700)
diet_health_proportion <- prop.table(diet_health_count) * 100

# Displaying the results
cat("Proportion Excellent:", diet_health_proportion["1"], "%\n")
cat("Proportion Very Good:", diet_health_proportion["2"], "%\n")
cat("Proportion Good:", diet_health_proportion["3"], "%\n")
cat("Proportion Fair:", diet_health_proportion["4"], "%\n")
cat("Proportion Poor:", diet_health_proportion["5"], "%\n")
cat("Proportion Excellent:", diet_health_proportion["1"], "%\n")
cat("Proportion Very Good:", diet_health_proportion["2"], "%\n")
cat("Proportion Good:", diet_health_proportion["3"], "%\n")
cat("Proportion Fair:", diet_health_proportion["4"], "%\n")
cat("Proportion Poor:", diet_health_proportion["5"], "%\n")

##################################################

# Creating the training and testing set and using the training set to estimate at least three different linear models with HEI as the outcome. 

# Combining the datasets together for analysis 

demo_prep <- as.data.frame(read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DEMO.XPT")) 
physical_act_prep <- as.data.frame(read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_PAQ.XPT")) 
P_DBQ <- as.data.frame(read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DBQ.XPT")) 
hei_prep<-as.data.frame(read_sas("~/./Downloads/hei2015r1720.sas7bdat"))[,c("SEQN","HEI2015_TOTAL_SCORE")] 

# Selecting relevant variables from each dataset and prepare for merging
demo_comb <- demo_prep[, c("SEQN", "RIDAGEYR", "RIAGENDR", "RIDRETH3", "DMDMARTZ", "DMDEDUC2")]
physical_act_comb <- physical_act_prep[, c("SEQN", "PAQ610")]
diet_comb <- P_DBQ[, c("SEQN", "DBQ935", "DBQ700")]

# Combining all datasets based on SEQN
df_combined <- Reduce(function(x, y) merge(x, y, by = "SEQN", all.x = TRUE), list(demo_comb, physical_act_comb, diet_comb, hei_prep))

# Filtering to include only participants aged 20 and above
df_combined <- df_combined[df_combined$RIDAGEYR >= 20, ]

str(df_combined)

#Creating the training and testing set

library(caret)
set.seed(123)  # Setting a seed for reproducibility

df_combined <- df_combined[!is.na(df_combined$HEI2015_TOTAL_SCORE), ] # Remove rows with missing HEI2015_TOTAL_SCORE values

# Dividing into 70% training and 30% test
train_index <- createDataPartition(df_combined$HEI2015_TOTAL_SCORE, p = 0.7, list = FALSE)
train_data <- df_combined[train_index, ]
test_data <- df_combined[-train_index, ]

##################################################

# Creating the models

# Model 1: Age and Gender as predictors
model1 <- lm(HEI2015_TOTAL_SCORE ~ RIDAGEYR + RIAGENDR, data = train_data)

# Model 2: Adding Race/Ethnicity to the predictors
model2 <- lm(HEI2015_TOTAL_SCORE ~ RIDAGEYR + RIAGENDR + RIDRETH3, data = train_data)

# Model 3: Including Education Level and Physical Activity
model3 <- lm(HEI2015_TOTAL_SCORE ~ RIDAGEYR + RIAGENDR + RIDRETH3 + DMDEDUC2 + PAQ610, data = train_data)

# Model 4: Includes Age, Gender, Race/Ethnicity, Education, Physical Activity, and Shared meal planning/preparing duty
model4 <- lm(HEI2015_TOTAL_SCORE ~ RIDAGEYR + RIAGENDR + RIDRETH3 + DMDEDUC2 + PAQ610 + DBQ935, data = train_data)

# Model 5: Adds 'How healthy is the diet?' to previous predictors
model5 <- lm(HEI2015_TOTAL_SCORE ~ RIDAGEYR + RIAGENDR + RIDRETH3 + DMDEDUC2 + PAQ610 + DBQ935 + DBQ700, data = train_data)

# Sumarizing the models
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)

##################################################

# Evaluating the mean residual sum of squares for each model using data from the test set

# Model 1: Age and Gender as predictors
pred1 <- predict(model1, test_data)
mrss1 <- mean((test_data$HEI2015_TOTAL_SCORE - pred1)^2)

# Model 2: Adding Race/Ethnicity to the predictors
pred2 <- predict(model2, test_data)
mrss2 <- mean((test_data$HEI2015_TOTAL_SCORE - pred2)^2)

# Filtering test data to remove rows with missing values in relevant predictors for each model
test_data_model3 <- test_data[complete.cases(test_data[, c("RIDAGEYR", "RIAGENDR", "RIDRETH3", "DMDEDUC2", "PAQ610")]), ]
test_data_model4 <- test_data_model4[complete.cases(test_data_model4$HEI2015_TOTAL_SCORE), ]
test_data_model5 <- test_data[complete.cases(test_data[, c("RIDAGEYR", "RIAGENDR", "RIDRETH3", "DMDEDUC2", "PAQ610", "DBQ935", "DBQ700")]), ]

# Model 3: Including Education Level and Physical Activity
pred3 <- predict(model3, test_data_model3)
mrss3 <- mean((test_data_model3$HEI2015_TOTAL_SCORE - pred3)^2)


#*** # Ensuring prediction and outcome have matching lengths
if (length(pred4) > nrow(test_data_model4)) {
  pred4 <- pred4[1:nrow(test_data_model4)]
} else if (nrow(test_data_model4) > length(pred4)) {
  test_data_model4 <- test_data_model4[1:length(pred4), ]
}

# Model 4: Includes Age, Gender, Race/Ethnicity, Education, Physical Activity, and Shared meal planning/preparing duty
pred4 <- predict(model4, test_data_model4) # Aligning predictions to available outcomes
mrss4 <- mean((test_data_model4$HEI2015_TOTAL_SCORE - pred4)^2)

# Model 5: Adds 'How healthy is the diet?' to previous predictors
pred5 <- predict(model5, test_data_model5)
mrss5 <- mean((test_data_model5$HEI2015_TOTAL_SCORE - pred5)^2)

# Displaying MRSS for each model
cat("MRSS for Model 1:", mrss1, "\n")
cat("MRSS for Model 2:", mrss2, "\n")
cat("MRSS for Model 3:", mrss3, "\n")
cat("MRSS for Model 4:", mrss4, "\n")
cat("MRSS for Model 5:", mrss5, "\n")