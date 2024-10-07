knitr::opts_chunk$set(echo = TRUE)

# Loading Necessary Libraries
library(tidyverse)
library(ggplot2)
library(effsize)
library(readxl)

# Loading the data
health_data<-read_xlsx(path="health_data.xlsx")

# Summary and structure of the data
summary(health_data)
str(health_data)

# 1. Distribution of Age
ggplot(health_data, aes(x = age, fill = factor(heart_disease, 
                                               levels = c(0, 1), 
                                               labels = c("No Heart Disease", "Heart Disease")))) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("No Heart Disease" = "blue", "Heart Disease" = "red")) +
  labs(title = "Age Distribution by Heart Disease Status",
       x = "Age",
       y = "Count",
       fill = "Heart Disease Status") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 2. Scatter Plot of BMI vs. Cholesterol
ggplot(health_data, aes(x = bmi, y = cholesterol)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal() +
  ggtitle("BMI vs Cholesterol")

# 3. Boxplot of BMI by Heart Disease Status
ggplot(health_data, aes(x = factor(heart_disease), y = bmi)) +
  geom_boxplot() +
  ggtitle("BMI by Heart Disease Status") +
  xlab("Heart Disease (0 = No, 1 = Yes)")

# 4. Correlation Analysis
round(cor(health_data[, c("age", "bmi", "cholesterol", "glucose")]),3)

# 5. T-test for BMI between Heart Disease Groups
t.test(bmi ~ heart_disease, data = health_data)

# 6. Cohen's d Calculation
cohens_d_bmi <- cohen.d(health_data$bmi ~ health_data$heart_disease)
cohens_d_cholesterol <- cohen.d(health_data$cholesterol ~ health_data$heart_disease)
print(cohens_d_bmi)
print(cohens_d_cholesterol)
