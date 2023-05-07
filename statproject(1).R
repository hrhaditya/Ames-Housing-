# Install required packages
install.packages(c("readr", "ggplot2", "dplyr", "car"))

# Load required libraries
library(readr)
library(ggplot2)
library(dplyr)
library(car)

# Load dataset
ames_data <- read_csv("https://raw.githubusercontent.com/hrhaditya/Ames-Housing-/main/train.csv")

# Research Question 1: One-sample t-test
# Compare the average sale price of houses with a central air conditioning system to the overall average sale price
central_air_houses <- ames_data %>% filter(CentralAir == "Y")
t_test_result <- t.test(central_air_houses$SalePrice, mu = mean(ames_data$SalePrice))
print(t_test_result)

# Research Question 2: Two-sample t-test
# Compare the mean sale price between houses with different building types (e.g., one-family detached vs. townhouse)
one_family_houses <- ames_data %>% filter(BldgType == "1Fam")
townhouse_houses <- ames_data %>% filter(BldgType == "TwnhsE")
t_test_result_2 <- t.test(one_family_houses$SalePrice, townhouse_houses$SalePrice)
print(t_test_result_2)

# Research Question 3: Multiple regression analysis
# Assess the impact of overall house quality on its sale price, controlling for other factors like house size and age
model <- lm(SalePrice ~ OverallQual + GrLivArea + YearBuilt, data = ames_data)
print(summary(model))

# Research Question 4: Chi-Square Test of Independence
# Analyze the relationship between building types and neighborhoods
contingency_table <- table(ames_data$BldgType, ames_data$Neighborhood)
chi_square_test_result <- chisq.test(contingency_table)
print(chi_square_test_result)

# Install ggplot2 
if(!require(ggplot2)) install.packages("ggplot2")

# Load ggplot2
library(ggplot2)

# Create bar chart
ggplot(ames_data, aes(Neighborhood)) + 
  geom_bar(aes(fill = BldgType), position = "dodge") +
  labs(x = "Neighborhood", y = "Count", fill = "Building Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
