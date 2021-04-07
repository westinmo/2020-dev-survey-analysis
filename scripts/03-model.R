#### Preamble ####
# Purpose: 
# Author: Morgaine Westin
# Date: 6 April 2021
# Contact: morgaine.westin@mail.utoronto.ca
# License: MIT
# Pre-requisites: 

#
survey_matched$Ethnicity <- relevel(survey_matched$Ethnicity, ref = "White or of European descent")
survey_matched$EdLevel <- relevel(survey_matched$EdLevel, ref = "Bachelor's")

income_model <- 
  lm(Income ~ Gender + Ethnicity + Ethnicity * Gender + EdLevel + DevType + YearsCodePro, 
     data = survey_matched)

#Assumptions
check_model(income_model)
#Linearity: the response can be written as a linear combination of  the predictors.
#Independence: the errors are independent (not correlated).
#Normality: the distribution of the errors should follow a normal  distribution.
#Equal Variance: the error variance is the same at any set of  predictor values	(homoscedasticity).



income_model_new <- 
  lm((Income^(-0.8)) ~ Gender + Ethnicity + Ethnicity * Gender + EdLevel + DevType + YearsCodePro, 
     data = survey_matched)
summary(income_model)
