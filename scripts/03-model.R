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
survey_matched$DevType <- relevel(survey_matched$DevType, ref = "Full-Stack Developer")

income_model <- 
  lm(log(Salary) ~ Gender + Ethnicity + Ethnicity * Gender + EdLevel + DevType + YearsCodeProNew + YearsCode, 
     data = survey_matched)

#Assumptions
summary(income_model)
#Linearity: the response can be written as a linear combination of  the predictors.
#Independence: the errors are independent (not correlated).
#Normality: the distribution of the errors should follow a normal  distribution.
#Equal Variance: the error variance is the same at any set of  predictor values	(homoscedasticity).


tukey <- transformTukey(survey_matched$Salary, plotit = FALSE) #rcompanion
skewness(survey_matched$Income)

income_model_new <- 
  lm((Salary^(0.225)) ~ Gender + Ethnicity + Ethnicity * Gender + EdLevel + DevType + YearsCodePro + YearsCodePro * Gender, 
     data = survey_matched)
summary(income_model_new)

income_model_new <- 
  lm((log(Salary)) ~ Gender + Ethnicity + Ethnicity * Gender + EdLevel + DevType + YearsCodeProNew, 
     data = survey_matched)
summary(income_model_new)

check_model(income_model_new)
shapiro.test(resid(income_model_new))
#p > 0.05 means residuals not normally distributed

income_model_new2 <- 
  lm(log(Salary) ~ Gender + Ethnicity + Ethnicity * Gender + EdLevel + DevType + YearsCodePro, 
     data = survey_matched)
summary(income_model_new2)
check_model(income_model_new2)
shapiro.test(resid(income_model_new))

income_model_new2 <- 
  lm((-1*Salary^(-0.8)) ~ Gender + Ethnicity + Ethnicity * Gender + EdLevel + DevType + YearsCodeProNew + YearsCodePro * Gender, 
     data = survey_matched)
summary(income_model_new2) #After accounting for the interaction between professional exp and gender, gender no lomger significant

