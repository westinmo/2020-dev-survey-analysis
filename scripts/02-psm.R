#### Preamble ####
# Purpose: 
# Author: Morgaine Westin
# Date: 6 April 2021
# Contact: morgaine.westin@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data

library(arm)
survey_test <- survey_unnest
#Factorizing gender
survey_test$Gender <- as.factor(survey_test$Gender)
survey_test$Ethnicity <- as.factor(survey_test$Ethnicity)
survey_test$Ethnicity <- relevel(survey_test$Ethnicity, ref = "White or of European descent")

#Removing NAs from categories used for PSM
survey_test<- survey_test[!is.na(survey_test$EdLevel), ]
survey_test <- survey_test[!is.na(survey_test$YearsCodePro), ] 
survey_test <- survey_test[!is.na(survey_test$DevType), ]
survey_test <- survey_test[!is.na(survey_test$Age), ]

table(survey_test$Gender)

#PSM for gender
propensity_score <- glm(Gender ~ Ethnicity + EdLevel + Ethnicity + DevType + Age + YearsCodePro,
                        family = binomial,
                        data = survey_test)

survey_test <- 
  augment(propensity_score, 
          data = survey_test,
          type.predict = "response") %>% 
  dplyr::select(-.resid, -.std.resid, -.hat, -.sigma, -.cooksd) 

survey_test <- 
  survey_test %>% 
  arrange(.fitted, Gender)

survey_test$Gender2 <- revalue(survey_test$Gender, c("Man"=0, "Woman"=1, "Non-binary, genderqueer, or gender non-conforming"=2))
survey_test$Gender2 <- as.integer(as.character(survey_test$Gender2))

#Matching
matches <- arm::matching(z = survey_test$Gender2, score = survey_test$.fitted, replace = F)
survey_test <- cbind(survey_test, matches)

survey_matched <- 
  survey_test %>% 
  filter(match.ind != 0) %>% 
  dplyr::select(-match.ind, -pairs, -Gender2)




