#### Preamble ####
# Purpose: This script creates a propensity score matched dataset using previously cleaned data and prepares it for regression analysis.
# Author: Morgaine Westin
# Date: 25 April 2021
# Contact: morgaine.westin@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Must have run 01-data_cleaning.R script before running this script


library(arm) #used for matching

#copying data for propensity score matching
survey_unnest <- read_csv(here::here("inputs/data/survey_unnest.csv")) 
survey_test <- survey_unnest

#Factorizing 
survey_test$UndergradMajor <- as.factor(survey_test$UndergradMajor)
survey_test$Gender <- as.factor(survey_test$Gender)
survey_test$Ethnicity <- as.factor(survey_test$Ethnicity)
survey_test$DevType <- as.factor(survey_test$DevType)


#Replacing NAs for matching variables or removing NAs 
#(for attributes with less than 60 NAs -> replaced with mean; attributes with many NAs -> NAs were thrown out)
survey_test$EdLevel <- fct_explicit_na(survey_test$EdLevel) %>%
  fct_recode("Bachelor's" = "(Missing)") #replace 57 NAs with most common (bachelor's)
survey_test <- survey_test[!is.na(survey_test$DevType), ] #remove 77 NAs
survey_test <- survey_test[!is.na(survey_test$Age), ] #remove 635 NAs
survey_test <- survey_test[!is.na(survey_test$UndergradMajor), ] #remove 648 NAs
survey_test$YearsCodeProNew[is.na(survey_test$YearsCodeProNew)] = mean(survey_test$YearsCodeProNew, na.rm = TRUE) #11 NAs replaced
survey_test$Age1stCode[is.na(survey_test$Age1stCode)] = mean(survey_test$Age1stCode, na.rm = TRUE) #replace 24 NAS
survey_test$YearsCode[is.na(survey_test$YearsCode)] = mean(survey_test$YearsCode, na.rm = TRUE) #replace 36 NAs

#Transforming yearscodepro into a categorical variable for analysis (intervals of 5 years)
survey_test$YearsCodeProCat <- cut(survey_test$YearsCodeProNew, 
                                   breaks = unique(c(0,4,9,14,19,24,29,34,39,44,49,50,
                                              max(survey_test$YearsCodeProNew))),
                                   labels = c("Less than 5 years","5-9 years","10-14 years","15-19 years",
                                              "20-24 years","25-29 years","30-34 years","35-39 years",
                                              "40-44 years","45-49 years", "50+"), include.lowest = F) %>%
  relevel(survey_test$YearsCodeProCat, ref = "Less than 5 years")


#PSM for gender 
propensity_score <- glm(Gender ~ Ethnicity + EdLevel + DevType + Age + Age1stCode + YearsCode + YearsCodeProNew
                        + UndergradMajor,
                        family = binomial,
                        data = survey_test) #creation of lm model to "predict" gender and match based on selected characteristics
#adding predictions from lm to the dataset
survey_test <- 
  augment(propensity_score, 
          data = survey_test,
          type.predict = "response") %>% 
  dplyr::select(-.resid, -.std.resid, -.hat, -.sigma, -.cooksd) %>%
  arrange(.fitted, Gender)
#nb will be removed from matched set; can only be 0 or 1
survey_test$Gender2 <- revalue(survey_test$Gender, c("Man"=0, "Woman"=1, "Non-binary, genderqueer, or gender non-conforming"=2)) 
survey_test$Gender2 <- as.integer(as.character(survey_test$Gender2))

#Creating matched dataset to find closest matches (only including responses which were matched with treatment group)
matches <- arm::matching(z = survey_test$Gender2, score = survey_test$.fitted, replace = F)
survey_test <- cbind(survey_test, matches)
survey_matched <- 
  survey_test %>% 
  filter(match.ind != 0) %>% 
  dplyr::select(-match.ind, -pairs, -Gender2)

#Save matched dataset for analysis on rmd file
write_csv(survey_matched, here::here("inputs/data/survey_matched.csv"))


