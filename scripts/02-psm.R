library(arm)
survey_unnest$Gender <- as.factor(survey_unnest$Gender)

survey_unnest <- survey_unnest [!is.na(survey_unnest$EdLevel), ]
survey_unnest <- survey_unnest [!is.na(survey_unnest$YearsCodePro), ] 
survey_unnest <- survey_unnest [!is.na(survey_unnest$DevType), ]
survey_unnest <- survey_unnest [!is.na(survey_unnest$Age), ]

propensity_score <- glm(Gender ~ Ethnicity + EdLevel + Ethnicity + DevType + Age + YearsCodePro,
                        family = binomial,
                        data = survey_unnest)
summary(propensity_score)
survey_unnest <- 
  augment(propensity_score, 
          data = survey_unnest,
          type.predict = "response") %>% 
  dplyr::select(-.resid, -.std.resid, -.hat, -.sigma, -.cooksd) 

survey_unnest <- 
  survey_unnest %>% 
  arrange(.fitted, Gender)

survey_unnest$Gender2 <- revalue(survey_unnest$Gender, c("Man"=1, "Woman"=2, "Non-binary, genderqueer, or gender non-conforming"=3))
survey_unnest$Gender2 <- as.integer(survey_unnest$Gender2)

matches <- arm::matching(z = survey_unnest$Gender2, score = survey_unnest$.fitted)
survey_unnest <- cbind(survey_unnest, matches)

table(survey_unnest$Gender)

survey_matched <- 
  survey_unnest %>% 
  filter(match.ind != 1) %>% 
  dplyr::select(-match.ind, -pairs, -Gender2)