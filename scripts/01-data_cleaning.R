library(tidyverse)
library(here)
library(readr)
library(visdat)
library(janitor)
library(questionr)
library(plyr)



#Reading in Data
survey_raw <- readr::read_csv(here::here("inputs/data/survey_results_public.csv"))
#survey schema outlining which questions correspond to each column name
schema <- read_csv(here::here("inputs/data/survey_results_schema.csv"))

#Filtering data for individuals who live in the United States and are employed full time
survey_clean <- survey_raw %>%
  filter(Country == "United States", Employment == "Employed full-time")
#9765 individuals in the United States who are Employed full-time

#Removing rows with NAs
survey_clean <- survey_clean[!is.na(survey_clean$ConvertedComp), ] #7628 after removing NA income
survey_clean <- survey_clean[!is.na(survey_clean$Gender), ] #7097 after removing NA gender
survey_clean <- survey_clean[!is.na(survey_clean$Ethnicity), ] #6860 after removing NA Ethnicity
survey_clean <- rename(survey_clean, c("Income" = "ConvertedComp"))
#6860 reported their income, gender, and ethnicity

#Re-categorizing gender responses into Man, Women, or Non-binary/genderqueer/gender non-conforming
survey_clean$Gender <- case_when(str_detect(survey_clean$Gender, "Non-binary, genderqueer, or gender non-conforming")
                                ~ "Non-binary, genderqueer, or gender non-conforming",
                                TRUE ~ survey_clean$Gender)
survey_clean$Gender[survey_clean$Gender == "Woman;Man" ] <- "Non-binary, genderqueer, or gender non-conforming"

survey_clean$Gender <- as.factor(survey_clean$Gender) %>%
  droplevels()

#table(survey_clean$Ethnicity)
#Ethnicity

#Recfactoring Education levels
survey_clean$EdLevel <- as.factor(survey_clean$EdLevel)

survey_clean$EdLevel <- fct_collapse(survey_clean$EdLevel,
                                     `Less than Bachelor's Degree` = c("I never completed any formal education", 
                                              "Primary/elementary school",
                                              "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)",
                                              "Some college/university study without earning a degree"),
                                     `Associate` = "Associate degree (A.A., A.S., etc.)",
                                     `Bachelor's` = "Bachelor’s degree (B.A., B.S., B.Eng., etc.)",
                                     `Graduate` = c("Master’s degree (M.A., M.S., M.Eng., MBA, etc.)",
                                                    "Other doctoral degree (Ph.D., Ed.D., etc.)",
                                                    "Professional degree (JD, MD, etc.)"))
                                      
                                     
#Mainly interested in industry working tech professionals who are individual contributors
#Removing researchers, educators, and scientists, managers, students, senior execs
remove <- survey_clean %>%
  filter(str_detect(DevType, 
                    "Academic researcher|Scientist|Educator|Student|Other|Marketing or sales professional|Product manager|Senior Executive (C-Suite, VP, etc.)|Engineering manager"))

survey_clean <- survey_clean %>%
  anti_join(remove)

#Splitting DevType and Ethnicity
##survey_clean 2 has unnested dev types and ethnicities (potentially multiple rows representing 1 respondent)
survey_clean2 <- survey_clean %>% 
  mutate(DevType = str_split(DevType, pattern = ";")) %>%
  unnest(DevType) %>%
  mutate(Ethnicity = str_split(Ethnicity, pattern = ";")) %>%
  unnest(Ethnicity)


survey_clean2$Ethnicity <- case_when(
  str_detect(survey_clean2$Ethnicity, "Indigenous") ~ "Indigenous", TRUE ~ survey_clean2$Ethnicity)

write_csv(survey_clean, here::here("inputs/data/survey_clean.csv"))
write_csv(survey_clean2, here::here("inputs/data/survey_clean2.csv"))



