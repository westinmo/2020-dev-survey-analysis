#### Preamble ####
# Purpose: 
# Author: Morgaine Westin
# Date: 6 April 2021
# Contact: morgaine.westin@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data

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
  filter(Country == "United States", 
         Employment == "Employed full-time",
         ConvertedComp > 20000
  )
#6384 individuals in the United States who are Employed full-time after trimming very low and very high salaries

#Removing rows with NAs
survey_clean <- survey_clean[!is.na(survey_clean$ConvertedComp), ] #7628 after removing NA income
survey_clean <- survey_clean[!is.na(survey_clean$Gender), ] #7097 after removing NA gender
survey_clean <- survey_clean[!is.na(survey_clean$Ethnicity), ] #6860 after removing NA Ethnicity
survey_clean <- plyr::rename(survey_clean, c("ConvertedComp" = "Income"))
#6860 reported their income, gender, and ethnicity

#Gender
#Re-categorizing gender responses into Man, Women, or Non-binary/genderqueer/gender non-conforming
survey_clean$Gender <- case_when(str_detect(survey_clean$Gender, "Non-binary, genderqueer, or gender non-conforming")
                                ~ "Non-binary, genderqueer, or gender non-conforming",
                                TRUE ~ survey_clean$Gender)
survey_clean$Gender[survey_clean$Gender == "Woman;Man" ] <- "Non-binary, genderqueer, or gender non-conforming"

survey_clean$Gender <- as.factor(survey_clean$Gender) %>%
  droplevels()

#Ethnicity
#To simplify analysis, removing cases who selected more than one ethnicity (489)
multiple_eth <- survey_clean %>%
  filter(str_detect(Ethnicity, ";|Biracial|Multiracial"))

survey_clean <- survey_clean %>%
  anti_join(multiple_eth)

survey_clean$YearsCodePro <- as.integer(as.character(survey_clean$YearsCodePro))

#Relabelling Indigenous
survey_clean$Ethnicity <- case_when(
  str_detect(survey_clean$Ethnicity, "Indigenous") ~ "Indigenous", TRUE ~ survey_clean$Ethnicity)
#6371 cases remaining

#Education
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
                    "Academic researcher|Scientist|Educator|Student|Other|Marketing or sales professional|Product manager|Senior Executive (C-Suite, VP, etc.)|Senior executive/VP|Engineering manager"))

survey_clean <- survey_clean %>%
  anti_join(remove)

#Splitting DevType
survey_unnest <- survey_clean %>% 
  mutate(DevType = str_split(DevType, pattern = ";")) %>%
  unnest(DevType) #

write_csv(survey_clean, here::here("inputs/data/survey_clean.csv"))
write_csv(survey_unnest, here::here("inputs/data/survey_unnest.csv"))



