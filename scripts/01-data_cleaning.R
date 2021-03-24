library(tidyverse)
library(here)
library(readr)
library(visdat)
library(janitor)
library(questionr)

#Reading in Data
survey_raw <- readr::read_csv(here("inputs/data/survey_results_public.csv"))
#survey schema outlining which questions correspond to each column name
schema <- read_csv(here("inputs/data/survey_results_schema.csv"))

#Filtering data for individuals who live in the United States and are employed full time
survey_clean <- survey %>%
  filter(Country == "United States", Employment == "Employed full-time")

#Removing rows with NAs
survey_clean <- survey_clean[!is.na(survey_clean$ConvertedComp), ]
survey_clean <- survey_clean[!is.na(survey_clean$Gender), ]
survey_clean <- survey_clean[!is.na(survey_clean$Ethnicity), ]
survey_clean <- rename(survey_clean, c("Income" = "ConvertedComp"))


#Re-categorizing gender responses into Man, Women, or Non-binary/genderqueer/gender non-conforming
survey_clean$Gender <- case_when(str_detect(survey_clean$Gender, "Non-binary, genderqueer, or gender non-conforming")
                                ~ "Non-binary, genderqueer, or gender non-conforming",
                                TRUE ~ survey_clean$Gender)
survey_clean$Gender[survey_clean$Gender == "Woman;Man" ] <- "Non-binary, genderqueer, or gender non-conforming"

survey_clean$Gender <- as.factor(survey_clean$Gender) %>%
  droplevels()

table(survey_clean$Ethnicity)
#Ethnicity

#Individuals who selected multiracial or biracial (among their other selected ethnic groups), were placed in each respective group
survey_clean$Ethnicity <- case_when(
  str_detect(survey_clean$Ethnicity, "Multiracial") ~ "Multiracial",
  str_detect(survey_clean$Ethnicity, "Biracial") ~ "Biracial", TRUE ~ survey_clean$Ethnicity
  )

#Individuals who selected multiple ethnic groups (but did not select multiracial or biracial) 
#Placed into either biracial (if selected 2 ethic groups) or multiracial (if selected 3)
survey_clean$Ethnicity <- case_when(
  str_detect(survey_clean$Ethnicity, "Black or of African descent;Hispanic or Latino/a/x") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, "Black or of African descent;Middle Eastern") ~ "Biracial", 
  str_detect(survey_clean$Ethnicity, "Black or of African descent;Hispanic or Latino/a/x;
             White or of European descent;Indigenous (such as Native American, Pacific Islander, or Indigenous Australian)") ~ "Multiracial",
  str_detect(survey_clean$Ethnicity, "Black or of African descent;South Asian") ~ "Biracial", 
  str_detect(survey_clean$Ethnicity, "Black or of African descent;White or of European descent") ~ "Biracial", 
  str_detect(survey_clean$Ethnicity, "Black or of African descent;Southeast Asian") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, "East Asian;Hispanic or Latino/a/x") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, "East Asian;Hispanic or Latino/a/x;White or of European descent") ~ "Multiracial",
  str_detect(survey_clean$Ethnicity, "East Asian;Indigenous (such as Native American, Pacific Islander, or Indigenous Australian)") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, "East Asian;Middle Eastern") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, "East Asian;South Asian") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, "East Asian;Southeast Asian") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, "East Asian;White or of European descent") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, "East Asian;White or of European descent;Southeast Asian") ~ "Multiracial",
  str_detect(survey_clean$Ethnicity, "Hispanic or Latino/a/x;Indigenous (such as Native American, Pacific Islander, or Indigenous Australian)") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, "Hispanic or Latino/a/x;Middle Eastern") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, "Hispanic or Latino/a/x;Middle Eastern;White or of European descent") ~ "Multiracial",
  str_detect(survey_clean$Ethnicity, "Hispanic or Latino/a/x;White or of European descent") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, "South Asian;Southeast Asian") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, "White or of European descent;South") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, "White or of European descent;Indigenous") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, "Hispanic or Latino/a/x;Indigenous") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, ";Southeast Asian") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, "Middle Eastern;") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, "East Asian;Indigenous") ~ "Biracial", 
  str_detect(survey_clean$Ethnicity, "Indigenous") ~ "Indigenous",
  TRUE ~ survey_clean$Ethnicity
)

survey_clean$Ethnicity <- case_when(
  str_detect(survey_clean$Ethnicity, "Black or of African descent;Hispanic or Latino/a/x") ~ "Biracial",
  str_detect(survey_clean$Ethnicity, "Black or of African descent;Middle Eastern") ~ "Biracial", 
  TRUE ~ survey_clean$Ethnicity
)

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

survey_results_parsed <- survey_clean %>%
  mutate(DevType = str_split(DevType, pattern = ";")) %>%
  unnest(DevType) %>%
  mutate(
    DevType = case_when(
      str_detect(str_to_lower(DevType), "data scientist") ~ "Data scientist",
      str_detect(str_to_lower(DevType), "data or business") ~ "Data analyst",
      str_detect(str_to_lower(DevType), "desktop") ~ "Desktop",
      str_detect(str_to_lower(DevType), "embedded") ~ "Embedded",
      str_detect(str_to_lower(DevType), "devops") ~ "DevOps",
      str_detect(DevType, "Engineer, data") ~ "Data engineer",
      str_detect(str_to_lower(DevType), "site reliability") ~ "DevOps",
      TRUE ~ DevType
    ),
    DevType = str_remove_all(DevType, "Developer, "),
    DevType = str_to_sentence(DevType),
    DevType = str_replace_all(DevType, "Qa", "QA"),
    DevType = str_replace_all(DevType, "Sre", "SRE"),
    DevType = str_replace_all(DevType, "Devops", "DevOps")
  ) 

