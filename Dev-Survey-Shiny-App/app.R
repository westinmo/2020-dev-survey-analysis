#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(dplyr)
library(ggplot2)
library(stats)
library(graphics)
library(here)
library(readr)

###Preparing dataset for app
survey_app <- readr::read_csv("survey_results_public.csv")
survey_app  <- survey_app %>%
    dplyr::filter(Country == "United States", 
           Employment == "Employed full-time",
           MainBranch == "I am a developer by profession" | MainBranch == "I am not primarily a developer, but I write code sometimes as part of my work",
    ) %>% #selecting survey questions to include in the app (mostly ones that don't have too many levels to plot; focus on demographics)
    select("Age", "Gender", "Ethnicity", "Sexuality", "Trans", "EdLevel", "UndergradMajor", "Age1stCode",
           "MainBranch", "DevType", "Hobbyist", "YearsCode", "YearsCodePro", "OrgSize", "JobSat", "NEWOvertime", 
           "SOAccount") %>%
    mutate(YearsCodePro = parse_number(YearsCodePro),) %>%
    mutate(Age1stCode = parse_number(Age1stCode),) %>%
    mutate(YearsCode = parse_number(YearsCode),)

#cleaning gender
survey_app$Gender <- case_when(str_detect(survey_app$Gender, "Non-binary, genderqueer, or gender non-conforming")
                                 ~ "Non-binary, genderqueer, or gender non-conforming",
                                 TRUE ~ survey_app$Gender) 

survey_app$Gender[survey_app$Gender == "Woman;Man" ] <- "Non-binary, genderqueer, or gender non-conforming"
survey_app <- survey_app[!is.na(survey_app$Gender), ]

#cleaning devtype
survey_app <- survey_app %>% 
    mutate(DevType = str_split(DevType, pattern = ";")) %>%
    unnest(DevType) #
survey_app$DevType <- as.factor(survey_app$DevType)

#cleaning ethnicity
survey_app <- survey_app %>% 
    mutate(Ethnicity = str_split(Ethnicity, pattern = ";")) %>%
    unnest(Ethnicity) #
survey_app$Ethnicity <- as.factor(survey_app$Ethnicity)

#cleaning sexuality
survey_app <- survey_app %>% 
    mutate(Sexuality = str_split(Sexuality, pattern = ";")) %>%
    unnest(Sexuality) #
survey_app$Sexuality <- as.factor(survey_app$Sexuality)

table(survey_app$Gender)

survey_app <- plyr::rename(survey_app, c("EdLevel" = "Education", "OrgSize" = "OrganizationSize", 
                                          "JobSat" = "JobSatisfaction", "NEWOvertime" = "HowOftenOvertime",
                                          "SOAccount" = "StackOverflowAccount", "Trans" = "Transgender", 
                                          "DevType" = "DeveloperType", "Hobbyist" = "CodeHobbyist",
                                         "YearsCodePro" = "YearsProfessionalCodingExp", "YearsCode" = "YearsCodingExp"))

#dataset for Salary
survey_app2  <- readr::read_csv(here::here("survey_results_public.csv"))
survey_app2  <- survey_app2 %>% filter(Country == "United States", 
                                      Employment == "Employed full-time",
                                      MainBranch == "I am a developer by profession" | MainBranch == "I am not primarily a developer, but I write code sometimes as part of my work",
) 
survey_app2 <- plyr::rename(survey_app2, c("ConvertedComp" = "Salary"))
survey_app2$Gender <- case_when(str_detect(survey_app2$Gender, "Non-binary, genderqueer, or gender non-conforming")
                               ~ "Non-binary, genderqueer, or gender non-conforming",
                               TRUE ~ survey_app2$Gender) 

survey_app2$Gender[survey_app2$Gender == "Woman;Man" ] <- "Non-binary, genderqueer, or gender non-conforming"

#NAs
survey_app2 <- survey_app2[!is.na(survey_app2$Gender), ] 
survey_app2 <- survey_app2[!is.na(survey_app2$Gender), ]

#UI
ui <- fluidPage(
    #theme
    theme = shinytheme("lumen"),                       
    multiple = TRUE,
    
    # Application title
    titlePanel("Stack Overflow 2020 Developer Survey"),
    tabsetPanel(
    tabPanel("Survey",
             sidebarPanel(p('This Shiny App is a companion piece to my', a(href = 'https://github.com/westinmo/2020-dev-survey-analysis', ' analysis ', .noWS = "outside"), 
                            'of gender-based salary differences in the tech industry, which uses data from the',  
                            a(href = 'https://insights.stackoverflow.com/survey', " 2020 Stack Overflow Developer Survey. ", .noWS = "outside"), 
                            "You can explore some (but not all) of the responses to the survey by gender. While the survey received a diverse range of responses from all over the world, the data on this app focuses on exploring responses from full-time tech workers in the United States, as they were the focus of my analysis. A more comprehensive overview of the full set of responses is available", 
                            a(href = 'https://insights.stackoverflow.com/survey/2020', " here. ", .noWS = "outside"), "The ‘salary’ tab contains an interactive density plot of the respondents’ salary distributions by gender.", .noWS = c("after-begin", "before-end")),
                          #selecting x-axis
                          selectInput(inputId = "x", label = "Survey Questions:",
                                      choices = colnames(survey_app),
                                      selected = "DeveloperType")),
             mainPanel(plotOutput("surveyplot")),
    ),
    tabPanel("Salary",
             fillPage(plotlyOutput("surveyplot2", height  = "100%", width = "100%")),

    )
        
    )
    
)
        
                 
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$surveyplot <- renderPlot({
        # generate bins based on input$bins from ui.R
        ggplot(survey_app, aes_string(x = input$x, fill = "Gender")) + geom_bar(stat = "count") +
            scale_fill_manual(values = c("#FC8D62", "#66C2A5", "#8DA0CB")) + 
            coord_flip() +
            theme_light() +
            theme(axis.title=element_text(size=16, face="bold"), 
                  legend.position="bottom", legend.title = element_text(size = 14), 
                  title =element_text(size=18, face="bold")) +
            labs(y = "# of Responses")
    })
    output$surveyplot2 <- renderPlotly({
        # generate bins based on input$bins from ui.R
            ggplotly(ggplot(survey_app2, aes_string(x = "Salary", color = "Gender", fill = "Gender")) +
            geom_density(alpha = 0.5) +
            scale_x_continuous(labels = scales::dollar_format()) +
            labs(x = "Annual Salary in USD", y = "Density", title = "Salary Distribution for Developers by Gender") +
            theme_light() + 
            scale_color_manual(values = c("#FC8D62", "#66C2A5", "#8DA0CB")) +
            scale_fill_manual(values = c("#FC8D62", "#66C2A5", "#8DA0CB")) +
            theme(plot.title = element_text(face = "bold")), height = 700) %>%
            layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
