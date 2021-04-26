# 2020-dev-survey-analysis

## About

This repo contains R code for my analysis and report examining representation and gender-based wage inequalities in the United States tech industry. This study utilized data from the [2020 Stack Overflow Developer Survey](https://insights.stackoverflow.com/survey/2020), which is available to download [here](https://insights.stackoverflow.com/survey) under the [Open Database License (ODbL)](https://opendatacommons.org/licenses/odbl/1-0/). The final report can be viewed [here](https://github.com/westinmo/2020-dev-survey-analysis/blob/main/outputs/report/report.pdf).

The main components of this repo are: `inputs`, `scripts`, `outputs`, and `shiny`.

- `inputs` 
    - Contains raw csv and schema csv with for the 2020 Developer survey, shared under the ODbl.
- `scripts`
    - Contains scripts to clean the survey data and prepare it for analysis, and 
- `outputs`
    - Contains an Rmd file to generate the final report, a pdf of the report, and a bibtex file containing all references cited in the report
- `shiny`
    - Contains a shiny application file used to build a [companion shiny app](https://mwestin.shinyapps.io/Dev-Survey-Shiny-App/) which can be used to explore some of the survey data covered in this report

## Setup

To generate the report:
- Clone this repository and open `2020-dev-survey-analysis.Rproj` in R Studio
- Install any missing R packages
- Run `01-data_cleaning.R` in the `scripts` folder
- Run `02-psm.R` 
- Run `report.Rmd` in outputs/report/ and knit pdf to compile the final report
