# CPUE Calculations for 2014-2015 ESA Genetics survey

library(tidyverse)
library(here)
library(lubridate)

## ----Load data----------------------------------------------------------------

gen_survey_path <- here("hook_and_line_data","2014_2015_genetics_survey.xlsx")
excel_sheets(gen_survey_path)
gen_survey <- read_excel(gen_survey_path, sheet = "data")
