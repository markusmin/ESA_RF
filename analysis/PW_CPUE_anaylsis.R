# CPUE Calculations for Percy Washington 1974-1977 hook & line survey

library(tidyverse)
library(readxl)
library(here)

## ----Load data----------------------------------------------------------------
PW_survey_path <- here("hook_and_line_data","Washington_et_al_74-77_Puget_Sound_data.xlsx")
excel_sheets(PW_survey_path)
PW_survey <- read_excel(PW_survey_path, sheet = "catch_species_names")

# Set figure directory
fig_dir <- here("exploratory_figures")

## ----Prepare effort data------------------------------------------------------
colnames(PW_survey)

# Subset data to only columns relevant to effort; rename columns
dplyr::select(PW_survey, c("Year", "Survey No.", "Month", "Day", "Time Start", "Location", "Time Stop", "No. Anglers")) %>%
  dplyr::rename("survey_no" = "Survey No.",
                "start_time" = "Time Start",
                "stop_time" = "Time Stop",
                "nAnglers" = "No. Anglers") %>%
  distinct() -> PW_effort

# I believe that you can determine a distinct fishing "event" (same as set in recent surveys) by a combination
# of Year and survey_no - it appears they restarted numbering of surveys at the beginning of the year

PW_effort %>% mutate(., ID = paste0(Year,".",survey_no)) -> PW_effort

sort(PW_effort$survey_no)



