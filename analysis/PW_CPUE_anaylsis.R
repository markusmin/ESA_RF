# CPUE Calculations for Percy Washington 1974-1977 hook & line survey

library(tidyverse)
library(readxl)
library(here)
library(lubridate)

## ----Load data----------------------------------------------------------------
PW_survey_path <- here("hook_and_line_data","Washington_et_al_74-77_Puget_Sound_data.xlsx")
excel_sheets(PW_survey_path)
PW_survey <- read_excel(PW_survey_path, sheet = "catch_species_names")

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

# Significant issue: "An unrecorded number of sites were fished with no encounters."
# CPUE will be an overestimate of catchability; 

PW_effort %>% mutate(., ID = paste0(Year,".",survey_no)) -> PW_effort

# Convert times to date using regex and lubridate
PW_effort$start_time <- gsub('^(.{2})(.*)$', '\\1:\\2', PW_effort$start_time)
PW_effort$stop_time <- gsub('^(.{2})(.*)$', '\\1:\\2', PW_effort$stop_time)

PW_effort$start_time <- hm(PW_effort$start_time)
PW_effort$stop_time <- hm(PW_effort$stop_time)

# Calculate total time per fishing event
PW_effort %>%
  mutate(., total_time = as.duration(stop_time - start_time)) -> PW_effort

# Calculate angler hours per fishing event
PW_effort %>%
  mutate(nAnglers = as.numeric(nAnglers)) %>%
  mutate(., angler_time = total_time*nAnglers) -> PW_effort

subset(PW_effort, total_time < 0)
# One odd observation has negative time - says that the start time was "6500"
# Did they mean 6:50?
# The earliest start time was 6:35, so 6:50 seems like the most likely start time
subset(PW_effort, total_time <= 900)
# 4 fishing events were 10 minutes long, 5 were 15 minutes long. So entirely possible that it was ten minutes,
# from 6:50 to 7:00
# Look into how many fish were caught on the short trips
PW_survey %>% 
  dplyr::rename("survey_no" = "Survey No.",
                "start_time" = "Time Start",
                "stop_time" = "Time Stop",
                "nAnglers" = "No. Anglers")%>%
  mutate(., ID = paste0(Year,".",survey_no)) -> PW_survey
as.data.frame(subset(PW_survey, PW_survey$ID %in% unique(subset(PW_effort, total_time <= 900)$ID)))
# Usually only one, on 2 of 9 occasions they caught two fish. Seems unlikely that they caught 4 fish
# in ten minutes.

# Replace time with NA for 74.040 (where start time was 6500)
PW_effort[PW_effort$ID == "74.040",]$total_time <- NA
PW_effort[PW_effort$ID == "74.040",]$angler_time <- NA

# Calculate total angler hours with NAs missing
sum(PW_effort$angler_time, na.rm = TRUE)/3600

# Figure out how much missing data we have
length(unique(subset(PW_effort, is.na(angler_time)))$ID)
length(unique(subset(PW_effort, is.na(total_time)))$ID)
length(unique(subset(PW_effort, is.na(nAnglers)))$ID)
length(unique(subset(PW_effort, !(is.na(angler_time))))$ID)

# 29 of 305 fishing events don't have time and/or number of anglers, so no angler hours

# Could we interpolate these values?

# How many anglers were usually on the trip?
table(PW_effort$nAnglers)
mean(PW_effort$nAnglers, na.rm = TRUE)
# Mean of 2.8 anglers, mode is 3, median is 3

# How long did they usually fish for?
table(as.character(PW_effort$total_time))
mean(PW_effort$total_time, na.rm = TRUE)/3600
# Mean was 2.08 hours

# Interpolate all missing values
length(unique(subset(PW_effort, is.na(total_time) & !(is.na(nAnglers))))$ID) # 14 where total time is missing but nAnglers is not
length(unique(subset(PW_effort, is.na(total_time) & is.na(nAnglers)))$ID) # 8 where both total time and nAnglers are missing
length(unique(subset(PW_effort, !(is.na(total_time)) & is.na(nAnglers)))$ID) # 7 where total time is not missing but nAnglers is

for (i in 1:dim(PW_effort)[1]){
  # Fill in where total_time is missing but nAnglers is not
  if (is.na(PW_effort$total_time[i]) & !(is.na(PW_effort$nAnglers[i]))){
    PW_effort$angler_time[i] <- mean(PW_effort$total_time, na.rm = TRUE)*PW_effort$nAnglers[i]
  }
  # Fill in where number of nAnglers is missing but total_time is not
  else if (is.na(PW_effort$nAnglers[i]) & !(is.na(PW_effort$total_time[i]))){
    PW_effort$angler_time[i] <- mean(PW_effort$nAnglers, na.rm = TRUE)*PW_effort$total_time[i]
  }
  # Fill in where both total_time and nAnglers is missing
  else if (is.na(PW_effort$total_time[i]) & is.na(PW_effort$nAnglers[i])){
    PW_effort$angler_time[i] <- mean(PW_effort$total_time, na.rm = TRUE)*mean(PW_effort$nAnglers, na.rm = TRUE)
  }

}

## ----Calculate CPUE by angler hours-------------------------------------------

# Create angler_hours field by conversion from seconds
PW_effort %>% mutate(., angler_hours = angler_time/3600) -> PW_effort

# Calculate total number of angler hours
PW_angler_hours <- sum(PW_effort$angler_hours)
# 1761.955 angler hours total (estimate)

# How many total yelloweye did they catch?
PW_survey_YE <- subset(PW_survey, Species == "Yellow Eye")
PW_nYE <- dim(PW_survey_YE)[1]

# Yelloweye CPUE
PW_yelloweye_nominal_CPUE_angler_hours <- PW_nYE/PW_angler_hours

# How many total Bocaccio did they catch?
PW_survey_boc <- subset(PW_survey, Species == "Bocaccio")
PW_nboc <- dim(PW_survey_boc)[1]

# Bocaccio CPUE
PW_bocaccio_nominal_CPUE_hours <- PW_nboc/PW_hours


## ----Calculate CPUE by angler days-------------------------------------------

# Create date field
PW_effort %>% mutate(., date = paste0("19",Year,"-",Month,"-",Day)) -> PW_effort

# Calculate total number of angler days
PW_angler_days <- length(unique((PW_effort$date)))
# 1761.955 angler hours total (estimate)

# How many total yelloweye did they catch?
PW_survey_YE <- subset(PW_survey, Species == "Yellow Eye")
PW_nYE <- dim(PW_survey_YE)[1]

# Yelloweye CPUE
PW_yelloweye_nominal_CPUE_angler_days <- PW_nYE/PW_angler_days
PW_yelloweye_nominal_CPUE_angler_days

# How many total Bocaccio did they catch?
PW_survey_boc <- subset(PW_survey, Species == "Bocaccio")
PW_nboc <- dim(PW_survey_boc)[1]

# Bocaccio CPUE
PW_bocaccio_nominal_CPUE_angler_days <- PW_nboc/PW_angler_days
PW_bocaccio_nominal_CPUE_angler_days






## ----Supplements -------------------------------------------------------------

# How many fishing events did they not catch any fish?
# We can't know this for sure, but we can get a sense using the survey IDs

## 1974
IDs_74 <- sort(unique(subset(PW_effort, Year == 74)$survey_no))
# 1974: Goes from 001 to 045, with 33 not having a leading zero
vec_74 <- sprintf("%03d", seq(1,45,1))
setdiff(IDs_74, vec_74)
# No missing values (33 is just coded differently)

## 1975
IDs_75 <- sort(unique(subset(PW_effort, Year == 75)$survey_no))
# 1975: Goes from 001 to 060, with three odd values: 901, 903, and 904
vec_75 <- sprintf("%03d", seq(1,60,1))
setdiff(IDs_75, vec_75)
# There's an extra 03, in addition to the 003

## 1976
IDs_76 <- sort(unique(subset(PW_effort, Year == 76)$survey_no))
# 1976: Goes from 001 to 154, with three odd values: 803, 810, and UW1
vec_76 <- sprintf("%03d", seq(1,154,1))
setdiff(IDs_76, vec_76)
# No missing values

## 1977
IDs_77 <- sort(unique(subset(PW_effort, Year == 77)$survey_no))
# 1976: Goes from 001 to 027
vec_77 <- sprintf("%03d", seq(1,27,1))
setdiff(IDs_77, vec_77)
# No missing values

# So it appears that the issue of missing effort in terms of unrecorded trips is not
# as big a deal as I thought!



