# CPUE Calculations for 2017-2019 Lingcod Bycatch survey

library(tidyverse)
library(here)
library(lubridate)

## ----Load data----------------------------------------------------------------

bycatch_survey_catch_path <- here("hook_and_line_data","2017_2019_bycatch_survey_catch.csv")
bycatch_catch <- read.csv(bycatch_survey_catch_path, header = TRUE)

bycatch_survey_effort_path <- here("hook_and_line_data","2017_2019_bycatch_survey_effort.csv")
bycatch_effort <- read.csv(bycatch_survey_effort_path, header = TRUE)


## ----Calculate CPUE by angler hours-------------------------------------------

# Total days
bycatch_angler_days <- max(bycatch_effort$fishing.day)

# Total hours
bycatch_angler_hours <- sum(bycatch_effort$effort..angler.hours.)

# How many total yelloweye did they catch?
bycatch_catch_YE <- subset(bycatch_catch, Species == "yelloweye rockfish")
bycatch_nYE <- dim(bycatch_catch_YE)[1]

# Yelloweye CPUE
bycatch_yelloweye_nominal_CPUE_angler_hours <- bycatch_nYE/bycatch_angler_hours
bycatch_yelloweye_nominal_CPUE_angler_hours

# How many total Bocaccio did they catch?
bycatch_catch_boc <- subset(bycatch_catch, Species == "bocaccio")
bycatch_nboc <- dim(bycatch_catch_boc)[1]

# Bocaccio CPUE
bycatch_bocaccio_nominal_CPUE_hours <- bycatch_nboc/bycatch_angler_hours
bycatch_bocaccio_nominal_CPUE_hours

## ----Calculate CPUE by angler days-------------------------------------------

# How many total yelloweye did they catch?
bycatch_catch_YE <- subset(bycatch_catch, Species == "yelloweye rockfish")
bycatch_nYE <- dim(bycatch_catch_YE)[1]

# Yelloweye CPUE
bycatch_yelloweye_nominal_CPUE_angler_days <- bycatch_nYE/bycatch_angler_days
bycatch_yelloweye_nominal_CPUE_angler_days

# How many total Bocaccio did they catch?
bycatch_catch_boc <- subset(bycatch_catch, Species == "bocaccio")
bycatch_nboc <- dim(bycatch_catch_boc)[1]

# Bocaccio CPUE
bycatch_bocaccio_nominal_CPUE_angler_days <- bycatch_nboc/bycatch_angler_days
bycatch_bocaccio_nominal_CPUE_angler_days