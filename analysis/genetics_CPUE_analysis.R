# CPUE Calculations for 2014-2015 ESA Genetics survey

library(tidyverse)
library(here)
library(lubridate)
library(readxl)
library(broom)
library(rgdal)

## ----Load data----------------------------------------------------------------

gen_survey_path <- here("hook_and_line_data","2014_2015_genetics_survey.xlsx")
excel_sheets(gen_survey_path)
gen_survey <- read_excel(gen_survey_path, sheet = "data")

# Warnings are okay, they're just in reference to fin clip IDs

# Replace all spaces in column names with underscores
gen_survey %>% select_all(funs(gsub(" ", "_", .))) -> gen_survey

# Add underscores to captain names
gen_survey$Captain <- gsub(" ", "_", gen_survey$Captain)

# Extract date from the times
gen_survey$time <- ymd_hms(gen_survey$time)

gen_survey %>%
  mutate(date = date(time)) -> gen_survey

## --Interpolate number of anglers based on boats-------------------------------

# First create DFs of the record number of anglers per captain
for (i in 1:length(unique(gen_survey$Captain))){
  captain_df <- subset(gen_survey, Captain == unique(gen_survey$Captain)[i])
  captain_df %>%
    group_by(days_of_fishing) %>% 
    summarise(mean_anglers = mean(anglers)) -> captain_anglers_per_day
  # Drop NAs
  captain_anglers_per_day <-   subset(captain_anglers_per_day, !(is.na(mean_anglers)))
  captain_anglers_per_day <- captain_anglers_per_day$mean_anglers
  assign(paste0(unique(gen_survey$Captain)[i],"_anglers"), captain_anglers_per_day)  
}
# For Tom Burlingame and Brett Rosson who have no angler data, just make vector c(3,4)
Tom_Burlingame_anglers <- c(3,4)
Brett_Rosson_anglers <- c(3,4)

# Now fill in the missing angler data in gen_survey
# Only look at data from 2014, because this is where angler info is missing
# For the captains with no angler information available (Tom and Brett), randomly select 3 or 4 anglers
# tom_brett_days <- unique(subset(gen_survey, Captain %in% c("Tom_Burlingame", "Brett_Rosson"))$days_of_fishing)
# for (i in tom_brett_days){
#   gen_survey[gen_survey$days_of_fishing == i,]$anglers <- sample(c(3,4),1)
# }


# For the captains with some angler information available, randomly select from existing data
# Set seed for reproducibility
set.seed(123)
  for (i in 1:max(subset(gen_survey, year == 2014)$days_of_fishing)){
    gen_survey[gen_survey$days_of_fishing == i,]$anglers <- sample(eval(parse(text = paste0(unique(subset(gen_survey, 
                                                           days_of_fishing == i)$Captain), "_anglers"))),1)
  }


# Calculate total effort per day

# There is no need to have different code for 2014 and 2015, despite the difference
# in recording - this is because the first and last times will be informative, whether
# they are start/end times or fish caught

gen_survey %>%
  group_by(date) %>%
  mutate(total_time = as.numeric(max(time) - min(time), units = "hours")) %>% 
# Calculate the mean number of anglers that day
  mutate(mean_anglers = mean(anglers)) %>%
  mutate(total_angler_hours = total_time * mean_anglers) %>%
  dplyr::select(date, total_time, mean_anglers,total_angler_hours)-> gen_survey_effort_per_day
gen_survey_effort_per_day <- gen_survey_effort_per_day[!duplicated(gen_survey_effort_per_day[,c("date")]),]

# Sum effot
gen_total_angler_hours <- sum(gen_survey_effort_per_day$total_angler_hours)

## ----Sum yelloweye and bocaccio catch-----------------------------------------

# How many total yelloweye did they catch?
gen_catch_YE <- subset(gen_survey, Species == "yelloweye rockfish")
gen_nYE <- dim(gen_catch_YE)[1]

# How many total Bocaccio did they catch?
gen_catch_boc <- subset(gen_survey, Species == "bocaccio")
gen_nboc <- dim(gen_catch_boc)[1]


##--Calculate CPUE--------------------------------------------------------------
gen_YE_CPUE <- gen_nYE/gen_total_angler_hours
gen_boc_CPUE <- gen_nboc/gen_total_angler_hours

gen_total_angler_hours
gen_YE_CPUE
gen_boc_CPUE


## ---CREATE HEATMAP of effort--------------------------------------------------

## ---- Prepare data for map ---------------------------------------------------

# Estimate effort by specific location

# For each time point (fish caught), subtract the current time point from the next
# time point to approximate effort at that location
# Must have a hard cutoff between days
# This will be an underestimate because the time before the first fish of the day
# and the time after the last fish of the day will be missing
# Time before first fish also missing in original effort calculation

gen_survey$time_estimate <- 0

# Estimate time at each spot
for (i in 1:(nrow(gen_survey)-1)){
  if (gen_survey[i,]$days_of_fishing == gen_survey[i+1,]$days_of_fishing){
    gen_survey[i,]$time_estimate <- as.numeric(gen_survey[i+1, ]$time - gen_survey[i,]$time, units = "hours")
  }
}

# Estimate angler hours per location
gen_survey %>% 
  mutate(angler_hours_estimate = anglers * time_estimate) -> gen_survey



## ----Base map of Puget sound--------------------------------------------------

# Load data
usa_spdf <- readOGR(dsn = here("map_files", "USA_adm0.shp"))

# Convert to df(ish)
usa_spdf_fort <- tidy(usa_spdf)

# Load BC data
BC_shp <- readOGR(dsn = here("map_files", "canada", "lpr_000b16a_e.shp"))
# proj4string(BC_shp)
BC_shp_transform <- spTransform(BC_shp, "+init=epsg:4326")
BC_spdf_fort <- tidy(BC_shp_transform)


# Create base map in ggplot
greater_PS_map <- ggplot(usa_spdf_fort, aes(x = long, y = lat, group = group))+
  # Create grid using geom_vline and geom_hline
  # geom_vline(xintercept = seq(-123.1, min_lon, 1/nm), color = "gray50", size = 0.1)+
  # geom_hline(yintercept = seq(min_lat, 48.5, 1/60), color = "gray50", size = 0.1)+
  #base map
  geom_polygon(color = "black", fill = "black")+
  geom_polygon(data = BC_spdf_fort, aes(x = long, y = lat, group = group), color = "black", fill = "black", inherit.aes = FALSE) +
  ylab("Latitude")+
  xlab("Longitude")+
  coord_fixed(ylim = c(47.1,48.8),  xlim = c(-124.7,-122.1), ratio = 1.3)+
  theme(panel.background = element_rect(fill = "gray50"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


## --Plot map of effort (estimated angler hours)--------------------------------
# Set figure directory
fig_dir <- here("figures", "map_figures")

genetics_effort_map <- greater_PS_map + 
  stat_summary_2d(data = gen_survey, aes(x = lon, y = lat, z = angler_hours_estimate),
                  binwidth = c(0.02, 0.02),inherit.aes = FALSE)+
  scale_fill_gradientn(limits=c(0,60), breaks=seq(0, 60, by = 10), colours=c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", 
                                                                             "#fc4e2a", "#e31a1c", "#b10026"))+
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12))+
  labs(fill = "Angler Hours")

ggsave(paste0(fig_dir,"/genetics_effort_map.png"),genetics_effort_map, width = 12, height = 10)


