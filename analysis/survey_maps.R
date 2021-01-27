# Effort maps for three surveys

library(tidyverse)
library(readxl)
library(here)
library(rgdal)
library(broom)
library(ggpubr)

## ----Load data----------------------------------------------------------------
PW_survey_path <- here("hook_and_line_data","Washington_et_al_74-77_Puget_Sound_data.xls")
excel_sheets(PW_survey_path)
PW_survey <- read_excel(PW_survey_path, sheet = "Main Data")

# Set figure directory
fig_dir <- here("figures", "map_figures")

sort(unique(PW_survey$Location))

# Investigate locations that don't have a letter/number format
as.data.frame(subset(PW_survey, Location %in% c("485", "490", "491", "492")))
# All coppers

# Determine whether letters or numbers correspond to lat/long

# Extract letters/numbers
PW_survey %>%
  mutate(letter_loc = gsub("[^a-zA-Z]", "", Location)) %>%
  mutate(number_loc = gsub("\\D", "", Location)) -> PW_survey

match(substring(PW_survey$letter_loc,1,1),LETTERS[1:26])*nchar(PW_survey$letter_loc)

# Convert letters to numeric
# Match letters to order in alphabet, then add 26 for each additional letter in string
PW_survey %>%
  mutate(letter_loc_numeric = match(substring(letter_loc,1,1),
                                    LETTERS[1:26])+(nchar(letter_loc)-1)*26) -> PW_survey


sort(unique(PW_survey$letter_loc))
sort(unique(PW_survey$number_loc))

# Calculate distance of longitude at this latitude
lon_miles <- cos(47.66*pi/180) * 69.172
nm <- lon_miles/1.151


# Load data
usa_spdf <- readOGR(dsn = here("map_files", "USA_adm0.shp"))

# Convert to df(ish)
usa_spdf_fort <- tidy(usa_spdf)

# Create base map

PS_map <- ggplot(usa_spdf_fort, aes(x = long, y = lat, group = group))+
  # Create grid using geom_vline and geom_hline
  geom_vline(xintercept = seq(-123.1, min_lon, 1/nm), color = "gray50", size = 0.1)+
  geom_hline(yintercept = seq(min_lat, 48.5, 1/60), color = "gray50", size = 0.1)+
  #base map
  geom_polygon(color = "gray20")+
  ylab("Latitude")+
  coord_fixed(ylim = c(47.1,48.25),  xlim = c(-123.1,-122.1), ratio = 1.3)+
  theme(plot.background = element_rect(fill = "white"))

# New starting lat: 49.01
max_lat <- 49.01
# Starting long: inlet to Everett Harbor (-122.20)
min_lon <- -122.19

PW_survey %>%
  # change number to numeric
  mutate(number_loc = as.numeric(number_loc)) %>% 
  mutate(lat_est = max_lat - number_loc/60) %>%
  mutate(lon_est = min_lon - letter_loc_numeric/nm) -> PW_survey

# Add sampling stations on top
sampling_station_map <- PS_map+
  geom_point(data = PW_survey, aes(x = lon_est, y = lat_est),
                                          inherit.aes = FALSE, shape = 22, size = 5, color = "black", fill = "white")

ggsave(paste0(fig_dir,"/PW_sampling_station_map.png"),sampling_station_map, width = 8, height = 12)


