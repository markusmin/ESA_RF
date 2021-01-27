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

# Load data
usa_spdf <- readOGR(dsn = here("map_files", "USA_adm0.shp"))

# Convert to df(ish)
usa_spdf_fort <- tidy(usa_spdf)

PS_map <- ggplot(usa_spdf_fort, aes(x = long, y = lat, group = group))+
  geom_vline(xintercept = seq(-123.2, -122, 1/60), color = "gray50", size = 0.1)+
  geom_hline(yintercept = seq(47, 48.5, 1/60), color = "gray50", size = 0.1)+
  #base map
  geom_polygon()+
  xlab("Longitude")+
  ylab("Latitude")+
  # Create grid using geom_vline and geom_hline
  coord_fixed(ylim = c(47.1,48.25),  xlim = c(-123.1,-122.1), ratio = 1.3)+
  theme(plot.background = element_rect(fill = "white"))

# PS_map

ggsave(paste0(fig_dir,"/PW_survey_map.png"),PS_map, width = 8, height = 12)



