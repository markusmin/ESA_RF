# Effort maps for three surveys

library(tidyverse)
library(readxl)
library(here)
library(rgdal)
library(broom)
library(ggpubr)
library(sf)

# Set figure directory
fig_dir <- here("figures", "map_figures")

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


# Create map in ggplot

PS_map <- ggplot(usa_spdf_fort, aes(x = long, y = lat, group = group))+
  # Create grid using geom_vline and geom_hline
  # geom_vline(xintercept = seq(-123.1, min_lon, 1/nm), color = "gray50", size = 0.1)+
  # geom_hline(yintercept = seq(min_lat, 48.5, 1/60), color = "gray50", size = 0.1)+
  #base map
  geom_polygon(color = "gray20")+
  ylab("Latitude")+
  coord_fixed(ylim = c(47.1,48.25),  xlim = c(-123.1,-122.1), ratio = 1.3)+
  theme(plot.background = element_rect(fill = "white"))

greater_PS_map <- ggplot(usa_spdf_fort, aes(x = long, y = lat, group = group))+
  # Create grid using geom_vline and geom_hline
  # geom_vline(xintercept = seq(-123.1, min_lon, 1/nm), color = "gray50", size = 0.1)+
  # geom_hline(yintercept = seq(min_lat, 48.5, 1/60), color = "gray50", size = 0.1)+
  #base map
  geom_polygon(color = "gray20", fill = "gray20")+
  geom_polygon(data = BC_spdf_fort, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  ylab("Latitude")+
  coord_fixed(ylim = c(47.1,48.8),  xlim = c(-124.7,-122.1), ratio = 1.3)+
  theme(plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


## ----Percy Washington Survey--------------------------------------------------

## Load data
PW_survey_path <- here("hook_and_line_data","Washington_et_al_74-77_Puget_Sound_data.xlsx")
excel_sheets(PW_survey_path)
PW_survey <- read_excel(PW_survey_path, sheet = "catch_species_names")

# Investigate locations that don't have a letter/number format
# as.data.frame(subset(PW_survey, Location %in% c("485", "490", "491", "492")))
# All coppers

# Determine whether letters or numbers correspond to lat/long

# Extract letters/numbers
PW_survey %>%
  mutate(letter_loc = gsub("[^a-zA-Z]", "", Location)) %>%
  mutate(number_loc = gsub("\\D", "", Location)) %>%
  mutate(number_loc_numeric = as.numeric(number_loc)) -> PW_survey

# match(substring(PW_survey$letter_loc,1,1),LETTERS[1:26])*nchar(PW_survey$letter_loc)

# Convert letters to numeric
# Match letters to order in alphabet, then add 26 for each additional letter in string
# Account for the fact that letters start at B, not A (hence -1 at the end)
PW_survey %>%
  mutate(letter_loc_numeric = match(substring(letter_loc,1,1),
                                    LETTERS[1:26])+(nchar(letter_loc)-1)*26-1) -> PW_survey

# Adjust the letters to be positive east of Point Monroe (M78) and negative west of it
# Adjust the numbers to be positive north of Point Monroe (M78) and negative south of it
PW_survey %>% 
  mutate(letter_loc_numeric = letter_loc_numeric-12) %>% 
  mutate(number_loc_numeric = number_loc_numeric - 78) -> PW_survey

sort(unique(PW_survey$letter_loc))
sort(unique(PW_survey$number_loc))

# Calculate distance of longitude at this latitude
lon_miles <- cos(47.66*pi/180) * 69.172
nm <- lon_miles/1.151

# Starting point is Point Monroe (M78)
point_monroe_lat <- 47.7088
point_monroe_lon <- -122.5115

# Calculate lat/lon using M78 as anchoring point
PW_survey %>%
  mutate(lat_est = point_monroe_lat - number_loc_numeric/60) %>%
  mutate(lon_est = point_monroe_lon - letter_loc_numeric/nm) -> PW_survey

# Map of names of sampling stations
sampling_station_map <- greater_PS_map+
  # geom_point(data = PW_survey, aes(x = lon_est, y = lat_est),
  #                                         inherit.aes = FALSE, shape = 22, size = 5, color = "black", fill = "white")
  geom_text(data = PW_survey, aes(x = lon_est, y = lat_est, label = Location), size = 1.1, color = "black", inherit.aes = FALSE)

ggsave(paste0(fig_dir,"/PW_sampling_station_map_locations_labeled.png"),sampling_station_map, width = 12, height = 10)

# Points for sampling stations
sampling_station_map <- greater_PS_map+
  geom_point(data = PW_survey, aes(x = lon_est, y = lat_est),
                                          inherit.aes = FALSE, shape = 21, size = 3, color = "black", fill = "white")

ggsave(paste0(fig_dir,"/PW_sampling_station_map.png"),sampling_station_map, width = 12, height = 10)

# Points that are totally landlocked: Q10, N10, I21 (these three are all near Bellingham bay); M110 & B110 (by Tacoma)

# Points that are marginally on land: N72, Q83, H80, H79

# What locations have no lat/longs?
sort(unique(PW_survey$Location))
# 485, 490, 491, 492


PW_effort <- left_join(PW_effort, PW_survey_locations, by = "Location")
# EFFORT HEATMAP in hours
PW_effort_map <- greater_PS_map + stat_summary_2d(data = PW_effort, aes(x = lon_est, y = lat_est, z = angler_time/3600),
                                                                       binwidth = c(0.02, 0.02),inherit.aes = FALSE)+
  scale_fill_gradientn(limits=c(0,30), breaks=seq(0, 30, by=5), colours=c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", 
                                                                          "#fc4e2a", "#e31a1c", "#b10026"))+
  theme(legend.title = element_text(size = 10))+
  labs(fill = "Angler Hours")

ggsave(paste0(fig_dir,"/PW_effort_map.png"),PW_effort_map, width = 12, height = 10)


## ----2014-2015 Genetics Survey------------------------------------------------

## Load data
gen_survey_path <- here("hook_and_line_data","2014_2015_genetics_survey.xlsx")
excel_sheets(gen_survey_path)
gen_survey <- read_excel(gen_survey_path, sheet = "data")

# Remove non-fish datapoints
gen_species_obs <- subset(gen_survey, !(Species %in% c("start", "end")))

# Look at where observations are
# At least one NA, no lat/long for a yelloweye
range(gen_species_obs$lat, na.rm = TRUE)
range(gen_species_obs$lon, na.rm = TRUE)

# Warnings are okay - they are for fields that we don't care about ("waypoint" is duplicated and "fin clip" has non-numeric values)
gen_catch_heatmap <- greater_PS_map + geom_bin2d(data = gen_species_obs, aes(x = lon, y = lat, fill=cut(..count.., c(0,5,10,20,50,100,150))),binwidth = c(0.02, 0.02),inherit.aes = FALSE) +
  scale_fill_manual(values = c("#2b83ba", "#abdda4","#ffffbf","#fdae61","#d7191c"))+
  guides(fill=guide_legend(title="Number of Fish"))

ggsave(paste0(fig_dir,"/genetics_catch_heatmap_allspecies.png"),gen_catch_heatmap, width = 12, height = 10)



## ----2017-2019 Bycatch Survey-------------------------------------------------

## Load data
bycatch_survey_path <- here("hook_and_line_data","2017_2019_bycatch_survey_catch.csv")
bycatch_survey <- read.csv(bycatch_survey_path, header = TRUE)

# Look at where observations are
# At least one NA, no lat/lon for a yelloweye
range(bycatch_survey$Latitude, na.rm = TRUE)
range(bycatch_survey$Longitude, na.rm = TRUE)

# Warnings are okay - they are for fields that we don't care about ("waypoint" is duplicated and "fin clip" has non-numeric values)
bycatch_catch_heatmap <- greater_PS_map + geom_bin2d(data = bycatch_survey, aes(x = Longitude, y = Latitude, fill=cut(..count.., c(0,5,10,20,50,100,150))),binwidth = c(0.02, 0.02),inherit.aes = FALSE) +
  scale_fill_manual(values = c("#2b83ba", "#abdda4","#ffffbf","#fdae61","#d7191c"))+
  guides(fill=guide_legend(title="Number of Fish"))

ggsave(paste0(fig_dir,"/bycatch_catch_heatmap_allspecies.png"),bycatch_catch_heatmap, width = 12, height = 10)






