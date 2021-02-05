# CPUE Calculations for 2017-2019 Lingcod Bycatch survey

library(tidyverse)
library(here)
library(lubridate)
library(broom)
library(rgdal)

## ----Load data----------------------------------------------------------------

bycatch_survey_catch_path <- here("hook_and_line_data","2017_2019_bycatch_survey_catch.csv")
bycatch_catch <- read.csv(bycatch_survey_catch_path, header = TRUE)

bycatch_survey_effort_path <- here("hook_and_line_data","2017_2019_bycatch_survey_effort.csv")
bycatch_effort <- read.csv(bycatch_survey_effort_path, header = TRUE)

## ----Recalculate effort as number of hours on the water (ignore up time)------

# Use lubridate to convert to date/time format
bycatch_effort %>%
  mutate(start.time = mdy_hm(start.time)) %>%
  mutate(end.time = mdy_hm(end.time)) -> bycatch_effort

# Number of anglers changes within the same day
# Calculate angler hours fished per day
bycatch_effort %>%
  group_by(Date) %>%
  mutate(total_hours = max(end.time) - min(start.time)) %>%
  # Calculate the mean number of anglers per drift on that day
  mutate(mean_anglers = mean(Anglers)) %>%
  mutate(total_angler_hours = total_hours * mean_anglers) -> bycatch_effort_per_day
bycatch_effort_per_day <- bycatch_effort_per_day[!duplicated(bycatch_effort_per_day[,c("Date")]),]

## ----Sum yelloweye and bocaccio catch-------------------------------------------

# How many total yelloweye did they catch?
bycatch_catch_YE <- subset(bycatch_catch, Species == "yelloweye rockfish")
bycatch_nYE <- dim(bycatch_catch_YE)[1]

# How many total Bocaccio did they catch?
bycatch_catch_boc <- subset(bycatch_catch, Species == "bocaccio")
bycatch_nboc <- dim(bycatch_catch_boc)[1]

## ----Calculate CPUE by time on water-------------------------------------
total_angler_hours_on_water <- as.numeric(sum(bycatch_effort_per_day$total_angler_hours))

# Yelloweye CPUE
bycatch_yelloweye_nominal_CPUE_angler_time_water <- bycatch_nYE/total_angler_hours_on_water
bycatch_yelloweye_nominal_CPUE_angler_time_water

# Bocaccio CPUE
bycatch_bocaccio_nominal_CPUE_angler_time_water <- bycatch_nboc/total_angler_hours_on_water
bycatch_bocaccio_nominal_CPUE_angler_time_water


## ----Calculate CPUE by angler hours (exact)-------------------------------------

# Total days
bycatch_angler_days <- max(bycatch_effort$fishing.day)

# Total hours
bycatch_angler_hours <- sum(bycatch_effort$effort..angler.hours.)

# Yelloweye CPUE
bycatch_yelloweye_nominal_CPUE_angler_hours <- bycatch_nYE/bycatch_angler_hours
bycatch_yelloweye_nominal_CPUE_angler_hours

# Bocaccio CPUE
bycatch_bocaccio_nominal_CPUE_angler_hours <- bycatch_nboc/bycatch_angler_hours
bycatch_bocaccio_nominal_CPUE_angler_hours

## ----Calculate CPUE by angler days-------------------------------------------

# Yelloweye CPUE
bycatch_yelloweye_nominal_CPUE_angler_days <- bycatch_nYE/bycatch_angler_days
bycatch_yelloweye_nominal_CPUE_angler_days

# Bocaccio CPUE
bycatch_bocaccio_nominal_CPUE_angler_days <- bycatch_nboc/bycatch_angler_days
bycatch_bocaccio_nominal_CPUE_angler_days

## ----Summarize CPUE stats-------------------------------------------
bycatch_CPUE_stats <- cbind(effort = c("Angler Days", "Fishing Time", "Drift Time"),
            yelloweye_CPUE = round(c(bycatch_yelloweye_nominal_CPUE_angler_days, bycatch_yelloweye_nominal_CPUE_angler_time_water, 
                                    bycatch_yelloweye_nominal_CPUE_angler_hours),3),            
            bocaccio_CPUE = c(bycatch_bocaccio_nominal_CPUE_angler_days, bycatch_bocaccio_nominal_CPUE_angler_time_water, 
                     bycatch_bocaccio_nominal_CPUE_angler_hours))

bycatch_CPUE_stats

## ---CREATE HEATMAP of effort--------------------------------------------------

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
  geom_polygon(color = "gray20", fill = "gray20")+
  geom_polygon(data = BC_spdf_fort, aes(x = long, y = lat, group = group), inherit.aes = FALSE) +
  ylab("Latitude")+
  coord_fixed(ylim = c(47.1,48.8),  xlim = c(-124.7,-122.1), ratio = 1.3)+
  theme(plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


## ---- Prepare data for map ---------------------------------------------------

# Calculate hours fished per site per day
bycatch_effort %>%
  group_by(Date, Site) %>%
  mutate(total_hours = max(end.time) - min(start.time)) %>% 
  mutate(mean_anglers = mean(Anglers)) %>%
  mutate(total_angler_hours = as.numeric(total_hours * mean_anglers,units = "hours")) -> bycatch_effort_per_site_day
bycatch_effort_per_site_day <- bycatch_effort_per_site_day[!duplicated(bycatch_effort_per_site_day[,c("Date", "Site")]),]

# sanity check - see if this value is between time on water and drift time
total_angler_hours_on_water
sum(bycatch_effort_per_site_day$total_angler_hours)
bycatch_angler_hours
# So it's about a 190 hour underestimate of the total time on the water if you look site by site

# Extract lat/long for each site
bycatch_catch %>%
  dplyr::select(Site, Latitude, Longitude) -> bycatch_catch_locations
bycatch_catch_locations <- bycatch_catch_locations[!duplicated(bycatch_catch_locations[,c("Site")]),]

# Join lat/long with effort
bycatch_effort_per_site_day <- left_join(bycatch_effort_per_site_day, bycatch_catch_locations, by = "Site")

bycatch_effort_per_site_day %>%
  group_by(Site) %>%
  summarise(angler_hours_site = sum(total_angler_hours)) %>%
  left_join(., bycatch_catch_locations, by = "Site") -> bycatch_total_effort_per_site

# Because nothing was caught at Waldron Island, we don't have any lat/long data
# Add data manually
bycatch_total_effort_per_site[bycatch_total_effort_per_site$Site == "Waldron Island",]$Latitude <- 48.70
bycatch_total_effort_per_site[bycatch_total_effort_per_site$Site == "Waldron Island",]$Longitude <- -123.00

## --Plot map of effort (angler hours)------------------------------------------
# Set figure directory
fig_dir <- here("figures", "map_figures")

bycatch_effort_map <- greater_PS_map + 
  stat_summary_2d(data = bycatch_total_effort_per_site, aes(x = Longitude, y = Latitude, z = angler_hours_site),
                                                  binwidth = c(0.02, 0.02),inherit.aes = FALSE)+
  scale_fill_gradientn(limits=c(0,60), breaks=seq(0, 60, by = 10), colours=c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", 
                                                                          "#fc4e2a", "#e31a1c", "#b10026"))+
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12))+
  labs(fill = "Angler Hours")+
  xlab("longitude")

ggsave(paste0(fig_dir,"/bycatch_effort_map.png"),bycatch_effort_map, width = 12, height = 10)
