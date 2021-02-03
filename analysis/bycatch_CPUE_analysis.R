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

# NEED TO RECALCULATE ANGLER HOURS BY EACH DAY, SIMILAR TO PW SURVEY

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

# Need to recalculate lat/longs, because the site lat/long is not accurate enough

# Extract lat/long for each site
bycatch_catch %>%
  dplyr::select(Site, Latitude, Longitude) -> bycatch_catch_locations
bycatch_catch_locations <- bycatch_catch_locations[!duplicated(bycatch_catch_locations[,c("Site")]),]

# Join lat/long with effort
bycatch_effort <- left_join(bycatch_effort, bycatch_catch_locations, by = "Site")
sum(subset(bycatch_effort, Site == "4 mile rock")$effort..angler.hours.)

## --Plot map of effort (angler hours)------------------------------------------
# Set figure directory
fig_dir <- here("figures", "map_figures")

bycatch_effort_map <- greater_PS_map + 
  stat_summary_2d(data = bycatch_effort, aes(x = Longitude, y = Latitude, z = effort..angler.hours.),
                                                  binwidth = c(0.02, 0.02),inherit.aes = FALSE)+
  scale_fill_gradientn(limits=c(0,30), breaks=seq(0, 30, by=5), colours=c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", 
                                                                          "#fc4e2a", "#e31a1c", "#b10026"))+
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12))+
  labs(fill = "Angler Hours")+
  xlab("longitude")

ggsave(paste0(fig_dir,"/bycatch_effort_map.png"),bycatch_effort_map, width = 12, height = 10)
