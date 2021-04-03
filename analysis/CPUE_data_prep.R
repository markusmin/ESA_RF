## Prepare data for CPUE modeling comparisons

library(here)
library(lubridate)
library(readxl)
library(tidyverse)
library(broom)
library(rgdal)

## LOAD DATA


## ----Genetics survey----------------------------------------------------------------
gen_survey_path <- here("hook_and_line_data","2014_2015_genetics_survey.xlsx")
excel_sheets(gen_survey_path)
gen_survey <- read_excel(gen_survey_path, sheet = "data")

# Warnings are okay, they're just in reference to fin clip IDs

# Change one time (the one at 12:23 am on 5/27/15)
gen_survey[gen_survey$time == as_datetime("2015-05-27 00:23:00"),]$time <- as_datetime("2015-05-27 12:23:00")

# Replace all spaces in column names with underscores
gen_survey %>% select_all(funs(gsub(" ", "_", .))) -> gen_survey

# Convert depth to meters
gen_survey %>% 
  rename(depth_ft = `Depth_(ft)`) %>% 
  mutate(., depth_ft = as.numeric(depth_ft)) %>% 
  mutate(.,depth_m = 0.3048*depth_ft) -> gen_survey

# Add underscores to captain names
gen_survey$Captain <- gsub(" ", "_", gen_survey$Captain)

# Extract month, year, and time of day
gen_survey %>% 
  rename(date_time = time) %>% 
  mutate(date_time = ymd_hms(gen_survey$time)) -> gen_survey


gen_survey %>%
  mutate(date = date(date_time)) %>% 
  mutate(month = month(date_time)) %>% 
  mutate(time = hms::as_hms(date_time)) -> gen_survey

# Add the missing site information
gen_survey %>% 
  # Sites numbered 1:16 on day 44 - closest samples at Mukilteo Shoreline
  mutate(Site = ifelse(days_of_fishing == 44 & Site %in% c(1:16), "Mukilteo Shoreline",
         # Missing site info on day 59, closest site is point evans
         # There are a lot of missing data on this day... they're all in the general vicinity, hard to get finer resolution.
         ifelse(days_of_fishing == 59 & is.na(Site), "Point Evans",
                # Sites numbered 431:439 on day 41 - 431:435 and 437:439 look like two different places
                # 431:435 = "Pier on South Whidbey", 437:439 = "Sandy Point"
                ifelse(days_of_fishing == 41 & Site %in% c(431:435), "Pier on South Whidbey",
                       ifelse(days_of_fishing == 41 & Site %in% c(437:439), "Sandy Point",
                              # Missing site info on day 47. Morning (prior to 10:30 am) at Eagle Point, 
                              # afternoon (after 11:30 am) at Deception Island
                              ifelse(days_of_fishing == 47 & is.na(Site) & time < hms::as_hms(10:30:00), "Eagle Point",
                              ifelse(days_of_fishing == 47 & is.na(Site) & time > hms::as_hms(11:30:00), "Deception Island",
                                     # Missing site info on day 61, closest site is Cypress Reef
                                     # Need finer resolution, other times definitely more like Point Lawrence and then Sisters Island and then Sucia/Puffin Island
                                     # No clear breaks in time, seems like they just drifted realy far without driving the boat
                                     ifelse(days_of_fishing == 61 & is.na(Site), "Cypress Reef", Site)))))))) -> gen_survey

sort(unique(gen_survey$Site))

# Create an index field (site + day = set)
gen_survey %>% 
  # First change spaces to underscores in site
  mutate(., Site = gsub(" ", "_", Site)) %>% 
  # Concatenate fishing day and site with "gen" prefix for genetics
  mutate(set = paste0("gen_", days_of_fishing, "_", Site)) -> gen_survey


## Interpolate number of anglers based on boats

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
# For the captains with some angler information available, randomly select from existing data
# Set seed for reproducibility
set.seed(123)
for (i in 1:max(subset(gen_survey, year == 2014)$days_of_fishing)){
  gen_survey[gen_survey$days_of_fishing == i,]$anglers <- sample(eval(parse(text = paste0(unique(subset(gen_survey, 
                                                                                                        days_of_fishing == i)$Captain), "_anglers"))),1)
}


# Group the various bait/lure types to facilitate comparison across studies

# Group lure
unique(gen_survey$Lure)
# There are issues here where the bait and lure are combined - need to separate for analysis
gen_survey %>% 
  mutate(lure_type = ifelse(Lure %in% c("double hook", "double hooks", "single hook", "no lure", "double hook squid"), 
                            "Hook only",
                    ifelse(Lure %in% c("shrimp fly", "jig", "jig", "Dart jig", "buzz bomb", "squid skirt", "pipe jig", "shrimp flies", "white scampi",
                                       "lead head jigs", "pixie jig", "rubber worm", "dart", "white dart", "double shrimp fly",  "split wormtail", 
                                       "gulp lure", "hoochie","red shrimp fly", "blue shrimp fly", "green squid", "Yellow shrimp fly", 
                                       "Green squid skirt", "brown rubber worm",  "red scampi", "yellow scampi", "Lead head jig with green worm",
                                       "Lead head jig with clear worm", "Lead head jig with pink worm", "Lead head jig with worm",  
                                       "Lead head jig with black/green glitter worm", "wormtail double", "shrimp flies octopus", 
                                       "shriimp flies", "squid skirts", "scampi tails","shrimp flies (3)"),
                           "Artificial Lure", 
                           ifelse(Lure %in% c("NA", NA) & !is.na(bait), "Hook only", "unknown")))) -> gen_survey

# Group bait
unique(gen_survey$bait)
# "NA"s are from things that aren't actual catch in data, i.e. regurgitated catch, water samples
# Change all "herring" values to "Herring"
gen_survey[gen_survey$bait == "herring" & !is.na(gen_survey$bait),]$bait <- "Herring"
PW_survey[PW_survey$set == "PW_74_010_L73",]$set <- "PW_74_010_L73"

gen_survey %>% 
  mutate(bait_type = ifelse(bait %in% c("live shiner perch", "Pacific sanddab", "shiner perch"), "Live large", 
                            # If there is lure info but NA for bait, then it's "no bait"
                            ifelse(!is.na(Lure) & is.na(bait), "no bait",
                                   ifelse(bait %in% c("octopus and squirmy tail", "octopus"), "octopus",
                                          ifelse(Lure %in% c("shrimp flies octopus"), "octopus",
                                                 ifelse(bait == "unknown" | is.na(bait), "unknown", 
                                                        bait)))))) -> gen_survey
unique(gen_survey$bait_type)

# The model isn't able to fit anything with bait/lure as explanatory variables, because there are still too many unique combinations.
# Let's group things further: 1) artificial lure, no bait. 2) artificial lure + any bait. 3) Herring 4) Squid 5) Live large 6) Other/unknown

gen_survey %>% 
  mutate(., lure_bait_type = ifelse(bait_type == "no bait" & lure_type == "Artificial Lure", "Artificial lure, no bait",
                                    ifelse(lure_type == "Artificial Lure" & bait_type != "no bait", "Artificial lure + bait",
                                           ifelse(bait_type == "Herring" & lure_type == "Hook only", "Herring",
                                                  ifelse(bait_type == "squid" & lure_type == "Hook only", "Squid",
                                                         ifelse(bait_type == "Live large" & lure_type == "Hook only", "Live large",
                                                                ifelse(bait_type %in% c("unknown", "Other") | lure_type == "unknown", "other/unknown", NA
                                                  ))))))) -> gen_survey

# table(gen_survey$lure_bait_type)
# table(gen_survey$bait_type)
# table(gen_survey$lure_type)

# Vast majority of other/unknowns are start/stop recordings
table(subset(gen_survey, lure_bait_type == "other/unknown")$Species)

# Check how many sites don't have any bait or lure information
gen_survey %>% 
  group_by(set) %>% 
  count(bait_type) %>% 
  pivot_wider(names_from = bait_type, values_from = n)-> genetics_bait_type_counts

no_bait <- genetics_bait_type_counts[rowSums(genetics_bait_type_counts[,2:7], na.rm = TRUE) == 0,]
# There are a bunch of drifts where nothing was caught so no bait or lure information was ever recorded  


# Remove non-catches
gen_survey <- subset(gen_survey, !(Species == "water sample"))
gen_survey <- subset(gen_survey, !(notes %in% c("regurgitated from yelloweye fin clip #428 or 409",  "species identification is unsure - regurgitated from canary fin clip 415")))
                                  
# Determine location (basin) based on same scheme as in Essington et al. 2021
# Boundary between South Sound and Central Sound: 47.27, -122.55
# Entrance to Hood Canal: 47.95, -122.65
# Entrance to Straits of Juan de Fuca: 48.18, -122.76
# Entrance to Whidbey Basin and Skagit Bay: 47.89, -122.36
# San Juan Islands: North of 48.40

# Based on a map of regions, they don't line up with the Essington et al. 2021 boundaries and are inconsistent. So must recalculate.
# Hood Canal is okay though
gen_survey %>% 
  mutate(basin = ifelse(lon > -122.36 & lat > 47.847 | lon > -122.53 & lat > 48.02, "Whidbey Island", 
                        ifelse(lat < 47.27 & lon < -122.55 | lat < 47.35 & lon < -122.60 & lon > -123.09, "South Sound",
                               ifelse(Region == "San Juan Islands" & lat < 48.22 & lon >-122.77 | Region == "South Puget Sound" & lon > -122.55 |
                                        Region == "Central Puget Sound", "Central Sound",
                                      ifelse(Region == "San Juan Islands" & lat < 48.40 | Region == "Strait of Juan de Fuca", "Straits of Juan de Fuca",
                                             Region))))) -> gen_survey

# Remove all data from outside DPS (western straits)
# Western Boundary of DPS in straits: longitude -123.3
gen_survey <- subset(gen_survey, lon > -123.3)


## ----Check to make sure that location designations are accurate---------------

## Base map of Puget sound
# 
# # Load data
# usa_spdf <- readOGR(dsn = here("map_files", "USA_adm0.shp"))
# 
# # Convert to df(ish)
# usa_spdf_fort <- tidy(usa_spdf)
# 
# # Load BC data
# BC_shp <- readOGR(dsn = here("map_files", "canada", "lpr_000b16a_e.shp"))
# # proj4string(BC_shp)
# BC_shp_transform <- spTransform(BC_shp, "+init=epsg:4326")
# BC_spdf_fort <- tidy(BC_shp_transform)
# 
# 
# # Create base map in ggplot
# greater_PS_map <- ggplot(usa_spdf_fort, aes(x = long, y = lat, group = group))+
#   # Create grid using geom_vline and geom_hline
#   # geom_vline(xintercept = seq(-123.1, min_lon, 1/nm), color = "gray50", size = 0.1)+
#   # geom_hline(yintercept = seq(min_lat, 48.5, 1/60), color = "gray50", size = 0.1)+
#   #base map
#   geom_polygon(color = "black", fill = "black")+
#   geom_polygon(data = BC_spdf_fort, aes(x = long, y = lat, group = group), color = "black", fill = "black", inherit.aes = FALSE) +
#   ylab("Latitude")+
#   xlab("Longitude")+
#   coord_fixed(ylim = c(47.1,48.8),  xlim = c(-124.7,-122.1), ratio = 1.3)+
#   theme(panel.background = element_rect(fill = "gray50"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())


## Plot map of basin labels
# Set figure directory
fig_dir <- here("figures", "map_figures")

genetics_effort_map <- greater_PS_map + 
  geom_point(data = gen_survey, aes(x = lon, y = lat, color = basin), inherit.aes = FALSE)+
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12))+
  labs(fill = "Angler Hours")

ggsave(paste0(fig_dir,"/genetics_basin_map.png"),genetics_effort_map, width = 12, height = 10)








# Create a drift field using a combination of fishing day and site
gen_survey %>% 
  # First change spaces to underscores in site
  mutate(., Site = gsub(" ", "_", Site)) %>% 
  # Concatenate fishing day and site
  mutate(set = paste0(days_of_fishing, "_", Site)) -> gen_survey



# Group by the set, drop everything except drift and species
gen_survey %>% 
  group_by(set) %>% 
  count(Species) %>% 
  pivot_wider(names_from = Species, values_from = n)-> gen_survey_idx



##--Reformat data for analysis---------------------------

# Group by index (set)
gen_survey %>% 
  group_by(set) %>% 
  # Here we encounter an issue where if no fish were caught, no depths were recorded
  # Could use the mean depth at that site from other days to interpolate these values?
  summarise(mean_depth = mean(depth_m, na.rm = TRUE),
            # Count how many yelloweye
            yelloweye_catch = sum(Species == "yelloweye rockfish"),
            bocaccio_catch = sum(Species == "bocaccio"),
            # Have to take mean of anglers here because sometimes anglers would take a break at one location
            # There's also the issue that in 2014, only caught fish were recorded. This means that
            # if you only fish once at a location, there is only one time for that location
            effort_sec = (max(date_time) - min(date_time)),
            date = unique(date),
            month = unique(month),
            start_time = hms::as_hms(min(time)),
            end_time = hms::as_hms(max(time)),
            avg_anglers = mean(anglers, na.rm = TRUE),
            # Add site info
            Site = unique(Site),
            # There are some drifts where they started in one basin and wound up in another, therefore take the most common value
            basin = names(which.max(table(basin)))) %>% 
mutate(survey = "ESA_Genetics") -> gen_data_for_model

# Now, remove all of the start and end times and figure out the most commonly used baits/lures
gen_survey %>% 
  dplyr::filter(!(Species %in% c("start", "end"))) %>% 
  group_by(set) %>% 
  # Many different lures/baits often used at the same site... most frequent is decent estimate, but not great
  # There's also the issue of drift where nothing was caught, so no bait/lure information was recorded
  # Take most frequent, ignoring start/end times (which have no bait or lure info)
  summarise(bait = names(which.max(table(bait_type))),
            lure = names(which.max(table(lure_type))),
            lure_bait = names(which.max(table(lure_bait_type)))) -> gen_survey_baits_lures

# Join bait/lure data with other data
gen_data_for_model <- left_join(gen_data_for_model, gen_survey_baits_lures, by = "set")
# For drifts that were start/end times only (no bait/lure info), use "unknown"
gen_data_for_model[is.na(gen_data_for_model$bait),]$bait <- "unknown"
gen_data_for_model[is.na(gen_data_for_model$lure),]$lure <- "unknown"
gen_data_for_model[is.na(gen_data_for_model$lure_bait),]$lure_bait <- "unknown"

# How are we going to estimate effort at sites where only one fish was caught?
# Calculate the mean amount of time it takes to catch a fish and use that
# OR, take mean amount of time at a sit
# Average time at site is 10x the time to first catch... let's use time to first catch
gen_survey %>%
  group_by(set) %>% 
  summarise(effort_hours = (max(date_time) - min(date_time))*mean(anglers)) -> gen_survey_effort

# Average amount of time at a site on a day
avg_time_at_site <- mean(gen_survey_effort$effort_hours)
# Average amount of time between fish caught (or between start time and first fish caught)
# Use the 2015 data, since we have start times
gen_survey %>% 
  subset(year == 2015) %>% 
  # Group into start, end, and fish
  mutate(., species_groups = ifelse(Species == "start", "start", 
                                    ifelse(Species == "end", "end", "fish"))) %>% 
  group_by(set, species_groups) %>% 
  summarise(first_incidence = min(date_time)) -> gen_survey_catch_timing
gen_survey_catch_timing %>% 
  pivot_wider(names_from = species_groups, values_from = first_incidence) %>% 
  mutate(time_to_first_catch = fish - start) -> gen_survey_catch_timing_2

# Average time to first catch
avg_time_first_catch <- mean(gen_survey_catch_timing_2$time_to_first_catch, na.rm = TRUE)

# Use average time to first catch as estimate of effort at a location where only one fish was caught
gen_data_for_model %>% 
  mutate(effort_sec = ifelse(effort_sec == 0, avg_time_first_catch, effort_sec)) -> gen_data_for_model
  
# Use the mean depth for sites where depth info is missing, if no other depth info for site then NA

# Calculate mean depth per site
gen_survey %>% 
  group_by(Site) %>% 
  summarise(mean_depth_at_site = mean(depth_m, na.rm = TRUE)) -> gen_survey_mean_site_depths

# Add mean depth for sites that are missing depth info
gen_data_for_model %>% 
  left_join(., gen_survey_mean_site_depths, by = "Site") %>% 
  mutate(mean_depth = ifelse(is.na(mean_depth), mean_depth_at_site, mean_depth)) %>% 
  dplyr::select(-mean_depth_at_site) -> gen_data_for_model

# Calculate angler hours
gen_data_for_model %>% 
  mutate(angler_hours = as.numeric(effort_sec)/3600 * avg_anglers) -> gen_data_for_model


## ----Bycatch survey----------------------------------------------------------------

bycatch_survey_catch_path <- here("hook_and_line_data","2017_2019_bycatch_survey_catch.csv")
bycatch_catch <- read.csv(bycatch_survey_catch_path, header = TRUE)

bycatch_survey_effort_path <- here("hook_and_line_data","2017_2019_bycatch_survey_effort.csv")
bycatch_effort <- read.csv(bycatch_survey_effort_path, header = TRUE)

# Rename set column
bycatch_effort %>% 
  rename(unique_set = set) -> bycatch_effort
bycatch_catch %>% 
  rename(unique_set = set) -> bycatch_catch

# Change depth to numeric, convert to m
bycatch_catch %>% 
  rename(depth_ft = `Depth..ft.`) %>% 
  mutate(., depth_ft = as.numeric(depth_ft)) %>% 
  mutate(.,depth_m = 0.3048*depth_ft) -> bycatch_catch

# Recalculate effort as number of hours on the water (ignore up time)

# Use lubridate to convert to date/time format
bycatch_effort %>%
  mutate(start_date_time = mdy_hm(start.time)) %>%
  mutate(end_date_time = mdy_hm(end.time)) -> bycatch_effort

# Extract time, date, and month
bycatch_effort %>%
  mutate(date = date(start_date_time)) %>% 
  mutate(month = month(start_date_time)) %>% 
  mutate(start_time = hms::as_hms(start_date_time)) %>% 
  mutate(end_time = hms::as_hms(end_date_time))-> bycatch_effort

# Effort file: effort in sec, date, time, month, start time, avg anglers, site, bait, lure, angler hours
# Catch file: yelloweye catch, mean depth, basin (using lat/long of catches)

# Group baits/lures same as for genetics survey
bycatch_effort %>% 
  mutate(bait_type = ifelse(Bait.type == "Artificial lure", "no bait", Bait.type)) %>% 
  mutate(lure_type = ifelse(Bait.type == "Artificial lure", "Artificial Lure", "Hook only")) -> bycatch_effort

# Further simplify lure and bait into one variable
bycatch_effort %>% 
  mutate(., lure_bait_type = ifelse(Bait.type == "Artificial lure", "Artificial lure, no bait",
                                    ifelse(Bait.type == "Herring", "Herring",
                                           ifelse(Bait.type == "Live large", "Live large", NA)))) -> bycatch_effort

# Create an index field (site + day = set) for both effort and bycatch
bycatch_effort %>% 
  # First change spaces to underscores in site
  mutate(., Site = gsub(" ", "_", Site)) %>% 
  # Concatenate fishing day and site with "byc" prefix for genetics
  mutate(set = paste0("byc_", fishing.day, "_", Site)) -> bycatch_effort

bycatch_catch %>% 
  # First change spaces to underscores in site
  mutate(., Site = gsub(" ", "_", Site)) %>% 
  # Concatenate fishing day and site with "byc" prefix for genetics
  mutate(set = paste0("byc_", Fishing.day, "_", Site)) -> bycatch_catch



## Determine basins for bycatch survey catch data
# Based on a map of sampling locations, there are only three regions: San Juan Islands, Central PS, and Whidbey Island
bycatch_catch %>% 
  mutate(basin = ifelse(Longitude > -122.36 & Latitude > 47.847 | Longitude > -122.53 & Latitude > 48.02, "Whidbey Island", 
                       ifelse(Latitude > 48.4, "San Juan Islands",
                                      "Central Sound"))) -> bycatch_catch

# Create a key of site/basin, necessary for the effort data where nothing was caught


##--Check with map-----------------------------------
bycatch_basin_map <- greater_PS_map + 
  geom_point(data = bycatch_catch, aes(x = Longitude, y = Latitude, color = basin), inherit.aes = FALSE)+
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12))

ggsave(paste0(fig_dir,"/bycatch_basin_map.png"),bycatch_basin_map, width = 12, height = 10)

##--Reformat data for analysis---------------------------

# Extract information from effort data
# Group by index (set)
bycatch_effort %>% 
  group_by(set) %>% 
  # Have to take mean of anglers here because sometimes anglers would take a break at one location
  summarise(effort_sec = (max(end_date_time) - min(start_date_time)),
            date = unique(date),
            month = unique(month),
            start_time = hms::as_hms(min(start_time)),
            end_time = hms::as_hms(max(end_time)),
            avg_anglers = mean(Anglers),
            bait = names(which.max(table(bait_type))),
            lure = names(which.max(table(lure_type))),
            lure_bait = names(which.max(table(lure_bait_type))),
            # Add site info
            Site = unique(Site)) %>% 
  mutate(survey = "lingcod_bycatch") -> bycatch_effort_data_for_model


# Extract information from catch
# Group by index (set)
bycatch_catch %>% 
  group_by(set) %>% 
  # Here we encounter an issue where if no fish were caught, no depths were recorded
  # Could use the mean depth at that site from other days to interpolate these values?
  summarise(mean_depth = mean(depth_m, na.rm = TRUE),
            # Count how many yelloweye
            yelloweye_catch = sum(Species == "yelloweye rockfish"),
            basin = names(which.max(table(basin))),
            bocaccio_catch = sum(Species == "bocaccio")) -> bycatch_catch_data_for_model


# Join effort and catch data together
setdiff(bycatch_effort_data_for_model$set, bycatch_catch_data_for_model$set)
# these are the sets where nothing was caught
bycatch_data_for_model <- left_join(bycatch_effort_data_for_model, bycatch_catch_data_for_model, by = "set")

# Fill in missing values for sets where nothing was caught
bycatch_data_for_model %>% 
  mutate(yelloweye_catch = ifelse(is.na(yelloweye_catch), 0, yelloweye_catch)) %>% 
  mutate(bocaccio_catch = ifelse(is.na(bocaccio_catch), 0, bocaccio_catch)) -> bycatch_data_for_model
  
# Figure out mean depth for each location, in order to fill in effort where nothing was caught
bycatch_catch %>% 
  group_by(Site) %>% 
  summarise(mean_depth_at_site = mean(depth_m, na.rm = TRUE)) -> bycatch_survey_mean_site_depths

# Add mean depth for sites that are missing depth info
bycatch_data_for_model %>% 
  left_join(., bycatch_survey_mean_site_depths, by = "Site") %>% 
  mutate(mean_depth = ifelse(is.na(mean_depth), mean_depth_at_site, mean_depth)) %>% 
  dplyr::select(-mean_depth_at_site) -> bycatch_data_for_model

### Add basin names for effort where nothing was caught
# Create key of site/basin
bycatch_catch %>% 
  dplyr::select(Site, basin) %>% 
  rename(basin_key = basin) %>% 
  .[!duplicated(.), ] -> bycatch_site_basin_key

bycatch_data_for_model %>% 
  left_join(., bycatch_site_basin_key, by = "Site") %>% 
  mutate(., basin = ifelse(Site == "Waldron_Island", "San Juan Islands", 
                           ifelse(is.na(basin), basin_key, basin))) %>% 
  dplyr::select(-basin_key) -> bycatch_data_for_model


# Calculate angler hours
bycatch_data_for_model %>% 
  mutate(effort_sec = as.numeric(effort_sec)) %>% 
  mutate(angler_hours = as.numeric(effort_sec)/3600 * avg_anglers) -> bycatch_data_for_model


## ----Percy Washington Survey----------------------------------------------------------------

PW_survey_path <- here("hook_and_line_data","Washington_et_al_74-77_Puget_Sound_data.xlsx")
excel_sheets(PW_survey_path)
PW_survey <- read_excel(PW_survey_path, sheet = "catch_species_names")

# Rename some columns
PW_survey %>% 
  dplyr::rename("survey_no" = "Survey No.",
                "start_time" = "Time Start",
                "stop_time" = "Time Stop",
                "nAnglers" = "No. Anglers") -> PW_survey

# Edit the odd locations
# See "PW_survey_site_adjustment_justifications" for reasoning
PW_survey %>% 
  mutate(Location = ifelse(Location == "485", "H85", 
                           ifelse(Location == "490", "H90", 
                                  ifelse(Location == "491", "H91", 
                                         ifelse(Location == "492", "H92", 
                                                ifelse(Location == "B110", "R110", 
                                                       ifelse(Location == "M110", NA, 
                                                              ifelse(Location == "I21", "I82", 
                                                                     ifelse(Location == "N10", NA, 
                                                                            ifelse(Location == "Q10", NA, Location)))))))))) -> PW_survey

# Make slight adjustments to sites that are barely on land
subset(PW_data_for_model, is.na(mean_depth))$location
# On land: H80, N72, H55 (probably), Q83, O85 (maybe - this is in water, but Google maps thinks it's in 14cm of water... not great!), P57, P60 (maybe - 3 m deep, okay)

# Worth checking these against log book eventually
PW_survey %>% 
  mutate(., Location = ifelse(Location == "H80", "I79", # What Hilary did
                       ifelse(Location == "N72", "L72", # What Hilary did      
                       ifelse(Location == "H55", "H54", # Move just north
                       ifelse(Location == "Q83", "P83", # What Hilary did
                       ifelse(Location == "P57", "Q57", # move just west
                       Location)))))) -> PW_survey



# Change anglers to numeric
PW_survey$nAnglers <- as.numeric(PW_survey$nAnglers)

# Add the depth data from Hilary
PW_GIS_data <- read_excel(here("hook_and_line_data", "PW_additional_files", "PW_GIS_data.xlsx"))
PW_GIS_data %>% 
  rename(Location = "Site Name") %>% 
  rename(mean_depth_m = "Avg Depth (meters)") %>% 
  mutate(., mean_depth_m = as.numeric(mean_depth_m)) %>% 
  dplyr::select(Location, mean_depth_m) -> PW_depth_data

PW_survey <- left_join(PW_survey, PW_depth_data, by = "Location")

# Add set as unique index

# Can't just use survey_no, because these can often cover multiple locations - must include location
# Multiple locations at the same time (??) - e.g. 74.010
# Multiple survey numbers for the same day - 74.018 and 74.019 are on the same day, 074.018 has multiple times
PW_survey %>% mutate(., set = paste0("PW_", Year,"_",survey_no, "_", Location)) -> PW_survey

###### DATA FIXES

# PW_74_010_H66 - location should be L73
PW_survey[PW_survey$set == "PW_74_010_H66",]$set <- "PW_74_010_L73"
PW_survey[PW_survey$set == "PW_74_010_L73",]$Location <- "L73"

# PW_74_031_P82 - location should be H79
PW_survey[PW_survey$set == "PW_74_031_P82",]$set <- "PW_74_031_H79"
PW_survey[PW_survey$set == "PW_74_031_H79",]$Location <- "H79"

# Fix 74.018, looks like a typo
PW_survey[PW_survey$set == "PW_74_018_H78" & PW_survey$start_time == "0830",]$set <- "PW_74_019_H78"
# 
# # PW_74_030_H79 looks like location is wrong, should be I82
PW_survey[PW_survey$set == "PW_74_030_H79",]$set <- "PW_74_030_I82"
PW_survey[PW_survey$set == "PW_74_030_I82",]$Location <- "I82"

# PW_75_03_N72 is missing a leading zero
PW_survey[PW_survey$set == "PW_75_03_N72",]$set <- "PW_75_003_N72"
PW_survey[PW_survey$set == "PW_75_003_N72",]$survey_no <- "003"

# PW_75_036_Q108 is missing angler info for two catches (other catch is 3 anglers)
PW_survey[PW_survey$set == "PW_75_036_Q108",]$nAnglers <- 3

# PW_75_036_N104 - one entry has start/end times which are the same as PW_75_036_R110, other entries have no info
# Better off changing the one entry with incorrect start/end times to NA
PW_survey[PW_survey$set == "PW_75_036_N104",]$start_time <- NA
PW_survey[PW_survey$set == "PW_75_036_N104",]$stop_time <- NA

# Exact same start/stop times at same site, same survey no. on two days: PW_75_046_N77
# I believe these should all be on the same day, since on 1975-09-23 it says they were at a different site at the same time
PW_survey[PW_survey$set == "PW_75_046_N77",]$Day <- "24"

# There's a whole big mess with PW_75_057_L78/PW_75_058_L78 - missing info, same site/adjacent days, and they caught yelloweye...
# Change date for one sample that looks to be on the same day as previous samples
PW_survey[PW_survey$set == "PW_75_057_L78",]$Day <- "11"
# Change date for two samples - still missing effort, but can interpolate later
PW_survey[PW_survey$set == "PW_75_058_L78",]$Day <- "12"

# Error in day on PW_76_022_O78
PW_survey[PW_survey$set == "PW_76_022_O78",]$Day <- "05"

# PW_76_086_S63 has "C1" as day... pretty sure it should be 04
# Also one "C1" on 	PW_76_085_S63
# There are also two separate sets at this location on this day, but only two minutes apart so not a big deal
# However, the unique(start_time) part of the code should take care of this, and separate them out
PW_survey[PW_survey$set == "PW_76_086_S63",]$Day <- "04"
PW_survey[PW_survey$set == "PW_76_085_S63",]$Day <- "04"

# PW_76_098_H66 has end time (1070) - this clearly should be 1040, seeing as the next start time on that day is 1050
PW_survey[PW_survey$set == "PW_76_098_H66",]$stop_time <- "1040"

# Error in month/day on PW_76_130_M91
PW_survey[PW_survey$set == "PW_76_130_M91",]$Day <- "05"
PW_survey[PW_survey$set == "PW_76_130_M91",]$Month <- "10"

# Overlap in time between PW_76_137_K90 and PW_76_138_L90 (1400-1500), then overlaps with PW_76_139_L90 as well (1310-1620)
# I really don't know what's going on with this site, but we'll have to make some assumptions (hopefully can check with logbook)
# Change the day for PW_76_137_K90 and PW_76_138_L90, survey no. and location for PW_76_137_K90
PW_survey[PW_survey$set == "PW_76_137_K90",]$survey_no <- "138"
PW_survey[PW_survey$set == "PW_76_137_K90",]$Location <- "L90"
PW_survey[PW_survey$set == "PW_76_137_K90",]$set <- "PW_76_138_L90"
# Change day to 18, but have no justification for this
PW_survey[PW_survey$set == "PW_76_138_L90",]$Day <- "18"

# PW_77_011_L90 shouldn't exist, survey no. should be 010
PW_survey[PW_survey$set == "PW_77_011_L90",]$survey_no <- "010"
PW_survey[PW_survey$set == "PW_77_011_L90",]$set <- "PW_77_010_L90"

# PW_77_013_M92 shouldn't exist, survey no. should be 012
PW_survey[PW_survey$set == "PW_77_013_M92",]$survey_no <- "012"
PW_survey[PW_survey$set == "PW_77_013_M92",]$set <- "PW_77_012_M92"

# Add missing depth info to multiple sites: PW_74_021_M66, PW_74_029_H80, PW_75_003_N72, PW_75_043_H55, PW_75_049_L81, PW_75_053_J82,
# PW_76_010_H77, PW_76_037_Q83, PW_76_048_BB53, PW_76_049_BB52, PW_76_050_LL49, PW_76_067_O85, PW_76_134_L83, PW_77_016_O85, 
# PW_77_018_T54, PW_77_019_P57, PW_77_020_P60


# Missing start/end times: PW_74_040_I79 (6500), PW_76_106_M110 (question mark)
  # Not editable (just totally missing): PW_74_042_M69, PW_74_045_M69, PW_75_022_E65, PW_75_026_E65, 	
  # PW_75_028_E65, PW_75_031_F60, PW_75_031_G57, PW_75_041_L89, PW_75_049_L81, PW_75_053_J82, PW_75_058_L78

# Replace time with NA for 74_040_I79 (where start time was 6500)
PW_survey[PW_survey$set == "PW_74_040_I79",]$start_time <- NA
PW_survey[PW_survey$set == "PW_74_040_I79",]$stop_time <- NA

# Replace question marks with NAs (76_106_M110)
PW_survey[PW_survey$set == "PW_76_106_M110",]$start_time <- NA
PW_survey[PW_survey$set == "PW_76_106_M110",]$stop_time <- NA





###### END DATA FIXES

# Create date field
PW_survey %>% 
  mutate(., date = paste0("19",Year,"-",Month,"-",Day)) %>%  
  mutate(date = ymd(date)) %>% 
  # rename month for consistency
  rename(month = Month) -> PW_survey

# Convert times to date using regex and lubridate
PW_survey$start_time <- gsub('^(.{2})(.*)$', '\\1:\\2', PW_survey$start_time)
PW_survey$stop_time <- gsub('^(.{2})(.*)$', '\\1:\\2', PW_survey$stop_time)
PW_survey %>% 
  mutate(start_time = ifelse(!is.na(start_time), paste0(PW_survey$start_time,":00"), NA)) %>%
  mutate(stop_time = ifelse(!is.na(stop_time), paste0(PW_survey$stop_time,":00"), NA)) %>%
  mutate(start_date_time = ymd_hms(paste0(date, " ", start_time))) %>%
  mutate(stop_date_time = ymd_hms(paste0(date, " ", stop_time)))-> PW_survey

PW_survey$start_time <- hms::as_hms(PW_survey$start_date_time)
PW_survey$stop_time <- hms::as_hms(PW_survey$stop_date_time)


# Add the gear codes
PW_equipment_code_key <- read_excel(here("hook_and_line_data", "PW_additional_files", "gear_codes.xlsx"))
# Split into three 
subset(PW_equipment_code_key, Type == "Gear") %>% 
  rename(Gear = Name, gear_code = Code) %>% 
  dplyr::select(-Type) -> PW_gear_code_key

subset(PW_equipment_code_key, Type == "Bait") %>% 
  rename(Bait = Name, bait_code = Code) %>% 
  dplyr::select(-Type) -> PW_bait_code_key

subset(PW_equipment_code_key, Type == "Lure") %>% 
  rename(Lure = Name, lure_code = Code) %>% 
  dplyr::select(-Type) -> PW_lure_code_key

# Add to data
PW_survey %>% 
  rename(gear_code = Gear, lure_code = Lure, bait_code = Bait) %>% 
  left_join(., PW_gear_code_key, by = "gear_code") %>% 
  left_join(., PW_lure_code_key, by = "lure_code") %>% 
  left_join(., PW_bait_code_key, by = "bait_code") -> PW_survey

# Remove all data not from hook and line methods
PW_survey <- subset(PW_survey, !(Gear %in% c("Otter Trawling", "Fish Pot") | Bait == "Speargun"))

#  Create categories bait_type and lure_type
PW_survey %>% 
  mutate(bait_type = ifelse(Bait %in% c("Herring plug", "Herring whole","Herring Chunks") |
                              Lure %in% c("Herring whole", "Herring plug", "Rubberworm Herring Chunk", "Herring Chunks"), "Herring",
                    ifelse(Bait == "Squid" | Lure == "Squid", "Squid",
                    ifelse(Bait %in% c("Pork Rind", "Mussel") | Lure == "Mussel" | Lure == "Polychaete", "Other",
                    ifelse(Bait == "No bait", "no bait", NA))))) %>% 
  mutate(lure_type = ifelse(Lure %in% c("Jig", "Rubber Worm", "Rubberworm Herring Chunk", "Tuna Jig", "Pipe Jig",
                                        "Fly", "Spinner", "Stingsilda", "Hoochie") |
                              Bait %in% c("Spinner", "Jig", "Tuna Jig", "Rubber Worm", "Hoochie", "Stingsilda"), "Artificial Lure",
                    ifelse(Lure %in% c("67", "Unknown"), "unknown",
                    ifelse(Lure == "No bait", "Hook only", NA)))) -> PW_survey

PW_survey %>% 
  mutate(bait_type = ifelse(lure_type == "Artificial Lure" & is.na(bait_type), "no bait", bait_type)) %>% 
  mutate(lure_type = ifelse(bait_type %in% c("Herring", "Squid", "Other") & is.na(lure_type), "Hook only", lure_type)) -> PW_survey

# Further simplify for comparison
# Let's group things further: 1) artificial lure, no bait. 2) artificial lure + any bait. 3) Herring 4) Squid 5) Live large 6) Other/unknown

PW_survey %>% 
  mutate(., lure_bait_type = ifelse(bait_type == "no bait" & lure_type == "Artificial Lure", "Artificial lure, no bait",
                                    ifelse(lure_type == "Artificial Lure" & bait_type != "no bait", "Artificial lure + bait",
                                           ifelse(bait_type == "Herring" & lure_type == "Hook only", "Herring",
                                                  ifelse(bait_type == "Squid" & lure_type == "Hook only", "Squid",
                                                         ifelse(bait_type == "Live large" & lure_type == "Hook only", "Live large",
                                                                ifelse(bait_type %in% c("unknown", "Other") | lure_type == "unknown", "other/unknown", NA
                                                                ))))))) -> PW_survey


# Translate letter/number locations into lat/lons
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

## Determine basins for PW survey
# Boundary between South Sound and Central Sound: 47.27, -122.55
# Entrance to Hood Canal: 47.95, -122.65
# Entrance to Straits of Juan de Fuca: 48.18, -122.76
# Entrance to Whidbey Basin and Skagit Bay: 47.89, -122.36
# San Juan Islands: North of 48.40

# Based on a map of sampling locations, there are five regions: 
# Straits of Juan de Fuca, Central PS, South Sound, Hood Canal, and Whidbey Island
PW_survey %>% 
  mutate(basin = ifelse(lon_est > -122.36 & lat_est > 47.847 | lon_est > -122.53 & lat_est > 48.02, "Whidbey Island", 
                        ifelse(lat_est > 48 & lon_est < -122.76, "Straits of Juan de Fuca",
                               ifelse(lat_est < 47.95 & lon_est < -122.65, "Hood Canal",
                                      ifelse(lat_est < 47.27 & lon_est < -122.55, "South Sound",
                               "Central Sound"))))) -> PW_survey

# Remove all data from outside DPS (western straits)
# Western Boundary of DPS in straits: longitude -123.3
PW_survey <- subset(PW_survey, lon_est > -123.3)

##--Check with map-------------
PW_basin_map <- greater_PS_map + 
  geom_point(data = PW_survey, aes(x = lon_est, y = lat_est, color = basin), inherit.aes = FALSE)+
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12))

ggsave(paste0(fig_dir,"/PW_basin_map.png"), PW_basin_map, width = 12, height = 10)

##--Reformat data for analysis---------------------------

# Group by index (set)
PW_survey %>% 
  group_by(set) %>% 
  # Here we encounter an issue where if no fish were caught, no depths were recorded
  # Could use the mean depth at that site from other days to interpolate these values?
  summarise(mean_depth = mean(mean_depth_m, na.rm = TRUE),
            # Count how many yelloweye
            yelloweye_catch = sum(Species == "Yellow Eye"),
            # Count bocaccio
            bocaccio_catch = sum(Species == "Bocaccio"),
            bait = names(which.max(table(bait_type))),
            lure = names(which.max(table(lure_type))),
            lure_bait = names(which.max(table(lure_bait_type))),
            effort_sec = (max(stop_date_time) - min(start_date_time)),
            date = unique(date),
            month = as.numeric(unique(month)),
            start_time = hms::as_hms(unique(start_time)),
            end_time = hms::as_hms(unique(stop_time)),
            avg_anglers = mean(nAnglers, na.rm = TRUE),
            location = unique(Location),
            lat_est = unique(lat_est),
            lon_est = unique(lon_est),
            # There are some drifts where they started in one basin and wound up in another, therefore take the most common value
            basin = names(which.max(table(basin)))) %>% 
  mutate(survey = "Percy_Washington") -> PW_data_for_model


# ESTIMATE MISSING DATA

# Add missing angler information
table(PW_data_for_model$avg_anglers)
# Most common value is 3 - use that
PW_data_for_model %>% 
  mutate(., avg_anglers = ifelse(is.na(avg_anglers), 3, avg_anglers)) -> PW_data_for_model

# Add missing effort time information

mean(PW_data_for_model$effort_sec, na.rm = TRUE)
PW_data_for_model %>% 
  mutate(., effort_sec = ifelse(is.na(effort_sec), mean(PW_data_for_model$effort_sec, na.rm = TRUE), effort_sec)) -> PW_data_for_model

# Calculate angler hours
PW_data_for_model %>% 
  mutate(angler_hours = as.numeric(effort_sec)/3600 * avg_anglers) -> PW_data_for_model

# Missing depth data: PW_74_021_M66, PW_74_029_H80, PW_75_003_N72, PW_75_043_H55, PW_75_049_L81, PW_75_053_J82,
# PW_76_010_H77, PW_76_037_Q83, PW_76_048_BB53, PW_76_049_BB52, PW_76_050_LL49, PW_76_067_O85, PW_76_134_L83, PW_77_016_O85, 
# PW_77_018_T54, PW_77_019_P57, PW_77_020_P60
# Ideally, we would recalculate all of these values... but for now need a substitute

# Let's use Google Maps Elevation API
subset(PW_data_for_model, is.na(mean_depth)) %>% 
  mutate(lat_lon = paste0(lat_est, ",", lon_est)) -> PW_latlons

elevation_locations <- paste(PW_latlons$lat_lon, collapse = "|")

# Create request
elevation_request <- paste0("https://maps.googleapis.com/maps/api/elevation/json?locations=", elevation_locations,"&key=AIzaSyCMa6yrcuSXedy1Vb6Kmdi9oJygNI0FH6c")

# This is converted from json to csv, then saved as "missing_depth_from_google.csv"
# Read it in
missing_depth_data <- read.csv(here("hook_and_line_data", "PW_additional_files", "missing_depth_from_google.csv"))
missing_depth_data %>% 
  mutate(missing_depth = elevation * -1) %>% 
  dplyr::select(-c(latitude, longitude, elevation)) -> missing_depth_data

# Add in the locations
missing_depth_data$location <- PW_latlons$location

# Add to data for model
PW_data_for_model %>% 
  left_join(., missing_depth_data, by = "location") %>% 
  mutate(., mean_depth = ifelse(is.na(mean_depth), missing_depth, mean_depth)) %>% 
  dplyr::select(-missing_depth) -> PW_data_for_model


# Join all three surveys together
colnames(PW_data_for_model)
colnames(gen_data_for_model)
colnames(bycatch_data_for_model)

# PW vs. genetics
setdiff(colnames(PW_data_for_model), colnames(gen_data_for_model))
setdiff(colnames(gen_data_for_model), colnames(PW_data_for_model))

# Genetics vs. bycatch
setdiff(colnames(bycatch_data_for_model), colnames(gen_data_for_model))
setdiff(colnames(gen_data_for_model), colnames(bycatch_data_for_model))

# PW vs. bycatch
setdiff(colnames(bycatch_data_for_model), colnames(PW_data_for_model))
setdiff(colnames(PW_data_for_model), colnames(bycatch_data_for_model))

# Drop lat_est and lon_est from PW_survey data, rename location as Site
PW_data_for_model %>% 
  dplyr::select(-c(lat_est, lon_est)) %>% 
  rename(Site = location) -> PW_data_for_model

# Join all together!!!
PW_data_for_model %>% 
  union(., gen_data_for_model) %>% 
  union(., bycatch_data_for_model) -> CPUE_data_for_model

# Change all "unknown" to "other/unknown"

CPUE_data_for_model[CPUE_data_for_model$lure_bait == "unknown",]$lure_bait <- "other/unknown"

# Save as CSV to read in to modeling script
write.csv(CPUE_data_for_model, here("hook_and_line_data", "CPUE_data_for_model.csv"))


## --Explore other species caught with yelloweye or bocaccio ----

# Genetics survey

# Create a drift field using a combination of fishing day and site
gen_survey %>% 
  # First change spaces to underscores in site
  mutate(., Site = gsub(" ", "_", Site)) %>% 
  # Concatenate fishing day and site
  mutate(set = paste0(days_of_fishing, "_", Site)) -> gen_survey

# So this isn't quite accurate because as seen from the 2015 data, there are multiple start/end
# times on the same day at the same site... but it'll do for now

# Group by the set, drop everything except drift and species
gen_survey %>% 
  dplyr::select(set, Species) %>% 
  group_by(set) %>% 
  count(Species) %>% 
  pivot_wider(names_from = Species, values_from = n)-> genetics_counts_species

gen_YE_sets <- subset(genetics_counts_species, !(is.na(`yelloweye rockfish`)))

# Count other species caught with yelloweye
YE_friends_counts <- as.data.frame(colSums(gen_YE_sets[,2:ncol(gen_YE_sets)], na.rm = TRUE))
YE_friends_counts <- rownames_to_column(YE_friends_counts, "Species") 
colnames(YE_friends_counts) <- c("Species", "Count")
YE_friends_counts <- YE_friends_counts[order(-YE_friends_counts$Count),]
# Most common other catch: quillback (95), then coppers (60) and lingcod (59)

# Count number of times other species were caught with yelloweye
YE_friends_presence <- as.data.frame(colSums(!is.na(gen_YE_sets[,2:ncol(gen_YE_sets)])))
YE_friends_presence <- rownames_to_column(YE_friends_presence, "Species") 
colnames(YE_friends_presence) <- c("Species", "Presence")
YE_friends_presence <- YE_friends_presence[order(-YE_friends_presence$Presence),]

# Quillbacks found in 23/30; lingcod in 17/30; coppers in 13/30


## --Explore habitat characteristics where yelloweye or bocaccio were caught----

# Can we cut off depth at 30 m?

##### Genetics survey
gen_YE <- subset(gen_survey, Species == "yelloweye rockfish")
hist(gen_YE$depth_m)
mean(gen_YE$depth_m)
range(gen_YE$depth_m)
# Caught 35 - 122 m

##### Bycatch survey
bycatch_YE <- subset(bycatch_catch, Species == "yelloweye rockfish")
hist(bycatch_YE$depth_m)
mean(bycatch_YE$depth_m)
range(bycatch_YE$depth_m)

##### PW survey
PW_YE <- subset(PW_survey, Species == "Yellow Eye")
hist(PW_YE$mean_depth_m)
mean(PW_YE$mean_depth_m)
range(PW_YE$mean_depth_m)





