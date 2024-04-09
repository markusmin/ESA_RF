# Mapping in R using sf


library(tidyverse)
library(tidyverse)
library(readxl)
library(here)
# library(rgdal)
library(broom)
library(ggpubr)
library(sf)
library(lubridate)
# library(ggsn)

# Set figure directory
fig_dir <- here("figures", "map_figures")

# load USA
usa_spdf <- st_read(here("map_files", "USA_adm0.shp"))

# load BC
BC_spdf <- st_read(here("map_files", "canada", "lpr_000b16a_e.shp"))

# Here is the base map of the Salish Sea - you should be able to add geoms on top!
ggplot(usa_spdf) +
  geom_sf() +
  geom_sf(data = BC_spdf) +
  coord_sf(ylim = c(46.8,51.2),  xlim = c(-128.5,-122.1))


# Re-create the Salish Sea map with some annotations, without using rgdal
salish_sea_map <- ggplot(usa_spdf) +
  geom_sf() +
  geom_sf(data = BC_spdf) +
  coord_sf(ylim = c(46.8,51.2),  xlim = c(-128.5,-122.1)) +
  ylab("Latitude")+
  xlab("Longitude")+
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill="white", color = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.14, 0.2),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))+
  # annotations to label important features
  annotate("segment", x = -122.84, y = 47.68, xend = -123.18, yend = 47.68, size = 0.5, lty = 1) + # Hood Canal
  annotate("text", x = -123.2, y = 47.68, label = "Hood Canal", size = 3, hjust = 1) + # Hood Canal
  annotate("text", x = -126, y = 49.9, label = "Vancouver Island", size = 4, angle = -25) + # Vancouver Island
  annotate("text", x = -126.153, y = 50.474, label = "Johnstone Strait", size = 3, hjust = 1) + # Johnstone Strait
  annotate("text", x = -127.45, y = 50.75, label = "Queen\nCharlotte\nStrait", size = 3) + # Queen Charlotte Strait
  annotate("text", x = -123.88, y = 49.33, label = "Strait of Georgia", size = 3, angle = -12) + # Strait of Georgia
  annotate("segment", x = -124.232920, y = 48.345919, xend = -125.117319, yend = 48.145603, size = 0.5, lty = 1) + # Strait of JDF
  annotate("text", x = -125.137319, y = 48.145603, label = "Strait of Juan de Fuca", size = 3, hjust =1) + # Strait of JDF
  annotate("segment", x = -122.887366, y = 48.570709, xend = -123.775885, yend = 48.790568, size = 0.5, lty = 1) + # San Juan Islands
  annotate("text", x = -123.795885, y = 48.795568, label = "San Juan Islands", size = 3, hjust = 1) + # San Juan Islands
  annotate("segment", x = -122.821448, y = 47.202315, xend = -123.3, yend = 47.202315, size = 0.5, lty = 1) + # South Puget Sound
  annotate("text", x = -123.5, y = 47.202315, label = "South\nPuget\nSound", size = 3) + # South Puget Sound
  annotate("text", x = -122.53, y = 47.95, label = "Puget\nSound", size = 4) + # Puget Sound
  annotate("text", x = -123, y = 50.5, label = "British\nColumbia", size = 5) + # British Columbia
  annotate("text", x = -123.7, y = 47.95, label = "Washington", size = 5) # Washington


ggsave(paste0(fig_dir, "/salish_sea_map_2.pdf"), salish_sea_map, height = 6, width  = 7.1)  

