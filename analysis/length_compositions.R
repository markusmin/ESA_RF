###### Length composition data for 5-year review
# Author: Markus Min
# email: mmin@uw.edu
# Last updated: 20.05.2021

#---- Load libraries ------
library(tidyverse)
library(readxl)
library(here)
library(ggthemes)
library(ggpubr)

# Set figure directory
fig_dir <- here("figures", "lengths")



# Load data
rec_lengths_path <- here("catch_reconstruction_data", "biological_data","PugetSportRFLength.xls")

rec_len <- read_excel(rec_lengths_path, sheet = "tblPugetSportRFLength")

# Convert all lengths to cm
rec_len %>% 
  mutate(length_cm = ifelse(LengthUnits == "M", Length/10, Length)) %>% 
  dplyr::select(-c(LengthUnits, Length)) -> rec_len


# Subset species of interest
# From "SPECIES" sheet of same Excel spreadsheet: 
# Bocaccio = 410125
# Yelloweye = 410129

boc_len <- subset(rec_len, SpeciesID == 410125)
ye_len <- subset(rec_len, SpeciesID == 410129)


#------ Yelloweye: Data Prep ----------

# Subset Hood Canal from the rest of yelloweye
# Hood Canal = Punch Card Area 12
colnames(ye_len)
ye_len_HC <- subset(ye_len, PunchCardArea == 12)

# Check years of data from Hood Canal
ye_len_HC$SampleYear

# Subset US DPS
ye_len_USDPS <- subset(ye_len, PunchCardArea != 12)
# Check years of data from US DPS
table(ye_len_USDPS$SampleYear)

# Subset into smaller time periods (decades)
ye_len_USDPS_75_79 <- subset(ye_len_USDPS, SampleYear <= 1979)

ye_len_USDPS_80_87 <- subset(ye_len_USDPS, SampleYear >= 1980 & SampleYear <= 1987)

ye_len_USDPS_92_98 <- subset(ye_len_USDPS, SampleYear >= 1992)

# Look at range of lengths for limits
range(ye_len_USDPS$length_cm)

#------ Yelloweye: Make Plots ----------

# Yelloweye, Hood Canal (1977-1985)
ye_HC_length_plot <- ggplot(ye_len_HC, aes(x = length_cm))+
  geom_histogram(binwidth = 2)+
  # Add mean line
  geom_vline(aes(xintercept = mean(length_cm, na.rm = TRUE)), color = "black", lty = 2)+
  # scale_x_continuous(breaks = seq(20,80,10), limits = c(20,80))+
  ggtitle("Yelloweye, Hood Canal (1977-1985)")

ye_HC_length_plot

ggsave(paste0(fig_dir, "/YE_HC_lengths.png"), ye_HC_length_plot, height = 6, width = 8)

# Yelloweye, US DPS (1975-1979)
ye_USDPS_75_79_length_plot <- ggplot(ye_len_USDPS_75_79, aes(x = length_cm))+
  geom_histogram(binwidth = 2)+
  # Add mean line
  geom_vline(aes(xintercept = mean(length_cm, na.rm = TRUE)), color = "black", lty = 2)+
  scale_x_continuous(breaks = seq(10,90,10), limits = c(10,92), oob = scales::oob_keep)+
  ggtitle("Yelloweye, non-Hood Canal (1975-1979)")

ye_USDPS_75_79_length_plot

ggsave(paste0(fig_dir, "/YE_USDPS_75_79_lengths.png"), ye_USDPS_75_79_length_plot, height = 6, width = 8)


# Yelloweye, US DPS (1980-1987)
ye_USDPS_80_87_length_plot <- ggplot(ye_len_USDPS_80_87, aes(x = length_cm))+
  geom_histogram(binwidth = 2)+
  # Add mean line
  geom_vline(aes(xintercept = mean(length_cm, na.rm = TRUE)), color = "black", lty = 2)+
  scale_x_continuous(breaks = seq(10,90,10), limits = c(10,92), oob = scales::oob_keep)+
  ggtitle("Yelloweye, non-Hood Canal (1980-1987)")

ye_USDPS_80_87_length_plot

ggsave(paste0(fig_dir, "/YE_USDPS_80_87_lengths.png"), ye_USDPS_80_87_length_plot, height = 6, width = 8)


# Yelloweye, US DPS (1992-1998)
ye_USDPS_92_98_length_plot <- ggplot(ye_len_USDPS_92_98, aes(x = length_cm))+
  geom_histogram(binwidth = 2)+
  # Add mean line
  geom_vline(aes(xintercept = mean(length_cm, na.rm = TRUE)), color = "black", lty = 2)+
  scale_x_continuous(breaks = seq(10,90,10), limits = c(10,92), oob = scales::oob_keep)+
  ggtitle("Yelloweye, non-Hood Canal (1992-1998)")

ye_USDPS_92_98_length_plot

ggsave(paste0(fig_dir, "/YE_USDPS_92_98_lengths.png"), ye_USDPS_92_98_length_plot, height = 6, width = 8)






#------ Bocaccio: Data Prep ----------

# Check years of data from US DPS
table(ye_len$SampleYear)

# Subset into smaller time periods (decades)
boc_len_75_79 <- subset(boc_len, SampleYear <= 1979)

boc_len_80_87 <- subset(boc_len, SampleYear >= 1980 & SampleYear <= 1987)

boc_len_92_98 <- subset(boc_len, SampleYear >= 1992)

# Look at range of lengths for limits
range(boc_len$length_cm)

#------ Bocaccio: Plot Data ----------

# Bocaccio (1975-1979)
boc_len_75_79_length_plot <- ggplot(boc_len_75_79, aes(x = length_cm))+
  geom_histogram(binwidth = 2)+
  # Add mean line
  geom_vline(aes(xintercept = mean(length_cm, na.rm = TRUE)), color = "black", lty = 2)+
  scale_x_continuous(breaks = seq(10,90,10), limits = c(10,90), oob = scales::oob_keep)+
  ggtitle("Bocaccio (1975-1979)")

boc_len_75_79_length_plot

ggsave(paste0(fig_dir, "/boc_len_75_79_lengths.png"), boc_len_75_79_length_plot, height = 6, width = 8)


# Bocaccio (1980-1987)
boc_len_80_87_length_plot <- ggplot(boc_len_80_87, aes(x = length_cm))+
  geom_histogram(binwidth = 2)+
  # Add mean line
  geom_vline(aes(xintercept = mean(length_cm, na.rm = TRUE)), color = "black", lty = 2)+
  scale_x_continuous(breaks = seq(10,90,10), limits = c(10,90), oob = scales::oob_keep)+
  ggtitle("Bocaccio (1980-1987)")

boc_len_80_87_length_plot

ggsave(paste0(fig_dir, "/boc_len_80_87_lengths.png"), boc_len_80_87_length_plot, height = 6, width = 8)


# Bocaccio (1992-1998)
boc_len_92_98_length_plot <- ggplot(boc_len_92_98, aes(x = length_cm))+
  geom_histogram(binwidth = 2)+
  # Add mean line
  geom_vline(aes(xintercept = mean(length_cm, na.rm = TRUE)), color = "black", lty = 2)+
  scale_x_continuous(breaks = seq(10,90,10), limits = c(10,90), oob = scales::oob_keep)+
  ggtitle("Bocaccio (1992-1998)")

boc_len_92_98_length_plot

ggsave(paste0(fig_dir, "/boc_len_92_98_lengths.png"), boc_len_92_98_length_plot, height = 6, width = 8)

# Arrange all figures and export
length_comps_gg <- ggarrange(plotlist = list(ye_USDPS_75_79_length_plot, boc_len_75_79_length_plot, 
                                             ye_USDPS_80_87_length_plot, boc_len_80_87_length_plot, 
                                             ye_USDPS_92_98_length_plot, boc_len_92_98_length_plot), ncol = 2, nrow = 3)

fig_dir <- here("figures", "lengths")

ggsave(length_comps_gg, height = 11, width = 8, path = fig_dir, filename = "rec_catch_lengths.png", device = "png")

# Export length comps as CSV

# Yelloweye, US DPS
write.csv(ye_len_USDPS, paste0(here("outputs", "length_comps"), "/yelloweye_USDPS_rec_lengths.csv"), row.names = FALSE)

# Yelloweye, Hood Canal
write.csv(ye_len_HC, paste0(here("outputs", "length_comps"), "/yelloweye_HC_rec_lengths.csv"), row.names = FALSE)

# Bocaccio
write.csv(boc_len, paste0(here("outputs", "length_comps"), "/bocaccio_rec_lengths.csv"), row.names = FALSE)


