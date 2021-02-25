# Demographics by survey

library(tidyverse)
library(here)
library(lubridate)
library(readxl)
library(ggpubr)

# Would it be interesting to fit length/weight curves?
# We should look into the depths at which fish were caught

## ----Load data----------------------------------------------------------------

## PW
PW_survey_path <- here("hook_and_line_data","Washington_et_al_74-77_Puget_Sound_data.xlsx")
excel_sheets(PW_survey_path)
PW_survey <- read_excel(PW_survey_path, sheet = "catch_species_names")

## ESA Genetics
gen_survey_path <- here("hook_and_line_data","2014_2015_genetics_survey.xlsx")
excel_sheets(gen_survey_path)
gen_survey <- read_excel(gen_survey_path, sheet = "data")

## Bycatch catch
bycatch_survey_catch_path <- here("hook_and_line_data","2017_2019_bycatch_survey_catch.csv")
bycatch_catch <- read.csv(bycatch_survey_catch_path, header = TRUE)

## DFO North
DFO_north_bio_path <- here("hook_and_line_data", "DFO_inside_north_hard_bottom_longline_surveys", "INS-N_biology.csv")
DFO_north_bio <- read.csv(DFO_north_bio_path, header = TRUE)
DFO_north_effort_path <- here("hook_and_line_data", "DFO_inside_north_hard_bottom_longline_surveys", "INS-N_EFFORT.csv")
DFO_north_effort <- read.csv(DFO_north_effort_path, header = TRUE)

## DFO South
DFO_south_bio_path <- here("hook_and_line_data", "DFO_inside_south_hard_bottom_longline_surveys", "INS-S_biology.csv")
DFO_south_bio <- read.csv(DFO_south_bio_path, header = TRUE)
DFO_south_effort_path <- here("hook_and_line_data", "DFO_inside_south_hard_bottom_longline_surveys", "INS-S_EFFORT.csv")
DFO_south_effort <- read.csv(DFO_south_effort_path, header = TRUE)

## Combine data from all surveys together

# Subset out common columns
gen_survey %>%
  dplyr::rename(FL_cm = `Fork Length (cm)`, weight_kg = `Weight (kg)`,
                depth_ft = `Depth (ft)`, Date = time,
                Latitude = lat, Longitude = lon, Sex = Gender) %>% 
  dplyr::select(weight_kg, FL_cm, Species) %>% 
  mutate(FL_cm = as.numeric(FL_cm)) %>% 
  mutate(weight_kg = as.numeric(weight_kg)) %>% 
  mutate(survey = "ESA_genetics") %>% 
  subset(Species == "yelloweye rockfish")-> gen_survey_demo

bycatch_catch %>%
  dplyr::rename(FL_cm = Length, weight_kg = Weight,
                depth_ft = Depth..ft.) %>% 
  mutate(FL_cm = as.numeric(FL_cm)) %>% 
  mutate(weight_kg = as.numeric(weight_kg)) %>% 
  subset(Species == "yelloweye rockfish") %>% 
  mutate(survey = "lingcod_bycatch") %>% 
  dplyr::select(weight_kg, FL_cm, Species, survey) -> bycatch_catch_demo

PW_survey %>%
  dplyr::rename(TL_mm = `Total Length (mm)`, weight_kg = `Body Weight (kg)`) %>% 
  mutate(TL_cm = as.numeric(TL_mm)/10) %>% 
  subset(Species == "Yellow Eye") %>% 
  mutate(FL_cm = 0.9824*TL_cm+0.7522) %>% 
  mutate(FL_cm = as.numeric(FL_cm)) %>% 
  mutate(weight_kg = as.numeric(weight_kg)) %>% 
  mutate(survey = "Percy_Washington") %>% 
  dplyr::select(weight_kg, FL_cm, Species, survey) -> PW_survey_demo

DFO_north_bio %>%
  dplyr::rename(FL_mm = Fork.length..mm., weight_g = Weight..g., Species = English.common.name) %>% 
  mutate(FL_cm = as.numeric(FL_mm)/10) %>% 
  mutate(weight_kg = as.numeric(weight_g)/1000) %>% 
  subset(Species == "YELLOWEYE ROCKFISH") %>% 
  mutate(FL_cm = as.numeric(FL_cm)) %>% 
  mutate(weight_kg = as.numeric(weight_kg)) %>% 
  mutate(survey = "DFO_inside_north") %>% 
  dplyr::select(weight_kg, FL_cm, Species, survey) -> DFO_north_demo

DFO_south_bio %>%
  dplyr::rename(FL_mm = Fork.length..mm., weight_g = Weight..g., Species = English.common.name) %>% 
  mutate(FL_cm = as.numeric(FL_mm)/10) %>% 
  mutate(weight_kg = as.numeric(weight_g)/1000) %>% 
  subset(Species == "YELLOWEYE ROCKFISH") %>% 
  mutate(FL_cm = as.numeric(FL_cm)) %>% 
  mutate(weight_kg = as.numeric(weight_kg)) %>% 
  mutate(survey = "DFO_inside_south") %>% 
  dplyr::select(weight_kg, FL_cm, Species, survey) -> DFO_south_demo

gen_survey_demo %>% 
  union(., bycatch_catch_demo) %>% 
  union(., PW_survey_demo) %>% 
  union(., DFO_north_demo) %>% 
  union(., DFO_south_demo) -> all_surveys_YE_demo

# Take log of weight and length
all_surveys_YE_demo %>% 
  mutate(ln_weight = log(weight_kg)) %>% 
  mutate(ln_length = log(FL_cm)) -> all_surveys_YE_demo

## ----Plot Length vs. Weight for all surveys----------------------------------------------------
# Create colorblind safe color scheme
survey_colors <- c("Percy_Washington" =  "#ffffcc",
  "ESA_genetics" = "#a1dab4",
  "lingcod_bycatch" = "#41b6c4",
  "DFO_inside_south" = "#2c7fb8",
  "DFO_inside_north" = "#253494")

# Create non-colorblind safe color scheme
survey_colors <- c("Percy_Washington" =  "#4daf4a",
                   "ESA_genetics" = "#984ea3",
                   "lingcod_bycatch" = "#377eb8",
                   "DFO_inside_south" = "#a65628",
                   "DFO_inside_north" = "#999999")

all_surveys_YE_demo$survey = factor(all_surveys_YE_demo$survey, levels = c("DFO_inside_north", "DFO_inside_south", 
                                                                    "lingcod_bycatch", "ESA_genetics", "Percy_Washington"))

all_surveys_YE_demo <- all_surveys_YE_demo[order(all_surveys_YE_demo$survey), ]

subset(all_surveys_YE_demo, weight_kg < 0.3 & FL_cm >30)

# These three individuals have implausible weights for their lengths - must be a mistake in entry
# Remove from df
all_surveys_YE_demo <- subset(all_surveys_YE_demo, !(weight_kg < 0.3 & FL_cm >30))


# fit a linear regression to all data
lm_data <- lm(ln_weight~ln_length, data = all_surveys_YE_demo)

# fit linear regressions to each survey
lm_gen <- lm(ln_weight~ln_length, data = subset(all_surveys_YE_demo, survey == "ESA_genetics"))
lm_bycatch <- lm(ln_weight~ln_length, data = subset(all_surveys_YE_demo, survey == "lingcod_bycatch"))
lm_PW <- lm(ln_weight~ln_length, data = subset(all_surveys_YE_demo, survey == "Percy_Washington"))
lm_DFO_north <- lm(ln_weight~ln_length, data = subset(all_surveys_YE_demo, survey == "DFO_inside_north"))
lm_DFO_south <- lm(ln_weight~ln_length, data = subset(all_surveys_YE_demo, survey == "DFO_inside_south"))

ggplot(all_surveys_YE_demo, aes(x = FL_cm, y = weight_kg, color = survey))+
  geom_point(size = 0.1)+
  geom_abline(intercept = lm_data$coefficients[1], slope = lm_data$coefficients[2], lty = 2)+
  ylab("Weight (kg)")+
  xlab("Length (cm)")+
  labs(color = "Location")+
  scale_color_manual(values = survey_colors)+
  theme(panel.background = element_rect(color = "black", fill = "white"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.25, 0.85),
        legend.box.background = element_rect(color = "black"),
        legend.key=element_blank(),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15))

length_weight_plot <- ggplot(all_surveys_YE_demo, aes(x = ln_length, y = ln_weight, color = survey))+
  geom_point(size = 1)+
  # All data
  geom_abline(intercept = lm_data$coefficients[1], slope = lm_data$coefficients[2], lty = 2)+
  # Genetics
  geom_abline(intercept = lm_gen$coefficients[1], slope = lm_gen$coefficients[2], lty = 2, color = "#984ea3")+
  # Bycatch
  geom_abline(intercept = lm_bycatch$coefficients[1], slope = lm_bycatch$coefficients[2], lty = 2, color = "#377eb8")+
  # Percy Washington
  geom_abline(intercept = lm_PW$coefficients[1], slope = lm_PW$coefficients[2], lty = 2, color =  "#4daf4a")+
  # DFO north
  geom_abline(intercept = lm_DFO_north$coefficients[1], slope = lm_DFO_north$coefficients[2], lty = 2, color = "#999999")+
  # DFO south
  geom_abline(intercept = lm_DFO_south$coefficients[1], slope = lm_DFO_south$coefficients[2], lty = 2, color = "#a65628")+
  ylab("ln(weight (kg))")+
  xlab("ln(length (cm))")+
  labs(color = "Survey")+
  scale_color_manual(values = survey_colors)+
  theme(panel.background = element_rect(color = "black", fill = "white"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.75, 0.2),
        legend.box.background = element_rect(color = "black"),
        legend.key=element_blank(),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15))+
  annotate(geom = "text", x = min(all_surveys_YE_demo$ln_length, na.rm = TRUE), y = max(all_surveys_YE_demo$ln_weight, na.rm = TRUE), hjust = 0,
           label = paste0("y = ", round(lm_data$coefficients[2], 3), "x ", round(lm_data$coefficients[1],3)))

fig_dir <- here("figures")

ggsave(length_weight_plot, height = 8, width = 8, path = fig_dir, filename = "length_weight_plot.png", device = "png")

## ----Plot PW survey lengths-----------------------------------------------------------
# Conversion from total length to fork length from 2017 Yelloweye stock assessment
# 𝐹𝐿 = 0.9824 ∙ 𝑇𝐿 + 0.7522

# Change some column names
PW_survey %>%
  dplyr::rename(TL_mm = `Total Length (mm)`) %>% 
  mutate(TL_cm = as.numeric(TL_mm)/10) -> PW_survey

subset(PW_survey, Species == "Yellow Eye") %>% 
  mutate(FL_cm = 0.9824*TL_cm+0.7522) -> PW_YE

# Plot length distributions
PW_YE_lengths <- ggplot(PW_YE, aes(x = FL_cm))+
  geom_histogram(binwidth = 2)+
  # Add mean line
  geom_vline(aes(xintercept = mean(FL_cm)), color = "black", lty = 2)+
  scale_x_continuous(breaks = seq(20,80,10), limits = c(20,80))+
  ggtitle("Percy Washington")

PW_YE_lengths

## ----Plot genetics survey lengths-----------------------------------------------------
# Change some column names
gen_survey %>%
  dplyr::rename(FL_cm = `Fork Length (cm)`, weight_kg = `Weight (kg)`,
                depth_ft = `Depth (ft)`, Date = time,
                Latitude = lat, Longitude = lon, Sex = Gender) %>% 
  mutate(Date = mdy_hm(Date)) %>% 
  mutate(FL_cm = as.numeric(FL_cm)) %>% 
  mutate(depth_ft = as.numeric(depth_ft)) -> gen_survey

gen_YE <- subset(gen_survey, Species == "yelloweye rockfish")

# Plot length distributions
gen_YE_lengths <- ggplot(gen_YE, aes(x = FL_cm))+
  geom_histogram(binwidth = 2)+
  # Add mean line
  geom_vline(aes(xintercept = mean(FL_cm, na.rm = TRUE)), color = "black", lty = 2)+
  scale_x_continuous(breaks = seq(20,80,10), limits = c(20,80))+
  ggtitle("ESA Genetics")


gen_YE_lengths


## ----Plot bycatch survey lengths-----------------------------------------------------
# Change some column names
bycatch_catch %>%
  dplyr::rename(FL_cm = Length, weight_kg = Weight,
                depth_ft = Depth..ft.) %>% 
  mutate(FL_cm = as.numeric(FL_cm)) %>% 
  mutate(Date = mdy(Date)) -> bycatch_catch

bycatch_YE <- subset(bycatch_catch, Species == "yelloweye rockfish")

# Plot length distributions
bycatch_YE_lengths <- ggplot(bycatch_YE, aes(x = FL_cm))+
  geom_histogram(binwidth = 2)+
  # Add mean line
  geom_vline(aes(xintercept = mean(FL_cm, na.rm = TRUE)), color = "black", lty = 2)+
  scale_x_continuous(breaks = seq(20,80,10), limits = c(20,80))+
  ggtitle("Lingcod Bycatch")

bycatch_YE_lengths

## ----Plot bycatch and genetics surveys lengths together-------------------------------
# Subset only common columns
bycatch_catch_subset <- dplyr::select(bycatch_catch, Date, Region, Site, Latitude, Longitude, depth_ft, Species, FL_cm, Sex)
gen_survey_subset <- dplyr::select(gen_survey, Date, Region, Site, Latitude, Longitude, depth_ft, Species, FL_cm, Sex)

bycatch_gen_comb <- union(bycatch_catch_subset, gen_survey_subset)

bycatch_gen_comb_YE <- subset(bycatch_gen_comb, Species == "yelloweye rockfish")

# Plot length distributions
bycatch_gen_comb_YE_lengths <- ggplot(bycatch_gen_comb_YE, aes(x = FL_cm))+
  geom_histogram(binwidth = 2)+
  # Add mean line
  geom_vline(aes(xintercept = mean(FL_cm, na.rm = TRUE)), color = "black", lty = 2)+
  scale_x_continuous(breaks = seq(20,80,10), limits = c(20,80))+
  ggtitle("ESA Genetics + Lingcod Bycatch")

bycatch_gen_comb_YE_lengths

## ----Plot DFO Inside North lengths----------------------------------------------------
# Change some column names
DFO_north_bio %>%
  dplyr::rename(FL_mm = Fork.length..mm., weight_g = Weight..g.) %>% 
  mutate(FL_cm = as.numeric(FL_mm)/10) %>% 
  mutate(weight_kg = as.numeric(weight_g)/1000) -> DFO_north_bio

DFO_north_bio_YE <- subset(DFO_north_bio, Scientific.name == "SEBASTES RUBERRIMUS")

# Plot length distributions
DFO_north_bio_YE_lengths <- ggplot(DFO_north_bio_YE, aes(x = FL_cm))+
  geom_histogram(binwidth = 2)+
  # Add mean line
  geom_vline(aes(xintercept = mean(FL_cm, na.rm = TRUE)), color = "black", lty = 2)+
  scale_x_continuous(breaks = seq(20,80,10), limits = c(20,80))+
  ggtitle("DFO Inside North")

DFO_north_bio_YE_lengths

# Plot length distributions 2003-2010
DFO_north_bio_YE_lengths_2003_2010 <- ggplot(subset(DFO_north_bio_YE, Survey.Year <= 2010), aes(x = FL_cm))+
  geom_histogram(binwidth = 2)+
  # Add mean line
  geom_vline(aes(xintercept = mean(FL_cm, na.rm = TRUE)), color = "black", lty = 2)+
  scale_x_continuous(breaks = seq(20,80,10), limits = c(20,80))+
  ggtitle("DFO Inside North (2003-2010)")

DFO_north_bio_YE_lengths_2003_2010

# Plot length distributions 2012-2019
DFO_north_bio_YE_lengths_2012_2019 <- ggplot(subset(DFO_north_bio_YE, Survey.Year >= 2012), aes(x = FL_cm))+
  geom_histogram(binwidth = 2)+
  # Add mean line
  geom_vline(aes(xintercept = mean(FL_cm, na.rm = TRUE)), color = "black", lty = 2)+
  scale_x_continuous(breaks = seq(20,80,10), limits = c(20,80))+
  ggtitle("DFO Inside North (2012-2019)")

DFO_north_bio_YE_lengths_2012_2019


## ----Plot DFO Inside South lengths----------------------------------------------------
# Change some column names
DFO_south_bio %>%
  dplyr::rename(FL_mm = Fork.length..mm., weight_g = Weight..g.) %>% 
  mutate(FL_cm = as.numeric(FL_mm)/10) %>% 
  mutate(weight_kg = as.numeric(weight_g)/1000) -> DFO_south_bio

DFO_south_bio_YE <- subset(DFO_south_bio, Scientific.name == "SEBASTES RUBERRIMUS")

# Plot length distributions
DFO_south_bio_YE_lengths <- ggplot(DFO_south_bio_YE, aes(x = FL_cm))+
  geom_histogram(binwidth = 2)+
  # Add mean line
  geom_vline(aes(xintercept = mean(FL_cm, na.rm = TRUE)), color = "black", lty = 2)+
  scale_x_continuous(breaks = seq(20,80,10), limits = c(20,80))+
  ggtitle("DFO Inside South")

DFO_south_bio_YE_lengths

# Plot length distributions 2005-2011
DFO_south_bio_YE_lengths_2005_2011 <- ggplot(subset(DFO_south_bio_YE, Survey.Year <= 2011), aes(x = FL_cm))+
  geom_histogram(binwidth = 2)+
  # Add mean line
  geom_vline(aes(xintercept = mean(FL_cm, na.rm = TRUE)), color = "black", lty = 2)+
  scale_x_continuous(breaks = seq(20,80,10), limits = c(20,80))+
  ggtitle("DFO Inside South (2005-2011)")

DFO_south_bio_YE_lengths_2005_2011

# Plot length distributions 2013-2018
DFO_south_bio_YE_lengths_2013_2018 <- ggplot(subset(DFO_south_bio_YE, Survey.Year >= 2013), aes(x = FL_cm))+
  geom_histogram(binwidth = 2)+
  # Add mean line
  geom_vline(aes(xintercept = mean(FL_cm, na.rm = TRUE)), color = "black", lty = 2)+
  scale_x_continuous(breaks = seq(20,80,10), limits = c(20,80))+
  ggtitle("DFO Inside South (2013-2018)")

DFO_south_bio_YE_lengths_2013_2018

## ----Plot all surveys together----------------------------------------------------

# Eight plots:
# PW survey, genetics survey, bycatch survey, bycatch+genetics survey,
# DFO inside north and DFO inside south for two time periods each (4 total)

lengths_allsurveys_gg <- ggarrange(PW_YE_lengths, bycatch_YE_lengths, gen_YE_lengths, bycatch_gen_comb_YE_lengths,
                                                DFO_south_bio_YE_lengths_2005_2011, DFO_south_bio_YE_lengths_2013_2018,
                                                DFO_north_bio_YE_lengths_2003_2010, DFO_north_bio_YE_lengths_2012_2019)

fig_dir <- here("figures")

ggsave(lengths_allsurveys_gg, height = 10, width = 20, path = fig_dir, filename = "survey_length_distributions.png", device = "png")
