# Exploratory plots/analysis for Percy Washington 1974-1977 hook & line survey

library(tidyverse)
library(readxl)
library(here)

## ----Load data----------------------------------------------------------------
PW_survey_path <- here("hook_and_line_data","Washington_et_al_74-77_Puget_Sound_data.xls")
excel_sheets(PW_survey_path)
PW_survey <- read_excel(PW_survey_path, sheet = "Main Data")

# Set figure directory
fig_dir <- here("exploratory_figures")

## ----Species composition------------------------------------------------------
PW_survey %>% group_by(Species) %>% count() -> species_counts

# Remove the odd species
unique(species_counts$Species)
species_counts <- subset(species_counts, !(Species %in% c("5107", "5118", "5205", "5713", NA)))

# Create a color legend to highlight Yelloweye and Bocaccio
ESA_colors <- ifelse(species_counts$Species %in% c("Bocaccio", "Yelloweye Rockfish"),"#ff7f00","gray50")

occurrence_plot <- ggplot(species_counts, aes(x = Species, y = n,  fill = factor(ifelse(Species %in% c("Bocaccio", "Yelloweye Rockfish"),"ESA","non-ESA"))))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("#ff7f00", "gray50"), name = "Listing Status")+
  # scale_fill_manual(values = ESA_colors)+
  theme(axis.text.x = element_text(angle = 90, color = ESA_colors,vjust = 0.45,hjust = 1),
        legend.position = "none")+
  ylab("Occurrences")+
  ggtitle("Total species occurrences from 1974-1977 Percy Washington Hook & Line Survey")

ggsave(paste0(fig_dir,"/PW_species_counts.png"),occurrence_plot, width = 8, height = 6)
