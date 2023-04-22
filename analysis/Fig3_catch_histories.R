### Fig 2 in Min et al. 2023

library(tidyverse)
library(here)
library(ggpubr)

# read in completed catch history files
YE_high <- read.csv(here::here("catch_reconstruction_data", "complete_catch_histories", 
                               "yelloweye_USDPS_catch_scenario_high.csv"))
YE_med <- read.csv(here::here("catch_reconstruction_data", "complete_catch_histories", 
                               "yelloweye_USDPS_catch_scenario_medium.csv"))
YE_low <- read.csv(here::here("catch_reconstruction_data", "complete_catch_histories", 
                               "yelloweye_USDPS_catch_scenario_low.csv"))

# reformat all rec catch into one df
dplyr::select(YE_high, year, US_rec_catch_lbs) %>% 
  dplyr::rename(high = US_rec_catch_lbs) -> YE_high_rec

dplyr::select(YE_med, year, US_rec_catch_lbs) %>% 
  dplyr::rename(medium = US_rec_catch_lbs) -> YE_med_rec

dplyr::select(YE_low, year, US_rec_catch_lbs) %>% 
  dplyr::rename(low = US_rec_catch_lbs) -> YE_low_rec

YE_high_rec %>% 
  left_join(., YE_med_rec, by = "year") %>% 
  left_join(., YE_low_rec, by = "year") %>% 
  pivot_longer(., cols = c("high", "medium", "low")) %>% 
  dplyr::rename(scenario = name) %>% 
  mutate(scenario = factor(scenario, levels = c("high", "medium", "low")))-> YE_rec

# reformat all comm catch into one df
dplyr::select(YE_high, year, US_comm_catch_lbs) %>% 
  dplyr::rename(high = US_comm_catch_lbs) -> YE_high_comm

dplyr::select(YE_med, year, US_comm_catch_lbs) %>% 
  dplyr::rename(medium = US_comm_catch_lbs) -> YE_med_comm

dplyr::select(YE_low, year, US_comm_catch_lbs) %>% 
  dplyr::rename(low = US_comm_catch_lbs) -> YE_low_comm

YE_high_comm %>% 
  left_join(., YE_med_comm, by = "year") %>% 
  left_join(., YE_low_comm, by = "year") %>% 
  pivot_longer(., cols = c("high", "medium", "low")) %>% 
  dplyr::rename(scenario = name) %>% 
  mutate(scenario = factor(scenario, levels = c("high", "medium", "low")))-> YE_comm


# Generate the plots
YE_rec_catch_history_plot <- ggplot(YE_rec, aes(x = year, y = value, fill = scenario))+
  geom_bar(stat = "identity", position = position_dodge(width = 0), width = 2.2)+
  scale_fill_manual(values = c("#9ecae1", "#4292c6", "#08519c"))+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        legend.position = c(0.9, 0.85),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        plot.margin = unit(c(0.5,0.5,0.5,1), "cm"))+
  xlab("Year")+
  ylab("Catch (lbs)")+
  scale_x_continuous(breaks = seq(1920, 2020, 10)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 20000))

ggsave(paste0(fig_dir, "/for_paper/YE_rec.pdf"), YE_rec_catch_history_plot, height = 6, width = 12)

YE_comm_catch_history_plot <- ggplot(YE_comm, aes(x = year, y = value, fill = scenario))+
  geom_bar(stat = "identity", position = position_dodge(width = 0), width = 2.2)+
  scale_fill_manual(values = c("#9ecae1", "#4292c6", "#08519c"))+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        legend.position = c(0.9, 0.85),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        plot.margin = unit(c(0.5,0.5,0.5,1), "cm"))+
  xlab("Year")+
  ylab("Catch (lbs)")+
  scale_x_continuous(breaks = seq(1920, 2020, 10)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 20000))

ggsave(paste0(fig_dir, "/for_paper/YE_comm.pdf"), YE_comm_catch_history_plot, height = 6, width = 12)


# Join into two-panel figure and export
# combine them into one figure (Fig 2) for the manuscript
catch_fig <- ggarrange(YE_comm_catch_history_plot, YE_rec_catch_history_plot, ncol = 1, nrow = 2,
                            labels = c("(a)", "(b)"), font.label = list(size = 25, face = "plain"),
                       label.x = -0.01)

ggsave(here::here("figures", "catch_reconstruction", "for_paper", "Fig3.pdf"), catch_fig, height = 12, width  = 12) 
