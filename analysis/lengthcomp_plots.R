# Length composition plots for SSEC presentation

library(tidyverse)
library(here)
library(readxl)

PW <- read.csv(here::here("hook_and_line_data", "1975_USDPS_yelloweye_lengths.csv"))

ESA <- read_excel(here::here("hook_and_line_data", "2015_USDPS_yelloweye_lengths.xlsx"))

PW_lengthcomps <- ggplot(data = PW, aes(x = TL_mm/10)) +
  geom_histogram(binwidth = 2) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA))+
  scale_x_continuous(expand = c(0,0), limits = c(25,85)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,6.5)) +
  xlab("Length (cm)") +
  ylab("Count") + 
  ggtitle("1975")

ESA_lengthcomps <- ggplot(data = ESA, aes(x = FL_cm)) +
  geom_histogram(binwidth = 2) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA))+
  scale_x_continuous(expand = c(0,0), limits = c(25,85)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,6.5)) +
  xlab("Length (cm)") +
  ylab("Count") + 
  ggtitle("2015")

PW_lengthcomps
ESA_lengthcomps

ggsave(here::here("figures", "lengths", "2015_lencomps.png"), ESA_lengthcomps, height = 5.5, width  = 6)  
ggsave(here::here("figures", "lengths", "1975_lencomps.png"), PW_lengthcomps, height = 5.5, width  = 6)  