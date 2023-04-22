# Length composition plots for SSEC presentation

library(tidyverse)
library(here)
library(readxl)
library(ggpubr)

PW <- read.csv(here::here("hook_and_line_data", "1975_USDPS_yelloweye_lengths.csv"))
ESA <- read_excel(here::here("hook_and_line_data", "2015_USDPS_yelloweye_lengths.xlsx"))

PW %>% 
  mutate(TL_cm = TL_mm/10) -> PW

PW_lengthcomps <- ggplot(data = PW, aes(x = TL_cm)) +
  geom_histogram(binwidth = 4) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25, color = "gray50"),
        plot.margin = unit(c(0.5,0.5,0.5,1), "cm"))+
  scale_x_continuous(expand = c(0,0), limits = c(18,82)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,12), breaks = c(2,4,6,8,10,12)) +
  xlab("Length (cm)") +
  ylab("Count")

ESA_lengthcomps <- ggplot(data = ESA, aes(x = FL_cm)) +
  geom_histogram(binwidth = 4) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 26),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25, color = "gray50"),
        plot.margin = unit(c(0.5,0.5,0.5,1), "cm"))+
  scale_x_continuous(expand = c(0,0), limits = c(18,82)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,12), breaks = c(2,4,6,8,10,12)) +
  xlab("Length (cm)") +
  ylab("Count")

PW_lengthcomps
ESA_lengthcomps

ggsave(here::here("figures", "lengths", "2015_lencomps.png"), ESA_lengthcomps, height = 5.5, width  = 6)  
ggsave(here::here("figures", "lengths", "1975_lencomps.png"), PW_lengthcomps, height = 5.5, width  = 6) 

# combine them into one figure (Fig 4) for the manuscript
lengthcomp_fig <- ggarrange(PW_lengthcomps, ESA_lengthcomps, ncol = 1, nrow = 2,
          labels = c("(a)", "(b)"), font.label = list(size = 25, face = "plain"),
          label.x = -0.01)

ggsave(here::here("figures", "lengths", "Fig2.pdf"), lengthcomp_fig, height = 11, width  = 6) 


