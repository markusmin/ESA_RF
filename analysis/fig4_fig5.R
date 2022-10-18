# R script to generate figures 4 and 5 for Min et al. 2023

# Note: These are large stock synthesis files and therefoer not uploaded to GitHub.
# If you need access to these files, please contact me at mmin@uw.edu

library(tidyverse)
library(here)
library(r4ss)
library(ggpubr)

##### Figure 4 #####

## Load data
# setwd("/Users/markusmin/Documents/ESA_RF_2021_SS_runs/YEY_USDPS/2022-09-30/")
setwd("/Users/markusmin/Documents/ESA_RF_2021_SS_runs/YEY_USDPS/10_17_22/")
YE_hi_comp <- SSgetoutput("USDPS-hi_profile_NatM_uniform_Fem_GP_1_prior_like_0", keyvec = c("1", "2", "3", "4", "5", "6"))
YE_med_comp <- SSgetoutput("USDPS-med_profile_NatM_uniform_Fem_GP_1_prior_like_0", keyvec = c("1", "2", "3", "4", "5", "6"))
YE_low_comp <- SSgetoutput("USDPS-low_profile_NatM_uniform_Fem_GP_1_prior_like_0", keyvec = c("1", "2", "3", "4", "5", "6"))


YE_hi_M1 <- YE_hi_comp$replist1$timeseries
YE_hi_M2 <- YE_hi_comp$replist2$timeseries
YE_hi_M3 <- YE_hi_comp$replist3$timeseries
YE_hi_M4 <- YE_hi_comp$replist4$timeseries
YE_hi_M5 <- YE_hi_comp$replist5$timeseries
YE_hi_M6 <- YE_hi_comp$replist6$timeseries

YE_med_M1 <- YE_med_comp$replist1$timeseries
YE_med_M2 <- YE_med_comp$replist2$timeseries
YE_med_M3 <- YE_med_comp$replist3$timeseries
YE_med_M4 <- YE_med_comp$replist4$timeseries
YE_med_M5 <- YE_med_comp$replist5$timeseries
YE_med_M6 <- YE_med_comp$replist6$timeseries

YE_low_M1 <- YE_low_comp$replist1$timeseries
YE_low_M2 <- YE_low_comp$replist2$timeseries
YE_low_M3 <- YE_low_comp$replist3$timeseries
YE_low_M4 <- YE_low_comp$replist4$timeseries
YE_low_M5 <- YE_low_comp$replist5$timeseries
YE_low_M6 <- YE_low_comp$replist6$timeseries


## Reformat data

### High catch
# 1 = 0.04
YE_hi_comp$replist1$M_at_age
# 2 = 0.036
YE_hi_comp$replist2$M_at_age
# 3 = 0.032
YE_hi_comp$replist3$M_at_age
# 4 = 0.028
YE_hi_comp$replist4$M_at_age
# 5 = 0.024
YE_hi_comp$replist5$M_at_age
# 6 = 0.044
YE_hi_comp$replist6$M_at_age

# Get SSB and unfished, calculate Bratio
YE_hi_M1_Bratio_df <- data.frame(Yr = YE_hi_M1$Yr, Bratio = YE_hi_M1$SpawnBio/YE_hi_M1$SpawnBio[1], M = 0.04)
YE_hi_M2_Bratio_df <- data.frame(Yr = YE_hi_M2$Yr, Bratio = YE_hi_M2$SpawnBio/YE_hi_M2$SpawnBio[1], M = 0.036)
YE_hi_M3_Bratio_df <- data.frame(Yr = YE_hi_M3$Yr, Bratio = YE_hi_M3$SpawnBio/YE_hi_M3$SpawnBio[1], M = 0.032)
YE_hi_M4_Bratio_df <- data.frame(Yr = YE_hi_M4$Yr, Bratio = YE_hi_M4$SpawnBio/YE_hi_M4$SpawnBio[1], M = 0.028)
YE_hi_M5_Bratio_df <- data.frame(Yr = YE_hi_M5$Yr, Bratio = YE_hi_M5$SpawnBio/YE_hi_M5$SpawnBio[1], M = 0.024)
YE_hi_M6_Bratio_df <- data.frame(Yr = YE_hi_M6$Yr, Bratio = YE_hi_M6$SpawnBio/YE_hi_M6$SpawnBio[1], M = 0.044)

YE_hi_M1_Bratio_df %>% 
  bind_rows(., YE_hi_M2_Bratio_df) %>% 
  bind_rows(., YE_hi_M3_Bratio_df) %>% 
  bind_rows(., YE_hi_M4_Bratio_df) %>% 
  bind_rows(., YE_hi_M5_Bratio_df) %>% 
  bind_rows(., YE_hi_M6_Bratio_df) -> YE_hi_Mcomp

### Medium catch
# 1 = 0.036
YE_med_comp$replist1$M_at_age
# 2 = 0.032
YE_med_comp$replist2$M_at_age
# 3 = 0.028
YE_med_comp$replist3$M_at_age
# 4 = 0.024
YE_med_comp$replist4$M_at_age
# 5 = 0.04
YE_med_comp$replist5$M_at_age
# 6 = 0.044
YE_med_comp$replist6$M_at_age

YE_med_M1_Bratio_df <- data.frame(Yr = YE_med_M1$Yr, Bratio = YE_med_M1$SpawnBio/YE_med_M1$SpawnBio[1], M = 0.036)
YE_med_M2_Bratio_df <- data.frame(Yr = YE_med_M2$Yr, Bratio = YE_med_M2$SpawnBio/YE_med_M2$SpawnBio[1], M = 0.032)
YE_med_M3_Bratio_df <- data.frame(Yr = YE_med_M3$Yr, Bratio = YE_med_M3$SpawnBio/YE_med_M3$SpawnBio[1], M = 0.028)
YE_med_M4_Bratio_df <- data.frame(Yr = YE_med_M4$Yr, Bratio = YE_med_M4$SpawnBio/YE_med_M4$SpawnBio[1], M = 0.024)
YE_med_M5_Bratio_df <- data.frame(Yr = YE_med_M5$Yr, Bratio = YE_med_M5$SpawnBio/YE_med_M5$SpawnBio[1], M = 0.04)
YE_med_M6_Bratio_df <- data.frame(Yr = YE_med_M6$Yr, Bratio = YE_med_M6$SpawnBio/YE_med_M6$SpawnBio[1], M = 0.044)

YE_med_M1_Bratio_df %>% 
  bind_rows(., YE_med_M2_Bratio_df) %>% 
  bind_rows(., YE_med_M3_Bratio_df) %>% 
  bind_rows(., YE_med_M4_Bratio_df) %>% 
  bind_rows(., YE_med_M5_Bratio_df) %>% 
  bind_rows(., YE_med_M6_Bratio_df) -> YE_med_Mcomp

### Low catch

# 1 = 0.04
YE_low_comp$replist1$M_at_age
# 2 = 0.036
YE_low_comp$replist2$M_at_age
# 3 = 0.032
YE_low_comp$replist3$M_at_age
# 4 = 0.028
YE_low_comp$replist4$M_at_age
# 5 = 0.024
YE_low_comp$replist5$M_at_age
# 6 = 0.044
YE_low_comp$replist6$M_at_age

YE_low_M1_Bratio_df <- data.frame(Yr = YE_low_M1$Yr, Bratio = YE_low_M1$SpawnBio/YE_low_M1$SpawnBio[1], M = 0.04)
YE_low_M2_Bratio_df <- data.frame(Yr = YE_low_M2$Yr, Bratio = YE_low_M2$SpawnBio/YE_low_M2$SpawnBio[1], M = 0.036)
YE_low_M3_Bratio_df <- data.frame(Yr = YE_low_M3$Yr, Bratio = YE_low_M3$SpawnBio/YE_low_M3$SpawnBio[1], M = 0.032)
YE_low_M4_Bratio_df <- data.frame(Yr = YE_low_M4$Yr, Bratio = YE_low_M4$SpawnBio/YE_low_M4$SpawnBio[1], M = 0.028)
YE_low_M5_Bratio_df <- data.frame(Yr = YE_low_M5$Yr, Bratio = YE_low_M5$SpawnBio/YE_low_M5$SpawnBio[1], M = 0.024)
YE_low_M6_Bratio_df <- data.frame(Yr = YE_low_M6$Yr, Bratio = YE_low_M6$SpawnBio/YE_low_M6$SpawnBio[1], M = 0.044)

YE_low_M1_Bratio_df %>% 
  bind_rows(., YE_low_M2_Bratio_df) %>% 
  bind_rows(., YE_low_M3_Bratio_df) %>% 
  bind_rows(., YE_low_M4_Bratio_df) %>% 
  bind_rows(., YE_low_M5_Bratio_df) %>% 
  bind_rows(., YE_low_M6_Bratio_df) -> YE_low_Mcomp


## Plot

YE_hi_Mcomp_gg <- ggplot(YE_hi_Mcomp, aes(x = Yr, y = Bratio, color = as.factor(M))) +
  geom_line(size = 1) +
  scale_y_continuous(expand = c(0,0), lim = c(0, 1.2), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_x_continuous(expand = c(0,0), lim = c(1919, 2021), breaks = c(1920, 1940, 1960, 1980, 2000, 2020)) +
  ylab("Fraction of unfished") +
  xlab("Year") +
  scale_color_manual(name = expression(paste("Natural Mortality (", italic("M"), ")")),
                     values = c("#9ecae1",
                     "#6baed6",
                     "#4292c6",
                     "#2171b5",
                     "#08519c",
                     "#08306b"),
                     labels = c("0.024", "0.028", "0.032", "0.036", "0.040", "0.044"),
                     guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = c(0.2,0.3),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5),"cm"))

YE_med_Mcomp_gg <- ggplot(YE_med_Mcomp, aes(x = Yr, y = Bratio, color = as.factor(M))) +
  geom_line(size = 1) +
  scale_y_continuous(expand = c(0,0), lim = c(0, 1.2), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_x_continuous(expand = c(0,0), lim = c(1919, 2021), breaks = c(1920, 1940, 1960, 1980, 2000, 2020)) +
  ylab("Fraction of unfished") +
  xlab("Year") +
  scale_color_manual(name = expression(paste("Natural Mortality (", italic("M"), ")")),
                     values = c("#9ecae1",
                                "#6baed6",
                                "#4292c6",
                                "#2171b5",
                                "#08519c",
                                "#08306b"),
                     labels = c("0.024", "0.028", "0.032", "0.036", "0.040", "0.044"),
                     guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = c(0.2,0.3),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5),"cm"))

YE_low_Mcomp_gg <- ggplot(YE_low_Mcomp, aes(x = Yr, y = Bratio, color = as.factor(M))) +
  geom_line(size = 1) +
  scale_y_continuous(expand = c(0,0), lim = c(0, 1.2), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_x_continuous(expand = c(0,0), lim = c(1919, 2021), breaks = c(1920, 1940, 1960, 1980, 2000, 2020)) +
  ylab("Fraction of unfished") +
  xlab("Year") +
  scale_color_manual(name = expression(paste("Natural Mortality (", italic("M"), ")")),
                     values = c("#9ecae1",
                                "#6baed6",
                                "#4292c6",
                                "#2171b5",
                                "#08519c",
                                "#08306b"),
                     labels = c("0.024", "0.028", "0.032", "0.036", "0.040", "0.044"),
                     guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = c(0.2,0.3),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5),"cm"))

# Arrange three panels
YE_Mcomp_combined <- ggarrange(YE_hi_Mcomp_gg, YE_med_Mcomp_gg, YE_low_Mcomp_gg,
          ncol = 1, nrow = 3,
          labels = c("(a)", "(b)", "(c)"),
          label.x = 0.12, label.y = 0.94,
          font.label = list(size = 20, color = "black", face = "plain"))

ggsave(here::here("figures", "SS_figs", "for_paper", "Fig4.pdf"), YE_Mcomp_combined, height = 12, width = 5)

 
##### Figure 5 #####

## Load data
setwd("/Users/markusmin/Documents/ESA_RF_2021_SS_runs/new low M runs/")

lowM_hiCt<-SS_output("1. USDPS-hi-Mlow/")
lowM_medCt<-SS_output("2. USDPS-med-Mlow/")
lowM_lowCt<-SS_output("3. USDPS-low-Mlow/")

## Reformat data
lowM_hiCt.df<-data.frame(Year=c(1921:2021),Bratio=lowM_hiCt$derived_quants$Value[405:505],SD=lowM_hiCt$derived_quants$StdDev[405:505],Scenario="A_High_Ct_Low_M")
lowM_medCt.df<-data.frame(Year=c(1921:2021),Bratio=lowM_medCt$derived_quants$Value[405:505],SD=lowM_medCt$derived_quants$StdDev[405:505],Scenario="B_Medium_Ct_Low_M")
lowM_lowCt.df<-data.frame(Year=c(1921:2021),Bratio=lowM_lowCt$derived_quants$Value[401:501],SD=lowM_lowCt$derived_quants$StdDev[401:501],Scenario="C_Low_Ct_Low_M")
LowM.scenarios<-rbind(lowM_hiCt.df,lowM_medCt.df,lowM_lowCt.df)
LowM.scenarios.ci<-mutate(LowM.scenarios,low = Bratio -(1.96*SD), high = Bratio +(1.96*SD))

# check probability of being less than 0.25 in terminal year for high catch low M
mean_2021 <- subset(lowM_hiCt.df, Year == 2021)$Bratio
sd_2021 <- subset(lowM_hiCt.df, Year == 2021)$SD
qlnorm(0.25, meanlog = mean_2021, sdlog = sd_2021)
qnorm(0.25, mean = mean_2021, sd = sd_2021)

fig5_gg <-ggplot(LowM.scenarios.ci,aes(Year,Bratio,color=Scenario))+
  geom_line(lwd=1.5,lty=1)+
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.05)+
  labs(x = "Year",
       y = "Relative stock Status")+
  scale_color_manual(name = "Catch scenario", labels = c("High", "Medium", "Low"), values = c("#9ecae1", "#4292c6", "#08519c"))+
  scale_y_continuous(expand = c(0,0), lim = c(-0.1, 1.2), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_x_continuous(expand = c(0,0), lim = c(1921, 2021), breaks = c(1921, 1940, 1960, 1980, 2000, 2020)) +
  theme(legend.position = c(0.15,0.3),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_blank(),
        # panel.grid.major.y = element_line(color = "grey50", size = 0.25),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5),"cm")) +
  geom_hline(yintercept = 1, lty = 2) + 
  geom_hline(yintercept = 0, lty = 2)

fig5_gg

ggsave(here::here("figures", "SS_figs", "for_paper", "Fig5.pdf"), fig5_gg, height = 5, width = 8)

