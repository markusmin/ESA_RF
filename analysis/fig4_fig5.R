# R script to generate figures 4 and 5 for Min et al. 2023

# Note: These are large stock synthesis files and therefoer not uploaded to GitHub.
# If you need access to these files, please contact me at mmin@uw.edu

library(tidyverse)
library(here)
library(r4ss)
library(ggpubr)

### Figure 4
setwd("/Users/markusmin/Documents/ESA_RF_2021_SS_runs/YEY_USDPS/2022-09-30/")
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
