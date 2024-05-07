
############### NOTE ###################

# No need to read data separately for each model
# Start directly from Sensitivity plots


# Input data from the main model
# dinput from the main model 


library(tidyverse)
filenames <- list.files(path="results", full.names=TRUE)


myMergedData <- 
  do.call(rbind,
          lapply(list.files(path = "./results/"), read.csv))



# ############# S1: Main results #############



tot_y1   <- read.csv(paste("./results/s1_tot_y.csv")) %>% 
  mutate(model = "S1:Main")

imm_y1   <- read.csv("./results/S1_imm_y.csv") %>% 
  mutate(model = "S1:Main")

emi_y1   <- read.csv("./results/S1_emi_y.csv") %>% 
  mutate(model = "S1:Main")

bilat_y1 <- read.csv("./results/S1_bilat_y.csv") %>%
  mutate(model = "S1:Main")

# ### S2: Main low FB precision ####


tot_y2   <- read.csv("S:/Rand/Paper/update_2019/tot_y_main_model_low_fb_cov_prec.csv") %>% select(-X) %>%
  mutate(model = "Main low precision")

imm_y2   <- read.csv("S:/Rand/Paper/update_2019/imm_y_main_model_low_fb_cov_prec.csv") %>% select(-X) %>%
  mutate(model = "Main low precision")

emi_y2   <- read.csv("S:/Rand/Paper/update_2019/emi_y_main_model_low_fb_cov_prec.csv") %>% select(-X) %>%
  mutate(model = "Main low precision")

bilat_y2 <- read.csv("S:/Rand/Paper/update_2019/bilat_y_main_model_low_fb_cov_prec.csv") %>% select(-X) %>%
  mutate(model = "Main low precision")
# 
# #########################################
# ######## Estimate Eurostat - 10% ########
# #########################################
# 
# 
# tot_y3   <- read.csv("S:/Rand/Paper/estimate_eurostat/tot_y_m_estimate_eu_10per.csv") %>% select(-X) %>% 
#   mutate(model = "Eurostat 10 percent")
# 
# imm_y3   <- read.csv("S:/Rand/Paper/estimate_eurostat/imm_y_m_estimate_eu_10per.csv") %>% select(-X) %>% 
#   mutate(model = "Eurostat 10 percent")
# 
# emi_y3   <- read.csv("S:/Rand/Paper/estimate_eurostat/emi_y_m_estimate_eu_10per.csv") %>% select(-X) %>% 
#   mutate(model = "Eurostat 10 percent")
# 
# bilat_y3 <- read.csv("S:/Rand/Paper/estimate_eurostat/bilat_y_m_estimate_eu_10per.csv") %>% select(-X) %>% 
#   mutate(model = "Eurostat 10 percent")
# 
# 
# #########################################
# ######## Only Eurostat ########
# #########################################
# 
# 
# tot_y4   <- read.csv("S:/Rand/Paper/only_eurostat/tot_y_m_eurostat_only.csv") %>% select(-X) %>% 
#   mutate(model = "Only Eurostat")
# 
# imm_y4   <- read.csv("S:/Rand/Paper/only_eurostat/imm_y_m_eurostat_only.csv") %>% select(-X) %>% 
#   mutate(model = "Only Eurostat")
# 
# emi_y4   <- read.csv("S:/Rand/Paper/only_eurostat/emi_y_m_eurostat_only.csv") %>% select(-X) %>% 
#   mutate(model = "Only Eurostat")
# 
# bilat_y4 <- read.csv("S:/Rand/Paper/only_eurostat/bilat_y_m_eurostat_only.csv") %>% select(-X) %>% 
#   mutate(model = "Only Eurostat")
# 
# 
# #########################################
# ######## Estimate Eurostat 2016 #########
# #########################################
# 
# 
# tot_y5   <- read.csv("S:/Rand/Paper/estimate_eurostat/tot_y_m_estimate_eu16.csv") %>% select(-X) %>% 
#   mutate(model = "Eurostat estimate 2016")
# 
# imm_y5   <- read.csv("S:/Rand/Paper/estimate_eurostat/imm_y_m_estimate_eu16.csv") %>% select(-X) %>% 
#   mutate(model = "Eurostat estimate 2016")
# 
# emi_y5   <- read.csv("S:/Rand/Paper/estimate_eurostat/emi_y_m_estimate_eu16.csv") %>% select(-X) %>% 
#   mutate(model = "Eurostat estimate 2016")
# 
# bilat_y5 <- read.csv("S:/Rand/Paper/estimate_eurostat/bilat_y_m_estimate_eu16.csv") %>% select(-X) %>% 
#   mutate(model = "Eurostat estimate 2016")
# 
# 
# 
# #########################################
# ########## Without Facebook  ############
# #########################################
# 
# 
# tot_y6   <- read.csv("S:/Rand/Paper/wo_facebook/wo_facebook_missing2019/tot_y_m_wo_facebook_est18.csv") %>% 
#   select(-X) %>% 
#   mutate(model = "Without Facebook")
# 
# imm_y6   <- read.csv("S:/Rand/Paper/wo_facebook/wo_facebook_missing2019/imm_y_m_wo_facebook_est18.csv") %>% 
#   select(-X) %>% 
#   mutate(model = "Without Facebook")
# 
# emi_y6   <- read.csv("S:/Rand/Paper/wo_facebook/wo_facebook_missing2019/emi_y_m_wo_facebook_est18.csv") %>% 
#   select(-X) %>% 
#   mutate(model = "Without Facebook")
# 
# bilat_y6 <- read.csv("S:/Rand/Paper/wo_facebook/wo_facebook_missing2019/bilat_y_m_wo_facebook_est18.csv") %>% 
#   select(-X) %>% 
#   mutate(model = "Without Facebook")
# 
# 
# 
# #########################################
# ######## Estimate Eurostat - 5% ########
# #########################################
# 
# 
# tot_y7   <- read.csv("S:/Rand/Paper/estimate_eurostat/tot_y_m_estimate_eu_5per.csv") %>% select(-X) %>%
#   mutate(model = "Eurostat 5 percent")
# 
# imm_y7   <- read.csv("S:/Rand/Paper/estimate_eurostat/imm_y_m_estimate_eu_5per.csv") %>% select(-X) %>%
#   mutate(model = "Eurostat 5 percent")
# 
# emi_y7   <- read.csv("S:/Rand/Paper/estimate_eurostat/emi_y_m_estimate_eu_5per.csv") %>% select(-X) %>%
#   mutate(model = "Eurostat 5 percent")
# 
# bilat_y7 <- read.csv("S:/Rand/Paper/estimate_eurostat/bilat_y_m_estimate_eu_5per.csv") %>% select(-X) %>%
#   mutate(model = "Eurostat 5 percent")
# 
# 
# 
# #########################################
# ######## Estimate Eurostat - 20% ########
# #########################################
# 
# 
# tot_y8   <- read.csv("S:/Rand/Paper/estimate_eurostat/tot_y_m_estimate_eu_20per.csv") %>% select(-X) %>%
#   mutate(model = "Eurostat 20 percent")
# 
# imm_y8   <- read.csv("S:/Rand/Paper/estimate_eurostat/imm_y_m_estimate_eu_20per.csv") %>% select(-X) %>%
#   mutate(model = "Eurostat 20 percent")
# 
# emi_y8   <- read.csv("S:/Rand/Paper/estimate_eurostat/emi_y_m_estimate_eu_20per.csv") %>% select(-X) %>%
#   mutate(model = "Eurostat 20 percent")
# 
# bilat_y8 <- read.csv("S:/Rand/Paper/estimate_eurostat/bilat_y_m_estimate_eu_20per.csv") %>% select(-X) %>%
#   mutate(model = "Eurostat 20 percent")
# 
# #########################################
# ######## Estimate Eurostat 2018 #########
# #########################################
# 
# 
# tot_y9   <- read.csv("S:/Rand/Paper/estimate_eurostat/tot_y_m_estimate_eu18.csv") %>% select(-X) %>% 
#   mutate(model = "Eurostat estimate 2018")
# 
# imm_y9   <- read.csv("S:/Rand/Paper/estimate_eurostat/imm_y_m_estimate_eu18.csv") %>% select(-X) %>% 
#   mutate(model = "Eurostat estimate 2018")
# 
# emi_y9   <- read.csv("S:/Rand/Paper/estimate_eurostat/emi_y_m_estimate_eu18.csv") %>% select(-X) %>% 
#   mutate(model = "Eurostat estimate 2018")
# 
# bilat_y9 <- read.csv("S:/Rand/Paper/estimate_eurostat/bilat_y_m_estimate_eu18.csv") %>% select(-X) %>% 
#   mutate(model = "Eurostat estimate 2018")
# 
# 

#########################################
######## Naive 1 #########
#########################################


tot_y10   <- read.csv("S:/Rand/Paper/estimate_eurostat/tot_y_m_estimate_eu18.csv") %>% select(-X) %>% 
  mutate(model = "Eurostat estimate 2018")

imm_y10   <- read.csv("S:/Rand/Paper/estimate_eurostat/imm_y_m_estimate_eu18.csv") %>% select(-X) %>% 
  mutate(model = "Eurostat estimate 2018")

emi_y10   <- read.csv("S:/Rand/Paper/estimate_eurostat/emi_y_m_estimate_eu18.csv") %>% select(-X) %>% 
  mutate(model = "Eurostat estimate 2018")

bilat_y10 <- read.csv("S:/Rand/Paper/estimate_eurostat/bilat_y_m_estimate_eu18.csv") %>% select(-X) %>% 
  mutate(model = "Eurostat estimate 2018")


