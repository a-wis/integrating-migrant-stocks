rm(list = ls())
library(tidyverse)
library(ggmcmc)
library(geofacet)
library(scales)
library(readxl)


source("code/read_functions.R")


# Input data from the main model
# ?????
dinput <- read.csv("data/data_july19/input_update_2019_march_june_fbmau_oldcovgr_prec.csv")

# data frames for building summary states
corridors <- dinput %>% 
  select(orig, dest, corridor) %>% 
  distinct()

years <- dinput %>%
  select(year) %>%
  mutate(node_id = 1:n())


########################################
############# Main results #############
########################################
# 
# load(file = "S:/Rand/Paper/m_update_2019_March_June_mau_est_old_fb_cov_5gr_prec.RData")
# # load(file = "C:/Users/dyildiz.VID-CALC1/Documents/m_update_2019_March_June_mau_est_old_fb_cov_5gr_prec.RData")
# 
# y1 <- ggs(m, family = "^y1") %>%
#   add_node() %>%
#   add_corridor() %>%
#   left_join(years)
# 
# 
# tot_y1 <- y_stats(y1)
# 
# # write.csv(tot_y1, "./Paper/update_2019/tot_y_main_model.csv")
# 
# imm_y1 <- y_stats(y1, type = "imm")
# # write.csv(imm_y1, "./Paper/update_2019/imm_y_main_model.csv")
# 
# 
# emi_y1 <- y_stats((y1), type = "emi") %>%
#   rename(flow = total)
# # write.csv(emi_y1, "./Paper/update_2019/emi_y_main_model.csv")
# 
# bilat_y1 <- y1 %>%
#   # filter(Iteration >= 500) %>%
#   y_stats(type = "bilat")
# # write.csv(bilat_y1, "./Paper/update_2019/bilat_y_main_model.csv")

tot_y1   <- read.csv("./Paper/update_2019/tot_y_main_model.csv") %>% select(-X) %>% 
  mutate(model = "Main")

imm_y1   <- read.csv("./Paper/update_2019/imm_y_main_model.csv") %>% select(-X) %>% 
  mutate(model = "Main")

emi_y1   <- read.csv("./Paper/update_2019/emi_y_main_model.csv") %>% select(-X) %>% 
  mutate(model = "Main")

bilat_y1 <- read.csv("./Paper/update_2019/bilat_y_main_model.csv") %>% select(-X) %>% 
  mutate(model = "Main")

#########################################
### Main model - lower prec in FB cov ###
#########################################

# load(file = "S:/Rand/Paper/m_update_2019_March_June_mau_est_old_fb_cov_5gr.RData")
# 
# y2 <- ggs(m, family = "^y1") %>%
#   add_node() %>%
#   add_corridor() %>%
#   left_join(years)
# 
# 
# tot_y2 <- y_stats(y2)
# 
# # write.csv(tot_y2, "./Paper/update_2019/tot_y_main_model_low_fb_cov_prec.csv")
# 
# imm_y2 <- y_stats(y2, type = "imm")
# # write.csv(imm_y2, "./Paper/update_2019/imm_y_main_model_low_fb_cov_prec.csv")
# 
# 
# emi_y2 <- y_stats((y2), type = "emi") %>%
#   rename(flow = total)
# # write.csv(emi_y2, "./Paper/update_2019/emi_y_main_model_low_fb_cov_prec.csv")
# 
# bilat_y2 <- y2 %>%
#   # filter(Iteration >= 500) %>%
#   y_stats(type = "bilat")
# # write.csv(bilat_y2, "./Paper/update_2019/bilat_y_main_model_low_fb_cov_prec.csv")

tot_y2   <- read.csv("./Paper/update_2019/tot_y_main_model_low_fb_cov_prec.csv") %>% select(-X) %>% 
  mutate(model = "Main low precision")

imm_y2   <- read.csv("./Paper/update_2019/imm_y_main_model_low_fb_cov_prec.csv") %>% select(-X) %>% 
  mutate(model = "Main low precision")

emi_y2   <- read.csv("./Paper/update_2019/emi_y_main_model_low_fb_cov_prec.csv") %>% select(-X) %>% 
  mutate(model = "Main low precision")

bilat_y2 <- read.csv("./Paper/update_2019/bilat_y_main_model_low_fb_cov_prec.csv") %>% select(-X) %>% 
  mutate(model = "Main low precision")

#########################################
######## Estimate Eurostat - 10% ########
#########################################

# 
# load(file = "S:/Rand/Paper/m_estimate_eu_10per.RData")
# 
# y3 <- ggs(m, family = "^y1") %>%
#   add_node() %>%
#   add_corridor() %>%
#   left_join(years)
# 
# 
# tot_y3 <- y_stats(y3)
# 
# # write.csv(tot_y3, "./Paper/update_2019/tot_y_m_estimate_eu_10per.csv")
# 
# imm_y3 <- y_stats(y3, type = "imm")
# # write.csv(imm_y3, "./Paper/update_2019/imm_y_m_estimate_eu_10per.csv")
# 
# 
# emi_y3 <- y_stats((y3), type = "emi") %>%
#   rename(flow = total)
# # write.csv(emi_y3, "./Paper/update_2019/emi_y_m_estimate_eu_10per.csv")
# 
# bilat_y3 <- y3 %>%
#   # filter(Iteration >= 500) %>%
#   y_stats(type = "bilat")
# # write.csv(bilat_y3, "./Paper/update_2019/bilat_y_m_estimate_eu_10per.csv")


tot_y3   <- read.csv("./Paper/estimate_eurostat/tot_y_m_estimate_eu_10per.csv") %>% select(-X) %>% 
  mutate(model = "Eurostat 10 percent")

imm_y3   <- read.csv("./Paper/estimate_eurostat/imm_y_m_estimate_eu_10per.csv") %>% select(-X) %>% 
  mutate(model = "Eurostat 10 percent")

emi_y3   <- read.csv("./Paper/estimate_eurostat/emi_y_m_estimate_eu_10per.csv") %>% select(-X) %>% 
  mutate(model = "Eurostat 10 percent")

bilat_y3 <- read.csv("./Paper/estimate_eurostat/bilat_y_m_estimate_eu_10per.csv") %>% select(-X) %>% 
  mutate(model = "Eurostat 10 percent")


#########################################
######## Only Eurostat ########
#########################################


# load(file = "S:/Rand/Paper/m_eurostat_only.RData")
# 
# y4 <- ggs(m, family = "^y1") %>%
#   add_node() %>%
#   add_corridor() %>%
#   left_join(years)
# 
# 
# tot_y4 <- y_stats(y4)
# 
# # write.csv(tot_y4, "./Paper/update_2019/tot_y_m_eurostat_only.csv")
# 
# imm_y4 <- y_stats(y4, type = "imm")
# # write.csv(imm_y4, "./Paper/update_2019/imm_y_m_eurostat_only.csv")
# 
# 
# emi_y4 <- y_stats((y4), type = "emi") %>%
#   rename(flow = total)
# # write.csv(emi_y4, "./Paper/update_2019/emi_y_m_eurostat_only.csv")
# 
# bilat_y4 <- y4 %>%
#   # filter(Iteration >= 500) %>%
#   y_stats(type = "bilat")
# # write.csv(bilat_y4, "./Paper/update_2019/bilat_y_m_eurostat_only.csv")


tot_y4   <- read.csv("./Paper/only_eurostat/tot_y_m_eurostat_only.csv") %>% select(-X) %>% 
  mutate(model = "Only Eurostat")

imm_y4   <- read.csv("./Paper/only_eurostat/imm_y_m_eurostat_only.csv") %>% select(-X) %>% 
  mutate(model = "Only Eurostat")

emi_y4   <- read.csv("./Paper/only_eurostat/emi_y_m_eurostat_only.csv") %>% select(-X) %>% 
  mutate(model = "Only Eurostat")

bilat_y4 <- read.csv("./Paper/only_eurostat/bilat_y_m_eurostat_only.csv") %>% select(-X) %>% 
  mutate(model = "Only Eurostat")


#########################################
######## Estimate Eurostat 2016 #########
#########################################

# 
# load(file = "S:/Rand/Paper/m_estimate_eu16_March_June_mau_est_old_fb_cov_5gr_prec.RData")
# 
# y5 <- ggs(m, family = "^y1") %>%
#   add_node() %>%
#   add_corridor() %>%
#   left_join(years)
# 
# 
# tot_y5 <- y_stats(y5)
# 
# # write.csv(tot_y5, "./Paper/update_2019/tot_y_m_estimate_eu16.csv")
# 
# imm_y5 <- y_stats(y5, type = "imm")
# # write.csv(imm_y5, "./Paper/update_2019/imm_y_m_estimate_eu16.csv")
# 
# 
# emi_y5 <- y_stats((y5), type = "emi") %>%
#   rename(flow = total)
# # write.csv(emi_y5, "./Paper/update_2019/emi_y_m_estimate_eu16.csv")
# 
# bilat_y5 <- y5 %>%
#   # filter(Iteration >= 500) %>%
#   y_stats(type = "bilat")
# # write.csv(bilat_y5, "./Paper/update_2019/bilat_y_m_estimate_eu16.csv")


tot_y5   <- read.csv("./Paper/estimate_eurostat/tot_y_m_estimate_eu16.csv") %>% select(-X) %>% 
  mutate(model = "Eurostat estimate 2016")

imm_y5   <- read.csv("./Paper/estimate_eurostat/imm_y_m_estimate_eu16.csv") %>% select(-X) %>% 
  mutate(model = "Eurostat estimate 2016")

emi_y5   <- read.csv("./Paper/estimate_eurostat/emi_y_m_estimate_eu16.csv") %>% select(-X) %>% 
  mutate(model = "Eurostat estimate 2016")

bilat_y5 <- read.csv("./Paper/estimate_eurostat/bilat_y_m_estimate_eu16.csv") %>% select(-X) %>% 
  mutate(model = "Eurostat estimate 2016")



#########################################
########## Without Facebook  ############
#########################################


 # load(file = "S:/Rand/Paper/m_wo_facebook_august19_est18.RData")
 # 
 # y6 <- ggs(m, family = "^y1") %>%
 #   add_node() %>%
 #   add_corridor() %>%
 #   left_join(years)
 # 
 # 
 # tot_y6 <- y_stats(y6)
 # 
 # write.csv(tot_y6, "./Paper/update_2019/tot_y_m_wo_facebook_est18.csv")
 # 
 # imm_y6 <- y_stats(y6, type = "imm")
 # write.csv(imm_y6, "./Paper/update_2019/imm_y_m_wo_facebook_est18.csv")
 # 
 # 
 # emi_y6 <- y_stats((y6), type = "emi") %>%
 #   rename(flow = total)
 # write.csv(emi_y6, "./Paper/update_2019/emi_y_m_wo_facebook_est18.csv")
 # 
 # bilat_y6 <- y6 %>%
 #   # filter(Iteration >= 500) %>%
 #   y_stats(type = "bilat")
 # write.csv(bilat_y6, "./Paper/update_2019/bilat_y_m_wo_facebook_est18.csv")


tot_y6   <- read.csv("./Paper/wo_facebook/tot_y_m_wo_facebook_est18.csv") %>% select(-X) %>% 
  mutate(model = "Without_Facebook")
imm_y6   <- read.csv("./Paper/wo_facebook/imm_y_m_wo_facebook_est18.csv") %>% select(-X)%>% 
  mutate(model = "Without_Facebook")
emi_y6   <- read.csv("./Paper/wo_facebook/emi_y_m_wo_facebook_est18.csv") %>% select(-X)%>% 
  mutate(model = "Without_Facebook")
bilat_y6 <- read.csv("./Paper/wo_facebook/bilat_y_m_wo_facebook_est18.csv") %>% select(-X)%>% 
  mutate(model = "Without_Facebook")



#########################################
######## Estimate Eurostat - 5% ########
#########################################


# load(file = "S:/Rand/Paper/m_estimate_eu_5per.RData")
# 
# y7 <- ggs(m, family = "^y1") %>%
#   add_node() %>%
#   add_corridor() %>%
#   left_join(years)
# 
# 
# tot_y7 <- y_stats(y7)
# 
# write.csv(tot_y7, "./Paper/update_2019/tot_y_m_estimate_eu_5per.csv")
# 
# imm_y7 <- y_stats(y7, type = "imm")
# write.csv(imm_y7, "./Paper/update_2019/imm_y_m_estimate_eu_5per.csv")
# 
# 
# emi_y7 <- y_stats((y7), type = "emi") %>%
#   rename(flow = total)
# write.csv(emi_y7, "./Paper/update_2019/emi_y_m_estimate_eu_5per.csv")
# 
# bilat_y7 <- y7 %>%
# y_stats(type = "bilat")
#  write.csv(bilat_y7, "./Paper/update_2019/bilat_y_m_estimate_eu_5per.csv")


tot_y7   <- read.csv("./Paper/estimate_eurostat/tot_y_m_estimate_eu_5per.csv") %>% select(-X) %>%
  mutate(model = "Eurostat 5 percent")

imm_y7   <- read.csv("./Paper/estimate_eurostat/imm_y_m_estimate_eu_5per.csv") %>% select(-X) %>%
  mutate(model = "Eurostat 5 percent")

emi_y7   <- read.csv("./Paper/estimate_eurostat/emi_y_m_estimate_eu_5per.csv") %>% select(-X) %>%
  mutate(model = "Eurostat 5 percent")

bilat_y7 <- read.csv("./Paper/estimate_eurostat/bilat_y_m_estimate_eu_5per.csv") %>% select(-X) %>%
  mutate(model = "Eurostat 5 percent")



#########################################
######## Estimate Eurostat - 20% ########
#########################################


 # load(file = "S:/Rand/Paper/m_estimate_eu_20per.RData")
 # 
 # y8 <- ggs(m, family = "^y1") %>%
 #   add_node() %>%
 #   add_corridor() %>%
 #   left_join(years)
 # 
 # 
 # tot_y8 <- y_stats(y8)
 # 
 # write.csv(tot_y8, "./Paper/update_2019/tot_y_m_estimate_eu_20per.csv")
 # 
 # imm_y8 <- y_stats(y8, type = "imm")
 # write.csv(imm_y8, "./Paper/update_2019/imm_y_m_estimate_eu_20per.csv")
 # 
 # 
 # emi_y8 <- y_stats((y8), type = "emi") %>%
 #   rename(flow = total)
 #  write.csv(emi_y8, "./Paper/update_2019/emi_y_m_estimate_eu_20per.csv")
 # 
 # bilat_y8 <- y8 %>%
 # y_stats(type = "bilat")
 #  write.csv(bilat_y8, "./Paper/update_2019/bilat_y_m_estimate_eu_20per.csv")


tot_y8   <- read.csv("./Paper/estimate_eurostat/tot_y_m_estimate_eu_20per.csv") %>% select(-X) %>%
  mutate(model = "Eurostat 20 percent")

imm_y8   <- read.csv("./Paper/estimate_eurostat/imm_y_m_estimate_eu_20per.csv") %>% select(-X) %>%
  mutate(model = "Eurostat 20 percent")

emi_y8   <- read.csv("./Paper/estimate_eurostat/emi_y_m_estimate_eu_20per.csv") %>% select(-X) %>%
  mutate(model = "Eurostat 20 percent")

bilat_y8 <- read.csv("./Paper/estimate_eurostat/bilat_y_m_estimate_eu_20per.csv") %>% select(-X) %>%
  mutate(model = "Eurostat 20 percent")

#########################################
######## Estimate Eurostat 2018 #########
#########################################

# 
# load(file = "S:/Rand/Paper/m_estimate_eu_2018.RData")
# 
# y9 <- ggs(m, family = "^y1") %>%
#   add_node() %>%
#   add_corridor() %>%
#   left_join(years)
# 
# 
# tot_y9 <- y_stats(y9)
# 
# # write.csv(tot_y9, "./Paper/update_2019/tot_y_m_estimate_eu18.csv")
# 
# imm_y9 <- y_stats(y9, type = "imm")
# # write.csv(imm_y9, "./Paper/update_2019/imm_y_m_estimate_eu18.csv")
# 
# 
# emi_y9 <- y_stats((y9), type = "emi") %>%
#   rename(flow = total)
# # write.csv(emi_y9, "./Paper/update_2019/emi_y_m_estimate_eu18.csv")
# 
# bilat_y9 <- y9 %>%
#   y_stats(type = "bilat")
# # write.csv(bilat_y9, "./Paper/update_2019/bilat_y_m_estimate_eu18.csv")


tot_y9   <- read.csv("./Paper/estimate_eurostat/tot_y_m_estimate_eu18.csv") %>% select(-X) %>% 
  mutate(model = "Eurostat estimate 2018")

imm_y9   <- read.csv("./Paper/estimate_eurostat/imm_y_m_estimate_eu18.csv") %>% select(-X) %>% 
  mutate(model = "Eurostat estimate 2018")

emi_y9   <- read.csv("./Paper/estimate_eurostat/emi_y_m_estimate_eu18.csv") %>% select(-X) %>% 
  mutate(model = "Eurostat estimate 2018")

bilat_y9 <- read.csv("./Paper/estimate_eurostat/bilat_y_m_estimate_eu18.csv") %>% select(-X) %>% 
  mutate(model = "Eurostat estimate 2018")



gg_color_hue <- function(n){hues = seq(15, 375, length = n + 1) 
hcl(h = hues, l = 65, c = 100)[1:n]}

col0 <- gg_color_hue(9)

tot2 <- bind_rows(tot_y1, tot_y2, tot_y3, tot_y4, tot_y5, tot_y6, tot_y7, tot_y8, tot_y9)

p <- ggplot(tot2, aes(x = year, y = total/1e6, color = model)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr80/1e6, ymax = upp80/1e6, fill = model), alpha = 0.3, colour = NA) +
  labs(x= "Year", y= "Stocks of EU movers (millions) and 80% PI, 15 - 64", colour = "", fill = "") +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  theme_bw() +
  scale_color_manual(values = col0, drop=FALSE) 

p <- ggplot(tot2, aes(x = year, y = total/1e6, color = model)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr50/1e6, ymax = upp50/1e6, fill = model), alpha = 0.3, colour = NA) +
  labs(x= "Year", y= "Stocks of EU movers (millions) and 50% PI, 15 - 64", colour = "", fill = "") +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  # scale_y_continuous(breaks = seq(0, 18, by =2)) +
  scale_y_continuous(breaks = seq(8, 18, by =2)) +
  theme_bw() +
  scale_color_manual(values = col0, drop=FALSE) 
# +
#   expand_limits(y = 0)


png("./Paper/sensitivity/model_comparison_total_80PI.png", height = 6, width = 5.5, units = "in", res = 300)
p
dev.off()


png("./Paper/sensitivity/model_comparison_total_50PI.png", height = 6, width = 5.5, units = "in", res = 300)
p
dev.off()



png("./Paper/sensitivity/model_comparison_total_median.png", height = 6, width = 5.5, units = "in", res = 300)
p
dev.off()


imm2 <- bind_rows(imm_y1, imm_y2, imm_y3, imm_y4, imm_y5, imm_y6, imm_y7, imm_y8, imm_y9)


imm2$name = plyr::revalue(imm2$name, c("United Kingdom" = "UK"))
imm2$name[imm2$name == "United Kingdom"] = "UK"
eu_grid1$name[eu_grid1$name == "Czech Republic"] = "Czechia"
eu_grid1$name[eu_grid1$name == "United Kingdom"] = "UK"

scaleFUN <- function(x) sprintf("%.2f", x)
p <- ggplot(data = imm2, 
       mapping = aes(x = year, y = total/1e06, ymin = lwr80/1e06, ymax = upp80/1e06)) +
  facet_geo(facets = "name", grid = eu_grid1) +
  geom_line(mapping = aes(colour = model)) +
  geom_ribbon(mapping = aes(fill = model), alpha = 0.3) +
  theme_bw() +
  scale_color_manual(values = col0, drop=FALSE) +
  # scale_fill_manual(values = col0, drop=FALSE) + 
  scale_x_continuous(breaks = seq(2011, 2018, 2)) +
  scale_y_continuous(labels = scaleFUN) +
  labs(x= "Year", y= "Stocks of EU movers (millions)", colour = "", fill = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  # guides(fill = FALSE) +
  ggtitle(paste0("Immigration"))


png("./Paper/sensitivity/model_comparison_imm_80PI.png", height = 10, width = 10, units = "in", res = 300)
p
dev.off()


emi2 <- bind_rows(emi_y1, emi_y2, emi_y3, emi_y4, emi_y5, emi_y6, emi_y7, emi_y8, emi_y9)

emi2$name = plyr::revalue(emi2$name, c("United Kingdom" = "UK"))
emi2$name[emi2$name == "United Kingdom"] = "UK"
eu_grid1$name[eu_grid1$name == "Czech Republic"] = "Czechia"
eu_grid1$name[eu_grid1$name == "United Kingdom"] = "UK"

scaleFUN <- function(x) sprintf("%.2f", x)
p <- ggplot(data = emi2, 
       mapping = aes(x = year, y = flow/1e06, ymin = lwr50/1e06, ymax = upp50/1e06)) +
  facet_geo(facets = "name", grid = eu_grid1) +
  geom_line(mapping = aes(colour = model)) +
  geom_ribbon(mapping = aes(fill = model), alpha = 0.3) +
  theme_bw() +
  scale_color_manual(values = col0, drop=FALSE) +
  # scale_fill_manual(values = col0, drop=FALSE) + 
  scale_x_continuous(breaks = seq(2011, 2018, 2)) +
  scale_y_continuous(labels = scaleFUN) +
  labs(x= "Year", y= "Stocks of EU movers (millions)", colour = "", fill = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  # guides(fill = FALSE) +
  ggtitle(paste0("Emigration"))



png("./Paper/sensitivity/model_comparison_emi_50PI.png", height = 10, width = 10, units = "in", res = 300)
p
dev.off()




#####################
# Estimate Eurostat #
#####################

#### 10 Percent #####
# compare estimated Eurostat with observed Eurostat values
# dinput has observed Eurostat values
# flag == 1 removed values
dinput_eu10per <- read.csv("./Paper/data_july19/input_estimate_eurostat_10per.csv")

eu_all <- dinput %>% 
  filter(source == "eurostat") %>% 
  rename(eu_all = flow)

eu_10per <- dinput_eu10per %>% 
  filter(source == "eurostat") %>% 
  rename(eu_10per = flow) %>% 
  left_join(eu_all) %>% 
  mutate(flag = ifelse(!is.na(eu_all)&is.na(eu_10per), 1, 0))

load(file = "S:/Rand/Paper/m_estimate_eu_10per.RData")

eu_estimate <- ggs(m, family = "eurostat")

eu_rm <- grep("tau", eu_estimate$Parameter)
eu_rm2 <- grep("gamma", eu_estimate$Parameter)
eu_rm3 <- c(eu_rm, eu_rm2)

eu_estimate2 <- eu_estimate[-eu_rm3,]

eu <- eu_estimate2 %>% 
  add_node() %>% 
  add_corridor() %>% 
  left_join(years) %>% 
  group_by(Chain, Iteration, year, orig, dest) %>%
  summarise(total_chit = sum(exp(value))) %>%
  group_by(year, orig, dest) %>% 
  summarise(total = median(total_chit),
            sd = sd(total_chit), 
            upp50 = quantile(x = total_chit, probs = 0.75),
            lwr50 = quantile(x = total_chit, probs = 0.25), 
            upp80 = quantile(x = total_chit, probs = 0.9),
            lwr80 = quantile(x = total_chit, probs = 0.1)) 
  
  
eu_test <- left_join(eu, eu_10per) %>% 
  filter(flag == 1) %>% 
  mutate(test50 = ifelse((eu_all >= lwr50 & eu_all <=upp50), 1, 0), 
         test80 = ifelse((eu_all >= lwr80 & eu_all <=upp80), 1, 0))


p <- ggplot(eu_test, aes(x = eu_all, y = total)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr50, ymax = upp50)) +
  geom_abline(intercept = 0, slope = 1) + 
  scale_x_continuous(breaks = seq(0, 1.5e6, 1e5), labels = scientific) +
  scale_y_continuous(breaks = seq(0, 1.5e6, 1e5), labels = scientific) +
  labs(x = "Eurostat", y = "Estimate")
  
png("./Paper/sensitivity/compare_eu10per_observed_50PI.png")
p
dev.off()







#### 2016 #####
# compare estimated Eurostat with observed Eurostat values
# dinput has observed Eurostat values
# flag == 1 removed values



eu_16 <- dinput %>% 
  filter(source == "eurostat") %>% 
  mutate(flow = ifelse(year==2016, NA, flow)) %>% 
  rename(eu_16 = flow) %>% 
  left_join(eu_all) %>% 
  mutate(flag = ifelse(year==2016, 1, 0))

load(file = "S:/Rand/Paper/m_estimate_eu16_March_June_mau_est_old_fb_cov_5gr_prec.RData")

eu_estimate_16 <- ggs(m, family = "eurostat")

eu_rm_16  <- grep("tau", eu_estimate_16$Parameter)
eu_rm2_16 <- grep("gamma", eu_estimate_16$Parameter)
eu_rm3_16 <- c(eu_rm_16, eu_rm2_16)

eu_estimate2_16 <- eu_estimate_16[-eu_rm3_16,]

# bilat Eurostat estimates 
eu_estimate2_16 <- eu_estimate2_16 %>% 
  add_node() %>% 
  add_corridor() %>% 
  left_join(years) %>% 
  group_by(Chain, Iteration, year, orig, dest) %>%
  summarise(total_chit = sum(exp(value))) %>%
  group_by(year, orig, dest) %>% 
  summarise(total = median(total_chit),
            sd = sd(total_chit), 
            upp50 = quantile(x = total_chit, probs = 0.75),
            lwr50 = quantile(x = total_chit, probs = 0.25), 
            upp80 = quantile(x = total_chit, probs = 0.9),
            lwr80 = quantile(x = total_chit, probs = 0.1)) 


eu_test <- left_join(eu_16, eu_estimate2_16) %>% 
  filter(flag == 1) %>% 
  mutate(test50 = ifelse((eu_all >= lwr50 & eu_all <=upp50), 1, 0), 
         test80 = ifelse((eu_all >= lwr80 & eu_all <=upp80), 1, 0))


p <- ggplot(eu_test, aes(x = eu_all, y = total)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr80, ymax = upp80)) +
  geom_abline(intercept = 0, slope = 1) + 
  scale_x_continuous(breaks = seq(0, 1.5e6, 1e5), labels = scientific) +
  scale_y_continuous(breaks = seq(0, 1.5e6, 1e5), labels = scientific) +
  labs(x = "Eurostat", y = "Estimate")

png("./Paper/sensitivity/compare_eu16_observed_80PI.png")
p
dev.off()



#### 2018 #####
# compare estimated Eurostat with observed Eurostat values
# dinput has observed Eurostat values
# flag == 1 removed values



eu_18 <- dinput %>% 
  filter(source == "eurostat") %>% 
  mutate(flow = ifelse(year==2018, NA, flow)) %>% 
  rename(eu_18 = flow) %>% 
  left_join(eu_all) %>% 
  mutate(flag = ifelse(year==2018, 1, 0))

load(file = "S:/Rand/Paper/m_estimate_eu_2018.RData")

eu_estimate_18 <- ggs(m, family = "eurostat")

eu_rm_18  <- grep("tau", eu_estimate_18$Parameter)
eu_rm2_18 <- grep("gamma", eu_estimate_18$Parameter)
eu_rm3_18 <- c(eu_rm_18, eu_rm2_18)

eu_estimate2_18 <- eu_estimate_18[-eu_rm3_18,]

# bilat Eurostat estimates 
eu_estimate2_18 <- eu_estimate2_18 %>% 
  add_node() %>% 
  add_corridor() %>% 
  left_join(years) %>% 
  group_by(Chain, Iteration, year, orig, dest) %>%
  summarise(total_chit = sum(exp(value))) %>%
  group_by(year, orig, dest) %>% 
  summarise(total = median(total_chit),
            sd = sd(total_chit), 
            upp50 = quantile(x = total_chit, probs = 0.75),
            lwr50 = quantile(x = total_chit, probs = 0.25), 
            upp80 = quantile(x = total_chit, probs = 0.9),
            lwr80 = quantile(x = total_chit, probs = 0.1)) 


eu_test <- left_join(eu_18, eu_estimate2_18) %>% 
  filter(flag == 1) %>% 
  mutate(test50 = ifelse((eu_all >= lwr50 & eu_all <=upp50), 1, 0), 
         test80 = ifelse((eu_all >= lwr80 & eu_all <=upp80), 1, 0))


p <- ggplot(eu_test, aes(x = eu_all, y = total)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr80, ymax = upp80)) +
  geom_abline(intercept = 0, slope = 1) + 
  scale_x_continuous(breaks = seq(0, 1.5e6, 1e5), labels = scientific) +
  scale_y_continuous(breaks = seq(0, 1.5e6, 1e5), labels = scientific) +
  labs(x = "Eurostat", y = "Estimate")

png("./Paper/sensitivity/compare_eu18_observed_80PI.png")
p
dev.off()








#### 5 Percent #####
# compare estimated Eurostat with observed Eurostat values
# dinput has observed Eurostat values
# flag == 1 removed values
dinput_eu5per <- read.csv("./Paper/data_july19/input_estimate_eurostat_5per.csv")

eu_5per <- dinput_eu5per %>% 
  filter(source == "eurostat") %>% 
  rename(eu_5per = flow) %>% 
  left_join(eu_all) %>% 
  mutate(flag = ifelse(!is.na(eu_all)&is.na(eu_5per), 1, 0))

load(file = "S:/Rand/Paper/m_estimate_eu_5per.RData")

eu_estimate_5per <- ggs(m, family = "eurostat")

eu_rm_5per  <- grep("tau", eu_estimate_5per$Parameter)
eu_rm2_5per <- grep("gamma", eu_estimate_5per$Parameter)
eu_rm3_5per <- c(eu_rm_5per, eu_rm2_5per)

eu_estimate2_5per <- eu_estimate_5per[-eu_rm3_5per,]

# bilat Eurostat estimates 
eu_estimate2_5per <- eu_estimate2_5per %>% 
  add_node() %>% 
  add_corridor() %>% 
  left_join(years) %>% 
  group_by(Chain, Iteration, year, orig, dest) %>%
  summarise(total_chit = sum(exp(value))) %>%
  group_by(year, orig, dest) %>% 
  summarise(total = median(total_chit),
            sd = sd(total_chit), 
            upp50 = quantile(x = total_chit, probs = 0.75),
            lwr50 = quantile(x = total_chit, probs = 0.25), 
            upp80 = quantile(x = total_chit, probs = 0.9),
            lwr80 = quantile(x = total_chit, probs = 0.1)) 


eu_test <- left_join(eu_5per, eu_estimate2_5per) %>% 
  filter(flag == 1) %>% 
  mutate(test50 = ifelse((eu_all >= lwr50 & eu_all <=upp50), 1, 0), 
         test80 = ifelse((eu_all >= lwr80 & eu_all <=upp80), 1, 0))


p <- ggplot(eu_test, aes(x = eu_all, y = total)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr80, ymax = upp80)) +
  geom_abline(intercept = 0, slope = 1) + 
  scale_x_continuous(breaks = seq(0, 1.5e6, 1e5), labels = scientific) +
  scale_y_continuous(breaks = seq(0, 1.5e6, 1e5), labels = scientific) +
  labs(x = "Eurostat", y = "Estimate")

png("./Paper/sensitivity/compare_eu5per_observed_80PI.png")
p
dev.off()



#### 20 Percent #####
# compare estimated Eurostat with observed Eurostat values
# dinput has observed Eurostat values
# flag == 1 removed values
dinput_eu20per <- read.csv("./Paper/data_july19/input_estimate_eurostat_20per.csv")

eu_20per <- dinput_eu20per %>% 
  filter(source == "eurostat") %>% 
  rename(eu_20per = flow) %>% 
  left_join(eu_all) %>% 
  mutate(flag = ifelse(!is.na(eu_all)&is.na(eu_20per), 1, 0))

load(file = "S:/Rand/Paper/m_estimate_eu_20per.RData")

eu_estimate_20per <- ggs(m, family = "eurostat")

eu_rm_20per  <- grep("tau", eu_estimate_20per$Parameter)
eu_rm2_20per <- grep("gamma", eu_estimate_20per$Parameter)
eu_rm3_20per <- c(eu_rm_20per, eu_rm2_20per)

eu_estimate2_20per <- eu_estimate_20per[-eu_rm3_20per,]

# bilat Eurostat estimates 
eu_estimate2_20per <- eu_estimate2_20per %>% 
  add_node() %>% 
  add_corridor() %>% 
  left_join(years) %>% 
  group_by(Chain, Iteration, year, orig, dest) %>%
  summarise(total_chit = sum(exp(value))) %>%
  group_by(year, orig, dest) %>% 
  summarise(total = median(total_chit),
            sd = sd(total_chit), 
            upp50 = quantile(x = total_chit, probs = 0.75),
            lwr50 = quantile(x = total_chit, probs = 0.25), 
            upp80 = quantile(x = total_chit, probs = 0.9),
            lwr80 = quantile(x = total_chit, probs = 0.1)) 


eu_test <- left_join(eu_20per, eu_estimate2_20per) %>% 
  filter(flag == 1) %>% 
  mutate(test50 = ifelse((eu_all >= lwr50 & eu_all <=upp50), 1, 0), 
         test80 = ifelse((eu_all >= lwr80 & eu_all <=upp80), 1, 0))


p <- ggplot(eu_test, aes(x = eu_all, y = total)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr50, ymax = upp50)) +
  geom_abline(intercept = 0, slope = 1) + 
  scale_x_continuous(breaks = seq(0, 1.5e6, 1e5), labels = scientific) +
  scale_y_continuous(breaks = seq(0, 1.5e6, 1e5), labels = scientific) +
  labs(x = "Eurostat", y = "Estimate")

png("./Paper/sensitivity/compare_eu20per_observed_50PI.png")
p
dev.off()

# Eurostat total number of residence born in another EU country

eu28_imm <- read.csv("./Paper/data_july19/Eurostat_total_born_EU28.csv") %>% 
  rename(name = dest, 
         Eurostat_total = total)

eu28_imm$dest = plyr::revalue(eu28_imm$dest, c("United Kingdom" = "UK"))
eu28_imm$dest[eu28_imm$dest == "United Kingdom"] = "UK"



imm3 <- imm2 %>% 
  select(year, name, total, model) %>% 
  spread(model, total)
  

imm_compare <- left_join(imm2, eu28_imm) %>% 
  mutate(Eurostat_total = as.numeric(gsub(",", "", Eurostat_total)), 
         model = fct_relevel(model, "Main"))
  
  
p <- ggplot(imm_compare, aes(x = log10(total), y = log10(Eurostat_total), color = (year))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  # scale_x_continuous(breaks = seq(0, 4e6, by = 500000)) +
  # scale_y_continuous(breaks = seq(0, 4e6, by = 500000), limits = c(0, 2.8e06)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Model estimates (log)",y = "Eurostat total immigration (log)") +
  labs(colour = "Year") +
  facet_wrap(~model, ncol = 3)

png("./Paper/sensitivity/compare_models_total_eurostat_log.png")
p
dev.off()
