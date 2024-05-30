rm(list = ls())
library(tidyverse)
library(ggmcmc)
library(geofacet)
library(scales)
library(readxl)
library(RColorBrewer)
library(patchwork)
library(eurostat)


############### NOTE ###################

# No need to read data separately for each model
# Start directly from Sensitivity plots

########################################

# Input data from the main model
# dinput <- read.csv("../data_july19/input_update_2019_march_june_fbmau_oldcovgr_prec.csv")

# ########################################
# ############# Main results #############
# ########################################
# 
# tot_y1   <- read.csv("S:/Rand/Paper/update_2019/tot_y_main_model.csv") %>% select(-X) %>% 
#   mutate(model = "Main")
# 
# imm_y1   <- read.csv("S:/Rand/Paper/update_2019/imm_y_main_model.csv") %>% select(-X) %>%
#   mutate(model = "Main")
# 
# emi_y1   <- read.csv("S:/Rand/Paper/update_2019/emi_y_main_model.csv") %>% select(-X) %>% 
#   mutate(model = "Main")
# 
# bilat_y1 <- read.csv("S:/Rand/Paper/update_2019/bilat_y_main_model.csv") %>% select(-X) %>%
#   mutate(model = "Main")
# 
# #########################################
# ### Main model - lower prec in FB cov ###
# #########################################
# 
# 
# tot_y2   <- read.csv("S:/Rand/Paper/update_2019/tot_y_main_model_low_fb_cov_prec.csv") %>% select(-X) %>% 
#   mutate(model = "Main low precision")
# 
# imm_y2   <- read.csv("S:/Rand/Paper/update_2019/imm_y_main_model_low_fb_cov_prec.csv") %>% select(-X) %>% 
#   mutate(model = "Main low precision")
# 
# emi_y2   <- read.csv("S:/Rand/Paper/update_2019/emi_y_main_model_low_fb_cov_prec.csv") %>% select(-X) %>% 
#   mutate(model = "Main low precision")
# 
# bilat_y2 <- read.csv("S:/Rand/Paper/update_2019/bilat_y_main_model_low_fb_cov_prec.csv") %>% select(-X) %>% 
#   mutate(model = "Main low precision")
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



########## Sensitivity plots ############

col1 <- c("grey", "lightblue1", "lightblue2", "lightblue3", "lightblue4", 
          "steelblue1", "steelblue2", "steelblue3", "steelblue4")

col2 <- c("grey", "royalblue1", "royalblue2", "royalblue3", "royalblue4", 
          "steelblue1", "steelblue2", "steelblue3", "steelblue4")

# tot2 <- bind_rows(tot_y1, tot_y2, tot_y3, tot_y4, tot_y5, tot_y6, tot_y7, tot_y8, tot_y9)
# write.csv(tot2, "./paper_allmodels_total.csv", row.names = F)
# imm2 <- bind_rows(imm_y1, imm_y2, imm_y3, imm_y4, imm_y5, imm_y6, imm_y7, imm_y8, imm_y9)
# write.csv(imm2, "./paper_allmodels_imm.csv", row.names = F)
# emi2 <- bind_rows(emi_y1, emi_y2, emi_y3, emi_y4, emi_y5, emi_y6, emi_y7, emi_y8, emi_y9)
# write.csv(emi2, "./paper_allmodels_emi.csv", row.names = F)
# bilat2 <- bind_rows(bilat_y1, bilat_y2, bilat_y3, bilat_y4, bilat_y5, bilat_y6, bilat_y7, bilat_y8, bilat_y9)
# write.csv(bilat2, "./paper_allmodels_bilat.csv", row.names = F)

tot2 <- read.csv("./data/paper_allmodels_total.csv")
bilat2 <- read.csv("./data//paper_allmodels_bilat.csv")
imm2 <- read.csv("./data/paper_allmodels_imm.csv")


myblue <- c("grey", "paleturquoise","turquoise", brewer.pal(9, "Blues")[4:9])
# [1] "grey"          "paleturquoise" "turquoise"     "#9ECAE1"       "#6BAED6"       "#4292C6"       "#2171B5"       "#08519C"       "#08306B"   

myblue2 <- c("paleturquoise", "turquoise", "#9ECAE1", "#6BAED6", "#4292C6", "springgreen", "#2171B5", "#08519C", "#08306B")
myblue3 <- c("paleturquoise", "turquoise", "#9ECAE1", "#6BAED6", "#4292C6", "springgreen", "#2171B5", "blue1", "#08306B")

mygrey <- brewer.pal(9, "Greys")
  
png("./paper figures/sens1.png",  width = 8,  height = 6, units = "in", res = 300)
png("./paper figures/sens2.png",  width = 10, height = 8, units = "in", res = 300)
png("./paper figures/sens3.png",  width = 10, height = 8, units = "in", res = 300)
png("./paper figures/sens4.png",  width = 10, height = 8, units = "in", res = 300)

# change model names to S1, s2

tot2_23 <- tot2 %>% 
  mutate(model2 = model,
         model2 = recode(model, Main = "S1:Main", 
                         "Main low precision" = "S2:Main low FB precision", 
                         "Eurostat 5 percent" = "S3:Eurostat 5 percent", 
                         "Eurostat 10 percent" = "S4:Eurostat 10 percent", 
                         "Eurostat 20 percent" = "S5:Eurostat 20 percent",
                         "Eurostat estimate 2016" = "S6:Eurostat excluded 2016", 
                         "Eurostat estimate 2018" = "S7:Eurostat excluded 2018",
                         "Only Eurostat"  = "S8:Only Eurostat", 
                         "Without Facebook" = "S9:Without Facebook"))
  

 p1 <- ggplot(tot2_23, mapping = aes(x = year, y = total/1e6, fill = model2)) +
  geom_ribbon(mapping = aes(ymin = lwr50/1e6, ymax = upp50/1e6), alpha = 0.55) +
   geom_line(data = tot2_23 %>% filter(model2 == "S1:Main"), aes(color = model2), size = 1.5) +
    # scale_y_log10() +
  labs(x= "Year", y= "Stocks of EU migrants (millions), age 15-64", colour = "", fill = "") +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  theme_bw()+
  scale_fill_manual(values = myblue3) +
  scale_color_manual(values = myblue3, guide = F) +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90), 
         text = element_text(size = 24)) +
  theme(legend.position = "none")
 dev.off()
 
 d1 <- bilat2 %>% 
   filter(orig == "Poland", dest == "Netherlands" | dest == "United Kingdom") %>% 
   mutate(dest = plyr::revalue(dest, c("United Kingdom" = "UK")))

 
 p2 <- ggplot(d1_23, mapping = aes(x = year, y = total/1e6, fill = model2)) +
      geom_ribbon(mapping = aes(ymin = lwr50/1e6, ymax = upp50/1e6), alpha = 0.55) +
    geom_line(data = d1_23 %>% filter(model2 == "S1:Main"), aes(color = model2), size = 1.5) +
    # scale_y_log10() +
   labs(x= "Year", y= "Stocks of migrants (age 15-64) from Poland (millions)", colour = "", fill = "") +
   scale_x_continuous(breaks = seq(2011, 2019, 1)) +
   theme_bw()+
   scale_fill_manual(values = myblue3) +
   scale_color_manual(values = myblue3, guide = F) +
   theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90), 
         text = element_text(size = 24)) +
   facet_grid(~dest)

 # png("./paper figures/sens_both1.png",  width = 10, height = 8, units = "in", res = 300)
 # png("./paper figures/sens_both2.png",  width = 15, height = 10, units = "in", res = 300)
 # png("./paper figures/sens_both3.png",  width = 20, height = 10, units = "in", res = 300)
 # png("./paper figures/sens_both4.png",  width = 20, height = 10, units = "in", res = 300)
 # png("./paper figures/sens_both5.png",  width = 20, height = 10, units = "in", res = 300)
 
 png("./figures/sens_both7_131223.png",  width = 20, height = 12, units = "in", res = 300)
  p1 + p2 + plot_layout(guides="collect") & theme(legend.position = "bottom")
 dev.off()
 
 png("./paper figures/sens_both5.png" , width = 20, height = 10, units = "in", res = 300)
 ggsave(filename = "./paper figures/sens_both6.pdf",plot = p2,device = "pdf",
        width = 12,height = 7.5)
 p1 + p2 + plot_layout(guides="collect") & theme(legend.position = "bottom")
 dev.off()
 
 # combine input and models
 
 shapedat <- c(17, 15, 18, 7, 8)
 col3 <- c( "peru", "red","green3", "royalblue", "magenta2")
 
 
 d2 <- dinput %>% 
   filter(orig == "Poland", dest == "Netherlands" | dest == "United Kingdom") %>% 
   mutate(dest = plyr::revalue(dest, c("United Kingdom" = "UK"))) %>% 
   rename(total = flow) %>% 
   mutate(source_name = case_when(source == "census" ~ "Census (w. Missing)", 
                                  source == "eurostat" ~ "Eurostat (w. Missing)",
                                  source == "fbdau" ~ "Facebook DAU (w. Missing)", 
                                  source == "fbmau" ~ "Facebook MAU (w. Missing)", 
                                  source == "LFS" ~ "LFS (w. Missing)"))

 ggplot(d1 %>% filter(dest == "UK"), aes(x = year, y = total/1e6)) +
   geom_line() +
   geom_ribbon(d1 %>% filter(dest == "UK"), mapping = aes(ymin = lwr50/1e6, ymax = upp50/1e6), alpha = 0.3) +
   geom_point(d2 %>% filter(dest == "UK"), mapping = aes( x= year, y = total/1e6, color = source_name, shape = source_name)) +
   scale_color_manual(values = col3) +
   scale_shape_manual(values = shapedat) +
   labs(x= "Year", y= "Stocks of migrants from Poland (millions)", colour = "", fill = "") +
   scale_x_continuous(breaks = seq(2011, 2019, 1)) +
   theme_bw()+
   scale_fill_manual(values = col3) +
   scale_color_manual(values = col3) +
   theme(legend.title = element_blank(), 
         axis.text.x = element_text(angle = 90), 
         text = element_text(size = 28))+
   facet_wrap(~model)
 
 # edit d1 to change model names to S1, S2...
 
 d1_23 <- d1 %>% 
   mutate(model2 = model,
          model2 = recode(model, Main = "S1:Main", 
                          "Main low precision" = "S2:Main low FB precision", 
                          "Eurostat 5 percent" = "S3:Eurostat 5 percent", 
                          "Eurostat 10 percent" = "S4:Eurostat 10 percent", 
                          "Eurostat 20 percent" = "S5:Eurostat 20 percent",
                          "Eurostat estimate 2016" = "S6:Eurostat excluded 2016", 
                          "Eurostat estimate 2018" = "S7:Eurostat excluded 2018",
                          "Only Eurostat"  = "S8:Only Eurostat", 
                          "Without Facebook" = "S9:Without Facebook"))
 # change d1 to d1_23
 p1 <- ggplot(d1_23 %>% filter(dest == "UK"), aes(x = year, y = total/1e6)) +
   geom_line() +
   geom_ribbon(d1_23 %>% filter(dest == "UK"), mapping = aes(ymin = lwr50/1e6, ymax = upp50/1e6), alpha = 0.3) +
   geom_point(d2 %>% filter(dest == "UK"), mapping = aes( x= year, y = total/1e6, color = source_name, shape = source_name)) +
   scale_color_manual("changed_legend", values = col3, drop=FALSE) +
   scale_shape_manual("changed_legend", values= shapedat, drop= FALSE) +
   labs(x= "Year", y= "Stocks of migrants (age 15-64) from Poland (millions)", colour = "", fill = "") +
   scale_x_continuous(breaks = seq(2011, 2019, 1)) +
   theme_bw()+
   theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90), 
         text = element_text(size = 28), strip.text = element_text(size = 12))+
   facet_wrap(~model2)
 
p2 <- ggplot(d1_23 %>% filter(dest == "Netherlands"), aes(x = year, y = total/1e6)) +
  geom_line() +
  geom_ribbon(d1_23 %>% filter(dest == "Netherlands"), mapping = aes(ymin = lwr50/1e6, ymax = upp50/1e6), alpha = 0.3) +
  geom_point(d2 %>% filter(dest == "Netherlands"), mapping = aes( x= year, y = total/1e6, color = source_name, shape = source_name)) +
  scale_color_manual("changed_legend", values = col3, drop=FALSE) +
  scale_shape_manual("changed_legend", values= shapedat, drop= FALSE) +
  labs(x= "Year", y= "Stocks of migrants (age 15-64) from Poland (millions)", colour = "", fill = "") +
  scale_x_continuous(breaks = seq(2011, 2019, 1)) +
  theme_bw()+
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90), 
        text = element_text(size = 28), strip.text = element_text(size = 12))+
  facet_wrap(~model2)


png("./figures/modelcomp_rev_23_131223.png",  width = 20, height = 12, units = "in", res = 300)
p2 + p1 + plot_layout(guides="collect") & theme(legend.position = "bottom")
dev.off()


# main model imm vs eurostat total imm
eu28_imm <- read.csv("../data_july19/Eurostat_total_born_EU28.csv") %>% 
  rename(name = dest, 
         Eurostat_total = total) %>% 
  mutate(Eurostat_total = as.numeric(gsub(",", "", Eurostat_total)))
  
  
d3 <- get_eurostat("migr_pop3ctb") 

eu_cnty <- eu_countries

d4 <- d3 %>% filter(sex == "T", age == "Y15-64", c_birth == "EU28_FOR", geo %in% eu_countries$code) %>% 
  left_join(eu_countries %>% rename(geo = code) %>% select(geo, name)) %>% 
  mutate(year = as.numeric(substr(time, 1,4))) %>% 
  select(name, year, values)



d5 <- left_join(imm_y1, d4 %>% rename(Eurostat = values)) %>% 
  # filter(Eurostat > 20000) %>% 
  mutate(diff = (Eurostat-total)/Eurostat,
         diffl10 = (log10(Eurostat)-log10(total)),
         MAPE1 = mean(diff, na.rm = T), 
         diff2  = (Eurostat-total)^2, 
         non_na_count = sum(!is.na(Eurostat)), 
         MSE = sum(diff2, na.rm = T) /non_na_count, 
         MAPE = 100 * sum(diff, na.rm = T)/non_na_count, 
         MAPE_abs = 100 * sum(abs(diff), na.rm = T)/non_na_count,
         MAE_log10 = 100 * sum(abs(diffl10), na.rm = T)/non_na_count,
         name=ifelse(name=="United Kingdom","UK",name)) %>% 
  group_by(name, year) %>% 
  mutate(MAPE_cc = mean(abs(diff), na.rm = T))



d7 <- d5 %>% select(name, year, MAPE_cc, Eurostat, total) %>% 
  ungroup() %>% 
  filter(!is.nan(MAPE_cc)) %>%  
  distinct() %>% 
  # group_by(name) %>% 
  summarise(MAPE = mean(MAPE_cc))


png("./paper figures/eurostat_compare_imm2.png",  width = 10, height = 10, units = "in", res = 300)

p3=ggplot(d5) +
  geom_pointrange(aes(x = total/1e6, xmin = lwr80/1e6, xmax = upp80/1e6, y = Eurostat/1e6), alpha = 0.3) +
 # geom_point(aes(x = total/1e6, y = Eurostat/1e6))+
  # geom_label(data=d5 %>% filter(Eurostat > 3.2e6), aes(x = total/1e6, y = Eurostat/1e6,label = name)) +
  geom_label(data=d5 %>% filter(Eurostat > 1.5e6) %>% filter (year==2019 | name%in%c("Germany","UK")), aes(x = total/1e6, y = Eurostat/1e6,label = name)) +
  # geom_label(data=d5 %>% filter(abs(diff) > 0.2), aes(x = total/1e6, y = Eurostat/1e6,label = name)) +
  
  xlab("Estimate") +
  ylab("Eurostat") +
  geom_abline() +
  theme_bw()+
  # xlim(0, 4)+
  theme(text = element_text(size = 28),
        ) 
dev.off()

ggsave(filename = "./paper figures/eurostat_compare_imm2.pdf",plot = p3,device = "pdf",
       width = 12,height = 7.5)

#log scale because Guy
p31=ggplot(d5) +
  geom_pointrange(aes(x = (total), xmin = (lwr80), xmax = (upp80), y = (Eurostat)), alpha = 0.3) +
  geom_text(data = d5 %>% filter(((diffl10< -0.24 || diffl10 >0.15) & year==2019) | (name%in%c("UK","Italy")& year==2019)), aes(x = (upp80), y = (Eurostat), label=name), nudge_x = 0.1) +
  geom_text(data = d5 %>% filter((name%in%c("Spain")& year==2019)), aes(x = (upp80), y = (Eurostat), label=name), nudge_x = 0.1,nudge_y = 0.06) +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  # geom_point(aes(x = total/1e6, y = Eurostat/1e6))+
  # geom_label(data=d5 %>% filter(Eurostat > 3.2e6), aes(x = total/1e6, y = Eurostat/1e6,label = name)) +
  # geom_label(data=d5 %>% filter(Eurostat > 1.5e6) %>% filter (year==2019 | name%in%c("Germany","UK")), aes(x = total/1e6, y = Eurostat/1e6,label = name)) +
  # geom_label(data=d5 %>% filter(abs(diff) > 0.2), aes(x = total/1e6, y = Eurostat/1e6,label = name)) +
  # geom_label(data=d5 %>% filter(MAPE_log10 > 1.5e6) %>% filter (year==2019 | name%in%c("Germany","UK")), aes(x = total/1e6, y = Eurostat/1e6,label = name)) +
  
  xlab("Persons (model-based estimate)") +
  ylab("Persons (Eurostat data)") +
  geom_abline(colour="grey4") +
  theme_bw()+
  # xlim(0, 4)+
  theme(text = element_text(size = 20),
  ) 
# dev.off()

# ggsave(filename = "./paper figures/eurostat_compare_imm21.pdf",plot = p31,device = "pdf",
#        width = 9,height = 7.5)

eurostat_bilat <- d3 %>% 
  filter(sex == "T", age == "Y15-64", c_birth  %in% eu_countries$code, geo %in% eu_countries$code) %>% 
  left_join(eu_countries %>% rename(geo = code) %>% select(geo, name)) %>% 
  rename(dest = name) %>% 
  select(-geo) %>% 
  left_join(eu_countries %>% rename(c_birth = code) %>% select(c_birth, name)) %>% 
  rename(orig = name) %>% 
  mutate(orig = (as.character(orig)), dest = (as.character(dest))) %>% 
  mutate(values = ifelse(orig == dest, NA, values), 
         year = as.numeric(substr(time, 1,4))) %>% 
  select(orig, dest, year, values) %>% 
  rename(Eurostat = values)

d6 <- left_join(bilat_y1, eurostat_bilat) %>% 
  mutate(diff = round(Eurostat-total)) 

d6_23 <- d6 %>% 
  filter(orig == "Poland", 
         dest == "Netherlands") %>% 
  mutate(mape = 100 * diff/Eurostat) %>% 
  summarise(mape = mean(abs(mape)))
  

d6_23_cc <- d6 %>% 
  filter(orig == "Poland", 
         dest == "Netherlands") %>% 
  mutate(mape = 100 * diff/Eurostat) %>% 
  group_by(dest)
  summarise(mape = mean(abs(mape)))

ggplot(d6) +
  geom_ribbon(aes(x = total, xmin = lwr80, xmax = upp80, y = Eurostat), alpha = 0.3) +
  geom_point(aes(x = total, y = Eurostat))+
  xlab("Estimate") +
  geom_abline() +
  ylim(0, 1500000) +
  theme_bw() 
