# this file reads in and prepares data to be used in 
# main model S1 
# naive model

rm(list = ls())
library(tidyverse)
library(stringr)
library(forcats)
library(readxl)
library(eurostat)


 
# 15 - 64 year olds #

 
# Eurostat ####
# updated in 1 July 2019
# includes 2018 data 

 # added this line because eu_countries do not include UK anymore
countries_name <- c(eu_countries$name, "United Kingdom")
 
# all corridors all years 
corrname4 <- expand.grid(dest = sort(countries_name), orig = sort(countries_name), year = seq(2011,2019, by = 1)) %>% 
  select(orig, dest, year) %>% 
  filter(orig != dest) %>% 
  mutate(corridor_name = paste0(orig, "->", dest), 
         corridor_name = as.factor(corridor_name))


d1 <- read.csv("data/Eurostat_2010_2018_15_64.csv") %>% 
  mutate(eurostat  = ifelse(eurostat == 0, NA, eurostat)) %>% 
  filter(year > 2010)

d1.4 <- left_join(corrname4, d1) %>%
  mutate(orig = as.factor(orig),
         dest = as.factor(dest),
         corridor_name = as.factor(corridor_name),
           corridor = as.numeric(corridor_name))

# Eurostat undercount groups from IMEM
d1.1 <-  read.csv("data/IMEM_undercount.csv", sep= ";") %>% 
  rename(cov_eurostat_gr = gamma_group) %>% 
  mutate(cov_eurostat_gr = ifelse(is.na(cov_eurostat_gr), 3, cov_eurostat_gr))



d1.2 <- left_join(d1.4, d1.1)

# Eurostat coverage group by year and undercount group
# Add 2018 

d1.3 <- d1.2 %>% 
  mutate(cov_eurostat_gr_yr = case_when((year == 2011 & cov_eurostat_gr == 1) ~ 1,
                                        (year == 2011 & cov_eurostat_gr == 2) ~ 2,
                                        (year == 2012 & cov_eurostat_gr == 1) ~ 3,
                                        (year == 2012 & cov_eurostat_gr == 2) ~ 4,
                                        (year == 2013 & cov_eurostat_gr == 1) ~ 5,
                                        (year == 2013 & cov_eurostat_gr == 2) ~ 6,
                                        (year == 2014 & cov_eurostat_gr == 1) ~ 7,
                                        (year == 2014 & cov_eurostat_gr == 2) ~ 8,
                                        (year == 2015 & cov_eurostat_gr == 1) ~ 9,
                                        (year == 2015 & cov_eurostat_gr == 2) ~ 11,
                                        (year == 2016 & cov_eurostat_gr == 1) ~ 11,
                                        (year == 2016 & cov_eurostat_gr == 2) ~ 12,
                                        (year == 2017 & cov_eurostat_gr == 1) ~ 13,
                                        (year == 2017 & cov_eurostat_gr == 2) ~ 14, 
                                        (year == 2018 & cov_eurostat_gr == 1) ~ 15,
                                        (year == 2018 & cov_eurostat_gr == 2) ~ 16,
                                        (year == 2019 & cov_eurostat_gr == 1) ~ 15,
                                        (year == 2019 & cov_eurostat_gr == 2) ~ 16,
                                        cov_eurostat_gr == 3 ~ 17))




# Eurostat census 2011 ####
 
d2 <- read.csv("data/Eurostat_census_2011.csv") %>% 
  mutate(census = ifelse(census == 0, NA, census))
 

# Facebook ####

# Facebook June 2019 fbmau = mau_est if mau_est==NA use mau

d6 <- read.csv("data/Fb_all_2016_2019_users_pop_coverage.csv") %>% 
  filter(date != "2018-03-01", 
         date != "2018-04-01",
         date != "2018-05-01",
         date != "2018-06-01") %>% 
  select(corridor_name, corridor, orig, dest, mau, year, month, date, mau_est, dau, mau_cov, dau_cov) 


# fbmau is average of mau_est in march, april, may, june 
# if is.nan(fbmau) then is average of mau

d6.7 <- d6 %>% 
  filter(year == 2019) %>% 
  select(-starts_with("dau")) %>% 
  mutate(mau_est2 = ifelse(mau_est==1000, NA, mau_est)) %>% 
  group_by(corridor_name, corridor, orig, dest, year) %>% 
  mutate(mau_est2 = round(mean(mau_est2, na.rm =T)), 
         fbmau2019 = ifelse(is.nan(mau_est2), round(mean(mau, na.rm = T),0), mau_est2), 
         mau_cov2019 = mean(mau_cov, na.rm = T)) %>% 
  select(-month, -date,  -mau, -mau_est, -mau_est2, -mau_cov) %>% 
  distinct() %>% 
  ungroup()

# fbdau average of dau in march, april, may and june

d6.8 <- d6 %>% 
  filter(year == 2019) %>% 
  select(-starts_with("mau")) %>% 
  group_by(corridor_name, corridor, orig, dest, year) %>% 
  summarise(fbdau2019 = round(mean(dau, na.rm =T)), 
            dau_cov2019 = mean(dau_cov, na.rm = T)) %>% 
  ungroup()

d6.9 <- d6 %>% 
  filter(date!= "2019-03-01", 
         date!= "2019-04-01", 
         date!= "2019-05-01") %>% 
  select(corridor_name, corridor, orig, dest, year, mau, mau_est, dau, mau_cov, dau_cov) %>% 
  left_join(d6.7 %>% select(orig, dest, year, fbmau2019, mau_cov2019)) %>% 
  left_join(d6.8 %>% select(orig, dest, year, fbdau2019, dau_cov2019)) %>% 
  mutate(fbmau = ifelse(year <= 2017, mau, NA), 
         fbmau = ifelse(year == 2018, mau_est, fbmau), 
         fbmau = ifelse(year == 2018 & is.na(fbmau), mau, fbmau), 
         fbmau = ifelse(year == 2019, fbmau2019, fbmau), 
         fbmau = ifelse(is.nan(fbmau), NA, fbmau),
         fbdau = fbdau2019, 
         fbdau = ifelse(is.na(fbdau), dau, fbdau), 
         fbdau = ifelse(is.nan(fbdau), NA, fbdau), 
         mau_cov = ifelse(year == 2019, mau_cov2019, mau_cov), 
         dau_cov = ifelse(year == 2019, dau_cov2019, dau_cov)) %>% 
  select(corridor_name, corridor, orig, dest, year, fbmau, fbdau, mau_cov, dau_cov)
  

# group and year specific facebook coverage 

d6.1 <- d6.9 %>% 
  select(-corridor_name, -corridor, -orig, -fbmau, -fbdau, -dau_cov) %>% 
  distinct() 

# MAU 2017 coverage use average of 2016 and 2018
for(i in 1:length(unique(d6.1$dest))){
  d6.1 %>% filter(dest == d6.1$dest[i])
  d6.1$test[d6.1$year==2017] = ((d6.1$mau_cov[d6.1$year==2016] + d6.1$mau_cov[d6.1$year==2018])/2)
}

d6.1 <- d6.1 %>% 
  mutate(mau_cov = ifelse(year == 2017, test, mau_cov)) %>% 
  select(-test)
 

# same coverage groups as the EC report (Gendronneau et al, 2019)
d6.2 <- d6.1 %>% 
  mutate(year = paste0("Y_",year)) %>% 
  spread(year, mau_cov) %>% 
  mutate(cov_mau_gr_2016 = dplyr::case_when((Y_2016 < 0.40) ~ 1,
                                            (Y_2016 >= 0.40 & Y_2016 < 0.60) ~ 2,
                                            (Y_2016 >= 0.60 & Y_2016 < 0.80) ~ 3, #Malta
                                            (Y_2016 >= 0.80) ~ 4), #Cyprus
         cov_mau_gr_2017 = dplyr::case_when((Y_2017 < 0.50) ~ 1,
                                            (Y_2017 >= 0.50 & Y_2017 < 0.70) ~ 2,
                                            (Y_2017 >= 0.70 & Y_2017 < 0.90) ~ 3, #Malta
                                            (Y_2017 >= 0.90) ~ 4), #Cyprus
         cov_mau_gr_2018 = dplyr::case_when((Y_2018 < 0.60) ~ 1,
                                            (Y_2018 >= 0.60 & Y_2018 < 0.80) ~ 2,
                                            (Y_2018 >= 0.80 & Y_2018 < 1) ~ 3,
                                            (Y_2018 >= 1 & Y_2018 < 1.4) ~ 4,
                                            (Y_2018 >= 1.4) ~ 5), #Cyprus
         cov_mau_gr_2019 = dplyr::case_when((Y_2019 < 0.60) ~ 1, # Germany
                                   (Y_2019 >= 0.60 & Y_2019 < 0.80) ~ 2,
                                   (Y_2019 >= 0.80 & Y_2019 < 0.99) ~ 3,
                                   (Y_2019 >= 0.99 & Y_2019 < 1.4) ~ 4, #Malta
                                   (Y_2019 >= 1.4) ~ 5)) %>%  #Cyprus
  select(-starts_with("Y_")) %>% 
  gather(year, cov_mau_gr, cov_mau_gr_2016:cov_mau_gr_2019) %>% 
  mutate(year = as.numeric(str_extract(year, "[[:digit:]]+")))




# DAU coverage group

d6.3 <- d6.9 %>% 
  select(-corridor_name, -corridor, -orig, -fbmau,  -fbdau, -mau_cov) %>% 
  distinct() %>% 
  filter(year >= 2018) %>% 
  mutate(year = paste0("Y_",year)) %>% 
  spread(year, dau_cov) %>% 
  mutate(cov_dau_gr_2018 = dplyr::case_when((Y_2018 < 0.40) ~ 1, # Germany
                                            (Y_2018 >= 0.40 & Y_2018 < 0.60) ~ 2,
                                            (Y_2018 >= 0.60 & Y_2018 < 0.80) ~ 3,
                                            (Y_2018 >= 0.80 & Y_2018 < 1.0) ~ 4, #Malta
                                            (Y_2018 >= 1.0) ~ 5), #Cyprus
         cov_dau_gr_2019 = dplyr::case_when((Y_2019 < 0.40) ~ 1, # Germany
                                             (Y_2019 >= 0.40 & Y_2019 < 0.60) ~ 2,
                                             (Y_2019 >= 0.60 & Y_2019 < 0.80) ~ 3,
                                             (Y_2019 >= 0.80 & Y_2019 < 1.0) ~ 4, #Malta
                                             (Y_2019 >= 1.0) ~ 5)) %>% #Cyprus
  select(-starts_with("Y_")) %>% 
  gather(year, cov_dau_gr, cov_dau_gr_2018:cov_dau_gr_2019) %>% 
  mutate(year = as.numeric(str_extract(year, "[[:digit:]]+")))



d6.4 <- d6.9 %>% 
  filter(year >= 2018) %>% 
  select(orig, dest, year, fbmau) %>% 
  spread(year, fbmau) %>% 
  mutate(mau_est_ratio = `2019`/`2018`, 
         mau_est_ratio = ifelse(is.na(mau_est_ratio), 1, mau_est_ratio)) %>% 
  select(orig, dest, mau_est_ratio) %>% 
  mutate(year = 2019)

d6.5 <- d6.9 %>% 
  filter(year >= 2018) %>% 
  select(orig, dest, year, fbdau) %>% 
  spread(year, fbdau) %>% 
  mutate(dau_ratio = `2019`/`2018`, 
         dau_ratio = ifelse(is.na(dau_ratio), 1, dau_ratio)) %>% 
  select(orig, dest, dau_ratio) %>% 
  mutate(year = 2019)


d6.6 <- left_join(corrname4, d6.9) %>%
  left_join(d6.2) %>% 
  left_join(d6.3) %>% 
  left_join(d6.4) %>% 
  left_join(d6.5) %>% 
  mutate(corridor = as.numeric(as.factor(corridor_name)))


# LFS ####
# LFS and year and destination specific coverage

d4 <- read.csv("./data/LFS_corrected_full_corridors.csv") %>% 
  mutate(dest_year = paste0(dest, "_", year), 
         cov_lfs_dest_year = as.numeric(as.factor(dest_year)))

# Combine all sources ####


d5 <- left_join(d1.3, d6.6) %>% 
  # left_join(d1.3) %>% 
  left_join(d4) %>%
  left_join(d2) %>% 
  mutate(fbmau_censored = ifelse((fbmau == 1000 & year >= 2018), NA, fbmau),
         fbmau_censored = ifelse((fbmau == 0 & year >= 2018), NA, fbmau),
         fbmau_censored_ind = ifelse((fbmau > 1000 & year >= 2018), 1, 0)) %>% 
  filter(year > 2010)



### S3
# Eurostat removal of 5% of data per year ####

# this can be rewritten as a function
d5.S3 <- d5 %>% 
  filter(!is.na(eurostat)) %>% 
  group_by(year) %>% 
  sample_frac(0.05) %>% 
  mutate(eurostat_na=1) %>% 
  select(orig, dest, year, eurostat_na)

# joininig with main data
d52.S3 <- left_join(d5, d5.S3) %>% 
  mutate(eurostat2 = ifelse(is.na(eurostat_na), eurostat, NA)) %>% 
  select(-eurostat_na, -eurostat) %>% 
  rename(eurostat = eurostat2)

d5 <- d52.S3
rm(d52.S3)

 
# the code below needs to be used for each model seperately to 
# account for the changes in input data

# indices to run the model for required rows
ind_eurostat <- which(!is.na(d5$eurostat))
ind_fbmau2016 <- which(!is.na(d5$fbmau)& d5$year == 2016)
ind_fbmau2017 <- which(!is.na(d5$fbmau)& d5$year == 2017)
ind_fbmau2018 <- which(!is.na(d5$fbmau)& d5$year == 2018)
ind_fbmau2019 <- which(!is.na(d5$fbmau)& d5$year == 2019)



ind_fbdau2018 <- which(!is.na(d5$fbdau)& d5$year == 2018)
ind_fbdau2019 <- which(!is.na(d5$fbdau)& d5$year == 2019)

ind_lfs <- which((!is.na(d5$LFS)))
ind_census <- which(!is.na(d5$census))

incov_fbmau <- d6.4$cov_mau_gr
incov_fbdau <- d6.4$cov_dau_gr

scale_fbmau <- d5$mau_est_ratio
scale_fbdau <- d5$dau_ratio 

dinput <- d5 %>% 
  select(orig, dest, year, eurostat, census, fbmau, fbdau, LFS, corridor) %>% 
  gather(eurostat, census, fbdau, fbmau, LFS, key = "source", value = "stock")


# data list for the JAGS model
d <- list(N = nrow(d5),
          n = max(d5$corridor),
          corridor = d5$corridor,
          year = d5$year, 
          census = log(d5$census),
          eurostat = log(d5$eurostat),
          lfs = log(d5$LFS),
          lfs2 = (d5$LFS),
          fbmau = log(d5$fbmau),
          fbmau_censored = log(d5$fbmau_censored),
          fbmau_censored_ind = d5$fbmau_censored_ind,
          fbdau = log(d5$fbdau),
          incensus = ind_census,
          ineurostat = ind_eurostat, 
          inlfs = ind_lfs, 
          infbmau2016 = ind_fbmau2016,
          infbmau2017 = ind_fbmau2017,
          infbmau2018 = ind_fbmau2018,
          infbmau2019 = ind_fbmau2019,
          infbdau2018 = ind_fbdau2018,
          infbdau2019 = ind_fbdau2019,
          cov_eurostat_gr_yr = d5$cov_eurostat_gr_yr,
          cov_lfs_dest_year = d5$cov_lfs_dest_year,
          cov_mau_gr = d5$cov_mau_gr,
          cov_dau_gr = d5$cov_dau_gr, 
          scale_fbmau = scale_fbmau, 
          scale_fbdau = scale_fbdau)

# you can save object d and other dXXX objects

rm(corrname4, d2, d4, d5, countries_name)
rm(list = ls(pattern = "d6.*"))
rm(list = ls(pattern = "d1.*"))