# Compare estimated Eurostat

library(tidyverse)
library(forcats)
library(stringr)
library(ggmcmc)
library(scales)
source("./code/fn1.R")

load("./results/m_estimate_eu16_March_June_mau_est_old_fb_cov_5gr_prec.Rdata")
m16 <- m

load("./results/m_estimate_eu_17_with_UK.Rdata")
m17 <- m

load("./results/m_estimate_eu_2018.Rdata")
m18 <- m


dinput <- read.csv("./data/dinput.csv")

# data frames for building summary states
corridors <- dinput %>%
  select(orig, dest, corridor) %>%
  distinct()

years <- dinput %>%
  select(year) %>%
  mutate(node_id = 1:n())

##### 2016 #####

eurostat16 <- ggs(m16, family = "eurostat") %>%  
  mutate(Para = gsub("\\[.*", "", Parameter)) %>% 
  filter(Para == "eurostat")

# add corridors to eurostat

eu16 <- eurostat16 %>% 
  add_node() %>% 
  add_corridor() %>% 
  left_join(years)


eurostat_2016_obs <-dinput %>% 
  filter(source == "eurostat", 
         year == "2016") %>% 
  mutate(stock = log(flow))

summary_eurostat_2016 <- eu16 %>% 
  filter(year == "2016") %>% 
  group_by(Chain, orig, dest, year) %>% 
  summarise(mean = mean(value), 
            median = median(value), 
            min = min(value), 
            max = max(value), 
            upp80 = quantile(x = value, probs = 0.9), 
            low80 = quantile(x = value, probs = 0.1), 
            upp50 = quantile(x = value, probs = 0.75),
            lwr50 = quantile(x = value, probs = 0.25)) %>% 
  left_join(eurostat_2016_obs)


ggplot(summary_eurostat_2016) +
  geom_pointrange(aes(x = exp(median), xmax = exp(upp80), xmin = exp(low80), y = flow), alpha = 0.1, size = 0.5) +
  geom_abline() +
  # xlim(c(0, 15)) +
  # ylim(c(0, 15)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  facet_grid(orig ~ dest)

#### 2017####


eurostat17 <- ggs(m17, family = "eurostat") %>%  
  mutate(Para = gsub("\\[.*", "", Parameter)) %>% 
  filter(Para == "eurostat")

# add corridors to eurostat

eu17 <- eurostat17 %>% 
  add_node() %>% 
  add_corridor() %>% 
  left_join(years)


eurostat_2017_obs <-dinput %>% 
  filter(source == "eurostat", 
         year == "2017") %>% 
  mutate(stock = log(flow))

summary_eurostat_2017 <- eu17 %>% 
  filter(year == "2017") %>% 
  group_by(Chain, orig, dest, year) %>% 
  summarise(mean = mean(value), 
            median = median(value), 
            min = min(value), 
            max = max(value), 
            upp80 = quantile(x = value, probs = 0.9), 
            low80 = quantile(x = value, probs = 0.1), 
            upp50 = quantile(x = value, probs = 0.75),
            lwr50 = quantile(x = value, probs = 0.25)) %>% 
  left_join(eurostat_2017_obs)


ggplot(summary_eurostat_2017) +
  geom_pointrange(aes(x = exp(median), xmax = exp(upp80), xmin = exp(low80), y = flow), alpha = 0.1, size = 0.5) +
  geom_abline() +
  # xlim(c(0, 15)) +
  # ylim(c(0, 15)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  facet_grid(orig ~ dest)

#### 2018 ####


eurostat18 <- ggs(m18, family = "eurostat") %>%  
  mutate(Para = gsub("\\[.*", "", Parameter)) %>% 
  filter(Para == "eurostat")

# add corridors to eurostat

eu18 <- eurostat18 %>% 
  add_node() %>% 
  add_corridor() %>% 
  left_join(years)


eurostat_2018_obs <-dinput %>% 
  filter(source == "eurostat", 
         year == "2018") %>% 
  mutate(stock = log(flow))

summary_eurostat_2018 <- eu18 %>% 
  filter(year == "2018") %>% 
  group_by(Chain, orig, dest, year) %>% 
  summarise(mean = mean(value), 
            median = median(value), 
            min = min(value), 
            max = max(value), 
            upp80 = quantile(x = value, probs = 0.9), 
            low80 = quantile(x = value, probs = 0.1), 
            upp50 = quantile(x = value, probs = 0.75),
            lwr50 = quantile(x = value, probs = 0.25)) %>% 
  left_join(eurostat_2018_obs)


ggplot(summary_eurostat_2018) +
  geom_pointrange(aes(x = exp(median), xmax = exp(upp80), xmin = exp(low80), y = flow), alpha = 0.1, size = 0.5) +
  geom_abline() +
  # xlim(c(0, 15)) +
  # ylim(c(0, 15)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  facet_grid(orig ~ dest)


summary_eurostat <- bind_rows(summary_eurostat_2016, summary_eurostat_2017, summary_eurostat_2018) %>% 
  mutate(Chain = as.factor(Chain))


within <- summary_eurostat %>% 
  mutate(within = ifelse((stock < (upp80) & stock > (low80)), 1, 0), 
         Chain = as.factor(Chain)) %>% 
  group_by(year, Chain) %>% 
  summarise(prop = sum(within, na.rm = T)/756)

ggplot(summary_eurostat %>% filter(year != "2016")) +
  geom_pointrange(aes(y = exp(median), ymax = exp(upp80), ymin = exp(low80), x = flow, color = Chain), alpha = 0.15, size = 0.5, shape = 21) +
  geom_abline() +
  # xlim(c(0, 15)) +
  # ylim(c(0, 15)) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  ylab("Estimated Eurostat and 80% PI") +
  xlab("Eurostat") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  facet_wrap(~year)

ggsave("./figures/estimate_eu_2017_2018.png")
