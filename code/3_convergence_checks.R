## convergence of parameters - not shown in the article
## 
##

library(tidyverse)
library(forcats)
library(stringr)
library(ggmcmc)
source("code/0_read_functions.R")

corridors <- dinput %>%
  select(orig, dest, corridor) %>%
  distinct()

years <- dinput %>%
  select(year) %>%
  mutate(node_id = 1:n())


#model_name <- "m_est_eu18_"

eurostat <- ggs(m, family = "eurostat") %>%  
  mutate(Para = gsub("\\[.*", "", Parameter)) %>% 
  filter(Para == "eurostat")

# add corridors to eurostat

eu <- eurostat %>% 
  add_node() %>% 
  add_corridor() %>% 
  left_join(years)


eurostat_2018_obs <-dinput %>% 
  filter(source == "eurostat", 
         year == "2018") %>% 
  mutate(stock = log(stock))

summary_eurostat_2018 <- eu %>% 
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
  geom_pointrange(aes(x = median, xmax = upp80, xmin = low80, y = stock), alpha = 0.1) +
  geom_abline() +
  xlim(c(0, 15)) +
  ylim(c(0, 15)) +
  theme_bw()

summary_eurostat_2015 <- eu %>% 
  filter(year == "2015") %>% 
  group_by(Chain, orig, dest, year) %>% 
  summarise(mean = mean(value), 
            median = median(value), 
            min = min(value), 
            max = max(value), 
            upp80 = quantile(x = value, probs = 0.9), 
            low80 = quantile(x = value, probs = 0.1), 
            upp50 = quantile(x = value, probs = 0.75),
            lwr50 = quantile(x = value, probs = 0.25))




eu_at_gr <- eu %>% 
  filter(orig == "Austria", 
         dest == "Greece", 
         year == 2011)
# also at to gr
eu_11 <- ggs(m, family = "eurostat\\[11]")

ggs_density(eu_11)

mu <- ggs(m, family = "mu") 
sigma <- ggs(m, family = "sigma")
beta <- ggs(m, family = "beta")
beta1 <- ggs(m, family = "^beta\\[1") 
beta2 <- ggs(m, family = "^beta\\[2,")
gamma <- ggs(m, family = "^gamma")
gamma_fbmau_2016 <- ggs(m, family = "gamma_fbmau_2016") 
gamma_fbmau_2017 <- ggs(m, family = "gamma_fbmau_2017") 
gamma_fbmau_2018 <- ggs(m, family = "gamma_fbmau_2018") 
gamma_fbdau <- ggs(m, family = "^gamma_fbdau")
gamma_eu <- ggs(m, family = "^gamma_eurostat")
gamma_fbmau <- ggs(m, family = "^gamma_fbmau")
gamma_lfs <- ggs(m, family = "^gamma_lfs")
gamma_census <- ggs(m, family = "^gamma_census")
tau_eurostat <- ggs(m, family = "tau_eurostat")
tau_fbmau <- ggs(m, family = "tau_fbmau")
tau_fbdau <- ggs(m, family = "tau_fbdau")
tau_lfs <- ggs(m, family = "tau_lfs")
intercept <- ggs(m, family = "^intercept")
slope <- ggs(m, family = "^slope")
tau_y1 <- ggs(m, family = "tau_y1")
tau_beta <- ggs(m, family = "tau_beta")
tau_census <- ggs(m, family = "tau_census")
summary_tau_lfs <- tau_lfs %>% 
  group_by(Chain) %>% 
  summarise(mean = mean(value), 
            median = median(value), 
            min = min(value), 
            max = max(value), 
            upp80 = quantile(x = value, probs = 0.9), 
            low80 = quantile(x = value, probs = 0.1))
kappa_fbmau_2019 <- ggs(m, family = "kappa_fbmau_2019")
kappa_fbdau_2019 <- ggs(m, family = "kappa_fbdau_2019")

#it is advisable to save this as a pdf - uncomment next two lines and l.134
# plot_name <- paste0("figures/", model_name, "traceplots", ".pdf")
# pdf(plot_name,  height = 12, width = 12)
ggs_traceplot(mu)
ggs_traceplot(gamma_eu)
ggs_traceplot(gamma_lfs)
ggs_traceplot(gamma_fbmau_2016)
ggs_traceplot(gamma_fbmau_2017)
ggs_traceplot(gamma_fbmau_2018)
ggs_traceplot(gamma_fbdau)
ggs_traceplot(gamma_census)
ggs_traceplot(tau_y1)
ggs_traceplot(tau_eurostat)
ggs_traceplot(tau_fbmau)
ggs_traceplot(tau_fbdau)
ggs_traceplot(tau_census)
ggs_traceplot(intercept)
ggs_traceplot(slope)
ggs_traceplot(sigma)
# dev.off()



# plot_name <- paste0("figures/", model_name, "density", ".pdf")
# pdf(plot_name,  height = 12, width = 12)
ggs_density(mu)
ggs_density(gamma_eu)
ggs_density(gamma_lfs)
ggs_density(gamma_fbmau_2016)
ggs_density(gamma_fbmau_2017)
ggs_density(gamma_fbmau_2018)
ggs_density(gamma_fbdau)
ggs_density(gamma_census)
ggs_density(tau_y1)
ggs_density(tau_eurostat)
ggs_density(tau_fbmau)
ggs_density(tau_fbdau)
ggs_density(tau_census)
ggs_density(intercept)
ggs_density(slope)
ggs_density(sigma)
ggs_density(kappa_fbmau_2019)
ggs_density(kappa_fbdau_2019)
# dev.off()
 


# plot_name <- plot_name <- paste0("figures/", model_name, "taus", ".pdf")
# pdf(plot_name,  height = 12, width = 12)
ggs_density(tau_eurostat)
ggs_density(tau_census)
ggs_density(tau_fbmau)
ggs_density(tau_fbdau)
ggs_density(tau_y1)
# dev.off()


ggs_traceplot(tau_eurostat)

summary_tau_eurostat <- tau_eurostat %>% 
  summarise(mean = mean(value), 
            median = median(value), 
            min = min(value), 
            max = max(value), 
            upp80 = quantile(x = value, probs = 0.9), 
            low80 = quantile(x = value, probs = 0.1), 
            upp50 = quantile(x = value, probs = 0.75),
            lwr50 = quantile(x = value, probs = 0.25))


summary_tau_lfs <- tau_lfs %>% 
  summarise(mean = mean(value), 
            median = median(value), 
            min = min(value), 
            max = max(value), 
            upp80 = quantile(x = value, probs = 0.9), 
            low80 = quantile(x = value, probs = 0.1), 
            upp50 = quantile(x = value, probs = 0.75),
            lwr50 = quantile(x = value, probs = 0.25))
