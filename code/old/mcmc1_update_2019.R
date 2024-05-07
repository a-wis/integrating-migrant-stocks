##
## mcmc1 parameter plots

## mcmc3 total, imm and emi plots
##

library(tidyverse)
library(forcats)
library(stringr)
library(ggmcmc)

model_name <- "naive_model_cor"

folder_name <- "results/"

eurostat <- ggs(m, family = "eurostat")


dinput <- read.csv("data/dinput.csv")
# add corridors to eurostat

eu <- eurostat %>% 
  add_node() %>% 
  add_corridor() %>% 
  left_join(years)


eu_at_gr <- eu %>% 
  filter(orig == "Austria", 
         dest == "Greece", 
         year == 2011)
# also at to gr
eu_11 <- ggs(m, family = "eurostat\\[11]")


ggs_density(eu_11)
ggs_density(w) + facet_grid(orig ~ dest)

w <- ggs(m, family = "w") %>% 
  add_node() %>% 
  add_corridor()

mu <- ggs(m, family = "mu") 
sigma <- ggs(m, family = "sigma")
beta <- ggs(m, family = "beta")
beta1 <- ggs(m, family = "^beta\\[1") 
beta2 <- ggs(m, family = "^beta\\[2,")
gamma <- ggs(m, family = "^gamma")
gamma_fbmau_2016 <- ggs(m, family = "gamma_fbmau_2016") 
gamma_fbmau_2017 <- ggs(m, family = "gamma_fbmau_2017") 
gamma_fbmau_2018 <- ggs(m, family = "gamma_fbmau_2018") 
gamma_fbmau_2019 <- ggs(m, family = "gamma_fbmau_2019") 
gamma_fbdau_2018 <- ggs(m, family = "gamma_fbdau_2018")
gamma_fbdau_2019 <- ggs(m, family = "gamma_fbdau_2019")
gamma_eu <- ggs(m, family = "^gamma_eurostat")
gamma_fbmau <- ggs(m, family = "^gamma_fbmau")
gamma_lfs <- ggs(m, family = "^gamma_lfs")
gamma_census <- ggs(m, family = "^gamma_census")
lambda <- ggs(m, family = "^lambda")
delta_covmau <- ggs(m, family = "delta_covmau")
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

kappa_fbmau <- kappa_fbmau_2019 %>% 
  add_node() %>% 
  add_corridor() %>% 
  left_join(years)


kappa_fbmau <- kappa_fbmau %>% 
  group_by(node_id, orig, dest, year, Chain) %>% 
  summarise(kappa_fbmau = median(value),
            upp50 = quantile(x = value, probs = 0.75),
            lwr50 = quantile(x = value, probs = 0.25), 
            upp80 = quantile(x = value, probs = 0.9),
            lwr80 = quantile(x = value, probs = 0.1)) %>% 
  mutate(corridor = paste0(orig, "->", dest))

ggplot(kappa_fbmau %>% filter(orig == "Romania"), aes(x = corridor, y = mau_est_ratio)) +
  geom_point() +
  theme(axis.text.x = element_text(angle= 90)) +
  geom_point(aes(x = corridor, y = `1`, color = "`1`")) +
  geom_point(aes(x = corridor, y = `2`, color = "`2`")) +
  geom_point(aes(x = corridor, y = `3`, color = "`3`"))

plot_name <- paste0(folder_name, model_name, "traceplots", ".pdf")
pdf(plot_name,  height = 12, width = 12)
ggs_traceplot(mu)
ggs_traceplot(gamma_eu)
ggs_traceplot(gamma_lfs)
ggs_traceplot(gamma_fbmau_2016)
ggs_traceplot(gamma_fbmau_2017)
ggs_traceplot(gamma_fbmau_2018)
ggs_traceplot(gamma_fbdau_2018)
ggs_traceplot(gamma_fbdau_2019)
ggs_traceplot(kappa_fbmau_2019)
ggs_traceplot(kappa_fbdau_2019)
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
dev.off()



plot_name <- paste0(folder_name, model_name, "density", ".pdf")
pdf(plot_name,  height = 12, width = 12)
ggs_density(mu)
ggs_density(gamma_eu)
ggs_density(gamma_lfs)
ggs_density(gamma_fbmau_2016)
ggs_density(gamma_fbmau_2017)
ggs_density(gamma_fbmau_2018)
ggs_density(gamma_fbmau_2019)
ggs_density(gamma_fbdau_2018)
ggs_density(gamma_fbdau_2019)
ggs_density(kappa_fbmau_2019)
ggs_density(kappa_fbdau_2019)
ggs_density(gamma_census)
ggs_density(tau_y1)
ggs_density(tau_eurostat)
ggs_density(tau_fbmau)
ggs_density(tau_fbdau)
ggs_density(tau_census)
ggs_density(intercept)
ggs_density(slope)
ggs_density(sigma)
dev.off()
 
# Densities for coverage parameters

plot_name <- paste0(folder_name, model_name, "gamma_census.png")
png(plot_name)
ggs_density(gamma_census)
dev.off()


# LFS Romania to UK

# dest and year for lfs coverage

lfs_dest_year <- read.csv("./Paper/data_july19/d5_est_eu18.csv") %>% 
 rownames_to_column(var = "node_id") %>% 
  filter(year %in% c(2016, 2017)) %>% 
  mutate(dest_year = paste0(dest, "_", year), 
         cov_lfs_dest_year = ifelse((dest == "Czechia" & year == 2016), 11, cov_lfs_dest_year), 
         cov_lfs_dest_year = ifelse((dest == "Czechia" & year == 2017), 12, cov_lfs_dest_year) )%>% 
  select(node_id, dest, year, dest_year, cov_lfs_dest_year) %>% 
  filter(!is.na(cov_lfs_dest_year)) %>% 
  distinct(dest_year, .keep_all = TRUE)



gamma_lfs_ODY <- gamma_lfs %>% 
  mutate(node = extract_numeric(Parameter)) %>% 
  left_join(lfs_dest_year %>%  rename(node=cov_lfs_dest_year))
write.csv(gamma_lfs_ODY, "./Paper/update_2019/gamma_lfs_ODY.csv", row.names = FALSE)

ggplot(gamma_lfs_ODY %>%  filter(year == 2017, dest == "Belgium"), aes(x = value, fill = Chain)) +
  geom_density() 



dene = gamma_lfs_ODY %>% filter(year == 2016)#, dest %in% c("Germany", "Netherlands", "United Kingdom"))

png("./Paper/update_2019/gamma_lfs_2016.png")
 ggplot(dene, aes(x = value))+
   geom_density(aes(fill = as.factor(Chain), color = as.factor(Chain)), alpha = 0.4) +
   facet_wrap(~ dest, scales = "free")
dev.off()

# Predictive intervals
gamma_lfs_ODY_PI <- gamma_lfs %>% 
  mutate(node = extract_numeric(Parameter)) %>% 
  left_join(lfs_dest_year %>%  rename(node=cov_lfs_dest_year)) %>% 
  group_by(Chain, dest, year) %>% 
  summarise(gamma_lfs = median(value), 
            upp50 = quantile(value, probs = 0.75), 
            lwr50 = quantile(value, probs = 0.25), 
            upp80 = quantile(value, probs = 0.90), 
            lwr80 = quantile(value, probs = 0.10))





# Densities for selected kappa (definition)
# Germany to Austria
kappa_fbmau_6319 <- ggs(m, family = "kappa_fbmau_2019\\[6319]")
ggs_density(kappa_fbmau_6319)

# Poland to UK
kappa_fbmau_6615 <- ggs(m, family = "kappa_fbmau_2019\\[6615]")
ggs_density(kappa_fbmau_6615)

# Romania to UK
kappa_fbmau_6669 <- ggs(m, family = "kappa_fbmau_2019\\[6669]")
ggs_density(kappa_fbmau_6669)

###

ggs_density(beta1)

plot_name <- plot_name <- paste0("./figures/", model_name, "taus", ".pdf")
pdf(plot_name,  height = 12, width = 12)
ggs_density(tau_eurostat)
ggs_density(tau_census)
ggs_density(tau_fbmau)
ggs_density(tau_fbdau)
ggs_density(tau_y1)
dev.off()


ggs_traceplot(tau_eurostat)


# precision to weight the age-sex estimates
summary_tau_census <- tau_census %>% 
  summarise(mean = mean(value), 
            median = median(value), 
            min = min(value), 
            max = max(value), 
            upp80 = quantile(x = value, probs = 0.9), 
            low80 = quantile(x = value, probs = 0.1), 
            upp50 = quantile(x = value, probs = 0.75),
            lwr50 = quantile(x = value, probs = 0.25))


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
