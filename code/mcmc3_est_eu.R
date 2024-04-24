##
## mcmc1 parameter plots
## mcmc3 total, imm and emi plots
##


library(forcats)
library(stringr)
library(ggmcmc)
library(geofacet)
library(metafolio)
library(ss3sim)
library(tidyverse)
library(scales)
# devtools::install_github("guiastrennec/ggplus")
library(ggplus)
library(rayshader)

# load(file = "C:/Users/dyildiz.VID-CALC1/Documents/m9_lfs_cov2_091218.RData")
# d5 from jags9_clean.R
# dinput <- d5 %>%
#   select(orig, dest, year, eurostat, mau, dau, LFS, corridor) %>%
#   gather(eurostat, dau, mau, LFS, key = "source", value = "flow")

# dinput <- read.csv("./data/input_15_64_LFS_corrected.csv")

# write.csv(dinput, "./data/jags_calc_input.csv", row.names = F)
#
# library(dplyr)
# dinput_source_year <- dinput %>%
#   select(year, source, flow) %>%
#   group_by(year, source) %>%
#   summarise(flow = sum(flow, na.rm = T))

# functions for building summary stats
source("code/read_functions.R")
# data for building summary stats
# load(file = "./model/d1.RData");

# data frames for building summary states
corridors <- dinput %>%
  select(orig, dest, corridor) %>%
  distinct()

years <- dinput %>%
  select(year) %>%
  mutate(node_id = 1:n())

# m5 changed to m
# from model
y <- ggs(m, family = "^y1") %>%
  add_node() %>%
  add_corridor() %>%
  left_join(years)








model_name <- "m_estimate_eu18_"
folder_name <- "Paper/estimate_eurostat/"


# colours for plots
# gg_color_hue <- function(n){hues = seq(15, 375, length = n + 1)
# hcl(h = hues, l = 65, c = 100)[1:n]}
# col0 <- c("darkgrey", gg_color_hue(4))
#
# col0 <- c("darkgrey", gg_color_hue(6))
col0 <- c("darkgrey", "red", "peru", "green3", "royalblue", "magenta2")

##
## totals
##
# calcuate stats for y


tot_y <- y_stats(y)

tot_un <- read_csv("C:/Users/dyildiz.VID-CALC1/Dropbox/ec-rand/data/total.csv") %>%
   filter(source =="un")

# 15-64
tot_d <- dinput %>%
  # bind_rows(tot_un) %>%
  group_by(year, source) %>%
  summarise(flow = sum(flow, na.rm = T)) %>%
  mutate(source_name = source,
    source_name = ifelse(source_name == "eurostat", "Eurostat", source_name))
# %>%
  # mutate(source_name = ifelse(source_name == "un", "UN DESA (total population)", source_name))

tot <- tot_y %>%
  mutate(source_name = "Estimate") %>%
  rename(flow = total) %>%
  bind_rows(tot_d) %>%
  mutate(source_name = ifelse(source_name == "Eurostat", "Eurostat (w. Missing)", source_name),
         source = source_name,
         source_name = source,
         source = ifelse(source_name == "eurostat", "Eurostat", source),
         source = ifelse(source_name == "dau", "Facebook DAU", source),
         source= ifelse(source_name == "mau", "Facebook MAU", source),
         source = ifelse(source_name == "Estimate", "Estimate", source),
         source = ifelse(source_name == "LFS", "LFS", source),
         source = ifelse(source_name == "census", "Census", source),
         source = fct_relevel(source, "Estimate")) %>%
  mutate(flow = ifelse(flow == 0, NA, flow))

table_name <- paste0("figures/", folder_name, model_name,"tot_y.csv")
# write.csv(tot_y, table_name, row.names = F)


p <- ggplot(data = (tot %>% filter(year>=2011, source != "UN DESA (total population)" )),
       # mapping = aes(x = as.integer(year), y = flow/1e06, ymin = (flow - sd)/1e06, ymax = (flow + sd)/1e06)) +
        mapping = aes(x = year, y = flow/1e06, ymin = lwr80/1e06, ymax = upp80/1e06)) +
  geom_line(mapping = aes(colour = source)) +
  geom_ribbon(mapping = aes(fill = source), alpha = 0.4) +
  geom_point(data = tot %>% filter(year >= 2011),
                                   mapping = aes(colour = source)) +
  theme_bw() +
  scale_color_manual(values = col0, drop=FALSE) +
  scale_fill_manual(values = col0, drop=FALSE) +
  scale_x_continuous(breaks = min(dinput$year):max(dinput$year)) +
  labs(x= "Year", y= "Migrant stocks (millions), 15 - 64", colour = "", fill = "") +
  guides(fill = FALSE)


plot_name <- paste0("figures/", folder_name,model_name,"total_80ci.pdf")
pdf(plot_name)
p
dev.off()

# without UN DESA

tot2 <- tot %>%
  filter(source != "UN DESA (total population)") %>%
  mutate(source = factor(source, levels = c("Estimate", "Census", "Eurostat (w. Missing)", "Facebook DAU", "Facebook MAU", "LFS")))

# col0 <- c("darkgrey", gg_color_hue(5))

col0 <- c("darkgrey",  "peru", "red","green3", "royalblue", "magenta2")

p <- ggplot(data = tot2,
            mapping = aes(x = year, y = flow/1e06, ymin = lwr80/1e06, ymax = upp80/1e06)) +
  geom_line(mapping = aes(colour = source)) +
  geom_ribbon(mapping = aes(fill = source), alpha = 0.4) +
  geom_point(data = tot2 %>% filter(year >= 2011),
             mapping = aes(colour = source)) +
  theme_bw() +
  scale_color_manual(values = col0, drop=FALSE) +
  scale_fill_manual(values = col0, drop=FALSE) +
  scale_x_continuous(breaks = min(dinput$year):max(dinput$year)) +
  labs(x= "Year", y= "Stocks of EU movers (millions), 15 - 64", colour = "", fill = "") +
  guides(fill = FALSE)

plot_name <- paste0("figures/", folder_name,model_name,"total_withoutUN_80ci.pdf")
pdf(plot_name)
p
dev.off()

plot_name <- paste0("./figures/",folder_name,model_name,"total.png")
png(plot_name, height = 6, width = 5.5, units = "in", res = 300)
p
dev.off()


##
## immm and emi flows
##
imm_y <- y_stats(y, type = "imm")

imm_d <- dinput %>%
  group_by(year, dest, source) %>%
  summarise(flow = sum(flow, na.rm = T)) %>%
  mutate(source_name = source,
         source_name = ifelse(source_name == "eurostat", "Eurostat", source_name)) %>%
  rename(name = dest) %>%
  select(year, name, flow, source, source_name )



imm <- imm_y %>%
  mutate(source_name = "Estimate") %>%
  rename(flow = total) %>%
  bind_rows(imm_d) %>%
  mutate(source_name = ifelse(source_name == "Eurostat", "Eurostat", source_name),
         source = source_name,
         source_name = source,
         source = ifelse(source_name == "eurostat", "Eurostat", source),
         source = ifelse(source_name == "dau", "Facebook DAU", source),
         source= ifelse(source_name == "mau", "Facebook MAU", source),
         source = ifelse(source_name == "Estimate", "Estimate", source),
         source = ifelse(source_name == "LFS", "LFS", source),
         source = ifelse(source_name == "census", "Census", source),
         source = fct_relevel(source, "Estimate"),
         flow = ifelse(flow == 0, NA, flow))

imm$name = plyr::revalue(imm$name, c("Czech Republic" = "Czechia"))
imm$name = plyr::revalue(imm$name, c("United Kingdom" = "UK"))
imm$name[imm$name == "Czech Republic"] = "Czechia"
imm$name[imm$name == "United Kingdom"] = "UK"
eu_grid1$name[eu_grid1$name == "Czech Republic"] = "Czechia"
eu_grid1$name[eu_grid1$name == "United Kingdom"] = "UK"

table_name <- paste0("figures/", folder_name, model_name,"imm_y.csv")
# write.csv(imm, table_name, row.names = F)

col0 <- c("darkgrey", "peru", "red", "green3", "royalblue", "magenta2")
scaleFUN <- function(x) sprintf("%.2f", x)
p <- ggplot(data = imm %>% filter(year>=2011),
        mapping = aes(x = year, y = flow/1e06, ymin = lwr80/1e06, ymax = upp80/1e06)) +
       # mapping = aes(x = as.integer(year), y = flow/1e06, ymin = (flow - sd)/1e06, ymax = (flow + sd)/1e06)) +
  facet_geo(facets = "name", grid = eu_grid1) +
  geom_line(mapping = aes(colour = source)) +
  geom_ribbon(mapping = aes(fill = source), alpha = 0.4) +
  # geom_ribbon(data = filter(tot, source =="Estimate"), fill = col0[1], alpha = 0.4) +
  geom_point(mapping = aes(colour = source)) +
  theme_bw() +
  scale_color_manual(values = col0, drop=FALSE) +
  scale_fill_manual(values = col0, drop=FALSE) +
  scale_x_continuous(breaks = seq(min(dinput$year),max(dinput$year), by = 2)) +
  scale_y_continuous(labels = scaleFUN) +
  labs(x= "Year", y= "Stocks of EU movers (millions)", colour = "", fill = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = FALSE)+
  ggtitle(paste0("Immigration"))


plot_name <- paste0("figures/", folder_name,model_name,"imm2.png")
png(plot_name, width = 10, height = 10, units = "in", res = 300)
p
dev.off()


emi_y <- y_stats((y), type = "emi") %>%
  rename(flow = total)

emi_d <- dinput %>%
  group_by(year, orig, source) %>%
  summarise(flow = sum(flow, na.rm = T)) %>%
  mutate(source_name = source) %>%
  rename(name = orig) %>%
  select(year, name, flow, source, source_name )

emi <- emi_y %>%
  mutate(source_name = "Estimate") %>%
  bind_rows(emi_d) %>%
  mutate(source = source_name,
         source_name = source,
         source = ifelse(source_name == "eurostat", "Eurostat", source),
         source = ifelse(source_name == "dau", "Facebook DAU", source),
         source= ifelse(source_name == "mau", "Facebook MAU", source),
         source = ifelse(source_name == "Estimate", "Estimate", source),
         source = ifelse(source_name == "LFS", "LFS", source),
         source = ifelse(source_name == "census", "Census", source),
         source = fct_relevel(source, "Estimate"),
         flow = ifelse(flow == 0, NA, flow))

emi$name = plyr::revalue(emi$name, c("Czech Republic" = "Czechia"))
emi$name = plyr::revalue(emi$name, c("United Kingdom" = "UK"))

emi$name[emi$name == "United Kingdom"] = "UK"

emi$name[emi$name == "Czech Republic"] = "Czechia"
eu_grid1$name[eu_grid1$name == "Czech Republic"] = "Czechia"
eu_grid1$name[eu_grid1$name == "United Kingdom"] = "UK"


table_name <- paste0("./figures/", folder_name, model_name,"emi_y.csv")
# write.csv(emi, table_name, row.names = F)

col0 <- c("darkgrey", "peru", "red", "green3", "royalblue", "magenta2")

p <- ggplot(data = emi %>% filter(year>=min(dinput$year)),
       mapping = aes(x = year, y = flow/1e06, ymin = lwr80/1e06, ymax = upp80/1e06)) +
       # mapping = aes(x = as.integer(year), y = flow/1e06, ymin = (flow - sd)/1e06, ymax = (flow + sd)/1e06)) +
  facet_geo(facets = "name", grid = eu_grid1) +
  geom_line(mapping = aes(colour = source)) +
  geom_ribbon(mapping = aes(fill = source), alpha = 0.4) +
  # geom_ribbon(data = filter(tot, source =="Estimate"), fill = col0[1], alpha = 0.4) +
  geom_point(mapping = aes(colour = source)) +
  theme_bw() +
  scale_color_manual(values = col0, drop=FALSE) +
  scale_fill_manual(values = col0, drop=FALSE) +
  scale_x_continuous(breaks = seq(min(dinput$year),max(dinput$year), by = 2)) +
  scale_y_continuous(labels = scaleFUN) +
    labs(x= "Year", y= "Stocks of EU movers (millions)", colour = "", fill = "") +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  guides(fill = FALSE)+
    ggtitle(paste0("Emigration"))



plot_name <- paste0("figures/", folder_name,model_name,"emi.png")
png(plot_name, width = 10, height = 10, units = "in", res = 300)
p
dev.off()





##
## bilat flows
##
bilat_y <- y %>%
   # filter(Iteration >= 500) %>%
  y_stats(type = "bilat")


bilat <- bilat_y %>%
  rename(flow = total) %>%
  mutate(source = "Estimate", line_split = source) %>%
  bind_rows(dinput %>% select(year, orig, dest, flow, source) %>%  mutate(line_split = source)) %>%
  mutate(source = fct_relevel(source, "Estimate"),
         source_name = source,
         source_name = ifelse(source == "eurostat", "Eurostat", source_name),
         source_name = ifelse(source == "dau", "Facebook DAU", source_name),
         source_name = ifelse(source == "mau", "Facebook MAU", source_name),
         source_name = ifelse(source == "Estimate", "Estimate", source_name),
         source_name = ifelse(source == "LFS", "LFS", source_name),
         source_name = ifelse(source == "census", "Census", source_name),
         dest = plyr::revalue(dest, c("Czech Republic" = "Czechia")),
         # orig = plyr::revalue(orig, c("Czech Republic" = "Czechia")),
         # orig = plyr::revalue(orig, c("United Kingdom" = "UK")),
         dest = plyr::revalue(dest, c("United Kingdom" = "UK")))
table_name <- paste0("./figures/", folder_name, model_name,"bilat_y.csv")
# write.csv(bilat_y, table_name, row.names = F)

ggplot(data = bilat,
       mapping = aes(x = year, y = flow/1e06, ymin = (flow-sd)/1e06, ymax = (flow + sd)/1e06,
                     colour = source, fill = source, group = line_split)) +
  facet_grid(facets = orig ~ dest, labeller = label_wrap_gen(12), switch = "y") +
  geom_line() +
  geom_ribbon(alpha = 0.4) +
  theme_bw() +
  scale_color_manual(values = col0, drop=FALSE) +
  scale_fill_manual(values = col0, drop=FALSE) +
  labs(x= "Year", y= "Migration (millions)", colour = "", fill = "")



# countries
country = "Netherlands"
# imm
bilat_country_imm <- bilat_y %>%
  rename(flow = total) %>%
  mutate(source = "Estimate", line_split = source) %>%
  bind_rows(dinput %>% select(year, orig, dest, flow, source) %>%  mutate(line_split = source)) %>%
  mutate(source_name = source,
         source = ifelse(source_name == "eurostat", "Eurostat", source),
         source = ifelse(source_name == "dau", "Facebook DAU", source),
         source= ifelse(source_name == "mau", "Facebook MAU", source),
         source = ifelse(source_name == "Estimate", "Estimate", source),
         source = ifelse(source_name == "LFS", "LFS", source),
         source = ifelse(source_name == "census", "Census", source),
         source = fct_relevel(source, "Estimate")) %>%
  filter(dest == country)


bilat_country_imm$orig = plyr::revalue(bilat_country_imm$orig, c("Czech Republic" = "Czechia"))
bilat_country_imm$orig = plyr::revalue(bilat_country_imm$orig, c("United Kingdom" = "UK"))
bilat_country_imm$orig[bilat_country_imm$orig == "Czech Republic"] = "Czechia"
bilat_country_imm$orig[bilat_country_imm$orig == "United Kingdom"] = "UK"
eu_grid1$name[eu_grid1$name == "Czech Republic"] = "Czechia"
eu_grid1$name[eu_grid1$name == "United Kingdom"] = "UK"

plotname <- paste0("figures/", model_name, country, "_logimm_11_7.pdf")

pdf(plotname, height = 7, width = 11)
ggplot(data = bilat_country_imm %>% filter(year>=min(dinput$year)),
       mapping = aes(x = year, y = log(flow), ymin = log(lwr80), ymax = log(upp80)))+
  facet_geo(facets = "orig", grid = eu_grid1) +
  geom_line(mapping = aes(colour = source)) +
  geom_ribbon(mapping = aes(fill = source), alpha = 0.4) +
  # geom_ribbon(data = filter(tot, source =="Estimate"), fill = col0[1], alpha = 0.4) +
  geom_point(mapping = aes(colour = source)) +
  theme_bw() +
  scale_color_manual(values = col0, drop=FALSE) +
  scale_fill_manual(values = col0, drop=FALSE) +
  scale_x_continuous(breaks = seq(min(dinput$year),max(dinput$year), by = 2)) +
  labs(x= "Year", y= "Migrants (log)", colour = "", fill = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = FALSE)
dev.off()


# Netherlands total immigration
total_country_imm <- imm %>%
   filter(name == country)

col0 <- c("darkgrey",  "peru", "red","green3", "royalblue", "magenta2", "blue")

p <- ggplot(data =(total_country_imm %>% filter(source_name != "mau", source_name != "dau")),
            mapping = aes(x = year, y = flow/1e06, ymin = lwr80/1e06, ymax = upp80/1e06)) +
  geom_line(mapping = aes(colour = source)) +
  geom_ribbon(mapping = aes(fill = source), alpha = 0.4) +
  geom_point(data = total_country_imm %>% filter(source_name=="mau" | source_name == "dau"),
             mapping = aes(colour = source)) +
  theme_bw() +
  scale_color_manual(values = col0, drop=FALSE) +
  scale_fill_manual(values = col0, drop=FALSE) +
  scale_x_continuous(breaks = min(dinput$year):max(dinput$year)) +
  labs(x= "Year", y= "Stocks of EU movers (millions), 15 - 64", colour = "", fill = "") +
  guides(fill = FALSE)


plot_name <- paste0("figures/",folder_name,model_name,"total_netherlands.png")
png(plot_name, height = 6, width = 5.5, units = "in", res = 300)
p
dev.off()



# all countries just imm estimates



for(i in 1:length(unique(imm$name))){
country <- unique(imm$name)[i]
plot_name <- paste0("figures/", country,"_allcountries.png")

p <-  ggplot(data =(imm %>%
                 filter(name == country) %>% filter(source_name == "Estimate")),
            mapping = aes(x = year, y = flow/1e06)) +
  geom_line(mapping = aes(colour = source)) +
  geom_ribbon(mapping = aes(ymin = lwr80/1e06, ymax = upp80/1e06), alpha = 0.4) +
  theme_bw() +
  expand_limits(y = c(0)) +
  scale_color_manual(values = col0, drop = FALSE) +
  scale_fill_manual(values = col0, drop = FALSE) +
  scale_x_continuous(breaks = min(dinput$year):max(dinput$year)) +
  labs(x= "Year", y= "Stocks of EU movers (millions), 15 - 64", colour = "", fill = "") +
  theme(legend.position = "none") +
  ggtitle(i) +
  guides(fill = FALSE)
png(plot_name)
print(p)
dev.off()
}

pdf("figures/allcountries.pdf")
for(i in seq(1, length(unique(imm$name)), 4)){

  p <- ggplot(data =(imm[imm$name %in% unique(imm$name)[i:(i+3)],] %>% filter(source=="Estimate")),
              mapping = aes(x = year, y = flow/1e06)) +
    geom_line(mapping = aes(colour = source)) +
    geom_ribbon(mapping = aes(ymin = lwr80/1e06, ymax = upp80/1e06), alpha = 0.4) +
    theme_bw() +
    expand_limits(y = c(0)) +
    scale_color_manual(values = col0, drop = FALSE) +
    scale_fill_manual(values = col0, drop = FALSE) +
    scale_x_continuous(breaks = min(dinput$year):max(dinput$year)) +
    labs(x= "Year", y= "Stocks of EU movers (millions), 15 - 64", colour = "", fill = "") +
    theme(legend.position = "none") +
    # ggtitle(i) +
    guides(fill = FALSE) +
    facet_wrap(~ name, ncol = 2)

print(p)
}
dev.off()




#emi
bilat_country_emi <- bilat_y %>%
  rename(flow = total) %>%
  # mutate(period = 1, source = "Estimate", line_split = source) %>%
  mutate(source = "Estimate", line_split = source) %>%
  bind_rows(dinput %>% select(year, orig, dest, flow, source) %>%  mutate(line_split = source)) %>%
  # filter(period == 1) %>%
  mutate(source_name = source,
         source = ifelse(source_name == "eurostat", "Eurostat", source),
         source = ifelse(source_name == "dau", "Facebook DAU", source),
         source = ifelse(source_name == "mau", "Facebook MAU", source),
         source = ifelse(source_name == "Estimate", "Estimate", source),
         source = ifelse(source_name == "LFS", "LFS", source),
         source = ifelse(source_name == "census", "Census", source),
         source = fct_relevel(source, "Estimate")) %>%
  filter(orig == country)


bilat_country_emi$dest = plyr::revalue(bilat_country_emi$dest, c("Czech Republic" = "Czechia"))
bilat_country_emi$dest = plyr::revalue(bilat_country_emi$dest, c("United Kingdom" = "UK"))
bilat_country_emi$dest[bilat_country_emi$dest == "Czech Republic"] = "Czechia"
bilat_country_emi$dest[bilat_country_emi$dest == "United Kingdom"] = "UK"
eu_grid1$name[eu_grid1$name == "Czech Republic"] = "Czechia"
eu_grid1$name[eu_grid1$name == "United Kingdom"] = "UK"




plotname <- paste0("figures/", model_name, country, "_logemi_11_7.pdf")

pdf(plotname, height = 7, width = 11)
ggplot(data = bilat_country_emi %>% filter(year>=min(dinput$year)),
       mapping = aes(x = year, y = log(flow), ymin = log(lwr80), ymax = log(upp80)))+
  facet_geo(facets = "dest", grid = eu_grid1) +
  geom_line(mapping = aes(colour = source)) +
  geom_ribbon(mapping = aes(fill = source), alpha = 0.4) +
  # geom_ribbon(data = filter(tot, source =="Estimate"), fill = col0[1], alpha = 0.4) +
  geom_point(mapping = aes(colour = source)) +
  theme_bw() +
  scale_color_manual(values = col0, drop=FALSE) +
  scale_fill_manual(values = col0, drop=FALSE) +
  scale_x_continuous(breaks = seq(min(dinput$year),max(dinput$year), by = 2)) +
  labs(x= "Year", y= "Migrants (log)", colour = "", fill = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = FALSE)
dev.off()




bilat_uk_imm <- dinput %>%
  filter(dest == "United Kingdom")

pdf("figures/input_uk_imm.pdf")
ggplot(data = bilat_uk_imm %>% filter(year>=min(dinput$year)),
       mapping = aes(x = year, y = log(flow)))+
  facet_geo(facets = "orig", grid = eu_grid1) +
  geom_line(mapping = aes(colour = source)) +
  # geom_ribbon(mapping = aes(fill = source), alpha = 0.4) +
  # geom_ribbon(data = filter(tot, source =="Estimate"), fill = col0[1], alpha = 0.4) +
  geom_point(mapping = aes(colour = source)) +
  theme_bw() +
  scale_color_manual(values = col0, drop=FALSE) +
  scale_fill_manual(values = col0, drop=FALSE) +
  scale_x_continuous(breaks = seq(min(dinput$year),max(dinput$year), by = 2)) +
  labs(x= "Year", y= "Migrants (log)", colour = "", fill = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = FALSE)
dev.off()


#Comparison plot

eu_tot_imm <- read_excel("data/migr_pop3ctb.xls", sheet = "Data", skip = 12)[1:38,-c(2,3,4)] %>%
  rename(name = "GEO/TIME") %>%
  gather(year, eu_tot, c(2,3,4,5)) %>%
  mutate(eu_tot = as.numeric(eu_tot),
         year = as.numeric(year)) %>%
  filter(!is.na(eu_tot))

imm_compare_eu_tot <- imm %>%
  filter(year %in% 2014:2017,
         source == "Estimate") %>%
  left_join(eu_tot_imm)


p <- ggplot(data = imm_compare_eu_tot, aes(x = flow, y = eu_tot, colour = as.factor(year))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(breaks = seq(0, max(imm_compare_eu_tot$flow), by = 500000)) +
  scale_y_continuous(breaks = seq(0, max(imm_compare_eu_tot$flow), by = 500000), limits = c(0, 2.8e06)) +
  labs(x = "Estimate",y = "Eurostat total immigration") +
 labs(colour = "Year")

plot_name <- paste0("figures/", folder_name, model_name, "EU_vs_est_imm.pdf")
pdf(plot_name)
p
dev.off()


