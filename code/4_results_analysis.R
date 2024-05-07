##  manipulation and saving of model results
## total, imm and emi plots
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
# this package was not updated for newer versions of R - proceed with caution
library(ggplus)
library(rayshader)

# source data: dinput
# for each model, read the data file by sourcing
source("code/read_data/S1_read.data.R")
# source("code/read_data/S3_read.data.R")
# source("code/read_data/S4_read.data.R")
# source("code/read_data/S5_read.data.R")
# source("code/read_data/S6_read.data.R")
# source("code/read_data/S7_read.data.R")
# source("code/read_data/oo1_read.data.R")



dinput_source_year <- dinput %>%
  select(year, source, stock) %>%
  group_by(year, source) %>%
  summarise(stock = sum(stock, na.rm = T))

# functions for building summary stats
source("code/0_read_functions.R")

# data frames for building summary states
corridors <- dinput %>%
  select(orig, dest, corridor) %>%
  distinct()

years <- dinput %>%
  select(year) %>%
  mutate(node_id = 1:n())

y <- ggs(m, family = "^y1") %>%
  add_node() %>%
  add_corridor() %>%
  left_join(years)

#name the model
model_name <- "main-model"
folder_name <- "figures"


##
## totals
##
# calculate stats for y


tot_y <- y_stats(y)

# 15-64
tot_d <- dinput %>%
  group_by(year, source) %>%
  summarise(stock = sum(stock, na.rm = T)) %>%
  mutate(source_name = source,
  source_name = ifelse(source_name == "eurostat", "Eurostat", source_name))

tot <- tot_y %>%
  mutate(source_name = "Estimate") %>%
  rename(stock = total) %>%
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
  mutate(stock = ifelse(stock == 0, NA, stock))

# READ ####
#if you want to save, uncomment
# this needs to be saved for validation
# table_name <- paste0("results/", model_name,"_tot_y.csv")
# write.csv(tot, table_name, row.names = F)



##
## immm and emi stocks
##
imm_y <- y_stats(y, type = "imm")

imm_d <- dinput %>%
  group_by(year, dest, source) %>%
  summarise(stock = sum(stock, na.rm = T)) %>%
  mutate(source_name = source,
         source_name = ifelse(source_name == "eurostat", "Eurostat", source_name)) %>%
  rename(name = dest) %>%
  select(year, name, stock, source, source_name )


imm <- imm_y %>%
  mutate(source_name = "Estimate") %>%
  rename(stock = total) %>%
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
         stock = ifelse(stock == 0, NA, stock))

imm$name = plyr::revalue(imm$name, c("Czech Republic" = "Czechia"))
imm$name = plyr::revalue(imm$name, c("United Kingdom" = "UK"))
imm$name[imm$name == "Czech Republic"] = "Czechia"
imm$name[imm$name == "United Kingdom"] = "UK"
eu_grid1$name[eu_grid1$name == "Czech Republic"] = "Czechia"
eu_grid1$name[eu_grid1$name == "United Kingdom"] = "UK"

# table_name <- paste0("results/", model_name,"_imm_y.csv")
# write.csv(imm, table_name, row.names = F)



emi_y <- y_stats((y), type = "emi") %>%
  rename(stock = total)

emi_d <- dinput %>%
  group_by(year, orig, source) %>%
  summarise(stock = sum(stock, na.rm = T)) %>%
  mutate(source_name = source) %>%
  rename(name = orig) %>%
  select(year, name, stock, source, source_name )

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
         stock = ifelse(stock == 0, NA, stock))

emi$name = plyr::revalue(emi$name, c("Czech Republic" = "Czechia"))
emi$name = plyr::revalue(emi$name, c("United Kingdom" = "UK"))

emi$name[emi$name == "United Kingdom"] = "UK"

emi$name[emi$name == "Czech Republic"] = "Czechia"
eu_grid1$name[eu_grid1$name == "Czech Republic"] = "Czechia"
eu_grid1$name[eu_grid1$name == "United Kingdom"] = "UK"


# table_name <- paste0("results/", model_name,"_emi_y.csv")
# write.csv(emi, table_name, row.names = F)




##
## bilat stocks
##
bilat_y <- y %>%
  y_stats(type = "bilat")


bilat <- bilat_y %>%
  rename(stock = total) %>%
  mutate(source = "Estimate", line_split = source) %>%
  bind_rows(dinput %>% select(year, orig, dest, stock, source) %>%  mutate(line_split = source)) %>%
  mutate(source = fct_relevel(source, "Estimate"),
         source_name = source,
         source_name = ifelse(source == "eurostat", "Eurostat", source_name),
         source_name = ifelse(source == "dau", "Facebook DAU", source_name),
         source_name = ifelse(source == "mau", "Facebook MAU", source_name),
         source_name = ifelse(source == "Estimate", "Estimate", source_name),
         source_name = ifelse(source == "LFS", "LFS", source_name),
         source_name = ifelse(source == "census", "Census", source_name),
         dest = plyr::revalue(dest, c("Czech Republic" = "Czechia")),
         dest = plyr::revalue(dest, c("United Kingdom" = "UK")))

# table_name <- paste0("results/", model_name,"_bilat_y.csv")
# write.csv(bilat, table_name, row.names = F)


# Figures ####
# Total 

col0 <- c("darkgrey", "red", "peru", "green3", "royalblue", "magenta2")
p <- ggplot(data = (tot %>% filter(year>=2011, source != "UN DESA (total population)" )),
            mapping = aes(x = year, y = stock/1e06, ymin = lwr80/1e06, ymax = upp80/1e06)) +
  # geom_line(mapping = aes(colour = source)) +
  geom_ribbon(mapping = aes(fill = source), alpha = 0.4) +
  geom_point(data = tot %>% filter(year >= 2011),
             mapping = aes(colour = source)) +
  theme_bw() +
  scale_color_manual(values = col0, drop=FALSE) +
  scale_fill_manual(values = col0, drop=FALSE) +
  scale_x_continuous(breaks = min(dinput$year):max(dinput$year)) +
  labs(x= "Year", y= "Migrant stocks (millions), 15 - 64", colour = "", fill = "") +
  guides(fill = FALSE)


# plot_name <- paste0("figures/", model_name,"_total_80ci.pdf")
# pdf(plot_name)
p
# dev.off()

# imm 

col0 <- c("darkgrey", "peru", "red", "green3", "royalblue", "magenta2")
scaleFUN <- function(x) sprintf("%.2f", x)
p <- ggplot(data = imm %>% filter(year>=2011),
            mapping = aes(x = year, y = stock/1e06, ymin = lwr80/1e06, ymax = upp80/1e06)) +
  facet_geo(facets = "name", grid = eu_grid1) +
  geom_line(mapping = aes(colour = source)) +
  geom_ribbon(mapping = aes(fill = source), alpha = 0.4) +
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


# plot_name <- paste0("results/", model_name,"imm2.png")
# png(plot_name, width = 10, height = 10, units = "in", res = 300)
p
# dev.off()


# emi


col0 <- c("darkgrey", "peru", "red", "green3", "royalblue", "magenta2")

p <- ggplot(data = emi %>% filter(year>=min(dinput$year)),
            mapping = aes(x = year, y = stock/1e06, ymin = lwr80/1e06, ymax = upp80/1e06)) +
  # mapping = aes(x = as.integer(year), y = stock/1e06, ymin = (stock - sd)/1e06, ymax = (stock + sd)/1e06)) +
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



# plot_name <- paste0("figures/", folder_name,model_name,"emi.png")
# png(plot_name, width = 10, height = 10, units = "in", res = 300)
p
# dev.off()


# bilat
ggplot(data = bilat,
       mapping = aes(x = year, y = stock/1e06, ymin = (stock-sd)/1e06, ymax = (stock + sd)/1e06,
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
  rename(stock = total) %>%
  mutate(source = "Estimate", line_split = source) %>%
  bind_rows(dinput %>% select(year, orig, dest, stock, source) %>%  mutate(line_split = source)) %>%
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

# plotname <- paste0("figures/", country, "_logimm_11_7.pdf")

# pdf(plotname, height = 7, width = 11)
ggplot(data = bilat_country_imm %>% filter(year>=min(dinput$year)),
       mapping = aes(x = year, y = log(stock), ymin = log(lwr80), ymax = log(upp80)))+
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
# dev.off()


# Netherlands total immigration
total_country_imm <- imm %>%
   filter(name == country)

col0 <- c("darkgrey",  "peru", "red","green3", "royalblue", "magenta2", "blue")

p <- ggplot(data =(total_country_imm %>% filter(source_name != "mau", source_name != "dau")),
            mapping = aes(x = year, y = stock/1e06, ymin = lwr80/1e06, ymax = upp80/1e06)) +
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


p



#emi
bilat_country_emi <- bilat_y %>%
  rename(stock = total) %>%
  # mutate(period = 1, source = "Estimate", line_split = source) %>%
  mutate(source = "Estimate", line_split = source) %>%
  bind_rows(dinput %>% select(year, orig, dest, stock, source) %>%  mutate(line_split = source)) %>%
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
       mapping = aes(x = year, y = log(stock), ymin = log(lwr80), ymax = log(upp80)))+
  facet_geo(facets = "dest", grid = eu_grid1) +
  geom_line(mapping = aes(colour = source)) +
  geom_ribbon(mapping = aes(fill = source), alpha = 0.4) +
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
       mapping = aes(x = year, y = log(stock)))+
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



