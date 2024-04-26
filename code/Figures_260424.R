# Figure 2 ####


tot_d <- dinput %>%
  # bind_rows(tot_un) %>%
  group_by(year, source) %>%
  summarise(flow = sum(flow, na.rm = T)) %>%
  mutate(source_name = case_when(source == "census" ~ "Census (w. Missing)", 
                                 source == "eurostat" ~ "Eurostat (w. Missing)",
                                 source == "fbdau" ~ "Facebook DAU (w. Missing)", 
                                 source == "fbmau" ~ "Facebook MAU (w. Missing)", 
                                 source == "LFS" ~ "LFS (w. Missing)"))


tot <- tot_y1 %>%
  mutate(source_name = "Estimate") %>%
  rename(flow = total) %>%
  bind_rows(tot_d) %>%
  mutate(source_name = fct_relevel(source_name, "Estimate")) %>%
  mutate(flow = ifelse(flow == 0, NA, flow))

table_name <- paste0(folder_name, model_name,"_tot.csv")
# write.csv(tot, table_name, row.names = F)

col0 <- c("darkgrey",  "peru", "red","green3", "royalblue", "magenta2")
shape0 <- c(16, 17, 15, 18, 7, 8)

png("./paper figures/total1.png",  width = 8,  height = 6, units = "in", res = 300)
png("./paper figures/total2.png",  width = 10, height = 8, units = "in", res = 300)
png("./paper figures/total3.png",  width = 10, height = 8, units = "in", res = 300)
png("./paper figures/total4_131223.png",  width = 16, height = 12, units = "in", res = 300)

ggplot(data = tot,
       mapping = aes(x = year, y = flow/1e06, ymin = lwr80/1e06, ymax = upp80/1e06)) +
  geom_line(data = tot %>%  filter(source_name == "Estimate"), mapping = aes(colour = source_name)) +
  geom_ribbon(mapping = aes(fill = source_name), alpha = 0.4) +
  geom_point(data = tot,
             mapping = aes(colour = source_name, shape = source_name), size = 5) +
  theme_bw() +
  scale_color_manual("changed_legend", values = col0, drop=FALSE) +
  scale_shape_manual("changed_legend", values= shape0, drop= FALSE) +
  scale_fill_manual("changed_legend", values = col0, drop=FALSE) +
  scale_x_continuous(breaks = min(dinput$year):max(dinput$year)) +
  scale_y_log10(breaks = seq(1, 17, 1)) +
  labs(x= "Year", y= "Migrant stocks (millions), age 15-64", colour = "", fill = "") +
  guides(fill = FALSE) +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90), 
        text = element_text(size = 32))

dev.off()

# Figure 4 ####


imm_y1   <- imm_y %>% 
  mutate(model = "Main", 
         mig = "Immigrants") %>% 
  rename(flow = total)

emi_y1   <- emi_y %>% 
  mutate(model = "Main", 
         mig = "Emigrants")

imm_y1$name = plyr::revalue(imm_y1$name, c("United Kingdom" = "UK"))
imm_y1$name[imm_y1$name == "United Kingdom"] = "UK"
eu_grid1$name[eu_grid1$name == "Czech Republic"] = "Czechia"
eu_grid1$name[eu_grid1$name == "United Kingdom"] = "UK"


emi_y1$name = plyr::revalue(emi_y1$name, c("United Kingdom" = "UK"))
emi_y1$name[emi_y1$name == "United Kingdom"] = "UK"
#emi_y1$name[emi_y1$name == "Czech Republic"] = "Czechia"
eu_grid1$name[eu_grid1$name == "Czech Republic"] = "Czechia"
eu_grid1$name[eu_grid1$name == "United Kingdom"] = "UK"


d3 <- bind_rows(imm_y1, emi_y1)

ggplot(data = d3,
       mapping = aes(x = year, y = (flow/1e06), ymin = (lwr80/1e06), ymax = (upp80/1e06), 
                     fill = mig, colour = mig)) +
  geom_line() +
  geom_ribbon(aes(fill = mig), alpha = 0.3) +
  facet_geo(facets = "name", grid = eu_grid1) +
  theme_bw() +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(min(dinput$year),max(dinput$year), by = 2)) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(x= "Year", y= "Stocks of EU migrants (in millions), age 15-64", colour = "", fill = "") +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90), 
        text = element_text(size = 18)) +
  # guides(fill = FALSE) +
  ggtitle(paste0("Immigrants and emigrants"))

# Figure 5 ####


bilat_d <- dinput %>% 
  select(year, orig, dest, flow, source) %>%
  mutate(source_name = case_when(source == "census" ~ "Census (w. Missing)", 
                                 source == "eurostat" ~ "Eurostat (w. Missing)",
                                 source == "fbdau" ~ "Facebook DAU (w. Missing)", 
                                 source == "fbmau" ~ "Facebook MAU (w. Missing)", 
                                 source == "LFS" ~ "LFS (w. Missing)"),
         # dest = plyr::revalue(dest, c("Czech Republic" = "Czechia")),
         # orig = plyr::revalue(orig, c("Czech Republic" = "Czechia")),
         orig = plyr::revalue(orig, c("United Kingdom" = "UK")),
         dest = plyr::revalue(dest, c("United Kingdom" = "UK"))) 

bilat <- bilat_y1 %>%
  mutate(source_name = "Estimate", 
         flow = total, 
         orig = plyr::revalue(orig, c("United Kingdom" = "UK")),
         dest = plyr::revalue(dest, c("United Kingdom" = "UK"))) %>%
  bind_rows(bilat_d) %>%
  mutate(source_name = fct_relevel(source_name, "Estimate"),
         flow = ifelse(flow == 0, NA, flow),  
         line_split = source_name) 


bilat_country_emi <- bilat %>% 
  filter(orig == "Poland", 
         dest == "UK" | dest == "Netherlands")


bilat_country_emi$dest = plyr::revalue(bilat_country_emi$dest, c("Czech Republic" = "Czechia"))
bilat_country_emi$dest = plyr::revalue(bilat_country_emi$dest, c("United Kingdom" = "UK"))
bilat_country_emi$dest[bilat_country_emi$dest == "Czech Republic"] = "Czechia"
bilat_country_emi$dest[bilat_country_emi$dest == "United Kingdom"] = "UK"
eu_grid1$name[eu_grid1$name == "Czech Republic"] = "Czechia"
eu_grid1$name[eu_grid1$name == "United Kingdom"] = "UK"


ggplot(data = bilat_country_emi,
       mapping = aes(x = year, y = flow/1e06, ymin = lwr80/1e06, ymax = upp80/1e06)) +
  geom_line(data = bilat_country_emi %>%  filter(source_name == "Estimate"), mapping = aes(colour = source_name)) +
  geom_ribbon(mapping = aes(fill = source_name), alpha = 0.4) +
  geom_point(data = bilat_country_emi,
             mapping = aes(colour = source_name, shape = source_name)) +
  theme_bw() +
  scale_color_manual("changed_legend", values = col0, drop=FALSE) +
  scale_shape_manual("changed_legend", values= shape0, drop= FALSE) +
  scale_fill_manual("changed_legend", values = col0, drop=FALSE) +
  scale_x_continuous(breaks = min(dinput$year):max(dinput$year)) +
  scale_y_log10() +
  labs(x= "Year", y= "Migrant stocks (millions), age 15-64", colour = "", fill = "") +
  guides(fill = FALSE) +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90), 
        text = element_text(size = 32)) +
  facet_grid(~dest)



