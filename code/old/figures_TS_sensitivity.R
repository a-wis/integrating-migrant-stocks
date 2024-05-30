# compare different time series models 

library(tidyverse)

dinput <- read.csv("./data/dinput.csv")


tot0 <- read.csv("S:/Rand/Paper/update_2019/tot_y_main_model.csv") %>% select(-X) %>% mutate(Model = "Main model")
tot1 <- read.csv("./results/Naive_globalW_tot_y.csv") %>% mutate(Model = "Naive with global drift model")
tot2 <- read.csv("./results/Naive2tot_y.csv") %>% mutate(Model = "Naive model")
# tot3 <- read.csv("./results/Naive_cortot_y.csv") %>% mutate(Model = "Corridor specific drift model")
# tot4 <- read.csv("./results/Naive_cor05_tot_y.csv") %>% mutate(Model = "Corridor specific drift model 0.5")
# tot5 <- read.csv("./results/Naivetot_y.csv") %>% mutate(Model = "Corridor & Time specific drift model")

tot <- bind_rows(tot0, tot1, tot2)

ggplot(data = tot, aes(x = year, y = total/1e06, ymin = lwr80/1e06, ymax = upp80/1e06, color = Model)) +
  geom_line() +
  geom_ribbon(mapping = aes(fill = Model), colour = NA , alpha = 0.2) +
  theme_bw() +
  # scale_color_manual(values = col0, drop=FALSE) +
  # scale_fill_manual(values = col0, drop=FALSE) +
  scale_x_continuous(breaks = min(dinput$year):max(dinput$year)) +
  labs(x= "Year", y= "Migrant stocks (millions), 15 - 64", colour = "", fill = "") +
  # ylim(c(0, 1e4)) +
  guides(fill = FALSE)


ggsave("./figures/compare_TS_models.png")
