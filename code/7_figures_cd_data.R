library(tidyverse)
library(countrycode)
library(RColorBrewer)
library(shades)

d0 <- read_csv("./data/paper_allmodels_bilat.csv") %>%
  rename(stock = total) %>%
  relocate(orig, dest, stock, year) %>%
  filter(model == "Main")


d1 <- d0 %>%
  select(orig, dest) %>%
  pivot_longer(cols = 1:ncol(.)) %>%
  distinct(value) %>%
  rename(name = value) %>%
  mutate(alpha2 = countrycode(sourcevar = name,
                              origin = 'country.name', 
                              destination = 'iso2c'),
         alpha3 = countrycode(sourcevar = name,
                              origin = 'country.name', 
                              destination = 'iso3c'),
         reg_name = countrycode(sourcevar = name, 
                                origin = 'country.name', 
                                destination = 'un.regionsub.name'),
         reg_name = ifelse(reg_name == "Western Asia", "Southern Europe", reg_name))

pal0 <- brewer.pal(n = d1 %>%
                     select(reg_name) %>%
                     distinct() %>%
                     nrow(),
                   name = "Set1")  

r <- d1 %>%
  distinct(reg_name) %>%
  pull() %>%
  .[c(4, 1, 3, 2)]

d2 <- d1 %>%
  mutate(reg_name = fct_relevel(reg_name, r)) %>%
  arrange(reg_name) %>%
  group_by(reg_name) %>%
  mutate(order0 = cur_group_id(),
         order1 = 1:n(), 
         n = n(), 
         gap = ifelse(reg_name == lead(reg_name), 2, 6),
         gap = ifelse(is.na(gap), 6, gap)) %>%
  rowwise() %>%
  mutate(
    col0 = pal0[order0],
    col1 = gradient(shades = c("black", col0, "white"), steps = n + 10) %>%
      .[5 + order1] 
  )

write_csv(d2, "./data/cd_reg.csv")
write_csv(d0, "./data/cd_bilat.csv")

# flags 
for(i in 1:nrow(d2)){
  download.file(url = paste0("https://raw.githubusercontent.com/lipis/flag-icons/main/flags/4x3/",
                             str_to_lower(d2$alpha2[i]), ".svg"), 
                destfile = paste0("./figures/flags/", d2$alpha2[i], ".svg"))
}


