library(tidyverse)
library(tweenr)
library(migest)
library(circlize)
library(magick)

d0 <- read_csv("./data/cd_bilat.csv") %>%
  filter(year == 2019) %>%
  mutate(stock = stock/1e6)
d1 <- read_csv(paste0("./data/cd_reg.csv")) %>%
  mutate(gap = as.numeric(gap),
         gap = ifelse(gap == 6, 5, gap),
         last = n == order1)

rr <- d1 %>% 
  pull(reg_name) %>%
  unique() 

reg_max <- d0 %>%
  filter(year == 2019) %>%
  complete(orig, dest, fill = list(stock = 0)) %>%
  sum_country(flow_col = "stock") %>%
  select(country, turn) %>%
  deframe()
# flags <- vector(mode = "list", length = nrow(d1))
# names(flags) <- d1$alpha2
# flags[length(flags)] <- NULL
# for(j in 1:length(flags)){
#   flags[[j]] <- image_read(paste0("E:\\ADRI\\project\\flags\\4x3-border\\", d1$alpha2[j], ".svg"))
# }
# 

# # # for paper - not videos - free margin
# d2 <- d0 %>%
#   filter(year == 2019)
#   mutate(.frame = year0 - min(year0))

# reg_max <- d0 %>%
#   group_by(year0) %>%
#   complete(orig, dest, fill = list(stock = 0)) %>%
#   sum_country(flow_col = "stock") %>%
#   ungroup() %>%
#   mutate(tot = imm + emi) %>%
#   group_by(country) %>%
#   summarise(tot_max = max(tot)) %>%
#   deframe()

# d3 <- d0 %>%
#   group_by(year0) %>%
#   summarise(stock = round(sum(stock), 1)) %>%
#   mutate(stock = sprintf("%.1f", stock))

##
## plot
##

pdf(file = "./figures/cd_paper.pdf", height = 7, width = 7)
circos.clear()
par(mar = rep(0, 4), cex=1)
circos.par(start.degree = 90, track.margin=c(-0.2, 0.2), 
           points.overflow.warning = FALSE, gap.degree = d1$gap)

chordDiagram(
  x = d0 %>%
    select(orig, dest, stock), 
  directional = 1, order = d1$name,
  grid.col = d1 %>%
    select(name, col1) %>%
    deframe(), 
  annotationTrack = "grid",
  preAllocateTracks = 
    list(track.height = uh(0.25, "mm"),
         track.margin = c(uh(0, "mm"), uh(0, "mm"))),
  transparency = 0.25,  annotationTrackHeight = c(0.05, 0.1),
  direction.type = c("diffHeight", "arrows"), 
  link.arr.type = "big.arrow",
  diffHeight  = -0.04, link.sort = TRUE, 
  link.largest.ontop = TRUE)

for(r in rr){
  highlight.sector(
    track.index = 1, 
    sector.index = d1 %>% 
      filter(reg_name == r) %>%
      pull(name), 
    facing = "bending", text = r, cex = 1, text.vjust = "5mm", col = "transparent")
}


# add labels and axis
circos.trackPlotRegion(track.index = 2, bg.border = NA, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  sector.index = get.cell.meta.data("sector.index")
  
  reg_lab = d1 %>% 
    filter(name == sector.index) %>% 
    pull(alpha3)
  reg_cod = d1 %>%
    filter(name == sector.index) %>%
    pull(alpha2) 
  reg_flag = image_read(path = paste0("./figures/flags/", reg_cod, ".svg"))
  
  theta = circlize(x = mean(xlim), y = 1.3)[1, 1] %% 360
  flag_rot = ifelse(theta < 90 || theta > 270, -90, 90)
  
  reg_last  <- d1 %>%
    filter(name == sector.index) %>%
    pull(last)
  circos.raster(
    image = image_rotate(image = reg_flag, degrees = flag_rot ),
    x = mean(xlim), y = 4.75, width = "0.3cm", facing = "inside"
  )
  circos.lines(
    x = xlim + 
      c(0, ifelse(reg_last, 0, min(d1$gap) * sum(reg_max) /  (360 - sum(d1$gap)))),
    # each degree is worth sum(reg_max) /  (360 - sum(d1$gap))) 
    y = c(5.5, 5.5), lwd = 2,
    col = d1 %>%
      filter(name == sector.index) %>%
      pull(col0))
  
  circos.text(x = mean(xlim),
              y = 3,
              facing = "clockwise",
              labels = reg_lab, cex = 0.9, niceFacing = TRUE)
  circos.axis(h = "top",
              major.at = seq(from = 0, to = max(reg_max)+1, by = 0.5),
              minor.ticks = FALSE, 
              labels.cex = 0.7,
              # labels.pos.adjust = FALSE,
              labels.niceFacing = FALSE)
  circos.axis(h = "top", 
              major.at = seq(from = 0, to = max(reg_max)+1, by = 0.5),
              minor.ticks = 4,
              labels = NULL )
})

dev.off()



