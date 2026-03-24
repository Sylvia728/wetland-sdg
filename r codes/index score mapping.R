library(readxl)
library(tidyverse)
library(countrycode)
library(sf)
library(rnaturalearth)
library(ggpubr)
library(cowplot)

SDG_data_panel<- read_xlsx('D:/Users/Sylvia/Desktop/wetlandSDGs/data/00result/SDG.xlsx',sheet = 1)

sdg_cols <- grep("SDG\\d+", colnames(SDG_data_panel), value = TRUE)

COLOR_SCHEMES <- list(
  SDG = list("#E66D50","#F3A361","#8AB07C","#299D8F","#297270","#174753"),
  SDG1 = list(low = "#efd1d5", high = "#E5243B"),
  SDG2 = list(low = "#f4ead6", high = "#DDA63A"),
  SDG3 = list(low = "#cce9c4", high = "#4C9F38"),
  SDG4 = list(low = "#edb4bb", high = "#C5192D"),
  SDG5 = list(low = "#fdc7c0", high = "#FF3A21"),
  SDG6 = list(low = "#cbf3fd", high = "#26BDE2"),
  SDG6_2 =list(low = "#eaa586", mid="white", high = "#26BDE2"),
  SDG7 = list(low = "#f4ead6", high = "#FCC30B"),
  SDG8 = list(low = "#ffc7de", high = "#A21942"),
  SDG9 = list(low = "#ffd2be", high = "#FD6925"),
  SDG10 = list(low = "#ffcfdd", high = "#DD1367"),
  SDG10_2 =list(low = "#b0e2cd", mid="white", high ="#DD1367"),
  SDG11 = list(low = "#ffe7c8", high = "#FD9D24"),
  SDG12 = list(low = "#decfb5", high = "#BF8B2E"),
  SDG12_2 =list(low = "#b2c4e6", mid="white", high ="#BF8B2E"),
  SDG13 = list(low = "#b8cab9", high = "#3F7E44"),
  SDG13_2 = list(low = "#983490" , mid="white", high = "#3F7E44"),
  SDG14 = list(low = "#c4e4f3", high = "#0A97D9"),
  SDG14_2 =list(low = "#ce7b55", mid="white", high ="#0A97D9"),
  SDG15 = list(low = "#d9ffc9", high = "#56C02B"),
  SDG15_2 =list(low = "#dea8f4", mid="white", high ="#56C02B"),
  SDG16 = list(low = "#c6deea", high = "#00689D"),
  SDG17 = list(low = "#afc5d5", high = "#19486A")
)

create_map <- function(world_data, fill_var, sdg_type) {
  ggplot(world_data) +
    geom_sf(aes(fill = {{fill_var}}), color = "white", size = 0.05) +
    scale_fill_steps(
      low = COLOR_SCHEMES[[sdg_type]]$low,
      high = COLOR_SCHEMES[[sdg_type]]$high,
      na.value = "lightgray", 
      n.breaks = 5,
      guide = guide_colorsteps(
        direction = "horizontal",  
        barheight = unit(0.2, "cm"),
        barwidth = unit(4, "cm"),
        title.position = "top",
        label.theme = element_text(size = 8),
        title.theme = element_text(size = 10)
      )) +
    
    theme_void()+
    theme(
      legend.position = "bottom",       
      legend.justification = "center", 
      legend.title.align = 0.5
    )}

create_map_2 <- function(world_data, fill_var, sdg_type) {
  ggplot(world_data) +
    geom_sf(aes(fill = {{fill_var}}), color = "white", size = 0.05) +
    scale_fill_steps2(
      low = COLOR_SCHEMES[[sdg_type]]$low,
      mid = COLOR_SCHEMES[[sdg_type]]$mid,
      high = COLOR_SCHEMES[[sdg_type]]$high,
      na.value = "lightgray", 
      n.breaks = 6,
      guide = guide_colorsteps(
        direction = "horizontal",  
        barheight = unit(0.2, "cm"),
        barwidth = unit(4, "cm"),
        title.position = "top",
        label.theme = element_text(size = 8),
        title.theme = element_text(size = 10)
      )
    ) +
    theme_void()+
    theme(
      legend.position = "bottom",       
      legend.justification = "center", 
      legend.title.align = 0.5,
    )}

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(ISO_A3 = countrycode(iso_a3_eh, "iso3c", "iso3c"))

world_ag <- world %>%
  left_join(SDG_data_panel, by = "ISO_A3")

map1 <- create_map(world_ag, SDG1, "SDG1")
map2 <- create_map(world_ag, SDG2, "SDG2")
map3 <- create_map(world_ag, SDG3, "SDG3")
map4 <- create_map(world_ag, SDG4, "SDG4")
map5 <- create_map(world_ag, SDG5, "SDG5")
map6 <- create_map_2(world_ag, SDG6, "SDG6_2")
map7 <- create_map(world_ag, SDG7, "SDG7")
map8 <- create_map(world_ag, SDG8, "SDG8")
map9 <- create_map(world_ag, SDG9, "SDG9")
map10 <- create_map_2(world_ag, SDG10, "SDG10_2")
map11 <- create_map(world_ag, SDG11, "SDG11")
map12 <- create_map_2(world_ag, SDG12, "SDG12_2")
map13 <- create_map_2(world_ag, SDG13, "SDG13_2")
map14 <- create_map_2(world_ag, SDG14, "SDG14_2")
map15 <- create_map_2(world_ag, SDG15, "SDG15_2")
map16 <- create_map(world_ag, SDG16, "SDG16")
map17 <- create_map(world_ag, SDG17, "SDG17")

plots_list <- mget(paste0("map", 1:17))

ggarrange(
  plotlist = plots_list,
  nrow = 9,
  ncol = 2
)

ggsave("D:/Users/Sylvia/Desktop/wetlandSDGs/resutls/section2/indicators/17SDGs.jpg", 
       width = 6, height =20, dpi = 600)
