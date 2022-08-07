#install.packages("geofacet")
library("tidyr")
library("dplyr")
library("ggplot2")

nordics <- c("Denmark", "Finland", "Iceland", "Norway", "Sweden")

pal <- c("#913238", "#3C6698", "#C4343A", "#091F57", "#D1AD38") # DNK, FIN, ISL, NOR, SWE
f2 <- "Fira Sans Condensed"


geofacet::europe_countries_grid2 %>% 
  ggplot(aes(col, -row)) +
  geom_tile(aes(fill = if_else(name%in% nordics, name, "x")), width = 0.95, height = 0.9) +
  geom_text(aes(label = code), size= 3, color = "white", family = f2, fontface = "bold") +
  scale_fill_manual(values = c(pal, "#CEBEB5")) +   ## CEBEB5 is greyish brown for non-nordic countries
  theme_void() +
  theme(legend.position = "none")


geofacet::world_countries_grid1 %>% 
  ggplot(aes(col, -row)) +
  geom_tile( width = 0.95, height = 0.9)
