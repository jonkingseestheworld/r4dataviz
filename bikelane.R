destdir = "geodata"
url = "https://opendata-ajuntament.barcelona.cat/data/dataset/e3497ea4-0bae-4093-94a7-119df50a8a74/resource/4608cf0c-2f11-4a25-891f-c5afc3af82c5/download"
download.file(url = url, destfile = here::here())


library(geojsonio)
library(sf)
library(tidyverse)

bcn <- geojson_sf("/cloud/project/geodata/CARRIL_BICI.geojson")

barcelona <- geojson_sf("/cloud/project/geodata/Bike Routes Barcelona.geojson")
berlin <- geojson_sf("geodata/Bike Routes Berlin.geojson")
london <- geojson_sf("geodata/Bike Routes London.geojson")

topmargin <- 1.75
linesize <- 1.05
color1 <- "#1c4831"
color4 <- "#572016"

LondonPlot <- ggplot() +
  geom_sf(data = london, aes(geometry=geometry), size=linesize, color = color4) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit="cm"))



bcnPlot <- ggplot() +
  geom_sf(data= bcn, aes(geometry=geometry), size=linesize, color = color1) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit="cm"))
