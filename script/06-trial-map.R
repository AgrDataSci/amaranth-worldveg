# ..........................................
# Prepare map with tricot trial location
# ..........................................
# ..........................................
library("tidyverse")
library("raster")
library("sf")
library("geodata")
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/refs/heads/master/modules/01_functions.R")

dat = read.csv("data/amaranth-tricot-data.csv")

# ADM data
# shape with adm units
adm = st_read("data/gadm/africa/africa_adm0.shp")
adm = st_as_sf(adm)
adm = adm[-c(2:3)]
adm

st_bbox(adm)

# adm = st_crop(adm, st_bbox(c(xmin = -17.5, ymin = -15, 
#                              xmax = 45,  ymax = 26)))

#adm = adm[adm$ADMIN != "Madagascar", ]

plot(adm)

coord = dat[,c("longitude", "latitude")]
coord$technology = "Amaranth"
coord = na.omit(coord)

plot_map(coord, c("longitude", "latitude"), minimap = F, map_provider = "OpenStreetMap.Mapnik")

# to ensure the privacy of participants location
# we can put the lonlat info into clusters of 0.5 resolution
h = dist(coord)

h = hclust(h)

h = cutree(h, h = 0.7)

# split the d by each defined cluster
d = split(coord[, 1:2], h)

# and take the mean 
d = lapply(d, function(x) {
  colMeans(x)
})

# back to data frame
d = do.call("rbind", d)

d = as.data.frame(d)

names(d) = c("longitude", "latitude")

coords = st_as_sf(d, coords = c("longitude", "latitude"), crs = 4326)

ggplot() +
  geom_sf(data = adm, fill = "#ffffe5", color = "black", alpha = 1) +
  geom_sf(data = coords, color = "blue", size = 1.5, shape = 21, fill = "red", stroke = 1) +
  theme_minimal() 

ggsave(filename = "output/map-tricot-plots.png",
       width = 10,
       height = 10,
       units = "cm")

ggsave(filename = "output/map-tricot-plots.pdf",
       width = 15,
       height = 15,
       units = "cm")

