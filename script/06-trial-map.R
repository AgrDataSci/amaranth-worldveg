# ..........................................
# Prepare map with tricot trial location
# ..........................................
# ..........................................
library("tidyverse")
library("raster")
library("sf")
library("geodata")

dat = read.csv("data/amaranth-tricot-data.csv")

# ADM data
# shape with adm units
adm = st_read("data/gadm/africa/africa_adm0.shp")
adm = st_as_sf(adm)
adm = adm[-c(2:3)]
adm

st_bbox(adm)

adm = st_crop(adm, st_bbox(c(xmin = -17.5, ymin = -15, 
                             xmax = 45,  ymax = 26)))

adm = adm[adm$ADMIN != "Madagascar", ]

plot(adm)

coord = dat[,c("longitude", "latitude")]
coord$technology = "Amaranth"
coord = na.omit(coord)

ggplot() +
  geom_sf(adm$geometry,
          mapping = aes(), 
          colour = "white", 
          fill = "#4d4d4d") +
  geom_jitter(data = coord, aes(x = longitude,
                               y = latitude, 
                               color = technology),
             size = 1) +
  theme_void() +
  scale_color_manual(values = c('#de2d26')) +
  theme(legend.position = "none")

ggsave(filename = "output/map-tricot-plots.png",
       width = 10,
       height = 10,
       units = "cm")

ggsave(filename = "output/map-tricot-plots.pdf",
       width = 15,
       height = 15,
       units = "cm")

