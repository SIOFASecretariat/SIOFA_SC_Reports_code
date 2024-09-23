
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # suppress math annotation
op <- options(gvis.plot.tag='chart')  # set gViz options
# load packages
require(OpenStreetMap)
require(DT)
require(RColorBrewer)
require(mapproj)
require(RgoogleMaps)
require(scales)
require(rworldmap)
require(maps)
require(tidyverse)
require(rnaturalearth)
require(rnaturalearthdata)
require(rgeos)
require(ggspatial)
require(maptools)
require(leaflet)
require(tmap)
require(here)
require(rgdal)
require(scales)
require(flextable)
require(sf)
require(sp)
require(spatialEco)
require(openxlsx)
require(writexl)
require(terra) 
require(spData)
require(spDataLarge)
require(readxl)
require(ggplot2)
require(cowplot)
require(googleway)
require(ggrepel)
require(ggspatial)
require(raster)
require(RColorBrewer)
require(wesanderson)
require(dplyr)
require(rclipboard)
require(classInt)
require(viridis)
require(units)

#load general data
setwd("D:/SIOFA/Data")   #### !!!check that this points to the parent folder!!!
sf::sf_use_s2(TRUE)

# load a number of base layers
land <- st_read('natural_earth/land')
ocean <- st_read('natural_earth/ocean')
bathy <- st_read('natural_earth/bathy')
grid <- st_read('natural_earth/grid/ne_50m_graticules_10.shp')
islands <- st_read('natural_earth/islands')
footprint <- st_read('PAEWG footprint within SIOFA')
fishing_area <- st_read('RFB_SIOFA')
fishing_boundaries <- st_read('siofa_subareas_edited')
st_crs(fishing_boundaries) <- 4326
st_crs(footprint) <- 4326


### reanalyze SIOFA footprint in R

# load data
fishing_footprint <- read_excel("Footprint/HBH footprint data_updated.xlsx", col_types = c("text", "text", "numeric","text","numeric","numeric"))

# Convert the fishing footprint dataframe to a spatial object. Note that the
# crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object
# and that missing values are removed
# exclude midwater trawls and handlines after MoP9 decisions
fishing_footprint <- filter(fishing_footprint, (!Gear == "Midwater trawls (nei)"))
fishing_footprint <- filter(fishing_footprint, (!Gear == "Single boat midwater otter trawls"))
fishing_footprint <- filter(fishing_footprint, (!Gear == "Handlines and hand-operated pole-and-lines"))
fishing_footprint <- filter(fishing_footprint, (!is.na(fishing_footprint$Longitude)))


fishing_footprint <- st_as_sf(fishing_footprint, coords = c("Longitude", "Latitude"), crs = 4326) 
class(fishing_footprint)
st_crs(fishing_footprint) <- 4326
fishing_footprint <- st_intersection(fishing_footprint, fishing_area)

st_write(fishing_footprint, "Footprint/SC8_footprint/raw_footprint_events.shp", driver="ESRI Shapefile", delete_layer=TRUE) 
write_xlsx(fishing_footprint,"Footprint/SC8_footprint/footprint_data.xlsx")

# load 30 minutes square grid, subset it for the SIOFA area only

grid30 <- st_read('30minutes_squares')
st_crs(grid30) <- 4326

# load 20 minutes square grid, subset it for the SIOFA area only

grid20 <- st_read('20minutes_squares')
st_crs(grid20) <- 4326

# calculate number of fishing events per 30 minutes grid cell

footprint_poly30 <- st_join(fishing_footprint, grid30)
count30 <- footprint_poly30 %>% group_by(id) %>% count()
count30 <- st_drop_geometry(count30)
footprint_poly30 <- left_join(grid30, count30, by="id")
footprint_poly30 <- filter(footprint_poly30, (!is.na(footprint_poly30$n)))
st_crs(footprint_poly30) <- 4326
st_write(footprint_poly30, "Footprint/SC8_footprint/footprint_poly30.shp", driver="ESRI Shapefile", delete_layer=TRUE) 

# calculate number of fishing events per 20 minutes grid cell

footprint_poly20 <- st_join(fishing_footprint, grid20)
count20 <- footprint_poly20 %>% group_by(id) %>% count()
count20 <- st_drop_geometry(count20)
footprint_poly20 <- left_join(grid20, count20, by="id")
footprint_poly20 <- filter(footprint_poly20, (!is.na(footprint_poly20$n)))
st_crs(footprint_poly20) <- 4326
st_write(footprint_poly20, "Footprint/SC8_footprint/footprint_poly20.shp", driver="ESRI Shapefile", delete_layer=TRUE) 

## add THA data (spatial layer) 
THA_footprint_20 <- st_read('THA_footprint/Merged THA footprint 20')
THA_footprint_30 <- st_read('THA_footprint/Merged THA footprint 30')
THA_footprint_20 <- st_union(THA_footprint_20)
THA_footprint_30 <- st_union(THA_footprint_30)

## select THA only data
fishing_footprint_THA <- filter(fishing_footprint, Flag == "THA")
# calculate number of fishing events per 30 minutes grid cell
footprint_poly30_THA <- st_join(fishing_footprint_THA, grid30)
count30_THA <- footprint_poly30_THA %>% group_by(id) %>% count()
count30_THA <- st_drop_geometry(count30_THA)
footprint_poly30_THA <- left_join(grid30, count30_THA, by="id")
footprint_poly30_THA <- filter(footprint_poly30_THA, (!is.na(footprint_poly30_THA$n)))
st_crs(footprint_poly30_THA) <- 4326
footprint_poly30_THA <- st_union(footprint_poly30_THA, THA_footprint_30)
st_write(footprint_poly30_THA, "Footprint/SC8_footprint/THA/footprint_poly30_THA.shp", driver="ESRI Shapefile", delete_layer=TRUE) 
# calculate number of fishing events per 20 minutes grid cell
footprint_poly20_THA <- st_join(fishing_footprint_THA, grid20)
count20_THA <- footprint_poly20_THA %>% group_by(id) %>% count()
count20_THA <- st_drop_geometry(count20_THA)
footprint_poly20_THA <- left_join(grid20, count20_THA, by="id")
footprint_poly20_THA <- filter(footprint_poly20_THA, (!is.na(footprint_poly20_THA$n)))
st_crs(footprint_poly20_THA) <- 4326
footprint_poly20_THA <- st_union(footprint_poly20_THA, THA_footprint_20)
st_write(footprint_poly20_THA, "Footprint/SC8_footprint/THA/footprint_poly20_THA.shp", driver="ESRI Shapefile", delete_layer=TRUE) 

## create hybrid map, delete all squares without fishing activity, clip out of boundaries
## create final hybrid footprint, including any new CCPs data

footprint_poly30 <- st_union(footprint_poly30)
footprint_poly20 <- st_union(footprint_poly20)
footprint_poly30 <- st_union(footprint_poly30, THA_footprint_30)
#footprint_poly20 <- st_union(footprint_poly20, THA_footprint_20)
footprint_hybrid <- st_union(footprint_poly30, footprint_poly20)
footprint_hybrid <- st_union(footprint_hybrid)
#footprint_hybrid_final <- st_make_valid(footprint_hybrid)
#sf::sf_use_s2(FALSE)
footprint_hybrid_final <- st_intersection(footprint_hybrid, fishing_area)
st_write(footprint_hybrid_final, "Footprint/SC8_footprint/hybrid_footprint_final.shp", driver="ESRI Shapefile", delete_layer=TRUE) 


## plot a map of the new footprint for comparison
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries, fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = footprint_hybrid_final, fill = "chartreuse") +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Updated SIOFA bottom fishing footprint (hybrid 20'+30', 1977–2020) 
          midwater and handline gears excluded") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("Footprint/SC8_footprint/SIOFAmap_SC8_footprint_hybrid_R_web.png", width = 10, height = 8, dpi = 150)


## plot a map of the new and old footprints for comparison
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = footprint, aes(fill = "orchid3")) +
  geom_sf(data = footprint_hybrid_final, aes(fill = "chartreuse")) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Updated SIOFA bottom fishing footprint (hybrid 20'+30', 1977–2020) 
          midwater and handline gears excluded
          compared with the interim footprint") +
  scale_fill_manual(values = c('chartreuse','orchid3'), labels = c('Updated footprint', 'Interim footprint'))  +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("Footprint/SC8_footprint/SIOFAmap_SC8_footprint_hybrid_comparison_R_web.png", width = 10, height = 8, dpi = 150)

## calculate area of updated footprint and of PAEWG footprint for comparison
sf::sf_use_s2(FALSE)
#calculate areas and compare
#set_units(st_area(p), "km^2")
new.footprint.area <- st_area(footprint_hybrid_final)
new.footprint.area <- sum(new.footprint.area)
old.footprint.area <- st_area(footprint)
new.footprint.area = set_units(new.footprint.area, "km^2")
old.footprint.area = set_units(old.footprint.area, "km^2")
ratio <- new.footprint.area/old.footprint.area

SIOFA.area <- st_area(fishing_area)
SIOFA.area = set_units(SIOFA.area, "km^2")
SIOFA.area <- sum(SIOFA.area)
new.footprint.area/SIOFA.area*100

fishable_area <- st_read('SIOFA_bathy')
shallow_area <- st_area(fishable_area)
shallow_area <- set_units(shallow_area, "km^2")
shallow_area <- sum(shallow_area)
shallow_area/SIOFA.area*100

shallow_area_footprint <- st_read('SIOFA_bathy/shallow_under_footprint')
shallow_area_footprint <- st_area(shallow_area_footprint)
shallow_area_footprint <- set_units(shallow_area_footprint, "km^2")
shallow_area_footprint <- sum(shallow_area_footprint)
shallow_area_footprint/shallow_area*100
shallow_area-shallow_area_footprint