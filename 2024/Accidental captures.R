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

# load data
setwd("D:/SIOFA/Data")   #### !!!check that this points to the parent folder!!!
captures <- read_excel("Accidental-captures-2023.xlsx", col_types = c("numeric", "text", "numeric", "numeric","numeric","numeric","numeric", "text", "text", "numeric", "numeric", "text", "text","text","numeric","numeric","numeric","numeric","numeric","numeric","text","text"))
observations <- read_excel("Bird-observations-2023.xlsx", col_types = c("numeric", "text", "text", "numeric", "numeric", "numeric","numeric", "numeric", "text", "numeric","numeric","text", "text","text","numeric", "text","numeric"))

captures <- captures %>%
  filter(Year<=2022)

observations <- observations %>%
  filter(Year<=2022)

# Convert the observation dataframe to a spatial object. Note that the
# crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object
# and that missing values are removed
captures_spatial <- filter(captures, (!is.na(captures$Longitude)))
captures_spatial <- st_as_sf(captures_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 
#take out sharks
#captures_spatial <- filter(captures_spatial,(CaptureType != "sharks"))
class(captures_spatial)
st_crs(captures_spatial) <- 4326

# load a number of base layers
land <- st_read('natural_earth/land')
ocean <- st_read('natural_earth/ocean')
bathy <- st_read('natural_earth/bathy')
grid <- st_read('natural_earth/grid/ne_50m_graticules_10.shp')
islands <- st_read('natural_earth/islands')
fishing_boundaries <- st_read('siofa_subareas_edited')

# plot world map and add SIOFA subareas + SIOFA accidental captures on world map
# delete sharks not in the CMM (WGSUM1)

captures_spatial_plot <- captures_spatial  %>%
        filter(!speciesEnglishName=='Shortfin mako' )

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = captures_spatial_plot, aes(color=CaptureType), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA accidental captures of seabirds, marine mammals, turtles and sharks (2012-2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  scale_colour_discrete("Captures") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 


ggsave("Ecosystem summary/SIOFAmap_accidental_captures_web.png", width = 10, height = 8, dpi = 150)


# aggregate captures in a table that can be used in reports

# turtles
captures_turtles <- filter(captures,(CaptureType == 'turtles'))

#captures_turtles_aggregate1 <- captures_turtles %>%
#  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
#  summarise(n = n()) 

captures_turtles_aggregate <- captures_turtles %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) 

write_xlsx(captures_turtles_aggregate,"Ecosystem summary/Tables/captures_turtles.xlsx")

# marine mammals
captures_mammals <- filter(captures,(CaptureType == 'marine mammals'))

captures_mammals_aggregate <- captures_mammals %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) 

write_xlsx(captures_mammals_aggregate,"Ecosystem summary/Tables/captures_mammals.xlsx")

# seabirds
captures_seabirds <- filter(captures,(CaptureType == 'seabirds'))

captures_seabirds_aggregate <- captures_seabirds %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) 

write_xlsx(captures_seabirds_aggregate,"Ecosystem summary/Tables/captures_seabirds.xlsx")

# sharks
captures_sharks <- filter(captures,(CaptureType == 'sharks'))

captures_sharks_aggregate <- captures_sharks %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) 

write_xlsx(captures_sharks_aggregate,"Ecosystem summary/Tables/captures_sharks.xlsx")


## analyze seabirds observations

observations_seabirds_aggregate <- aggregate(observations$birdAbundance, by=list(Year=observations$Year, CommonName=observations$speciesEnglishName, ScientificName=observations$speciesScientificName, Gear=observations$Gear), FUN=sum, na.rm = TRUE)

names(observations_seabirds_aggregate)[names(observations_seabirds_aggregate) == "x"] <- "Abundance"

write_xlsx(observations_seabirds_aggregate,"Ecosystem summary/Tables/observations_seabirds.xlsx")


## analyze observer coverage

observer_coverage <- read_excel("Observers_coverage.xlsx", col_types = c("numeric", "numeric", "text", "text", "numeric", "numeric","numeric", "numeric", "text", "text", "numeric","numeric","numeric","numeric","numeric","text"))


observer_coverage_table <- observer_coverage %>%
                  group_by(Year, Gear) %>%
                  summarise(observed= sum(foObserved=='Y',na.rm=T), total = n()) %>%
                  mutate(OBSratio = observed/total) %>%
                  mutate_at(vars(OBSratio), funs(round(., 3))) %>%
                  dplyr::select(-observed, - total) %>%
                  pivot_wider(names_from = Gear, values_from = OBSratio)

write_xlsx(observer_coverage_table,"Tables/observer_coverage.xlsx")
