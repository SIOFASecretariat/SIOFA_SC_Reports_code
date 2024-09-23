
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
require(cutpointr)
##load general data
setwd("D:/SIOFA/Data") 
BPAs <- st_read('BPAs')
# load data
setwd("D:/SIOFA/Data/VME incidental captures")   #### !!!check that this points to the parent folder!!!
VME_catch <- read_excel("D:/SIOFA/Data/SIOFA-VME_bycatch-2023.xlsx")
fishing <- read_excel("D:/SIOFA/Data/Catch-effort-2023.xlsx", col_types = c("numeric", "text", "text", "numeric", "numeric", "numeric","numeric","numeric","text","text","text","text","numeric","numeric","numeric","text","text","text","text","numeric","numeric","numeric","text","numeric"))
fishing_boundaries <- st_read('D:/SIOFA/Data/siofa_subareas_edited')

# load a number of base layers
land <- st_read('D:/SIOFA/Data/natural_earth/land')
ocean <- st_read('D:/SIOFA/Data/natural_earth/ocean')
bathy <- st_read('D:/SIOFA/Data/natural_earth/bathy')
grid <- st_read('D:/SIOFA/Data/natural_earth/grid/ne_50m_graticules_10.shp')
islands <- st_read('D:/SIOFA/Data/natural_earth/islands')

# make VME bycatch data spatial
VME_catch_spatial <- filter(VME_catch, (!is.na(VME_catch$Longitude)))
VME_catch_spatial <- filter(VME_catch, (!is.na(VME_catch$Latitude)))
VME_catch_spatial <- st_as_sf(VME_catch_spatial, coords = c("Longitude", "Latitude"), crs = 4326)
VME_catch_spatial <- VME_catch_spatial[fishing_boundaries,] 
st_write(VME_catch_spatial, "VME_catch_spatial.shp", driver="ESRI Shapefile", delete_layer=TRUE) 

# select fishing only with bottom fishing gears
fishing_spatial <- filter(fishing, (!is.na(fishing$Longitude)))
fishing_spatial <- filter(fishing, (!is.na(fishing$Latitude)))
fishing_spatial <- st_as_sf(fishing_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 
fishing_bottom <- filter(fishing_spatial, (Gear == "Traps (nei)")| (Gear == "Demersal longlines") | (Gear == "Dropline") | (Gear == "Set longlines") | (Gear == "Handlines and hand-operated pole-and-lines")| (Gear == "Vertical lines")| (Gear == "Bottom trawls (nei)") | (Gear == "Trawls (nei)") | (Gear == "Single boat bottom otter trawls") | (Gear == "Gillnets and entangling nets (nei)"))
fishing_trawls <- filter(fishing, (Gear == "Single boat bottom otter trawls") | (Gear == "Trawls (nei)") | (Gear == "Bottom trawls (nei)") | (Gear == "Midwater trawls (nei)"))
fishing_longlines <- filter(fishing, (Gear == "Set longlines") | (Gear == "Longlines (nei)") | (Gear == "Handlines and hand-operated pole-and-lines") | (Gear == "Vertical lines"))

## Plot VME catch in space to get a feeling of it
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  #geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  #geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = VME_catch_spatial, aes(color = FAOcode), alpha = 1/5, cex = 2) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA incidental captures of VME taxa") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("SIOFAmap_VME_catches_web.png", width = 10, height = 10, dpi = 150)


## Subset the data into trawls and longlines so that it can be analysed according to the CMM

VME_catch_trawl <- filter(VME_catch, (Gear == "Single boat bottom otter trawls") | (Gear == "Trawls (nei)") | (Gear == "Bottom trawls (nei)") | (Gear == "Midwater trawls (nei)"))
VME_catch_longline <- filter(VME_catch, (Gear == "Set longlines") | (Gear == "Longlines (nei)") | (Gear == "Vertical lines"))

# also spatial

VME_catch_trawl_spatial <- filter(VME_catch_spatial, (Gear == "Single boat bottom otter trawls") | (Gear == "Trawls (nei)") | (Gear == "Bottom trawls (nei)") | (Gear == "Midwater trawls (nei)"))
VME_catch_longline_spatial <- filter(VME_catch_spatial, (Gear == "Set longlines") | (Gear == "Longlines (nei)") | (Gear == "Handlines and hand-operated pole-and-lines") | (Gear == "Vertical lines"))

## Subset the data into classes corresponding to the VME taxa of the CMM for trawl

coral_species <- read_excel("corals.xlsx")
VME_catch_trawl_corals <- filter(VME_catch_trawl, FAOcode %in% coral_species$Code)

sponges_species <- read_excel("sponges.xlsx")
VME_catch_trawl_sponges <- filter(VME_catch_trawl, FAOcode %in% sponges_species$Code)

# also spatial

VME_catch_trawl_spatial_corals <- filter(VME_catch_trawl_spatial, FAOcode %in% coral_species$Code)
VME_catch_trawl_spatial_sponges <- filter(VME_catch_trawl_spatial, FAOcode %in% sponges_species$Code)


## Subset the longline data into taxa groups following the CMM for longlines

CMM_VME_species <- read_excel("CMM_VME.xlsx")
VME_catch_longlines <- filter(VME_catch_longline, FAOcode %in% CMM_VME_species$Code)

# also spatial

VME_catch_longlines_spatial <- filter(VME_catch_longline_spatial, FAOcode %in% CMM_VME_species$Code)


## create spatial dataframes from the layers so far 

#VME_catch_trawl_spatial_corals <- st_as_sf(VME_catch_trawl_spatial_corals, coords = c("Longitude", "Latitude"), crs = 4326) 

#VME_catch_trawl_spatial_sponges <- st_as_sf(VME_catch_trawl_spatial_sponges, coords = c("Longitude", "Latitude"), crs = 4326) 

#VME_catch_longlines_spatial <- st_as_sf(VME_catch_longlines_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 




### identify catch events that have exceeded the thresholds for trawls and longlines

## trawls
# need to make sure there is a level of aggregation within a fishing operation, don't assume the data has one fishing operation per row
# this keeps information of associated taxa to the fishing operation that had an encounter

VME_corals_trawl_grouped <- VME_catch_trawl_corals  %>%
  group_by(FOID) %>%
  mutate(Tot_Weight = sum(Weight)) %>%
  ungroup()
VME_sponges_trawl_grouped <- VME_catch_trawl_sponges  %>%
  group_by(FOID) %>%
  mutate(Tot_Weight = sum(Weight)) %>%
  ungroup()

VME_trawl_spatial_corals_grouped <- VME_catch_trawl_spatial_corals %>%
  group_by(FOID) %>%
  mutate(Tot_Weight = sum(Weight)) %>%
  ungroup() 
VME_trawl_spatial_sponges_grouped <- VME_catch_trawl_spatial_sponges %>%
  group_by(FOID) %>%
  mutate(Tot_Weight = sum(Weight)) %>%
  ungroup()


# trawl threshold 60 kg of corals and 300 kg of sponges *IN A SINGLE OPERATION*

VME_encounters_corals_trawl <- filter(VME_corals_trawl_grouped, (Tot_Weight >= 60))
VME_encounters_sponges_trawl <- filter(VME_sponges_trawl_grouped, (Tot_Weight >= 300))

VME_encounters_trawl_spatial_corals  <- filter(VME_trawl_spatial_corals_grouped, (Tot_Weight >= 60))
VME_encounters_trawl_spatial_sponges  <- filter(VME_trawl_spatial_sponges_grouped, (Tot_Weight >= 300))

write_xlsx(VME_encounters_corals_trawl,"Outputs/encounters_trawl_corals.xlsx")
write_xlsx(VME_encounters_sponges_trawl,"Outputs/encounters_trawl_sponges.xlsx")

# plot trawl encounter events on map 
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = BPAs, fill = "orchid2", color = "black") +
  geom_sf_label(data = BPAs, aes(fill = NULL, label = value), size = 2.5, col = "black", label.size = 0, nudge_y=c(1.2,1.2,1.2,1.2,2)) +
  #geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  #geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = VME_encounters_trawl_spatial_corals, aes(color = FAOcode), cex = 2) +
  #geom_sf(data = VME_encounters_sponges_trawl, aes(color = FAOcode), cex = 2) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA trawl encounters of potential VMEs") +
  coord_sf(xlim = c(25, 70), ylim = c(-20, -50), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("Outputs/SIOFAmap_VME_encounters_trawl_web.png", width = 10, height = 7, dpi = 150)

# add all other VME taxa captures within a 5 mile radius
# add also any other fishing events from the catch effort database that didn't capture VME taxa

# start by identifying 5 nautical mile radius around encounter coordinates
# convert to a metric projection 
#VME_encounters_trawl_spatial_corals <- VME_encounters_trawl_spatial_corals %>% st_transform(3035)
# create buffer circles of 9260m/5 nautical miles (approximate) around encounter points
encounter_circles <- st_buffer(VME_encounters_trawl_spatial_corals, dist = 4500)
st_write(encounter_circles, "Outputs/encounter_circles.shp", driver="ESRI Shapefile", delete_layer=TRUE) 
# select any other events which reported captures of VME taxa within that radius around encounters
VME_around_encounters <- VME_catch_spatial #%>% st_transform(3035)
VME_around_encounters <- st_intersection(VME_around_encounters, encounter_circles)
st_write(VME_around_encounters, "Outputs/VME_around_encounters.shp", driver="ESRI Shapefile", delete_layer=TRUE) 
# select any bottom fishing event from the CE data that falls within that radius around encounters
fishing_around_encounters <- fishing_bottom #%>% st_transform(3035)
fishing_around_encounters <- st_intersection(fishing_around_encounters, encounter_circles)
st_write(fishing_around_encounters, "Outputs/fishing_around_encounters.shp", driver="ESRI Shapefile", delete_layer=TRUE) 
# transform everything for plotting
#encounter_circles <- encounter_circles %>% st_transform(3035)
#VME_around_encounters <- VME_around_encounters %>% st_transform(4326)
#fishing_around_encounters <- fishing_around_encounters %>% st_transform(4326)
# plot an example around a high intensity encounter zone
# crop layers to a specific area
encounter_circles_1 <- st_crop(encounter_circles, xmin = 53.8, xmax = 54.8, ymin = -34.6, ymax = -35.5)
VME_around_encounters_1 <- st_crop(VME_around_encounters, xmin = 53.8, xmax = 54.8, ymin = -34.6, ymax = -35.5)
fishing_around_encounters_1 <- st_crop(fishing_around_encounters, xmin = 53.8, xmax = 54.8, ymin = -34.6, ymax = -35.5)

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  #geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = encounter_circles_1, color = "black") +
  geom_sf(data = VME_around_encounters_1, aes(color = FAOcode), cex = 5, alpha = 0.5) +
  geom_sf(data = fishing_around_encounters_1, fill = NA, color = "black", cex = 2) +
  #xlab("Longitude") + ylab("Latitude") +
  ggtitle("Example of SIOFA reported incidental captures of all VME taxa and 
          bottom fishing events within 5 nm of encounters of potential VMEs") +
  #scale_fill_manual(values = c('chartreuse','orchid3'), labels = c('Updated footprint', 'Interim footprint'))  +
  #coord_sf(crs = "3035") +
  #coord_sf(xlim = c(53.8, 54.6), ylim = c(-34.7, -35.3), expand = TRUE) +
  coord_sf(xlim = c(53.9, 54.5), ylim = c(-34.85, -35.15), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      rect = element_blank(),
      axis.title.y=element_blank(),
      axis.title.x=element_blank()) 

ggsave("Outputs/SIOFAmap_VME_around_encounters_trawl_web.png", width = 10, height = 6, dpi = 150)



## longlines

# need to make sure there is a level of aggregation within a fishing operation, but assume the data has one fishing operation per row
# longline threshold 10 kg or units every line segment/hooks *IN A SINGLE OPERATION* (assume this is a fishing operation)

VME_catch_longlines_group <- VME_catch_longlines %>%
  group_by(FOID) %>%
  mutate(Tot_Weight = sum(Weight)) %>%
  mutate(encounter_weight1 = Tot_Weight/(LonglineLength/1200)) %>%
  mutate(encounter_weight2 = Tot_Weight/(NbHooks/1000)) %>%
  ungroup()

VME_encounters_longline <- filter(VME_catch_longlines_group, (encounter_weight1 >= 10) | (encounter_weight2 >= 10) )
n_distinct(VME_encounters_longline$FOID) 
# no encounters


## calculate encounter rates
# count encounters during the data period 2003-2019 
VME_encounters_corals_trawl_n <- n_distinct(VME_encounters_corals_trawl$FOID) 
VME_encounters_corals_trawl_n
# calculate total effort (fishing events) by gear type (trawl/longline) over the same period
fishing_trawls_n <- n_distinct(fishing_trawls$activityID) 
fishing_trawls_n
#encounter rate
encounter_rate_trawl <- fishing_trawls_n/VME_encounters_corals_trawl_n
encounter_rate_trawl
## expected encounter rates
fishing_trawls_n_year <- fishing_trawls %>%
  group_by(Year) %>%
  summarise(operations_year = n_distinct(ActivityID))
mean(fishing_trawls_n_year$operations_year)

## encounter rates scenarios (double and half)
# trawl threshold 30 kg of corals and 150 kg of sponges *IN A SINGLE OPERATION*
VME_encounters_corals_trawl_half <- filter(VME_corals_trawl_grouped, (Tot_Weight >= 30))
VME_encounters_sponges_trawl_half <- filter(VME_sponges_trawl_grouped, (Tot_Weight >= 150))
n_distinct(VME_encounters_corals_trawl_half$FOID) 
n_distinct(VME_encounters_sponges_trawl_half$FOID) 
# trawl threshold 120 kg of corals and 600 kg of sponges *IN A SINGLE OPERATION*
VME_encounters_corals_trawl_double <- filter(VME_corals_trawl_grouped, (Tot_Weight >= 120))
VME_encounters_sponges_trawl_double <- filter(VME_sponges_trawl_grouped, (Tot_Weight >= 600))
n_distinct(VME_encounters_corals_trawl_double$FOID) 
n_distinct(VME_encounters_sponges_trawl_double$FOID) 

# longline
# half the threshold
VME_encounters_longline_half <- filter(VME_catch_longlines_group, (encounter_weight1 >= 5) | (encounter_weight2 >= 10) )
n_distinct(VME_encounters_longline_half$FOID) 
# double the threshold
VME_encounters_longline_double <- filter(VME_catch_longlines_group, (encounter_weight1 >= 20) | (encounter_weight2 >= 10) )
n_distinct(VME_encounters_longline_double$FOID)

## plot cumulative curves, by gear type
# start with trawls
# corals
VME_catch_corals_trawl <- filter(VME_catch, FAOcode %in% coral_species$Code)
VME_catch_corals_trawl <- filter(VME_catch_corals_trawl, (Gear == "Single boat bottom otter trawls") | (Gear == "Trawls (nei)") | (Gear == "Bottom trawls (nei)") | (Gear == "Midwater trawls (nei)") )
VME_catch_corals_trawl <- filter(VME_catch_corals_trawl, (!is.na(VME_catch_corals_trawl$Weight)))

ggplot(VME_catch_corals_trawl, aes(Weight)) + 
  stat_ecdf(color="purple") +
  labs(title="Cumulative curve for incidental catches of corals
        in trawls in the SIOFA area (2003-2022)", x="Incidental captures weight (kg)", y="Cumulative ratio") +
  theme_bw() 

ggsave("Outputs/SIOFA_cumulative_curve_corals_trawl.png", width = 10, height = 6, dpi = 150)  

#sponges
VME_catch_sponges_trawl <- filter(VME_catch, FAOcode %in% sponges_species$Code)
VME_catch_sponges_trawl <- filter(VME_catch_sponges_trawl, (Gear == "Single boat bottom otter trawls") | (Gear == "Trawls (nei)") | (Gear == "Bottom trawls (nei)") | (Gear == "Midwater trawls (nei)") )
VME_catch_sponges_trawl <- filter(VME_catch_sponges_trawl, (!is.na(VME_catch_sponges_trawl$Weight)))

ggplot(VME_catch_sponges_trawl, aes(Weight)) + 
  stat_ecdf(color="red") +
  coord_cartesian(xlim = c(0.5, 200)) +
  labs(title="Cumulative curve for incidental catches of sponges 
       in trawls in the SIOFA area (2003-2022)", x="Incidental captures weight (kg)", y="Cumulative ratio") +
  theme_bw() 

ggsave("Outputs/SIOFA_cumulative_curve_sponges_trawl.png", width = 10, height = 6, dpi = 150)  

# calculate youden cutpoints? replot manually?
VME_catch_corals_trawl <- VME_catch_corals_trawl %>% arrange(Weight)
VME_catch_sponges_trawl <- VME_catch_sponges_trawl %>% arrange(Weight)

VME_catch_corals_trawl <- VME_catch_corals_trawl %>% 
  mutate(cumulative_ratio = cumsum(VME_catch_corals_trawl$Weight)/sum(VME_catch_corals_trawl$Weight)) 
VME_catch_sponges_trawl <- VME_catch_sponges_trawl %>% 
  mutate(cumulative_ratio = cumsum(VME_catch_sponges_trawl$Weight)/sum(VME_catch_sponges_trawl$Weight)) 

ggplot(data=VME_catch_corals_trawl, aes(x=Weight,y=cumulative_ratio, ), inherit.aes = FALSE, size = 1) +
  geom_line(color="purple") +
  geom_point(color="purple") +
  theme_bw() +
  #scale_x_continuous(limits=c(2012, 2022)) +
  #scale_y_continuous(limits=c(0, 7)) +
  labs(title="Cumulative curve for incidental catches of corals 
       in trawls in the SIOFA area (2003-2022)", x="Incidental captures weight (kg)", y="Cumulative ratio") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Outputs/SIOFA_cumulative_curve_corals_trawl_2.png", width = 10, height = 6, dpi = 150)  

ggplot(data=VME_catch_sponges_trawl, aes(x=Weight,y=cumulative_ratio), inherit.aes = FALSE, size = 1) +
  geom_line(color="red") +
  geom_point(color="red") +
  theme_bw() +
  #scale_x_continuous(limits=c(2012, 2022)) +
  #scale_y_continuous(limits=c(0, 7)) +
  labs(title="Cumulative curve for incidental catches of sponges 
       in trawls in the SIOFA area (2003-2022)", x="Incidental captures weight (kg)", y="Cumulative ratio") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Outputs/SIOFA_cumulative_curve_sponges_trawl_2.png", width = 10, height = 6, dpi = 150)  

# continue with longlines
# corals
VME_catch_corals_longline <- filter(VME_catch, FAOcode %in% coral_species$Code)
VME_catch_corals_longline <- filter(VME_catch_corals_longline, (Gear == "Set longlines") | (Gear == "Longlines (nei)") | (Gear == "Handlines and hand-operated pole-and-lines") | (Gear == "Vertical lines") )
VME_catch_corals_longline <- filter(VME_catch_corals_longline, (!is.na(VME_catch_corals_longline$Weight)))

ggplot(VME_catch_corals_longline, aes(Weight)) + 
  stat_ecdf(color="purple") +
  labs(title="Cumulative curve for incidental catches of corals
        in longlines in the SIOFA area (2003-2022)", x="Incidental captures weight (kg)", y="Cumulative ratio") +
  theme_bw() 

ggsave("Outputs/SIOFA_cumulative_curve_corals_longline.png", width = 10, height = 6, dpi = 150)  

#sponges
VME_catch_sponges_longline <- filter(VME_catch, FAOcode %in% sponges_species$Code)
VME_catch_sponges_longline <- filter(VME_catch_sponges_longline, (Gear == "Set longlines") | (Gear == "Longlines (nei)") | (Gear == "Handlines and hand-operated pole-and-lines") | (Gear == "Vertical lines") )
VME_catch_sponges_longline <- filter(VME_catch_sponges_longline, (!is.na(VME_catch_sponges_longline$Weight)))

ggplot(VME_catch_sponges_longline, aes(Weight)) + 
  stat_ecdf(color="red") +
  coord_cartesian(xlim = c(0.5, 200)) +
  labs(title="Cumulative curve for incidental catches of sponges 
       in longlines in the SIOFA area (2003-2022)", x="Incidental captures weight (kg)", y="Cumulative ratio") +
  theme_bw() 

ggsave("Outputs/SIOFA_cumulative_curve_sponges_longline.png", width = 10, height = 6, dpi = 150)  

# calculate youden cutpoints? replot manually?
VME_catch_corals_longline <- VME_catch_corals_longline %>% arrange(Weight)
VME_catch_sponges_longline <- VME_catch_sponges_longline %>% arrange(Weight)

VME_catch_corals_longline <- VME_catch_corals_longline %>% 
  mutate(cumulative_ratio = cumsum(VME_catch_corals_longline$Weight)/sum(VME_catch_corals_longline$Weight)) 
VME_catch_sponges_longline <- VME_catch_sponges_longline %>% 
  mutate(cumulative_ratio = cumsum(VME_catch_sponges_longline$Weight)/sum(VME_catch_sponges_longline$Weight)) 

ggplot(data=VME_catch_corals_longline, aes(x=Weight,y=cumulative_ratio, ), inherit.aes = FALSE, size = 1) +
  geom_line(color="purple") +
  geom_point(color="purple") +
  theme_bw() +
  #scale_x_continuous(limits=c(2012, 2022)) +
  #scale_y_continuous(limits=c(0, 7)) +
  labs(title="Cumulative curve for incidental catches of corals 
       in longlines in the SIOFA area (2003-2022)", x="Incidental captures weight (kg)", y="Cumulative ratio") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Outputs/SIOFA_cumulative_curve_corals_longline_2.png", width = 10, height = 6, dpi = 150)  

ggplot(data=VME_catch_sponges_longline, aes(x=Weight,y=cumulative_ratio), inherit.aes = FALSE, size = 1) +
  geom_line(color="red") +
  geom_point(color="red") +
  theme_bw() +
  #scale_x_continuous(limits=c(2012, 2022)) +
  #scale_y_continuous(limits=c(0, 7)) +
  labs(title="Cumulative curve for incidental catches of sponges 
       in longlines in the SIOFA area (2003-2022)", x="Incidental captures weight (kg)", y="Cumulative ratio") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Outputs/SIOFA_cumulative_curve_sponges_longline_2.png", width = 10, height = 6, dpi = 150)  
