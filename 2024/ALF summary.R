
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
require(modelr)
require(ggstats)
# load data
setwd("D:/SIOFA/Data")   #### !!!check that this points to the parent folder!!!
fishing <- read_excel("Catch-effort-2023.xlsx", col_types = c("numeric", "text", "text", "numeric", "numeric", "numeric","numeric","numeric","text","text","text","text","numeric","numeric","numeric","text","text","text","text","numeric","numeric", "numeric","numeric","text"))
captures <- read_excel("Accidental-captures-2023.xlsx")
observations <- read_excel("Bird-observations-2023.xlsx")
VME_catch <- read_excel("SIOFA-VME_bycatch-2023.xlsx")
Observer_data <- read_excel("qry_overview_full_bio_sampling_data_2023.xlsx")
fishing_boundaries <- st_read('siofa_subareas_edited')

# drop 2023 data (still incomplete)
fishing <- fishing %>%
  filter(Year<=2022)
# drop 2023 data, if any, as it is still incomplete
Observer_data <- Observer_data %>%
  filter(Year<=2022)

# get a feel for the data as currently is
class(fishing_boundaries)
class(fishing)

# Convert the fishing dataframe to a spatial object. Note that the
# crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object
# and that missing values are removed
fishing_spatial <- filter(fishing, (!is.na(fishing$Longitude)))
fishing_spatial <- filter(fishing, (!is.na(fishing$Latitude)))
fishing_spatial <- st_as_sf(fishing_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 
class(fishing_spatial)
st_crs(fishing_boundaries) <- 4326

# load a number of base layers
land <- st_read('natural_earth/land')
ocean <- st_read('natural_earth/ocean')
bathy <- st_read('natural_earth/bathy')
grid <- st_read('natural_earth/grid/ne_50m_graticules_10.shp')
islands <- st_read('natural_earth/islands')

#check that layers are viable
class(land)
class(ocean)
class(fishing_boundaries)

# Overlay points and extract just the subarea column 
fishing_within <- st_join(fishing_spatial, left = FALSE, fishing_boundaries["SubAreaNo"])

#subset for  alfonsino fishing activity only in order to map it
# subset the whole data for all species, need catch, effort and subarea aggregations
yearly_global_catches_species <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, Longitude=fishing$Longitude, Latitude=fishing$Latitude), FUN=sum, na.rm = TRUE)
yearly_global_catches_subarea_species <- aggregate(fishing_within$CatchTon, by=list(Year=fishing_within$Year,SubArea=fishing_within$SubAreaNo, Species=fishing_within$SpeciesCode), FUN=sum, na.rm = TRUE)
yearly_trawl_effort_species <- aggregate(fishing$NbTows, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, ActivityID=fishing$ActivityID, Longitude=fishing$Longitude, Latitude=fishing$Latitude), FUN=sum, na.rm = TRUE)

# subset for  alfonsino only data 
# !!!!! check well how the trawl effort is calculated when missing data
yearly_global_catches_alfonsino <- filter(yearly_global_catches_species, (Species == "ALF") | (Species == "BYS")| (Species == "BRX")| (Species == "BYX")| (Species == "BXD"))
yearly_global_catches_alfonsino <- aggregate(yearly_global_catches_alfonsino$x, by=list(Year=yearly_global_catches_alfonsino$Year), FUN=sum, na.rm = TRUE)
yearly_global_catches_subarea_alfonsino <- filter(yearly_global_catches_subarea_species, (Species == "ALF") | (Species == "BYS")| (Species == "BRX")| (Species == "BYX")| (Species == "BXD"))
yearly_trawl_effort_alfonsino_activity <- filter(yearly_trawl_effort_species, (Species == "ALF") | (Species == "BYS")| (Species == "BRX")| (Species == "BYX")| (Species == "BXD"))
yearly_trawl_effort_alfonsino_activity_a <- filter(yearly_trawl_effort_alfonsino_activity, x > 0)
yearly_trawl_effort_alfonsino_activity_h <- filter(yearly_trawl_effort_alfonsino_activity, x == 0)

# turn the trawl effort locations in a spatial dataframe
# eliminate data points outside of the SIOFA area
fishing_spatial_alfonsino <- st_as_sf(yearly_trawl_effort_alfonsino_activity, coords = c( "Longitude", "Latitude"), crs = 4326)
fishing_spatial_alfonsino <- fishing_spatial_alfonsino[fishing_boundaries, ]

# load 5 degree square grid, subset it for the SIOFA area only
grid5 <- st_read('5degree_squares')
st_crs(grid5) <- 4326

# load 1 degree square grid, subset it for the SIOFA area only
grid1 <- st_read('1degree_squares')
st_crs(grid1) <- 4326

# load 30 minutes square grid, subset it for the SIOFA area only
grid30 <- st_read('30minutes_squares')
st_crs(grid30) <- 4326

# load 20 minutes square grid, subset it for the SIOFA area only
grid20 <- st_read('20minutes_squares')
st_crs(grid20) <- 4326

# calculate number of fishing events per 5 degree grid cell
alfonsino_poly5 <- st_join(fishing_spatial_alfonsino, grid5)
alfonsino_count5 <- alfonsino_poly5 %>% group_by(id) %>% count()
alfonsino_count5 <- st_drop_geometry(alfonsino_count5)
alfonsino_poly5 <- left_join(grid5, alfonsino_count5, by="id")
alfonsino_poly5 <- filter(alfonsino_poly5, (!is.na(alfonsino_poly5$n)))

# calculate number of fishing events per 1 degree grid cell
alfonsino_poly1 <- st_join(fishing_spatial_alfonsino, grid1)
alfonsino_count1 <- alfonsino_poly1 %>% group_by(id) %>% count()
alfonsino_count1 <- st_drop_geometry(alfonsino_count1)
alfonsino_poly1 <- left_join(grid1, alfonsino_count1, by="id")
alfonsino_poly1 <- filter(alfonsino_poly1, (!is.na(alfonsino_poly1$n)))

# calculate number of fishing events per 30 minutes grid cell
alfonsino_poly30 <- st_join(fishing_spatial_alfonsino, grid30)
alfonsino_count30 <- alfonsino_poly30 %>% group_by(id) %>% count()
alfonsino_count30 <- st_drop_geometry(alfonsino_count30)
alfonsino_poly30 <- left_join(grid30, alfonsino_count30, by="id")
alfonsino_poly30 <- filter(alfonsino_poly30, (!is.na(alfonsino_poly30$n)))

# calculate number of fishing events per 20 minutes grid cell
alfonsino_poly20 <- st_join(fishing_spatial_alfonsino, grid20)
alfonsino_count20 <- alfonsino_poly20 %>% group_by(id) %>% count()
alfonsino_count20 <- st_drop_geometry(alfonsino_count20)
alfonsino_poly20 <- left_join(grid20, alfonsino_count20, by="id")
alfonsino_poly20 <- filter(alfonsino_poly20, (!is.na(alfonsino_poly20$n)))

# create hybrid map, delete all squares without fishing activity, clip out of boundaries
alfonsino_footprint_hybrid <- st_join(alfonsino_poly30, alfonsino_poly20, left = T)
alfonsino_footprint_hybrid <- filter(alfonsino_footprint_hybrid, (!is.na(alfonsino_footprint_hybrid$n.x)))
alfonsino_footprint_hybrid <- filter(alfonsino_footprint_hybrid, (!is.na(alfonsino_footprint_hybrid$n.y)))
alfonsino_footprint_hybrid <- alfonsino_footprint_hybrid[fishing_boundaries,]

# classify interval of gridline cells for plotting
alfonsino_breaks_qt5 <- classIntervals(c(min(alfonsino_poly5$n) - .00001, alfonsino_poly5$n), n = 7, style = "pretty")
alfonsino_poly5 <- mutate(alfonsino_poly5, n.events5 = cut(n, alfonsino_breaks_qt5$brks))

alfonsino_breaks_qt1 <- classIntervals(c(min(alfonsino_poly1$n) - .00001, alfonsino_poly1$n), n = 7, style = "pretty")
alfonsino_poly1 <- mutate(alfonsino_poly1, n.events1 = cut(n, alfonsino_breaks_qt1$brks))

alfonsino_breaks_qt30 <- classIntervals(c(min(alfonsino_poly30$n) - .00001, alfonsino_poly30$n), n = 7, style = "pretty")
alfonsino_poly30 <- mutate(alfonsino_poly30, n.events30 = cut(n, alfonsino_breaks_qt30$brks))

alfonsino_breaks_qt20 <- classIntervals(c(min(alfonsino_poly20$n) - .00001, alfonsino_poly20$n), n = 7, style = "pretty")
alfonsino_poly20 <- mutate(alfonsino_poly20, n.events20 = cut(n, alfonsino_breaks_qt20$brks))


# crop layers on SIOFA agreement area boundaries
alfonsino_poly5 <- st_intersection(alfonsino_poly5, fishing_boundaries)
alfonsino_poly1 <- st_intersection(alfonsino_poly1, fishing_boundaries)
alfonsino_poly30 <- st_intersection(alfonsino_poly30, fishing_boundaries)
alfonsino_poly20 <- st_intersection(alfonsino_poly20, fishing_boundaries)

# plot world map and add SIOFA subareas + hybrid resolution SIOFA BYS fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = alfonsino_poly20, fill = "orchid3") +
  geom_sf(data = alfonsino_poly30, fill = "orchid3") +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA ALF fishing activities (hybrid 20'+30', 2013–2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("BYS summary/SIOFAmap_alfonsino_hybrid_web.png", width = 10, height = 10, dpi = 150)

# plot plot world map and add SIOFA subareas + 5 degree resolution SIOFA BYS fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf(data = alfonsino_poly5, aes(fill=n.events5)) +
  scale_fill_viridis(discrete = TRUE) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(1,3,3.5, -1, 0, -2, 0, -6), nudge_x=c(0,-2.5,0, 6, 0, 0, 0, -6)) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA ALF fishing activities (5 degrees, 2013–2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("BYS summary/SIOFAmap_alfonsino_heatmap_5_web.png", width = 10, height = 7, dpi = 150)

# plot plot world map and add SIOFA subareas + 1 degree resolution siofa BYS fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = alfonsino_poly1, aes(fill=n.events1)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA ALF fishing activities (1 degree, 2013–2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("BYS summary/SIOFAmap_alfonsino_heatmap_1_web.png", width = 10, height = 7, dpi = 150)

# plot plot world map and add SIOFA subareas + 30' resolution siofa BYS fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = alfonsino_poly30, aes(fill=n.events30)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA ALF fishing activities (30', 2013–2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("BYS summary/SIOFAmap_alfonsino_heatmap_30_web.png", width = 10, height = 7, dpi = 150)

# plot plot world map and add SIOFA subareas + 20' resolution siofa BYS fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = alfonsino_poly20, aes(fill=n.events20)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA ALF fishing activities (20', 2013–2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("BYS summary/SIOFAmap_alfonsino_heatmap_20_web.png", width = 10, height = 7, dpi = 150)

# test the export of layers as shapefiles
#alfonsino_poly1 <- st_collection_extract(alfonsino_poly1, "POLYGON")
#st_write(alfonsino_poly1,dsn = 'D:/SIOFA/Data/test', layer = 'SIOFAmap_alfonsino_heatmap_1', driver='ESRI Shapefile')


## Better definition of target/non-target catch: 
## if 70% of the catch in an operation
## is BYS, then that operation had BYS as a target 
## redo all analyses with this definition, for those events that didn't declare an BYS target
## i.e. in 2014-15-16-17-18

# select only operations that caught BYS (ignore all others)
# identify any operations that caught alfonsino in all the dataset, 
# whenever targets were not declares
# find total BYS catch by ActivityID
# need to retain year

yearly_operations_BYS <- fishing %>%
  #filter(Year==2014 | Year==2015 |Year==2016 |Year==2017 |Year==2018) %>%
  group_by(Year, ActivityID, datasetID, Gear) %>% 
  filter(SpeciesCode == "BYS") %>%
  filter(is.na(Target)) %>%
  summarize(CatchTonBYS = sum(CatchTon))

# find total catch by operation = sum weights of all species caught by 
# an ActivityID that caught BYS
# in years where target was not declared (in 2014-15-16-17-18)
yearly_operations_BYS_total <- fishing %>%
  filter(ActivityID %in% yearly_operations_BYS$ActivityID & datasetID %in% yearly_operations_BYS$datasetID) %>%
  group_by(Year, ActivityID, Gear) %>% 
  summarize(TotalCatch = sum(CatchTon))

# if BYS catch >70% in an acitivtyID then retain those ActivityIDs as operations targeting BYS
# target/non-target ratio is actually already defined  
yearly_operations_BYS_target <- full_join(yearly_operations_BYS, yearly_operations_BYS_total)
yearly_operations_BYS_target <- yearly_operations_BYS_target %>% 
  mutate(Ratio = CatchTonBYS/TotalCatch) %>%
  filter(Ratio >= 0.7) 

## non-target catch by species in BYS target operations in 2014-15-16-17-18
yearly_operations_BYS_species <- fishing %>%
  filter(ActivityID%in%yearly_operations_BYS_target$ActivityID & datasetID%in%yearly_operations_BYS_target$datasetID) %>%
  filter(!SpeciesCode == "BYS") %>%
  filter(Year<2019) %>% # this is introduced because the ActivityID+datasetID combinations are not unique in 2018/19
  group_by(Year,SpeciesCode) %>%
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  rename('Species' = SpeciesCode)

# join data in years where BYS targets were declared
# calculate target/non-target catches alfonsino 
yearly_target_catches_alfonsino <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, TargetSpecies=fishing$Target, Species=fishing$SpeciesCode, ActivityID=fishing$ActivityID), FUN=sum, na.rm = TRUE)
yearly_target_catches_alfonsino <- filter(yearly_target_catches_alfonsino,(TargetSpecies == "BYS"))
yearly_nontarget_catches_alfonsino <- filter(yearly_target_catches_alfonsino,(Species != "BYS"))
yearly_nontarget_catches_alfonsino <- aggregate(yearly_nontarget_catches_alfonsino$x, by=list(Year=yearly_nontarget_catches_alfonsino$Year, Species=yearly_nontarget_catches_alfonsino$Species), FUN=sum, na.rm = TRUE)
yearly_nontarget_catches_alfonsino <- yearly_nontarget_catches_alfonsino %>%  
  rename('NonTargetCatch' = x) 

# join non-target data from both years when catch was declared and not declared
yearly_nontarget_catches_alfonsino <- rbind(yearly_nontarget_catches_alfonsino,yearly_operations_BYS_species)

# sort bycatch data for plotting
sort_bycatch_BYS <- aggregate(yearly_nontarget_catches_alfonsino$NonTargetCatch, by=list(Species=yearly_nontarget_catches_alfonsino$Species), FUN=sum, na.rm = TRUE)
sort_bycatch_BYS <- arrange(sort_bycatch_BYS, desc(x)) 
top5_species <- sort_bycatch_BYS %>% slice(1:5)
other_species <- sort_bycatch_BYS %>% slice(6:89)
# plot
ggplot(data= subset(yearly_nontarget_catches_alfonsino, Species %in% top5_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_nontarget_catches_alfonsino, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly bycatch by species in the fisheries targeting ALF in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("BYS summary/SIOFAcatches_nontarget_BYS_web.png", width = 10, height = 5, dpi = 150)

# calculate target/non-target catch fraction in 2014-15-17-18
yearly_operations_BYS_target <- yearly_operations_BYS_target %>%
  mutate(NonTargetCatch = TotalCatch-CatchTonBYS) %>%
  mutate(TargetCatch = CatchTonBYS)

## sharks non-target catch in all BYS target operations 
shark_species <- read_excel("sharks.xlsx")
yearly_sharks_catches_BYS <- yearly_nontarget_catches_alfonsino %>%
  filter(Species %in% shark_species$Code) 

# plot sharks catch 
ggplot(data= yearly_sharks_catches_BYS, aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly bycatch of sharks in the fisheries targeting ALF in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("BYS summary/SIOFAcatches_sharks_BYS_web.png", width = 10, height = 6, dpi = 150)

## target/nontarget catch in 2014-15-16-17-18, spatial
# subareas
# non target
yearly_nontarget_BYS_spatial <- fishing_within %>%
  filter(ActivityID %in% yearly_operations_BYS_target$ActivityID & datasetID %in% yearly_operations_BYS_target$datasetID) %>%
  filter(Year<2019) %>% # this is introduced because the ActivityID+datasetID combinations are not unique in 2018/19
  filter(!SpeciesCode == "BYS") %>%
  group_by(Year, SubAreaNo) %>%
  summarize(NonTargetCatch = sum(CatchTon)) 

# target
yearly_target_BYS_spatial <- fishing_within %>%
  filter(ActivityID %in% yearly_operations_BYS_target$ActivityID & datasetID %in% yearly_operations_BYS_target$datasetID) %>%
  filter(Year<2019) %>% # this is introduced because the ActivityID+datasetID combinations are not unique in 2018/19
  filter(SpeciesCode == "BYS") %>%
  group_by(Year, SubAreaNo) %>%
  summarize(TargetCatch = sum(CatchTon)) 

# drop geometry 
yearly_nontarget_BYS_spatial$geometry <- NULL
yearly_target_BYS_spatial$geometry <- NULL
# join target and non-target 2014-15-17-18 catches, by subarea
target_nontarget_BYS <- left_join(yearly_target_BYS_spatial, yearly_nontarget_BYS_spatial)

# calculate target and non-target catch in years when target declarations were made
catch_target_subarea_BYS <- fishing_within %>%
  filter(Target == "BYS" & (SpeciesCode== "BYS"| SpeciesCode== "BXD"| SpeciesCode== "ALF")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year, SubAreaNo) %>% 
  summarize(TargetCatch = sum(CatchTon)) %>%
  filter(TargetCatch>0)
  
catch_nontarget_subarea_BYS <- fishing_within %>%
  filter(Target == "BYS" & (SpeciesCode!= "BYS"& SpeciesCode!= "BXD"&  SpeciesCode!= "ALF")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year, SubAreaNo) %>% 
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  filter(NonTargetCatch>0)

# drop geometry 
catch_target_subarea_BYS$geometry <- NULL
catch_nontarget_subarea_BYS$geometry <- NULL

# join target and nontarget catch in years when targets were declared
catch_bycatch_subarea_BYS <- left_join(catch_target_subarea_BYS, catch_nontarget_subarea_BYS)

# join 2014-15-17-18 with BYS declared targets operations
catch_bycatch_subarea_BYS <- rbind(catch_bycatch_subarea_BYS, target_nontarget_BYS)

# plot histograms of target and non-target catch in BYS fisheries by year and subarea
ggplot(catch_bycatch_subarea_BYS, aes(x = Year, y = TargetCatch)) +
  geom_bar(aes(fill=SubAreaNo), position="fill", stat="identity") +
  labs(title="Yearly catch of ALF in fisheries targeting ALF by SIOFA subareas (relative)", x="Year", y="Target catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("BYS summary/SIOFAtargetcatch_subarea_BYS_web.png", width = 10, height = 4, dpi = 150)

ggplot(catch_bycatch_subarea_BYS, aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=SubAreaNo), position="fill", stat="identity") +
  labs(title="Yearly bycatch in fisheries targeting ALF by SIOFA subareas (relative)", x="Year", y="Bycatch proportion") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("BYS summary/SIOFAnontargetcatch_subarea_BYS_web.png", width = 10, height = 4, dpi = 150)

# transform data for easier plotting
catch_bycatch_BYS <- catch_bycatch_subarea_BYS %>% 
  dplyr::select(-SubAreaNo) %>%
  pivot_longer(!Year, names_to = "Catch", values_to = "t")

# plot target/non-target catch totals
ggplot(catch_bycatch_BYS, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly target catch/bycatch in ALF fisheries in the SIOFA area (absolute)", x="Year", y="Total catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("BYS summary/SIOFAcatch_nontargetcatch_BYS_web.png", width = 10, height = 4, dpi = 150)

# plot target/non-target catch percentage
ggplot(catch_bycatch_BYS, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly target catch/bycatch in ALF fisheries in the SIOFA area (relative)", x="Year", y="Total catch proportion") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("BYS summary/SIOFAcatch_nontargetcatch_BYS_fill_web.png", width = 10, height = 4, dpi = 150)


## catch/bycatch in assessment units

# need to create management units as two polygons
# East/West units are separated at 80E longitude
lon_west <- c(30, 80, 30, 80)
lat_west <- c(-60, -60, -10, -10)
Poly_Coord_df_west = data.frame(lon_west, lat_west)
unit_west <- Poly_Coord_df_west %>% 
  st_as_sf(coords = c("lon_west", "lat_west"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

unit_west <- st_sf(st_sfc(unit_west))

lon_east <- c(80, 120, 80, 120)
lat_east <- c(-60, -60, -10, -10)
Poly_Coord_df_east = data.frame(lon_east, lat_east)
unit_east <- Poly_Coord_df_east %>% 
  st_as_sf(coords = c("lon_east", "lat_east"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

unit_east <- st_sf(st_sfc(unit_east))

#visually verify that areas correspond to those by Brandao et al 2019
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = unit_west, fill = "green", color = "black") +
  geom_sf(data = unit_east, fill = "orchid", color = "black") +
  #geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  #geom_sf(data = fishing_spatial, color = 'black', cex = .1) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,0,0, 0, 0, 0, 0, -6)) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA SubAreas") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

# filter data based on assessment units
fishing_within_BYS_East <- st_join(fishing_within, unit_east, left = FALSE)
fishing_within_BYS_West <- st_join(fishing_within, unit_west, left = FALSE)

# calculate target and non-target catch in years when target declarations were made
# keep the two areas separated
catch_target_BYS_East_dec <- fishing_within_BYS_East %>%
  filter(Target == "BYS" & (SpeciesCode== "BYS"| SpeciesCode== "BXD"| SpeciesCode== "ALF")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year) %>% 
  summarize(TargetCatch = sum(CatchTon)) %>%
  filter(TargetCatch>0)

catch_nontarget_BYS_East_dec <- fishing_within_BYS_East %>%
  filter(Target == "BYS" & (SpeciesCode!= "BYS"& SpeciesCode!= "BXD"&  SpeciesCode!= "ALF")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year) %>% 
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  filter(NonTargetCatch>0)

catch_target_BYS_West_dec <- fishing_within_BYS_West %>%
  filter(Target == "BYS" & (SpeciesCode== "BYS"| SpeciesCode== "BXD"| SpeciesCode== "ALF")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year) %>% 
  summarize(TargetCatch = sum(CatchTon)) %>%
  filter(TargetCatch>0)

catch_nontarget_BYS_West_dec <- fishing_within_BYS_West %>%
  filter(Target == "BYS" & (SpeciesCode!= "BYS"& SpeciesCode!= "BXD"&  SpeciesCode!= "ALF")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year) %>% 
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  filter(NonTargetCatch>0)

# calculate target and non-target catch in non declared fishing operations
# keep the two areas separated
catch_target_BYS_East_nondec <- fishing_within_BYS_East %>%
  filter(ActivityID %in% yearly_operations_BYS_target$ActivityID & datasetID %in% yearly_operations_BYS_target$datasetID) %>%
  filter(Year<2019) %>% # this is introduced because the ActivityID+datasetID combinations are not unique in 2018/19
  filter(SpeciesCode == "BYS") %>%
  group_by(Year) %>%
  summarize(TargetCatch = sum(CatchTon)) 

catch_nontarget_BYS_East_nondec <- fishing_within_BYS_East %>%
  filter(ActivityID %in% yearly_operations_BYS_target$ActivityID & datasetID %in% yearly_operations_BYS_target$datasetID) %>%
  filter(Year<2019) %>% # this is introduced because the ActivityID+datasetID combinations are not unique in 2018/19
  filter(!SpeciesCode == "BYS") %>%
  group_by(Year) %>%
  summarize(NonTargetCatch = sum(CatchTon)) 

catch_target_BYS_West_nondec <- fishing_within_BYS_West %>%
  filter(ActivityID %in% yearly_operations_BYS_target$ActivityID & datasetID %in% yearly_operations_BYS_target$datasetID) %>%
  filter(Year<2019) %>% # this is introduced because the ActivityID+datasetID combinations are not unique in 2018/19
  filter(SpeciesCode == "BYS") %>%
  group_by(Year) %>%
  summarize(TargetCatch = sum(CatchTon)) 

catch_nontarget_BYS_West_nondec <- fishing_within_BYS_West %>%
  filter(ActivityID %in% yearly_operations_BYS_target$ActivityID & datasetID %in% yearly_operations_BYS_target$datasetID) %>%
  filter(Year<2019) %>% # this is introduced because the ActivityID+datasetID combinations are not unique in 2018/19
  filter(!SpeciesCode == "BYS") %>%
  group_by(Year) %>%
  summarize(NonTargetCatch = sum(CatchTon)) 

# join declared and non declared target and non target catches by area
catch_target_BYS_East <- rbind(catch_target_BYS_East_dec, catch_target_BYS_East_nondec)
catch_nontarget_BYS_East <- rbind(catch_nontarget_BYS_East_dec, catch_nontarget_BYS_East_nondec)
catch_target_BYS_West <- rbind(catch_target_BYS_West_dec, catch_target_BYS_West_nondec)
catch_nontarget_BYS_West <- rbind(catch_nontarget_BYS_West_dec, catch_nontarget_BYS_West_nondec)
# drop geometry
catch_target_BYS_East <- st_drop_geometry(catch_target_BYS_East)
catch_nontarget_BYS_East <- st_drop_geometry(catch_nontarget_BYS_East)
catch_target_BYS_West <- st_drop_geometry(catch_target_BYS_West)
catch_nontarget_BYS_West <- st_drop_geometry(catch_nontarget_BYS_West)
# summarize
catch_target_BYS_East <- catch_target_BYS_East%>%
  group_by(Year) %>%
  summarize(TargetCatch = sum(TargetCatch))
catch_nontarget_BYS_East <- catch_nontarget_BYS_East%>%
  group_by(Year) %>%
  summarize(NonTargetCatch = sum(NonTargetCatch))
catch_target_BYS_West <- catch_target_BYS_West%>%
  group_by(Year) %>%
  summarize(TargetCatch = sum(TargetCatch))
catch_nontarget_BYS_West <- catch_nontarget_BYS_West%>%
  group_by(Year) %>%
  summarize(NonTargetCatch = sum(NonTargetCatch))
# join all in a single dataset # add column east/west
catch_BYS_East <- left_join(catch_target_BYS_East, catch_nontarget_BYS_East)
catch_BYS_West <- left_join(catch_target_BYS_West, catch_nontarget_BYS_West)
catch_BYS_West$Area <- "West"
catch_BYS_East$Area <- "East"
catch_BYS_East_West <- rbind(catch_BYS_East, catch_BYS_West)

#transform data for easier plotting
#catch_BYS_East_West <- catch_BYS_East_West %>% pivot_longer(cols = "TargetCatch":"NonTargetCatch", names_to = "Catch", values_to = "t")
catch_BYS_East_West <- catch_BYS_East_West %>%
  replace(is.na(.), 0)

## plot histograms of target and non-target catch in MUs per year

#totals target
ggplot(catch_BYS_East_West, aes(x = Year, y = t)) +
  geom_bar(aes(fill=Area, y=TargetCatch), position="stack", stat="identity") +
  labs(title="Yearly ALF catch in SIOFA ALF assessment areas (absolute)", x="Year", y="Target catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("BYS summary/SIOFAtargetcatch_BYS_MUs_web.png", width = 10, height = 4, dpi = 150)

#totals target table
catch_bycatch_by_BYS_MU_table <- catch_BYS_East_West %>%
  dplyr::select(-NonTargetCatch) %>%
  mutate_at(vars(TargetCatch), funs(round(., 2))) %>%
  pivot_wider(names_from = Area, values_from = c(TargetCatch),  names_sep = ' in ' )
write_xlsx(catch_bycatch_by_BYS_MU_table,"BYS summary/Tables/catch_bycatch_by_BYS_MU_table.xlsx")

#percentage of target catch in different MUs
ggplot(catch_BYS_East_West, aes(x = Year, y = t)) +
  geom_bar(aes(fill=Area, y=TargetCatch), position="fill", stat="identity") +
  labs(title="Yearly ALF catch in SIOFA ALF assessment areas (relative)", x="Year", y="Catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("BYS summary/SIOFAtargetcatch_BYS_MUs_fill_web.png", width = 10, height = 4, dpi = 150)

#totals non-target
ggplot(catch_BYS_East_West, aes(x = Year, y = t)) +
  geom_bar(aes(fill=Area, y=NonTargetCatch), position="stack", stat="identity") +
  labs(title="Yearly catch of all other species in SIOFA ALF assessment areas (absolute)", x="Year", y="Bycatch (t)") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("BYS summary/SIOFAnontargetcatch_BYS_MUs_web.png", width = 10, height = 4, dpi = 150)

#percentage of non-target catch in different MUs
ggplot(catch_BYS_East_West, aes(x = Year, y = t)) +
  geom_bar(aes(fill=Area, y=NonTargetCatch), position="fill", stat="identity") +
  labs(title="Yearly bycatch in SIOFA ALF assessment areas (relative)", x="Year", y="Catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("BYS summary/SIOFAnontargetcatch_BYS_MUs_fill_web.png", width = 10, height = 4, dpi = 150)

# target/non-target across all MUs
catch_bycatch_by_BYS_MU_across <- catch_BYS_East_West %>%
  drop_na(TargetCatch) %>%
  group_by(Year) %>%
  summarise_at(.vars = c("TargetCatch","NonTargetCatch"),
               .funs = "sum") %>%
  rename(BYS_catch = TargetCatch) %>%
  rename(Other_catch = NonTargetCatch)

#transform data for easier plotting
catch_bycatch_BYS_MU_across <- catch_bycatch_by_BYS_MU_across %>% 
  pivot_longer(!Year, names_to = "Catch", values_to = "t") 

#percentage of non-target catch in different MUs
ggplot(catch_bycatch_BYS_MU_across, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly BYS/all other species catch in SIOFA ALF assessment areas (relative)", x="Year", y="Catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("BYS summary/SIOFAnontargetcatch_BYS_MUs_across_fill_web.png", width = 10, height = 4, dpi = 150)

## VME catches in BYS fisheries
# filter only BYS target reports 
VME_catch_BYS <- filter(VME_catch, 
                        grepl('BYS', TargetSpecies)) 
          
# aggregate by year and taxon
yearly_BYS_VME_catch <- VME_catch_BYS %>%
  group_by(Year, FAOcode) %>%
  summarise_at(.vars = c("Weight"),
               .funs = "sum")
# plot BYS VME bycatch
ggplot(yearly_BYS_VME_catch, aes(x = Year, y = Weight, fill=FAOcode)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly incidental catch of VME indicator taxa in ALF fisheries in the SIOFA area", x="Year", y="Total weight (kg)") +
  theme_bw() +
  scale_x_continuous(limits=c(2002, 2023)) +
  #scale_fill_hue(labels = c("Non-target Catch", "Target Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("BYS summary/VME_captures_BYS_web.png", width = 10, height = 4, dpi = 150)

## Analysis of observer data
# Convert the observation dataframe to a spatial object. Note that the
# crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object
# and that missing values are removed
captures_spatial <- filter(captures, (!is.na(captures$Longitude)))
captures_spatial <- st_as_sf(captures_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 
#take out sharks
#captures_spatial <- filter(captures_spatial,(CaptureType != "sharks"))
class(captures_spatial)
st_crs(captures_spatial) <- 4326

# plot world map and add SIOFA subareas + SIOFA accidental captures on world map
# delete sharks not in the CMM (as decided in WGSUM1)
shark_species_CMM <- read_excel("protected_sharks.xlsx")
captures_spatial_plot_BYS <- captures_spatial  %>%
  filter(!(speciesGroup=='Chimaeras' |  speciesGroup=='Deepwater sharks' |  speciesGroup=='Sharks and rays nei' ) & species3ACode %in% shark_species_CMM$FAOcode
         | (speciesGroup=='Seabirds'| speciesGroup=='Cetaceans'| speciesGroup=='Turtles')) %>%
  filter(source=='OBS') %>%
  filter(grepl('BYS', fishopTargetSpecies) | grepl('EDR', fishopTargetSpecies))

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = captures_spatial_plot_BYS, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA incidental captures of seabirds, marine mammals, turtles and 
          sharks considered to be at high risk and/or of concern in ALF target operations (2012-2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  scale_colour_discrete("Captures") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("BYS summary/SIOFAmap_accidental_captures_web_BYS.png", width = 10, height = 8, dpi = 150)

# aggregate captures in a table that can be used in reports
# filter only operations that observers recorded as targeting BYS
captures_BYS <-   captures%>%
  filter(grepl('BYS', fishopTargetSpecies))
  
# turtles
captures_turtles <- filter(captures_BYS,(speciesGroup == 'Turtles'))

#captures_turtles_aggregate1 <- captures_turtles %>%
#  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
#  summarise(n = n()) 
captures_turtles_aggregate_BYS <- captures_turtles %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  relocate(speciesEnglishName, .after = Year) %>%
  relocate(speciesScientificName, .after = speciesEnglishName) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Captures" = foibcNbCaught) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(captures_turtles_aggregate_BYS,"BYS summary/Tables/captures_turtles_BYS.xlsx")

# marine mammals
captures_mammals <- filter(captures_BYS,(speciesGroup == 'Cetaceans'))

captures_mammals_aggregate_BYS <- captures_mammals %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) 

write_xlsx(captures_mammals_aggregate_BYS,"BYS summary/Tables/captures_mammals_BYS.xlsx")

# seabirds
captures_seabirds <- filter(captures_BYS,(speciesGroup == 'Seabirds'))

captures_seabirds_aggregate_BYS <- captures_seabirds %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Captures" = foibcNbCaught) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(captures_seabirds_aggregate_BYS,"BYS summary/Tables/captures_seabirds_BYS.xlsx")

# sharks
captures_sharks_BYS <- captures_BYS %>%
  filter(!(speciesGroup=='Chimaeras' |  speciesGroup=='Deepwater sharks' |  speciesGroup=='Sharks and rays nei' ) & species3ACode %in% shark_species_CMM$FAOcode)

captures_sharks_aggregate_BYS <- captures_sharks_BYS %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) %>%
  rename("Captures (n)" = foibcNbCaught)

write_xlsx(captures_sharks_aggregate_BYS,"BYS summary/Tables/captures_sharks_BYS.xlsx")

### Analyze observations of seabirds and mammals
# Convert the observation dataframe to a spatial object. Note that the
# crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object
# and that missing values are removed
observations_spatial <- filter(observations, (!is.na(observations$Longitude)))
observations_spatial <- st_as_sf(observations_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 
class(observations_spatial)
st_crs(observations_spatial) <- 4326

observations_spatial_plot_ALF <- observations_spatial  %>%
  filter(!(speciesGroup=='NO BIRDS OBSERVED' |  speciesGroup=='NO OBSERVER')) %>%
  filter(source=='OBS') %>%
  filter(grepl('BYS', fishopTargetSpecies))

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = observations_spatial_plot_ALF, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA observations of seabirds and marine mammals
            in ALF target operations (2012-2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  scale_colour_discrete("Observations") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("BYS summary/SIOFAmap_observations_web_BYS.png", width = 10, height = 8, dpi = 150)

## analyze seabirds observations
observations_seabirds_aggregate_BYS <- observations %>%
  filter(grepl('BYS', fishopTargetSpecies)) %>%
  filter(!speciesGroup=='MAMMALS') %>%
  filter(!speciesGroup=='NO BIRDS OBSERVED') %>%
  filter(!speciesGroup=='NO OBSERVER') %>%
  group_by(Year, speciesEnglishName, speciesScientificName, Gear, speciesGroup) %>%
  summarise(Abundance = sum(Abundance)) %>%
  dplyr::select(-speciesGroup) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(observations_seabirds_aggregate_BYS,"BYS summary/Tables/observations_seabirds_BYS.xlsx")

## analyze negative seabirds observations
negative_observations_seabirds_aggregate_BYS <- observations %>%
  filter(grepl('BYS', fishopTargetSpecies)) %>%
  filter(speciesGroup=='NO BIRDS OBSERVED') %>%
  group_by(Year, Gear) %>%  
  summarise(n = n()) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Fishing events with no birds observed" = n)

write_xlsx(negative_observations_seabirds_aggregate_BYS,"BYS summary/Tables/negative_observations_seabirds_BYS.xlsx")

## analyze non-performed seabirds observations
no_observations_seabirds_aggregate_BYS <- observations %>%
  filter(grepl('BYS', fishopTargetSpecies)) %>%
  filter(speciesGroup=='NO OBSERVER') %>%
  group_by(Year, Gear) %>%  
  summarise(n = n()) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Fishing events not observed for bird presence" = n)

write_xlsx(no_observations_seabirds_aggregate_BYS,"BYS summary/Tables/no_observations_seabirds_BYS.xlsx")

## analyze marine mammals observations during fishing events
observations_mammals_aggregate_BYS <- observations %>%
  filter(speciesGroup=='MAMMALS') %>%
  filter(!speciesGroup=='NO BIRDS OBSERVED') %>%
  filter(!speciesGroup=='NO OBSERVER') %>%
  filter(!speciesGroup=='SEABIRDS') %>%
  filter(grepl('BYS', fishopTargetSpecies)) %>%
  group_by(Year, speciesEnglishName, speciesScientificName, Gear, speciesGroup) %>%
  summarise(Abundance = sum(Abundance)) %>%
  dplyr::select(-speciesGroup) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(observations_mammals_aggregate_BYS,"BYS summary/Tables/observations_mammals_BYS.xlsx")

## analyze countries that submitted observer data (measures of fish) in BYS fishery
countries_observing_BYS <- Observer_data %>%
  #filter(Year>=2013) %>%
  filter((species3ACode == "BYS") | (species3ACode == "BXD")) %>%
  group_by(Year,CountryISOCode) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::select(-n) %>%
  rename(Country = CountryISOCode) 

write_xlsx(countries_observing_BYS,"BYS summary/Tables/countries_observing_BYS.xlsx")

# measures of maturity, sex and weight, and otoliths collected for BYS
measured_fish_BYS <- Observer_data %>%
  #filter(Year>=2013) %>%
  filter((species3ACode == "BYS") | (species3ACode == "BXD")) %>%
  filter(Year<=2022) %>%
  group_by(Year) %>%
  summarise_all(funs(sum(!is.na(.)))) %>%
  dplyr::select(Year, bsLength, bsWeight,bsOtolithCollected,bsSex,bsMaturity,bsGonadWeight,bsStomachSampled) %>%
  rename('Maturity (n)'=bsMaturity) %>%
  rename('Sex (n)'=bsSex) %>%
  rename('Gonad weight (n)'=bsGonadWeight) %>%
  rename('Length (n)'=bsLength) %>%
  rename('Weight (n)'=bsWeight) %>%
  rename('Otoliths collected (n)'=bsOtolithCollected) %>%
  rename('Stomachs sampled (n)'=bsStomachSampled) 

sums_measured_fish_BYS <- colSums(measured_fish_BYS)
measured_fish_BYS <- rbind(measured_fish_BYS,sums_measured_fish_BYS)

write_xlsx(measured_fish_BYS,"BYS summary/Tables/measured_fish_BYS.xlsx")

## analyze countries participating in BYS fishery
countries_fishing_ALF <- fishing %>%
  filter(ActivityID %in% yearly_operations_BYS_target$ActivityID 
         & datasetID %in% yearly_operations_BYS_target$datasetID |
           Target == "BYS") %>%
  group_by(Year,CCPCode1,dbSource) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::select(-n) %>%
  rename(Country = CCPCode1) %>%
  rename(Database = dbSource) 
write_xlsx(countries_fishing_ALF,"BYS summary/Tables/countries_fishing_BYS.xlsx")
