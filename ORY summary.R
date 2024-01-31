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

#subset for orange roughy fishing activity only in order to map it
# subset the whole data for all species, need catch, effort and subarea aggregations
yearly_global_catches_species <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, Longitude=fishing$Longitude, Latitude=fishing$Latitude), FUN=sum, na.rm = TRUE)
yearly_trawl_effort_species <- aggregate(fishing$NbTows, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, ActivityID=fishing$ActivityID, Longitude=fishing$Longitude, Latitude=fishing$Latitude), FUN=sum, na.rm = TRUE)

# subset for orange roughy only data 
# !!!!! check well how the trawl effort is calculated when missing data
yearly_global_catches_roughy <- filter(yearly_global_catches_species, (Species == "ORY") | (Species == "HPR") | (Species == "FSZ"))
yearly_trawl_effort_roughy_activity <- filter(yearly_trawl_effort_species, (Species == "ORY") | (Species == "HPR") | (Species == "FSZ"))

# turn the trawl effort locations in a spatial dataframe
# eliminate data points outside of the SIOFA area
fishing_spatial_roughy <- st_as_sf(yearly_trawl_effort_roughy_activity, coords = c( "Longitude", "Latitude"), crs = 4326)
fishing_spatial_roughy <- fishing_spatial_roughy[fishing_boundaries, ]

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

roughy_poly5 <- st_join(fishing_spatial_roughy, grid5)
roughy_count5 <- roughy_poly5 %>% group_by(id) %>% count()
roughy_count5 <- st_drop_geometry(roughy_count5)
roughy_poly5 <- left_join(grid5, roughy_count5, by="id")
roughy_poly5 <- filter(roughy_poly5, (!is.na(roughy_poly5$n)))

# calculate number of fishing events per 1 degree grid cell

roughy_poly1 <- st_join(fishing_spatial_roughy, grid1)
roughy_count1 <- roughy_poly1 %>% group_by(id) %>% count()
roughy_count1 <- st_drop_geometry(roughy_count1)
roughy_poly1 <- left_join(grid1, roughy_count1, by="id")
roughy_poly1 <- filter(roughy_poly1, (!is.na(roughy_poly1$n)))


# calculate number of fishing events per 30 minutes grid cell

roughy_poly30 <- st_join(fishing_spatial_roughy, grid30)
roughy_count30 <- roughy_poly30 %>% group_by(id) %>% count()
roughy_count30 <- st_drop_geometry(roughy_count30)
roughy_poly30 <- left_join(grid30, roughy_count30, by="id")
roughy_poly30 <- filter(roughy_poly30, (!is.na(roughy_poly30$n)))


# calculate number of fishing events per 20 minutes grid cell

roughy_poly20 <- st_join(fishing_spatial_roughy, grid20)
roughy_count20 <- roughy_poly20 %>% group_by(id) %>% count()
roughy_count20 <- st_drop_geometry(roughy_count20)
roughy_poly20 <- left_join(grid20, roughy_count20, by="id")
roughy_poly20 <- filter(roughy_poly20, (!is.na(roughy_poly20$n)))


# create hybrid map, delete all squares without fishing activity, clip out of boundaries

roughy_footprint_hybrid <- st_join(roughy_poly30, roughy_poly20, left = T)
roughy_footprint_hybrid <- filter(roughy_footprint_hybrid, (!is.na(roughy_footprint_hybrid$n.x)))
roughy_footprint_hybrid <- filter(roughy_footprint_hybrid, (!is.na(roughy_footprint_hybrid$n.y)))
roughy_footprint_hybrid <- roughy_footprint_hybrid[fishing_boundaries,]


# classify interval of gridline cells for plotting

roughy_breaks_qt5 <- classIntervals(c(min(roughy_poly5$n) - .00001, roughy_poly5$n), n = 7, style = "pretty")
roughy_poly5 <- mutate(roughy_poly5, n.events5 = cut(n, roughy_breaks_qt5$brks))

roughy_breaks_qt1 <- classIntervals(c(min(roughy_poly1$n) - .00001, roughy_poly1$n), n = 7, style = "pretty")
roughy_poly1 <- mutate(roughy_poly1, n.events1 = cut(n, roughy_breaks_qt1$brks))

roughy_breaks_qt30 <- classIntervals(c(min(roughy_poly30$n) - .00001, roughy_poly30$n), n = 7, style = "pretty")
roughy_poly30 <- mutate(roughy_poly30, n.events30 = cut(n, roughy_breaks_qt30$brks))

roughy_breaks_qt20 <- classIntervals(c(min(roughy_poly20$n) - .00001, roughy_poly20$n), n = 7, style = "pretty")
roughy_poly20 <- mutate(roughy_poly20, n.events20 = cut(n, roughy_breaks_qt20$brks))


# crop layers on SIOFA agreement area boundaries

roughy_poly5 <- st_intersection(roughy_poly5, fishing_boundaries)
roughy_poly1 <- st_intersection(roughy_poly1, fishing_boundaries)
roughy_poly30 <- st_intersection(roughy_poly30, fishing_boundaries)
roughy_poly20 <- st_intersection(roughy_poly20, fishing_boundaries)

# plot world map and add SIOFA subareas + hybrid resolution SIOFA ORY fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = roughy_poly20, fill = "orchid3") +
  geom_sf(data = roughy_poly30, fill = "orchid3") +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA ORY fishing activities (hybrid 20'+30', 2013–2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 


ggsave("ORY summary/SIOFAmap_roughy_hybrid_web.png", width = 10, height = 10, dpi = 150)

# plot plot world map and add SIOFA subareas + 5 degree resolution SIOFA ORY fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf(data = roughy_poly5, aes(fill=n.events5)) +
  scale_fill_viridis(discrete = TRUE) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(1,3,3.5, -1, 0, -2, 0, -6), nudge_x=c(0,-2.5,0, 6, 0, 0, 0, -6)) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA ORY fishing activities (5 degrees, 2013–2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("ORY summary/SIOFAmap_roughy_heatmap_5_web.png", width = 10, height = 7, dpi = 150)

# plot plot world map and add SIOFA subareas + 1 degree resolution siofa ORY fishing events on world map

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = roughy_poly1, aes(fill=n.events1)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA ORY fishing activities (1 degree, 2013–2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("ORY summary/SIOFAmap_roughy_heatmap_1_web.png", width = 10, height = 7, dpi = 150)

# plot plot world map and add SIOFA subareas + 30' resolution siofa ORY fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = roughy_poly30, aes(fill=n.events30)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA ORY fishing activities (30', 2013–2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("ORY summary/SIOFAmap_roughy_heatmap_30_web.png", width = 10, height = 7, dpi = 150)

# plot plot world map and add SIOFA subareas + 20' resolution siofa ORY fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = roughy_poly20, aes(fill=n.events20)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA ORY fishing activities (20', 2013–2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("ORY summary/SIOFAmap_roughy_heatmap_20_web.png", width = 10, height = 7, dpi = 150)

# test the export of layers as shapefiles
#roughy_poly1 <- st_collection_extract(roughy_poly1, "POLYGON")
#st_write(roughy_poly1,dsn = 'D:/SIOFA/Data/test', layer = 'SIOFAmap_roughy_heatmap_1', driver='ESRI Shapefile')

###catch/bycatch ratio ORY fisheries
#get the data ready to plot
yearly_global_catches_roughy #total target catch per year in ORY target fisheries

# non-target catch in ORY fisheries
yearly_nontarget_catches_roughy <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, TargetSpecies=fishing$Target, Species=fishing$SpeciesCode, ActivityID=fishing$ActivityID), FUN=sum, na.rm = TRUE)
yearly_nontarget_catches_roughy <- filter(yearly_nontarget_catches_roughy,(TargetSpecies == "ORY"))
yearly_nontarget_catches_roughy <- filter(yearly_nontarget_catches_roughy,(Species != "ORY"))
yearly_nontarget_catches_roughy <- aggregate(yearly_nontarget_catches_roughy$x, by=list(Year=yearly_nontarget_catches_roughy$Year, Species=yearly_nontarget_catches_roughy$Species), FUN=sum, na.rm = TRUE)

# plot a graph of non-target catch by species
ggplot(data=yearly_nontarget_catches_roughy, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2018, 2023)) +
  labs(title="Yearly bycatch by species in the fisheries targeting ORY in the SIOFA area", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("ORY summary/SIOFAcatches_nontarget_ORY_web.png", width = 10, height = 6, dpi = 150)  

# Calculate an overall target/non-target catch ratio
yearly_nontarget_catches_roughy <- aggregate(yearly_nontarget_catches_roughy$x, by=list(Year=yearly_nontarget_catches_roughy$Year), FUN=sum, na.rm = TRUE)
names(yearly_nontarget_catches_roughy)[names(yearly_nontarget_catches_roughy) == "x"] <- "NonTargetCatch"

yearly_target_catches_roughy <- aggregate(yearly_global_catches_roughy$x, by=list(Year=yearly_global_catches_roughy$Year), FUN=sum, na.rm = TRUE)

catch_bycatch_ORY <- full_join(yearly_target_catches_roughy, yearly_nontarget_catches_roughy)
names(catch_bycatch_ORY)[names(catch_bycatch_ORY) == "x"] <- "TargetCatch"

#transform data for easier plotting
catch_bycatch_ORY <- catch_bycatch_ORY %>% pivot_longer(!Year, names_to = "Catch", values_to = "t")

# plot target/non-target catch totals
ggplot(catch_bycatch_ORY, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly catch/bycatch in ORY fisheries in the SIOFA area (absolute)", x="Year", y="Total catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2018, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("ORY summary/SIOFAcatch_nontargetcatch_ORY_web.png", width = 10, height = 4, dpi = 150)

# plot target/non-target catch percentage
ggplot(catch_bycatch_ORY, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly catch/bycatch in ORY fisheries in the SIOFA area (relative)", x="Year", y="Total catch proportion") +
  theme_bw() +
  scale_x_continuous(limits=c(2018, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("ORY summary/SIOFAcatch_nontargetcatch_ORY_fill_web.png", width = 10, height = 4, dpi = 150)

#calculate total target and non-target catch per year and subarea in ORY fisheries
yearly_global_catches_subarea_species_ORY <- aggregate(fishing_within$CatchTon, by=list(Year=fishing_within$Year,SubArea=fishing_within$SubAreaNo, Target=fishing_within$Target, Species=fishing_within$SpeciesCode), FUN=sum, na.rm = TRUE)
yearly_global_catches_subarea_target_ORY <- filter(yearly_global_catches_subarea_species_ORY,(Target == "ORY"))
yearly_global_catches_subarea_nontarget_ORY <- filter(yearly_global_catches_subarea_target_ORY,(Species != "ORY"))
yearly_global_catches_subarea_target_ORY <- filter(yearly_global_catches_subarea_target_ORY,(Species == "ORY"))

names(yearly_global_catches_subarea_nontarget_ORY)[names(yearly_global_catches_subarea_nontarget_ORY) == "x"] <- "NonTargetCatch"
yearly_global_catches_subarea_nontarget_ORY$Species <- NULL
yearly_global_catches_subarea_nontarget_ORY$Target <- NULL
names(yearly_global_catches_subarea_target_ORY)[names(yearly_global_catches_subarea_target_ORY) == "x"] <- "TargetCatch"
yearly_global_catches_subarea_target_ORY$Species <- NULL
yearly_global_catches_subarea_target_ORY$Target <- NULL

catch_bycatch_subarea_ORY <- full_join(yearly_global_catches_subarea_nontarget_ORY, yearly_global_catches_subarea_target_ORY)


# plot histograms of target and non-target catch in ORY fisheries by year and subarea
ggplot(catch_bycatch_subarea_ORY, aes(x = Year, y = t)) +
  geom_bar(aes(fill=SubArea, y=TargetCatch), position="stack", stat="identity") +
  labs(title="Yearly targetcatch in fisheries targeting ORY by SIOFA subareas (absolute)", x="Year", y="Target catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2018, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("ORY summary/SIOFAtargetcatch_subarea_ORY_web.png", width = 10, height = 4, dpi = 150)

ggplot(catch_bycatch_subarea_ORY, aes(x = Year, y = t)) +
  geom_bar(aes(fill=SubArea, y=NonTargetCatch), position="stack", stat="identity") +
  labs(title="Yearly bycatch in fisheries targeting ORY by SIOFA subareas (absolute)", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2018, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("ORY summary/SIOFAnontargetcatch_subarea_ORY_web.png", width = 10, height = 4, dpi = 150)


# plot tables of target and non-target catch in ORY fisheries by year and subarea
catch_bycatch_subarea_ORY_table_subarea <- catch_bycatch_subarea_ORY %>%
  group_by(Year, SubArea) %>%
  summarise_all(sum) %>%
  mutate_at(vars(TargetCatch, NonTargetCatch), funs(round(., 1))) %>%
  rename('Catch (t)' = TargetCatch,
         'Bycatch (t)' = NonTargetCatch)

write_xlsx(catch_bycatch_subarea_ORY_table_subarea,"ORY summary/Tables/catch_bycatch_subarea_ORY_table_subarea.xlsx")

## report captures of sharks in the ORY fisheries
shark_species <- read_excel("sharks.xlsx")

yearly_global_catches_species_ORY <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, Target=fishing$Target, Species=fishing$SpeciesCode), FUN=sum, na.rm = TRUE)
yearly_global_catches_sharks_ORY <- filter(yearly_global_catches_species_ORY,(Target == "ORY"))
yearly_global_catches_sharks_ORY <- filter(yearly_global_catches_sharks_ORY, Species %in% shark_species$Code)

# it appears that there are no sharks caught when ORY was declared as a target


## plot length weight graph with a regression curve
# load data
#LW <- read_excel("ORY LW.xlsx", col_types = c("numeric", "numeric"))
# calculate a regression for the data
#fit <- lm(log(LW$Weight)~log(LW$Length),data=LW)
#summary(fit)
#aug.LW <- 
#  LW %>% 
#  add_predictions(fit) %>% 
#  add_residuals(fit) 
# plot data
#ggplot(aug.LW, aes(x=Length, y=Weight)) + 
#  geom_point() +
#  #geom_smooth(formula= y ~ , level= 0.9) +
#  geom_line(aes(y=exp(pred)),col='blue', size=1.2) +
#  labs(title="Lenght/weight relationship of ORY from observer records", x="Standard lenght (cm)", y="Weight (kg)") +
#  theme_bw() +
#  scale_x_continuous(limits=c(5, 75)) +
#  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 
#ggsave("ORY summary/SIOFAspecies_ORY_LW_web.png", width = 10, height = 4, dpi = 150)
                 

## catch/bycatch in management units

# load ORY management units boundaries
# load target species
ORY_MUs <- st_read('ORY_management_areas')

# plot world map and add SIOFA MUs + siofa fishing events just to check
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf(data = ORY_MUs, fill = "orchid2", color = "black") +
  #geom_sf(data = fishing_spatial, color = 'black', cex = .1) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf_label(data = ORY_MUs, aes(fill = NULL, label = name), size = 3, col = "black", label.size = 0.5) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA ORY Assessment Areas") +
  coord_sf(xlim = c(25, 65), ylim = c(-25, -50), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("ORY summary/SIOFA_ORY_MUs_web.png", width = 10, height = 7, dpi = 150)

# intersect fishing events with MUs
# Overlay points and extract just the subarea column 
fishing_within_ORY_MUs <- st_join(fishing_spatial, left = FALSE, ORY_MUs)

# create a data frame with only total catches in MUs, retaining MUs details
yearly_MUs_global_catches_ORY_MUs <- aggregate(fishing_within_ORY_MUs$CatchTon, by=list(Year=fishing_within_ORY_MUs$Year,MU=fishing_within_ORY_MUs$name, Species=fishing_within_ORY_MUs$SpeciesCode), FUN=sum, na.rm = TRUE)

# create a data frame with only total catches in all MUs, aggregated by year
yearly_ORY_MUs_global_catches <- aggregate(fishing_within_ORY_MUs$CatchTon, by=list(Year=fishing_within_ORY_MUs$Year), FUN=sum, na.rm = TRUE)

# create a data frame with only total catches in all MUs, aggregated by year AND MU
yearly_ORY_MUs_total_catches_MU <- aggregate(fishing_within_ORY_MUs$CatchTon, by=list(Year=fishing_within_ORY_MUs$Year,MU=fishing_within_ORY_MUs$name), FUN=sum, na.rm = TRUE)

#calculate only target catch per year within all MUs
yearly_ORY_MUs_catches_target <- filter(yearly_MUs_global_catches_ORY_MUs, Species == "ORY")
yearly_ORY_MUs_catches_target <- aggregate(yearly_ORY_MUs_catches_target$x, by=list(Year=yearly_ORY_MUs_catches_target$Year), FUN=sum, na.rm = TRUE)

#calculate only target catch per year and MU
yearly_ORY_MUs_catches_MU_target <- filter(yearly_MUs_global_catches_ORY_MUs, Species == "ORY")
yearly_ORY_MUs_catches_MU_target <- aggregate(yearly_ORY_MUs_catches_MU_target$x, by=list(Year=yearly_ORY_MUs_catches_MU_target$Year, MU=yearly_ORY_MUs_catches_MU_target$MU), FUN=sum, na.rm = TRUE)

#subtract total target catch from total catch within MUs
names(yearly_ORY_MUs_catches_target)[names(yearly_ORY_MUs_catches_target) == "x"] <- "TargetCatch"
names(yearly_ORY_MUs_global_catches)[names(yearly_ORY_MUs_global_catches) == "x"] <- "TotalCatch"
catch_bycatch_ORY_MUs <- full_join(yearly_ORY_MUs_global_catches, yearly_ORY_MUs_catches_target)
catch_bycatch_ORY_MUs <- catch_bycatch_ORY_MUs %>%  mutate(NonTargetCatch = TotalCatch-TargetCatch)
catch_bycatch_ORY_MUs$TotalCatch <- NULL

#subtract total target catch from total catch within EACH MU
names(yearly_ORY_MUs_catches_MU_target)[names(yearly_ORY_MUs_catches_MU_target) == "x"] <- "TargetCatch"
names(yearly_ORY_MUs_total_catches_MU)[names(yearly_ORY_MUs_total_catches_MU) == "x"] <- "TotalCatch"
catch_bycatch_by_ORY_MU <- full_join(yearly_ORY_MUs_catches_MU_target, yearly_ORY_MUs_total_catches_MU)
catch_bycatch_by_ORY_MU <- catch_bycatch_by_ORY_MU%>% mutate(NonTargetCatch = TotalCatch-TargetCatch)
catch_bycatch_by_ORY_MU$TotalCatch <- NULL

#transform data for easier plotting
catch_bycatch_ORY_MUs <- catch_bycatch_ORY_MUs %>% pivot_longer(!Year, names_to = "Catch", values_to = "t")
#catch_bycatch_by_MU <- catch_bycatch_by_MU %>% pivot_longer(cols = "TargetCatch":"NonTargetCatch", names_to = "Catch", values_to = "t")

## plot histograms of target/non-target catch in MUs per year 
#totals
ggplot(catch_bycatch_ORY_MUs, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly catch/bycatch in all SIOFA ORY assessment areas (absolute)", x="Year", y="Total catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("ORY summary/SIOFAcatch_nontargetcatch_ORY_MUs_web.png", width = 10, height = 4, dpi = 150)

#percentage
ggplot(catch_bycatch_ORY_MUs, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly catch/bycatch in all SIOFA ORY assessment areas (relative)", x="Year", y="Total catch proportion") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("ORY summary/SIOFAcatch_nontargetcatch_ORY_MUs_fill_web.png", width = 10, height = 4, dpi = 150)

## plot histograms of target and non-target catch in MUs per year

#totals target
ggplot(catch_bycatch_by_ORY_MU, aes(x = Year, y = t)) +
  geom_bar(aes(fill=MU, y=TargetCatch), position="stack", stat="identity") +
  labs(title="Yearly ORY catch in SIOFA ORY assessment areas (absolute)", x="Year", y="Target catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("ORY summary/SIOFAtargetcatch_ORY_MUs_web.png", width = 10, height = 4, dpi = 150)

#totals target table
catch_bycatch_by_ORY_MU_table <- catch_bycatch_by_ORY_MU %>%
  dplyr::select(-NonTargetCatch) %>%
  mutate_at(vars(TargetCatch), funs(round(., 2))) %>%
  pivot_wider(names_from = MU, values_from = c(TargetCatch),  names_sep = ' in ' )
write_xlsx(catch_bycatch_by_ORY_MU_table,"ORY summary/Tables/catch_bycatch_by_ORY_MU_table.xlsx")

#percentage of target catch in different MUs
ggplot(catch_bycatch_by_ORY_MU, aes(x = Year, y = t)) +
  geom_bar(aes(fill=MU, y=TargetCatch), position="fill", stat="identity") +
  labs(title="Yearly ORY catch in SIOFA ORY assessment areas (relative)", x="Year", y="Catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("ORY summary/SIOFAtargetcatch_ORY_MUs_fill_web.png", width = 10, height = 4, dpi = 150)


#totals non-target
ggplot(catch_bycatch_by_ORY_MU, aes(x = Year, y = t)) +
  geom_bar(aes(fill=MU, y=NonTargetCatch), position="stack", stat="identity") +
  labs(title="Yearly catch of all other species in SIOFA ORY assessment areas (absolute)", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("ORY summary/SIOFAnontargetcatch_ORY_MUs_web.png", width = 10, height = 4, dpi = 150)

#percentage of non-target catch in different MUs
ggplot(catch_bycatch_by_ORY_MU, aes(x = Year, y = t)) +
  geom_bar(aes(fill=MU, y=NonTargetCatch), position="fill", stat="identity") +
  labs(title="Yearly bycatch in SIOFA ORY assessment areas (relative)", x="Year", y="Catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("ORY summary/SIOFAnontargetcatch_ORY_MUs_fill_web.png", width = 10, height = 4, dpi = 150)

# target/non-target across all MUs
catch_bycatch_by_ORY_MU_across <- catch_bycatch_by_ORY_MU %>%
  drop_na(TargetCatch) %>%
  group_by(Year) %>%
  summarise_at(.vars = c("TargetCatch","NonTargetCatch"),
               .funs = "sum") %>%
  rename(ORY_catch = TargetCatch) %>%
  rename(Other_catch = NonTargetCatch)

#transform data for easier plotting
catch_bycatch_ORY_MU_across <- catch_bycatch_by_ORY_MU_across %>% 
  pivot_longer(!Year, names_to = "Catch", values_to = "t") 
  
#percentage of non-target catch in different MUs
ggplot(catch_bycatch_ORY_MU_across, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly ORY/all other species catch in SIOFA ORY assessment areas (relative)", x="Year", y="Catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("ORY summary/SIOFAnontargetcatch_ORY_MUs_across_fill_web.png", width = 10, height = 4, dpi = 150)

## alternative definition of target/non-target catch: if 50% of the catch in an operation
## is ORY, then that operation had ORY as a target 
## redo all analyses with this definition, for those events that didn't declare an ORY target
## i.e. prior to 2019

# select only operations that caught ORY (ignore all others)
# identify operations that caught roughy prior to 2019
# find total ORY catch by ActivityID
# need to retain year

yearly_operations_ORY <- fishing %>%
  filter(Year <2019) %>%
  group_by(Year, ActivityID, datasetID, Gear) %>% 
  filter(SpeciesCode == "ORY") %>%
  summarize(CatchTonORY = sum(CatchTon))

# find total catch by operation = sum weights of all species caught by an ActivityID that caught ORY
yearly_operations_ORY_total <- fishing %>%
  filter(ActivityID %in% yearly_operations_ORY$ActivityID & datasetID %in% yearly_operations_ORY$datasetID) %>%
  filter(Year <2019) %>%
  group_by(Year, ActivityID, Gear) %>% 
  summarize(TotalCatch = sum(CatchTon))

# if ORY catch >50% in an acitivtyID then retain those ActivityIDs as operations targeting ORY
# target/non-target ratio is actually already defined  
yearly_operations_ORY_target <- full_join(yearly_operations_ORY, yearly_operations_ORY_total)
yearly_operations_ORY_nontarget <- yearly_operations_ORY_target %>% 
  mutate(Ratio = CatchTonORY/TotalCatch) %>%
  filter(Ratio < 0.5) 
yearly_operations_ORY_target <- yearly_operations_ORY_target %>% 
  mutate(Ratio = CatchTonORY/TotalCatch) %>%
  filter(Ratio >= 0.5) 

# as a curousity, calculate catch proportion between target/non-target events
yearly_catch_nontarget <- yearly_operations_ORY_nontarget %>%
  group_by(Year) %>% 
  summarize(TotalCatchNontarget = sum(CatchTonORY))

yearly_catch_target <- yearly_operations_ORY_target %>%
  group_by(Year) %>% 
  summarize(TotalCatchTarget = sum(CatchTonORY))

combine <- left_join(yearly_catch_nontarget, yearly_catch_target)
combine <- combine %>%
  mutate(Percentage = TotalCatchNontarget/TotalCatchTarget*100)

ggplot(combine, aes(x = Year, y = Percentage)) +
  geom_bar(position="stack", stat="identity", fill="blue") +
  labs(title="% of ORY caught in hauls not identified as targeting ORY", x="Year", y="Catch (%)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2019)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

## as a curiousity, check what is the ratio in 2019 onwards
yearly_operations_ORY2019 <- fishing %>%
  filter(Year >=2019) %>%
  group_by(Year, ActivityID, datasetID, Gear) %>% 
  filter(Target == "ORY") 
yearly_operations_ORY_total2019 <- fishing %>%
  filter(ActivityID %in% yearly_operations_ORY2019$ActivityID & datasetID %in% yearly_operations_ORY2019$datasetID) %>%
  filter(Year >=2019) %>%
  group_by(Year, ActivityID, datasetID) %>% 
  summarize(TotalCatch = sum(CatchTon))
yearly_operations_ORY2019 <-  yearly_operations_ORY2019 %>%
  filter(SpeciesCode == "ORY") %>%
  summarize(CatchTonORY = sum(CatchTon))
yearly_operations_ORY_target2019 <- full_join(yearly_operations_ORY2019, yearly_operations_ORY_total2019)
yearly_operations_ORY2019 <- yearly_operations_ORY_target2019 %>% 
  mutate(Ratio = CatchTonORY/TotalCatch)
ggplot((data= yearly_operations_ORY2019), aes(x = ActivityID, y = Ratio)) +
  geom_point()

## analyze countries participating in ORY fishery
countries_fishing_ORY <- fishing %>%
  filter(ActivityID %in% yearly_operations_ORY_target$ActivityID 
         & datasetID %in% yearly_operations_ORY_target$datasetID |
         Target == "ORY") %>%
  group_by(Year,CCPCode1,dbSource) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::select(-n) %>%
  rename(Country = CCPCode1) %>%
  rename(Database = dbSource) 
write_xlsx(countries_fishing_ORY,"ORY summary/Tables/countries_fishing_ORY.xlsx")
    
## non-target catch by species in ORY target operations
yearly_operations_ORY_species <- fishing %>%
  filter(ActivityID %in% yearly_operations_ORY_target$ActivityID & datasetID %in% yearly_operations_ORY_target$datasetID) %>%
  filter(Year <2019) %>%
  filter(!SpeciesCode == "ORY") %>%
  group_by(Year,SpeciesCode) %>%
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  rename('Species' = SpeciesCode)

# join data after 2019
# calculate target/non-target catches roughy 2019 onwards
yearly_target_catches_roughy <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, TargetSpecies=fishing$Target, Species=fishing$SpeciesCode, ActivityID=fishing$ActivityID), FUN=sum, na.rm = TRUE)
yearly_target_catches_roughy <- filter(yearly_target_catches_roughy,(TargetSpecies == "ORY"))
yearly_nontarget_catches_roughy <- filter(yearly_target_catches_roughy,(Species != "ORY"))
yearly_nontarget_catches_roughy <- aggregate(yearly_nontarget_catches_roughy$x, by=list(Year=yearly_nontarget_catches_roughy$Year, Species=yearly_nontarget_catches_roughy$Species), FUN=sum, na.rm = TRUE)
yearly_nontarget_catches_roughy <- yearly_nontarget_catches_roughy %>%  
  rename('NonTargetCatch' = x) 

yearly_nontarget_catches_roughy <- rbind(yearly_nontarget_catches_roughy,yearly_operations_ORY_species)

# sort
sort_bycatch_ORY <- aggregate(yearly_nontarget_catches_roughy$NonTargetCatch, by=list(Species=yearly_nontarget_catches_roughy$Species), FUN=sum, na.rm = TRUE)
sort_bycatch_ORY <- arrange(sort_bycatch_ORY, desc(x)) 
top5_species <- sort_bycatch_ORY %>% slice(1:5)
other_species <- sort_bycatch_ORY %>% slice(6:89)
# plot
ggplot(data= subset(yearly_nontarget_catches_roughy, Species %in% top5_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_nontarget_catches_roughy, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly bycatch by species in the fisheries targeting ORY in the SIOFA area", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("ORY summary/SIOFAcatches_nontarget_ORY_web.png", width = 10, height = 6, dpi = 150)

# calculate target/non-target catch fraction pre 2019
yearly_operations_ORY_target <- yearly_operations_ORY_target %>%
  mutate(NonTargetCatch = TotalCatch-CatchTonORY) %>%
  mutate(TargetCatch = CatchTonORY)

### sharks non-target catch in ORY target operations prior to 2019
yearly_sharks_catches_ORY <- fishing %>%
  filter(ActivityID %in% yearly_operations_ORY_target$ActivityID & datasetID %in% yearly_operations_ORY$datasetID) %>%
  filter(Year <2019) %>%
  filter(!SpeciesCode == "ORY") %>%
  filter(SpeciesCode %in% shark_species$Code) %>%
  group_by(Year,SpeciesCode) %>%
  summarize(SharkCatch = sum(CatchTon)) %>%
  rename('Species' = SpeciesCode)

# plot sharks catch
ggplot(data= yearly_sharks_catches_ORY, aes(x = Year, y = SharkCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly bycatch of sharks in the fisheries targeting ORY in the SIOFA area", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("ORY summary/SIOFAcatches_sharks_ORY_web.png", width = 10, height = 6, dpi = 150)

## target/nontarget catch prior to 2019, spatial
# subareas
# non target
yearly_nontarget_ORY_spatial <- fishing_within %>%
  filter(ActivityID %in% yearly_operations_ORY_target$ActivityID & datasetID %in% yearly_operations_ORY_target$datasetID) %>%
  filter(Year <2019) %>%
  filter(!SpeciesCode == "ORY") %>%
  group_by(Year, SubAreaNo) %>%
  summarize(NonTargetCatch = sum(CatchTon)) 

# target
yearly_target_ORY_spatial <- fishing_within %>%
  filter(ActivityID %in% yearly_operations_ORY_target$ActivityID & datasetID %in% yearly_operations_ORY_target$datasetID) %>%
  filter(Year <2019) %>%
  filter(SpeciesCode == "ORY") %>%
  group_by(Year, SubAreaNo) %>%
  summarize(TargetCatch = sum(CatchTon)) 

# drop geometry join with 2019 onwards
yearly_nontarget_ORY_spatial$geometry <- NULL
yearly_target_ORY_spatial$geometry <- NULL
target_nontarget_ORY <- left_join(yearly_target_ORY_spatial, yearly_nontarget_ORY_spatial)
catch_bycatch_subarea_ORY <- catch_bycatch_subarea_ORY %>% rename('SubAreaNo'=SubArea)
#catch_bycatch_subarea_ORY <- catch_bycatch_subarea_ORY %>% rename('SubAreaNo' = SubArea)
catch_bycatch_subarea_ORY <- rbind(catch_bycatch_subarea_ORY, target_nontarget_ORY)

# change NAs with 0s before plotting
catch_bycatch_subarea_ORY <- catch_bycatch_subarea_ORY %>%
  replace(is.na(.), 0)
  
# plot histograms of target and non-target catch in ORY fisheries by year and subarea
ggplot(catch_bycatch_subarea_ORY, aes(x = Year, y = TargetCatch)) +
  geom_bar(aes(fill=SubAreaNo), position="fill", stat="identity") +
  labs(title="Yearly target catch in fisheries targeting ORY by SIOFA subareas (relative)", x="Year", y="Catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("ORY summary/SIOFAtargetcatch_subarea_ORY_web.png", width = 10, height = 4, dpi = 150)

ggplot(catch_bycatch_subarea_ORY, aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=SubAreaNo), position="fill", stat="identity") +
  labs(title="Yearly bycatch in fisheries targeting ORY by SIOFA subareas (relative)", x="Year", y="Catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("ORY summary/SIOFAnontargetcatch_subarea_ORY_web.png", width = 10, height = 4, dpi = 150)


# Calculate target/non-target catch 2019 onwards
yearly_target_catches_roughy <- yearly_operations_ORY2019 %>%
  mutate(NonTargetCatch = TotalCatch-CatchTonORY) %>%
  mutate(TargetCatch = CatchTonORY) %>%
  group_by(Year) %>%
  filter(CatchTonORY >0) %>%
  summarise_at(.vars = c("TargetCatch","NonTargetCatch"),
               .funs = "sum")

# join new and old data
yearly_operations_ORY_target <- yearly_operations_ORY_target %>%
    group_by(Year) %>%
    summarise_at(.vars = c("TargetCatch","NonTargetCatch"),
               .funs = "sum")

catch_bycatch_ORY <- rbind(yearly_target_catches_roughy, yearly_operations_ORY_target)
catch_bycatch_ORY <- catch_bycatch_ORY %>% arrange(Year)

# transform data for easier plotting
catch_bycatch_ORY <- catch_bycatch_ORY %>% pivot_longer(!Year, names_to = "Catch", values_to = "t")

# plot target/non-target catch totals
ggplot(catch_bycatch_ORY, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly catch/bycatch in ORY fisheries in the SIOFA area (absolute)", x="Year", y="Total catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("ORY summary/SIOFAcatch_nontargetcatch_ORY_web.png", width = 10, height = 4, dpi = 150)

# plot target/non-target catch percentage
ggplot(catch_bycatch_ORY, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly catch/bycatch in ORY fisheries in the SIOFA area (relative)", x="Year", y="Catch proportion") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("ORY summary/SIOFAcatch_nontargetcatch_ORY_fill_web.png", width = 10, height = 4, dpi = 150)

## VME catches in ORY fisheries

# filter only ORY target reports 
VME_catch_ORY <- filter(VME_catch, (TargetSpecies == "ORY") | 
                          (TargetSpecies == "ORY/EPI") |
                          (TargetSpecies == "ORY/RIB/BOE") |
                          (TargetSpecies == "ORY/BOE") |
                          (TargetSpecies == "ORY/SOR") )
# aggregate by year and taxon
yearly_ORY_VME_catch <- VME_catch_ORY %>%
  group_by(Year, FAOcode) %>%
  summarise_at(.vars = c("Weight"),
               .funs = "sum")
# plot ORY VME bycatch
ggplot(yearly_ORY_VME_catch, aes(x = Year, y = Weight, fill=FAOcode)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly incidental catch of VME indicator taxa in ORY fisheries in the SIOFA area", x="Year", y="Total weight (kg)") +
  theme_bw() +
  scale_x_continuous(limits=c(2002, 2023)) +
  #scale_fill_hue(labels = c("Non-target Catch", "Target Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("ORY summary/VME_captures_ORY_web.png", width = 10, height = 4, dpi = 150)

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
captures_spatial_plot_ORY <- captures_spatial  %>%
  filter(!(speciesGroup=='Chimaeras' |  speciesGroup=='Deepwater sharks' |  speciesGroup=='Sharks and rays nei' ) & species3ACode %in% shark_species_CMM$FAOcode
         | (speciesGroup=='Seabirds'| speciesGroup=='Cetaceans'| speciesGroup=='Turtles')) %>%
  filter(source=='OBS') %>%
  filter(grepl('ORY', fishopTargetSpecies))

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = captures_spatial_plot_ORY, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA incidental captures of seabirds, marine mammals, turtles and 
          sharks considered to be at high risk and/or of concern in ORY target operations (2012-2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  scale_colour_discrete("Captures") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("ORY summary/SIOFAmap_accidental_captures_web_ORY.png", width = 10, height = 8, dpi = 150)

# aggregate captures in a table that can be used in reports
# filter only operations that observers recorded as targeting ORY
captures_ORY <-   captures%>%
  filter(grepl('ORY', fishopTargetSpecies))

# turtles
captures_turtles <- filter(captures_ORY,(speciesGroup == 'Turtles'))

#captures_turtles_aggregate1 <- captures_turtles %>%
#  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
#  summarise(n = n()) 
captures_turtles_aggregate_ORY <- captures_turtles %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  relocate(speciesEnglishName, .after = Year) %>%
  relocate(speciesScientificName, .after = speciesEnglishName) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Captures" = foibcNbCaught) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(captures_turtles_aggregate_ORY,"ORY summary/Tables/captures_turtles_ORY.xlsx")

# marine mammals
captures_mammals <- filter(captures_ORY,(speciesGroup == 'Cetaceans'))

captures_mammals_aggregate_ORY <- captures_mammals %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) 

write_xlsx(captures_mammals_aggregate_ORY,"ORY summary/Tables/captures_mammals_ORY.xlsx")

# seabirds
captures_seabirds <- filter(captures_ORY,(speciesGroup == 'Seabirds'))

captures_seabirds_aggregate_ORY <- captures_seabirds %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Captures" = foibcNbCaught) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(captures_seabirds_aggregate_ORY,"ORY summary/Tables/captures_seabirds_ORY.xlsx")

# sharks
captures_sharks_ORY <- captures_ORY %>%
  filter(!(speciesGroup=='Chimaeras' |  speciesGroup=='Deepwater sharks' |  speciesGroup=='Sharks and rays nei' ) & species3ACode %in% shark_species_CMM$FAOcode)

captures_sharks_aggregate_ORY <- captures_sharks_ORY %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) %>%
  rename("Captures (n)" = foibcNbCaught)

write_xlsx(captures_sharks_aggregate_ORY,"ORY summary/Tables/captures_sharks_ORY.xlsx")

### Analyze observations of seabirds and mammals
# Convert the observation dataframe to a spatial object. Note that the
# crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object
# and that missing values are removed
observations_spatial <- filter(observations, (!is.na(observations$Longitude)))
observations_spatial <- st_as_sf(observations_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 
class(observations_spatial)
st_crs(observations_spatial) <- 4326

observations_spatial_plot_ORY <- observations_spatial  %>%
  filter(!(speciesGroup=='NO BIRDS OBSERVED' |  speciesGroup=='NO OBSERVER')) %>%
  filter(source=='OBS') %>%
  filter(grepl('ORY', fishopTargetSpecies))

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = observations_spatial_plot_ORY, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA observations of seabirds and marine mammals
            in ORY target operations (2012-2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  scale_colour_discrete("Observations") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("ORY summary/SIOFAmap_observations_web_ORY.png", width = 10, height = 8, dpi = 150)

## analyze seabirds observations
observations_seabirds_aggregate_ORY <- observations %>%
  filter(grepl('ORY', fishopTargetSpecies)) %>%
  filter(!speciesGroup=='MAMMALS') %>%
  filter(!speciesGroup=='NO BIRDS OBSERVED') %>%
  filter(!speciesGroup=='NO OBSERVER') %>%
  group_by(Year, speciesEnglishName, speciesScientificName, Gear, speciesGroup) %>%
  summarise(Abundance = sum(Abundance)) %>%
  dplyr::select(-speciesGroup) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(observations_seabirds_aggregate_ORY,"ORY summary/Tables/observations_seabirds_ORY.xlsx")

## analyze negative seabirds observations
negative_observations_seabirds_aggregate_ORY <- observations %>%
  filter(grepl('ORY', fishopTargetSpecies)) %>%
  filter(speciesGroup=='NO BIRDS OBSERVED') %>%
  group_by(Year, Gear) %>%  
  summarise(n = n()) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Fishing events with no birds observed" = n)

write_xlsx(negative_observations_seabirds_aggregate_ORY,"ORY summary/Tables/negative_observations_seabirds_ORY.xlsx")

## analyze non-performed seabirds observations
no_observations_seabirds_aggregate_ORY <- observations %>%
  filter(grepl('ORY', fishopTargetSpecies)) %>%
  filter(speciesGroup=='NO OBSERVER') %>%
  group_by(Year, Gear) %>%  
  summarise(n = n()) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Fishing events not observed for bird presence" = n)

write_xlsx(no_observations_seabirds_aggregate_ORY,"ORY summary/Tables/no_observations_seabirds_ORY.xlsx")

## analyze marine mammals observations during fishing events
observations_mammals_aggregate_ORY <- observations %>%
  filter(speciesGroup=='MAMMALS') %>%
  filter(!speciesGroup=='NO BIRDS OBSERVED') %>%
  filter(!speciesGroup=='NO OBSERVER') %>%
  filter(!speciesGroup=='SEABIRDS') %>%
  filter(grepl('ORY', fishopTargetSpecies)) %>%
  group_by(Year, speciesEnglishName, speciesScientificName, Gear, speciesGroup) %>%
  summarise(Abundance = sum(Abundance)) %>%
  dplyr::select(-speciesGroup) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(observations_mammals_aggregate_ORY,"ORY summary/Tables/observations_mammals_ORY.xlsx")

## analyze countries that submitted observer data (measures of fish) in ORY fishery
countries_observing_ORY <- Observer_data %>%
  #filter(Year>=2013) %>%
  filter((species3ACode == "ORY") | (species3ACode == "HPR") | (species3ACode == "FSZ")) %>%
  group_by(Year,CountryISOCode) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::select(-n) %>%
  rename(Country = CountryISOCode) 

write_xlsx(countries_observing_ORY,"ORY summary/Tables/countries_observing_ORY.xlsx")

# measures of maturity, sex and weight, and otoliths collected for ORY
measured_fish_ORY <- Observer_data %>%
  #filter(Year>=2013) %>%
  filter((species3ACode == "ORY") | (species3ACode == "HPR") | (species3ACode == "FSZ")) %>%
  group_by(Year) %>%
  summarise_all(funs(sum(.>=0)), na.rm = TRUE) %>%
  dplyr::select(Year, bsLength, bsWeight,bsOtolithCollected,bsSex,bsMaturity,bsGonadWeight,bsStomachSampled) %>%
  rename('Maturity (n)'=bsMaturity) %>%
  rename('Sex (n)'=bsSex) %>%
  rename('Gonad weight (n)'=bsGonadWeight) %>%
  rename('Length (n)'=bsLength) %>%
  rename('Weight (n)'=bsWeight) %>%
  rename('Otoliths collected (n)'=bsOtolithCollected) %>%
  rename('Stomachs sampled (n)'=bsStomachSampled) 

sums_measured_fish_ORY <- colSums(measured_fish_ORY)
measured_fish_ORY <- rbind(measured_fish_ORY,sums_measured_fish_ORY)

write_xlsx(measured_fish_ORY,"ORY summary/Tables/measured_fish_ORY.xlsx")