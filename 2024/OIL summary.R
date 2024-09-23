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

#subset for  oilfish fishing activity only in order to map it
# subset the whole data for all species, need catch, effort and subarea aggregations
yearly_global_catches_species <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, Longitude=fishing$Longitude, Latitude=fishing$Latitude), FUN=sum, na.rm = TRUE)
yearly_longline_effort_species <- aggregate(fishing$NbHooks, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, ActivityID=fishing$ActivityID, Longitude=fishing$Longitude, Latitude=fishing$Latitude), FUN=sum, na.rm = TRUE)
yearly_global_catches_subarea_species <- aggregate(fishing_within$CatchTon, by=list(Year=fishing_within$Year,SubArea=fishing_within$SubAreaNo, Species=fishing_within$SpeciesCode), FUN=sum, na.rm = TRUE)
yearly_longline_effort_species_activity <- aggregate(fishing$NbHooks, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, activityID=fishing$ActivityID), FUN=sum, na.rm = TRUE)

# subset for oilfish only data 
# !!!!! double check how the longline effort is calculated when missing data
yearly_global_catches_oilfish <- filter(yearly_global_catches_species, (Species == "OIL") | (Species == "LEC"))
yearly_global_catches_subarea_oilfish <- filter(yearly_global_catches_subarea_species, (Species == "OIL") | (Species == "LEC"))
yearly_longline_effort_oilfish_activity <- filter(yearly_longline_effort_species_activity, (Species == "OIL") | (Species == "LEC"))

# toggle off to disaggregate effort per subarea
#yearly_longline_effort_oilfish <- aggregate(yearly_longline_effort_oilfish_activity$x, by=list(Year=yearly_longline_effort_oilfish_activity$Year), FUN=sum, na.rm = TRUE)
yearly_longline_effort_oilfish <- aggregate(yearly_longline_effort_oilfish_activity$x, by=list(Year=yearly_longline_effort_oilfish_activity$Year), FUN=sum, na.rm = TRUE)

# transform effort so that it is at a scale comparable to the one of the catches
yearly_longline_T_effort_oilfish <- yearly_longline_effort_oilfish
yearly_longline_T_effort_oilfish$x <- yearly_longline_T_effort_oilfish$x/10000

## plot table of OIL catch and effort by year 
OIL_catch_table <- yearly_global_catches_oilfish  %>%
  dplyr::select(-Species) %>%
  group_by(Year) %>%
  summarise_all(sum) %>%
  mutate_at(vars(x), funs(round(., 1))) %>%
  rename('Total catch (t)' = x)
OIL_catch_effort_table <- full_join(OIL_catch_table, yearly_longline_T_effort_oilfish)
OIL_catch_effort_table <- OIL_catch_effort_table %>% rename('Effort (10 thousand hooks)' = x)

write_xlsx(OIL_catch_effort_table,"OIL summary/Tables/OIL_catch_effort_table.xlsx")

## plot table of OIL catch by subarea 
OIL_catch_subarea_table <- yearly_global_catches_subarea_oilfish  %>%
  dplyr::select(-Species) %>%
  group_by(Year, SubArea) %>%
  summarise_all(sum) %>%
  mutate_at(vars(x), funs(round(., 1))) %>%
  pivot_wider(names_from = SubArea, values_from = x) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(Year, everything())

write_xlsx(OIL_catch_subarea_table,"OIL summary/Tables/OIL_catch_subarea_table.xlsx")

# plotting the data for oilfish
# first graph catch (bars) and effort (line) per year
# second graph catch by year and subarea

ggplot() +
  geom_bar(data=yearly_global_catches_oilfish, aes(x = Year, y = x, fill=Species), stat="identity") +
  #geom_bar(data=yearly_global_catches_subarea_oilfish, stat="identity", fill="darkolivegreen2") +
  geom_line(data=yearly_longline_T_effort_oilfish,  aes(x=Year,y=x, color="Effort"), inherit.aes = FALSE, size = 1) +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Longline effort (10 000 hooks)")) +
  labs(title="Yearly oilfish (OIL/LEC) catch in the SIOFA area", x="Year", y="Catch (t)") +
  theme_bw() +
  theme( axis.line.y.right = element_line(color = "blue"), 
         axis.ticks.y.right = element_line(color = "blue"),
         axis.text.y.right = element_text(color = "blue"), 
         axis.title.y.right = element_text(color = "blue")) +
  scale_fill_manual(values = c(OIL = "darkgoldenrod2", LEC = "red"), 
                    guide = guide_legend(title = "Fill", override.aes = list(linetype = "blank", shape = NA))) +
  scale_colour_manual(values = c("Effort" = "blue"),
                      guide = guide_legend(title = "Line", override.aes = list(linetype = "solid", shape = NA))) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("OIL summary/SIOFAcatches_effort_OIL_web.png", width = 10, height = 4, dpi = 150)  

ggplot(yearly_global_catches_subarea_oilfish, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly oilfish (OIL/LEC) catch by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("OIL summary/SIOFAcatches_OIL_subarea_web.png", width = 10, height = 4, dpi = 150)


# turn the longline effort locations in a spatial dataframe
# eliminate data points outside of the SIOFA area

yearly_longline_effort_oilfish_activity <- filter(yearly_longline_effort_species, (Species == "OIL") | (Species == "LEC"))

fishing_spatial_oilfish <- st_as_sf(yearly_longline_effort_oilfish_activity, coords = c( "Longitude", "Latitude"), crs = 4326)

fishing_spatial_oilfish <- fishing_spatial_oilfish[fishing_boundaries, ]


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

oilfish_poly5 <- st_join(fishing_spatial_oilfish, grid5)
oilfish_count5 <- oilfish_poly5 %>% group_by(id) %>% count()
oilfish_count5 <- st_drop_geometry(oilfish_count5)
oilfish_poly5 <- left_join(grid5, oilfish_count5, by="id")
oilfish_poly5 <- filter(oilfish_poly5, (!is.na(oilfish_poly5$n)))

# calculate number of fishing events per 1 degree grid cell

oilfish_poly1 <- st_join(fishing_spatial_oilfish, grid1)
oilfish_count1 <- oilfish_poly1 %>% group_by(id) %>% count()
oilfish_count1 <- st_drop_geometry(oilfish_count1)
oilfish_poly1 <- left_join(grid1, oilfish_count1, by="id")
oilfish_poly1 <- filter(oilfish_poly1, (!is.na(oilfish_poly1$n)))

# calculate number of fishing events per 30 minutes grid cell

oilfish_poly30 <- st_join(fishing_spatial_oilfish, grid30)
oilfish_count30 <- oilfish_poly30 %>% group_by(id) %>% count()
oilfish_count30 <- st_drop_geometry(oilfish_count30)
oilfish_poly30 <- left_join(grid30, oilfish_count30, by="id")
oilfish_poly30 <- filter(oilfish_poly30, (!is.na(oilfish_poly30$n)))

# calculate number of fishing events per 20 minutes grid cell

oilfish_poly20 <- st_join(fishing_spatial_oilfish, grid20)
oilfish_count20 <- oilfish_poly20 %>% group_by(id) %>% count()
oilfish_count20 <- st_drop_geometry(oilfish_count20)
oilfish_poly20 <- left_join(grid20, oilfish_count20, by="id")
oilfish_poly20 <- filter(oilfish_poly20, (!is.na(oilfish_poly20$n)))

# create hybrid map, delete all squares without fishing activity, clip out of boundaries

oilfish_footprint_hybrid <- st_join(oilfish_poly30, oilfish_poly20, left = T)
oilfish_footprint_hybrid <- filter(oilfish_footprint_hybrid, (!is.na(oilfish_footprint_hybrid$n.x)))
oilfish_footprint_hybrid <- filter(oilfish_footprint_hybrid, (!is.na(oilfish_footprint_hybrid$n.y)))
oilfish_footprint_hybrid <- oilfish_footprint_hybrid[fishing_boundaries,]

# classify interval of gridline cells for plotting

oilfish_breaks_qt5 <- classIntervals(c(min(oilfish_poly5$n) - .00001, oilfish_poly5$n), n = 7, style = "pretty")
oilfish_poly5 <- mutate(oilfish_poly5, n.events5 = cut(n, oilfish_breaks_qt5$brks))

oilfish_breaks_qt1 <- classIntervals(c(min(oilfish_poly1$n) - .00001, oilfish_poly1$n), n = 7, style = "pretty")
oilfish_poly1 <- mutate(oilfish_poly1, n.events1 = cut(n, oilfish_breaks_qt1$brks))

oilfish_breaks_qt30 <- classIntervals(c(min(oilfish_poly30$n) - .00001, oilfish_poly30$n), n = 7, style = "pretty")
oilfish_poly30 <- mutate(oilfish_poly30, n.events30 = cut(n, oilfish_breaks_qt30$brks))

oilfish_breaks_qt20 <- classIntervals(c(min(oilfish_poly20$n) - .00001, oilfish_poly20$n), n = 7, style = "pretty")
oilfish_poly20 <- mutate(oilfish_poly20, n.events20 = cut(n, oilfish_breaks_qt20$brks))


# crop layers on SIOFA agreement area boundaries

oilfish_poly5 <- st_intersection(oilfish_poly5, fishing_boundaries)
oilfish_poly1 <- st_intersection(oilfish_poly1, fishing_boundaries)
oilfish_poly30 <- st_intersection(oilfish_poly30, fishing_boundaries)
oilfish_poly20 <- st_intersection(oilfish_poly20, fishing_boundaries)

# plot world map and add SIOFA subareas + hybrid resolution SIOFA OIL fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = oilfish_poly20, fill = "orchid3") +
  geom_sf(data = oilfish_poly30, fill = "orchid3") +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA OIL fishing activities (hybrid 20'+30', 2013–2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 


ggsave("OIL summary/SIOFAmap_oilfish_hybrid_web.png", width = 10, height = 10, dpi = 150)


# plot world map and add SIOFA subareas + 5 degree resolution SIOFA OIL fishing events on world map

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf(data = oilfish_poly5, aes(fill=n.events5)) +
  scale_fill_viridis(discrete = TRUE) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(1,3,3.5, -1, 0, -2, 0, -6), nudge_x=c(0,-2.5,0, 6, 0, 0, 0, -6)) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA OIL/LEC fishing activities (5 degrees, 2013–2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("OIL summary/SIOFAmap_oilfish_heatmap_5_web.png", width = 10, height = 7, dpi = 150)

# plot world map and add SIOFA subareas + 1 degree resolution siofa OIL fishing events on world map

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = oilfish_poly1, aes(fill=n.events1)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA OIL fishing activities (1 degree, 2013–2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("OIL summary/SIOFAmap_oilfish_heatmap_1_web.png", width = 10, height = 7, dpi = 150)

# plot plot world map and add SIOFA subareas + 30' resolution siofa OIL fishing events on world map

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = oilfish_poly30, aes(fill=n.events30)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA OIL fishing activities (30', 2013–2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("OIL summary/SIOFAmap_oilfish_heatmap_30_web.png", width = 10, height = 7, dpi = 150)

# plot world map and add SIOFA subareas + 20' resolution siofa OIL fishing events on world map

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = oilfish_poly20, aes(fill=n.events20)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA OIL fishing activities (20', 2013–2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("OIL summary/SIOFAmap_oilfish_heatmap_20_web.png", width = 10, height = 7, dpi = 150)

###catch/bycatch ratio OIL fisheries
#get the data ready to plot

yearly_global_catches_oilfish #total target catch per year in OIL target fisheries

# non-target catch in OIL fisheries
yearly_nontarget_catches_oilfish <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, Target=fishing$Target, Species=fishing$SpeciesCode, ActivityID=fishing$ActivityID), FUN=sum, na.rm = TRUE)
yearly_nontarget_catches_oilfish <- yearly_nontarget_catches_oilfish %>%
  filter(Target == "OIL/LEC"| Target == "OIL" | Target == "LEC" | Target == "LEC/OIL") %>%
  filter(Species != "OIL", Species != "LEC") 

yearly_nontarget_catches_oilfish <- aggregate(yearly_nontarget_catches_oilfish$x, by=list(Year=yearly_nontarget_catches_oilfish$Year, Species=yearly_nontarget_catches_oilfish$Species), FUN=sum, na.rm = TRUE)

# plot a graph of non-target catch by species
# sort
#sort_bycatch_OIL <- aggregate(yearly_nontarget_catches_oilfish$x, by=list(Species=yearly_nontarget_catches_oilfish$Species), FUN=sum, na.rm = TRUE)
#sort_bycatch_OIL <- arrange(sort_bycatch_OIL, desc(x)) 
#OIL5_species <- sort_bycatch_OIL %>% slice(1:5)
#other_species <- sort_bycatch_OIL %>% slice(6:38)
# plot
#plot 
ggplot(data= subset(yearly_nontarget_catches_oilfish), aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  #geom_bar(data= subset(yearly_nontarget_catches_tarakihi, Species %in% other_species$Species), aes(x = Year, y = x, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2016, 2023)) +
  labs(title="Yearly bycatch by species in OIL/LEC fisheries in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("OIL summary/SIOFAcatches_nontarget_OIL_web.png", width = 10, height = 4, dpi = 150)

# Calculate an overall target/non-target catch ratio

yearly_nontarget_catches_oilfish <- aggregate(yearly_nontarget_catches_oilfish$x, by=list(Year=yearly_nontarget_catches_oilfish$Year), FUN=sum, na.rm = TRUE)
names(yearly_nontarget_catches_oilfish)[names(yearly_nontarget_catches_oilfish) == "x"] <- "NonTargetCatch"

yearly_target_catches_oilfish <- aggregate(yearly_global_catches_oilfish$x, by=list(Year=yearly_global_catches_oilfish$Year), FUN=sum, na.rm = TRUE)

catch_bycatch_OIL <- full_join(yearly_target_catches_oilfish, yearly_nontarget_catches_oilfish)
names(catch_bycatch_OIL)[names(catch_bycatch_OIL) == "x"] <- "TargetCatch"

#transform data for easier plotting
catch_bycatch_OIL <- catch_bycatch_OIL %>% pivot_longer(!Year, names_to = "Catch", values_to = "t")

# plot target/non-target catch totals

ggplot(catch_bycatch_OIL, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly target catch/bycatch in OIL/LEC fisheries in the SIOFA area (absolute)", x="Year", y="Total catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2016, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("OIL summary/SIOFAcatch_nontargetcatch_OIL_web.png", width = 10, height = 4, dpi = 150)

# plot target/non-target catch percentage

ggplot(catch_bycatch_OIL, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly target catch/bycatch in OIL fisheries in the SIOFA area (relative)", x="Year", y="Total catch proportion") +
  theme_bw() +
  scale_x_continuous(limits=c(2018, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("OIL summary/SIOFAcatch_nontargetcatch_OIL_fill_web.png", width = 10, height = 4, dpi = 150)

#calculate total target and non-target catch per year and subarea in OIL fisheries
yearly_global_catches_subarea_species_OIL <- aggregate(fishing_within$CatchTon, by=list(Year=fishing_within$Year,SubArea=fishing_within$SubAreaNo, Target=fishing_within$Target, Species=fishing_within$SpeciesCode), FUN=sum, na.rm = TRUE)

yearly_global_catches_subarea_nontarget_OIL <- yearly_global_catches_subarea_species_OIL %>%
  filter(Target == "OIL/LEC" | Target == "OIL" | Target == "LEC" | Target == "LEC/OIL") %>%
  filter(Species != "OIL" , Species != "LEC")

names(yearly_global_catches_subarea_nontarget_OIL)[names(yearly_global_catches_subarea_nontarget_OIL) == "x"] <- "NonTargetCatch"
yearly_global_catches_subarea_nontarget_OIL$Species <- NULL
yearly_global_catches_subarea_nontarget_OIL$Target <- NULL
yearly_global_catches_subarea_nontarget_OIL <- yearly_global_catches_subarea_nontarget_OIL %>%
  group_by(Year, SubArea) %>%
  dplyr::summarise_all(sum) 

yearly_global_catches_subarea_target_OIL <- yearly_global_catches_subarea_species_OIL %>%
  filter(Target == "OIL/LEC" | Target == "OIL" | Target == "LEC/OIL") %>%
  filter(Species == "OIL" | Species == "LEC")

names(yearly_global_catches_subarea_target_OIL)[names(yearly_global_catches_subarea_target_OIL) == "x"] <- "TargetCatch"
yearly_global_catches_subarea_target_OIL$Species <- NULL
yearly_global_catches_subarea_target_OIL$Target <- NULL
yearly_global_catches_subarea_target_OIL <- yearly_global_catches_subarea_target_OIL %>%
  group_by(Year, SubArea) %>%
  dplyr::summarise_all(sum) 

catch_bycatch_subarea_OIL <- full_join(yearly_global_catches_subarea_nontarget_OIL, yearly_global_catches_subarea_target_OIL)


# plot histograms of target and non-target catch in OIL fisheries by year and subarea

ggplot(catch_bycatch_subarea_OIL, aes(x = Year, y = t)) +
  geom_bar(aes(fill=SubArea, y=TargetCatch), position="stack", stat="identity") +
  labs(title="Yearly catch in OIL/LEC fisheries by SIOFA subareas (absolute)", x="Year", y="Target catch (t)") +
  scale_fill_manual(values=c("#8dd3c7", "#ffffb3", "#fb8072")) +
  #scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2016, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("OIL summary/SIOFAtargetcatch_subarea_OIL_web.png", width = 10, height = 4, dpi = 150)

ggplot(catch_bycatch_subarea_OIL, aes(x = Year, y = t)) +
  geom_bar(aes(fill=SubArea, y=NonTargetCatch), position="stack", stat="identity") +
  labs(title="Yearly bycatch in OIL/LEC fisheries by SIOFA subareas (absolute)", x="Year", y="Bycatch (t)") +
  scale_fill_manual(values=c("#8dd3c7", "#ffffb3", "#fb8072")) +
  #scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2016, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("OIL summary/SIOFAnontargetcatch_subarea_OIL_web.png", width = 10, height = 4, dpi = 150)

# plot tables of target and non-target catch in OIL fisheries by year and subarea

catch_bycatch_subarea_OIL_table_subarea <- catch_bycatch_subarea_OIL %>%
  group_by(Year, SubArea) %>%
  summarise_all(sum) %>%
  mutate_at(vars(TargetCatch, NonTargetCatch), funs(round(., 1))) %>%
  rename('Target catch (t)' = TargetCatch,
         'Bycatch (t)' = NonTargetCatch)

write_xlsx(catch_bycatch_subarea_OIL_table_subarea,"OIL summary/Tables/catch_bycatch_subarea_OIL_table_subarea.xlsx")


###catch/bycatch ratio OIL fisheries
## Better definition of target/non-target catch: 
## if 20% of the catch in an operation
## is OIL, then that operation had OIL as a target 
## redo all analyses with this definition, for those events that didn't declare an OIL target

# select only operations that caught OIL (ignore all others)
# identify any operations that caught oilfish in all the dataset, 
# whenever targets were not declared
# find total OIL catch by ActivityID
# need to retain year

yearly_operations_OIL <- fishing %>%
  #filter(Year==2014 | Year==2015 |Year==2016 |Year==2017 |Year==2018) %>%
  group_by(Year, ActivityID, datasetID, Gear) %>% 
  filter(SpeciesCode == "OIL"| SpeciesCode== "LEC") %>%
  filter(is.na(Target)) %>%
  summarize(CatchTonOIL = sum(CatchTon))

# find total catch by operation = sum weights of all species caught by 
# an ActivityID that caught OIL
# in years where target was not declared (in 2014-15-16-17-18)
yearly_operations_OIL_total <- fishing %>%
  filter(ActivityID %in% yearly_operations_OIL$ActivityID & datasetID %in% yearly_operations_OIL$datasetID) %>%
  group_by(Year, ActivityID, Gear) %>% 
  summarize(TotalCatch = sum(CatchTon))

# if OIL catch >20% in an acitivtyID then retain those ActivityIDs as operations targeting OIL
# target/non-target ratio is actually already defined  
yearly_operations_OIL_target <- full_join(yearly_operations_OIL, yearly_operations_OIL_total)
yearly_operations_OIL_target <- yearly_operations_OIL_target %>% 
  mutate(Ratio = CatchTonOIL/TotalCatch) %>%
  filter(Ratio >= 0.2) 

## non-target catch by species in OIL target operations in 2014-15-16-17-18
yearly_operations_OIL_species <- fishing %>%
  filter(ActivityID%in%yearly_operations_OIL_target$ActivityID & datasetID%in%yearly_operations_OIL_target$datasetID) %>%
  filter(!SpeciesCode == "OIL" | !SpeciesCode== "LEC") %>%
  group_by(Year,SpeciesCode) %>%
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  rename('Species' = SpeciesCode)

# join data in years where OIL targets were declared
# calculate target/non-target catches oilfish 
yearly_target_catches_oilfish <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, TargetSpecies=fishing$Target, Species=fishing$SpeciesCode, ActivityID=fishing$ActivityID), FUN=sum, na.rm = TRUE)
yearly_target_catches_oilfish <- filter(yearly_target_catches_oilfish,
                                        (TargetSpecies == "OIL/LEC"| TargetSpecies == "OIL" | TargetSpecies == "LEC" | TargetSpecies == "LEC/OIL"))
yearly_nontarget_catches_oilfish <- filter(yearly_target_catches_oilfish,(Species != "OIL" | Species != "LEC"))
yearly_nontarget_catches_oilfish <- aggregate(yearly_nontarget_catches_oilfish$x, by=list(Year=yearly_nontarget_catches_oilfish$Year, Species=yearly_nontarget_catches_oilfish$Species), FUN=sum, na.rm = TRUE)
yearly_nontarget_catches_oilfish <- yearly_nontarget_catches_oilfish %>%  
  rename('NonTargetCatch' = x) 

# join non-target data from both years when catch was declared and not declared
yearly_nontarget_catches_oilfish <- rbind(yearly_nontarget_catches_oilfish,yearly_operations_OIL_species)

# sort bycatch data for plotting
sort_bycatch_OIL <- aggregate(yearly_nontarget_catches_oilfish$NonTargetCatch, by=list(Species=yearly_nontarget_catches_oilfish$Species), FUN=sum, na.rm = TRUE)
sort_bycatch_OIL <- arrange(sort_bycatch_OIL, desc(x)) 
OIL5_species <- sort_bycatch_OIL %>% slice(1:5)
other_species <- sort_bycatch_OIL %>% slice(6:11)
# plot
ggplot(data= subset(yearly_nontarget_catches_oilfish, Species %in% OIL5_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_nontarget_catches_oilfish, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly bycatch by species in OIL/LEC fisheries in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("OIL summary/SIOFAcatches_nontarget_OIL_web.png", width = 10, height = 5, dpi = 150)

# calculate target/non-target catch fraction in 2014-15-17-18
yearly_operations_OIL_target <- yearly_operations_OIL_target %>%
  mutate(NonTargetCatch = TotalCatch-CatchTonOIL) %>%
  mutate(TargetCatch = CatchTonOIL)

## sharks non-target catch in all OIL target operations 
shark_species <- read_excel("sharks.xlsx")
yearly_sharks_catches_OIL <- yearly_nontarget_catches_oilfish %>%
  filter(Species %in% shark_species$Code) 

# sort shark bycatch data for plotting
sort_shark_bycatch_OIL <- yearly_sharks_catches_OIL %>% 
  group_by(Species) %>%
  summarize(x = sum(NonTargetCatch))
sort_shark_bycatch_OIL <- arrange(sort_shark_bycatch_OIL, desc(x)) 
OIL5_shark_species <- sort_shark_bycatch_OIL %>% slice(1:5)
other_shark_species <- sort_shark_bycatch_OIL %>% slice(6:28)
# plot sharks catch 
ggplot(data= subset(yearly_sharks_catches_OIL, Species %in% OIL5_shark_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_sharks_catches_OIL, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly bycatch of sharks in OIL/LEC fisheries in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("OIL summary/SIOFAcatches_sharks_OIL_web.png", width = 10, height = 6, dpi = 150)

## target/nontarget catch in non-declared events, spatial
# subareas
# non target
yearly_nontarget_OIL_spatial <- fishing_within %>%
  filter(ActivityID %in% yearly_operations_OIL_target$ActivityID & datasetID %in% yearly_operations_OIL_target$datasetID) %>%
  filter(!SpeciesCode == "OIL") %>%
  filter(!SpeciesCode== "LEC") %>%
  group_by(Year, SubAreaNo) %>%
  summarize(NonTargetCatch = sum(CatchTon)) 

# target
yearly_target_OIL_spatial <- fishing_within %>%
  filter(ActivityID %in% yearly_operations_OIL_target$ActivityID & datasetID %in% yearly_operations_OIL_target$datasetID) %>%
  filter(SpeciesCode == "OIL"| SpeciesCode== "LEC") %>%
  group_by(Year, SubAreaNo) %>%
  summarize(TargetCatch = sum(CatchTon)) 

# drop geometry 
yearly_nontarget_OIL_spatial$geometry <- NULL
yearly_target_OIL_spatial$geometry <- NULL
# join target and non-target 2014-15-17-18 catches, by subarea
target_nontarget_OIL <- left_join(yearly_target_OIL_spatial, yearly_nontarget_OIL_spatial)

# calculate target and non-target catch in years when target declarations were made
catch_target_subarea_OIL <- fishing_within %>%
  filter((Target == "OIL/LEC"| Target == "OIL" | Target == "LEC" | Target == "LEC/OIL") & (SpeciesCode== "OIL"| SpeciesCode== "LEC")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year, SubAreaNo) %>% 
  summarize(TargetCatch = sum(CatchTon)) %>%
  filter(TargetCatch>0)

catch_nontarget_subarea_OIL <- fishing_within %>%
  filter((Target == "OIL/LEC"| Target == "OIL" | Target == "LEC" | Target == "LEC/OIL") & (SpeciesCode!= "OIL"& SpeciesCode!= "LEC")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year, SubAreaNo) %>% 
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  filter(NonTargetCatch>0)

# drop geometry 
catch_target_subarea_OIL$geometry <- NULL
catch_nontarget_subarea_OIL$geometry <- NULL

# join target and nontarget catch in years when targets were declared
catch_bycatch_subarea_OIL <- left_join(catch_target_subarea_OIL, catch_nontarget_subarea_OIL)

# join non declared with OIL declared targets operations
catch_bycatch_subarea_OIL <- rbind(catch_bycatch_subarea_OIL, target_nontarget_OIL)

# plot histograms of target and non-target catch in OIL fisheries by year and subarea
ggplot(catch_bycatch_subarea_OIL, aes(x = Year, y = TargetCatch)) +
  geom_bar(aes(fill=SubAreaNo), position="fill", stat="identity") +
  labs(title="Yearly target catch in OIL/LEC fisheries by SIOFA subareas (relative)", x="Year", y="Target catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("OIL summary/SIOFAtargetcatch_subarea_OIL_web.png", width = 10, height = 4, dpi = 150)

ggplot(catch_bycatch_subarea_OIL, aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=SubAreaNo), position="fill", stat="identity") +
  labs(title="Yearly bycatch in OIL/LEC fisheries by SIOFA subareas (relative)", x="Year", y="Bycatch proportion") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("OIL summary/SIOFAnontargetcatch_subarea_OIL_web.png", width = 10, height = 4, dpi = 150)

# transform data for easier plotting
catch_bycatch_OIL <- catch_bycatch_subarea_OIL %>% 
  dplyr::select(-SubAreaNo) %>%
  pivot_longer(!Year, names_to = "Catch", values_to = "t")

# plot target/non-target catch totals
ggplot(catch_bycatch_OIL, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly target catch/bycatch in OIL/LEC fisheries in the SIOFA area (absolute)", x="Year", y="Total catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("OIL summary/SIOFAcatch_nontargetcatch_OIL_web.png", width = 10, height = 4, dpi = 150)

# plot target/non-target catch percentage
ggplot(catch_bycatch_OIL, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly target catch/bycatch in OIL/LEC fisheries in the SIOFA area (relative)", x="Year", y="Total catch proportion") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("OIL summary/SIOFAcatch_nontargetcatch_OIL_fill_web.png", width = 10, height = 4, dpi = 150)

# check again bycatch by species
# declared targets
bycatch_declared_OIL <- fishing_within %>%
  filter((Target == "OIL/LEC"| Target == "OIL" | Target == "LEC" | Target == "LEC/OIL") & (SpeciesCode!= "OIL"& SpeciesCode!= "LEC")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year, SpeciesCode) %>% 
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  filter(NonTargetCatch>0)
bycatch_declared_OIL$geometry <- NULL
# non declared targets
bycatch_nondeclared_OIL <- fishing_within %>%
  filter(ActivityID %in% yearly_operations_OIL_target$ActivityID & datasetID %in% yearly_operations_OIL_target$datasetID) %>%
  filter(!SpeciesCode == "OIL") %>%
  filter(!SpeciesCode== "LEC") %>%
  group_by(Year, SpeciesCode) %>%
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  filter(NonTargetCatch>0)
bycatch_nondeclared_OIL$geometry <- NULL
# join them
bycatch_OIL_species <- rbind(bycatch_declared_OIL, bycatch_nondeclared_OIL)
# sort bycatch data for plotting
sort_bycatch_OIL <- aggregate(bycatch_OIL_species$NonTargetCatch, by=list(Species=bycatch_OIL_species$SpeciesCode), FUN=sum, na.rm = TRUE)
sort_bycatch_OIL <- arrange(sort_bycatch_OIL, desc(x)) 
OIL5_species <- sort_bycatch_OIL %>% slice(1:5)
other_species <- sort_bycatch_OIL %>% slice(6:9)
# plot
ggplot(data= subset(yearly_nontarget_catches_oilfish, Species %in% OIL5_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_nontarget_catches_oilfish, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly bycatch by species in the fisheries targeting OIL in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("OIL summary/SIOFAcatches_nontarget_OIL_web.png", width = 10, height = 5, dpi = 150)

# check again bycatch by shark species
yearly_sharks_catches_OIL <- bycatch_OIL_species %>%
  filter(SpeciesCode %in% shark_species$Code) %>%
  rename(Species = SpeciesCode)
# sort shark bycatch data for plotting
sort_shark_bycatch_OIL <- yearly_sharks_catches_OIL %>% 
  group_by(Species) %>%
  summarize(x = sum(NonTargetCatch))
sort_shark_bycatch_OIL <- arrange(sort_shark_bycatch_OIL, desc(x)) 
OIL5_shark_species <- sort_shark_bycatch_OIL %>% slice(1:5)
other_shark_species <- sort_shark_bycatch_OIL %>% slice(6:16)
# plot sharks catch 
ggplot(data= subset(yearly_sharks_catches_OIL, Species %in% OIL5_shark_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_sharks_catches_OIL, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly bycatch of sharks in OIL/LEC fisheries in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("OIL summary/SIOFAcatches_sharks_OIL_web.png", width = 10, height = 6, dpi = 150)

## catch/bycatch in management units
# no management units for OIL

## VME catches in OIL fisheries
# no VME catches possible in this fishery

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
captures_spatial_plot_OIL <- captures_spatial  %>%
  filter(!(speciesGroup=='Chimaeras' |  speciesGroup=='Deepwater sharks' |  speciesGroup=='Sharks and rays nei' ) & species3ACode %in% shark_species_CMM$FAOcode
         | (speciesGroup=='Seabirds'| speciesGroup=='Cetaceans'| speciesGroup=='Turtles')) %>%
  filter(source=='OBS') %>%
  filter(grepl('OIL', fishopTargetSpecies))

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = captures_spatial_plot_OIL, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA incidental captures of seabirds, marine mammals, turtles and 
          sharks considered to be at high risk and/or of concern in OIL/LEC target operations (2012-2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  scale_colour_discrete("Captures") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("OIL summary/SIOFAmap_accidental_captures_web_OIL.png", width = 10, height = 8, dpi = 150)

# aggregate captures in a table that can be used in reports
# filter only operations that observers recorded as targeting OIL
captures_OIL <-   captures%>%
  filter(grepl('OIL', fishopTargetSpecies))

# turtles
captures_turtles <- filter(captures_OIL,(speciesGroup == 'Turtles'))

#captures_turtles_aggregate1 <- captures_turtles %>%
#  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
#  summarise(n = n()) 
captures_turtles_aggregate_OIL <- captures_turtles %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  relocate(speciesEnglishName, .after = Year) %>%
  relocate(speciesScientificName, .after = speciesEnglishName) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Captures" = foibcNbCaught) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(captures_turtles_aggregate_OIL,"OIL summary/Tables/captures_turtles_OIL.xlsx")

# marine mammals
captures_mammals <- filter(captures_OIL,(speciesGroup == 'Cetaceans'))

captures_mammals_aggregate_OIL <- captures_mammals %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) 

write_xlsx(captures_mammals_aggregate_OIL,"OIL summary/Tables/captures_mammals_OIL.xlsx")

# seabirds
captures_seabirds <- filter(captures_OIL,(speciesGroup == 'Seabirds'))

captures_seabirds_aggregate_OIL <- captures_seabirds %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Captures" = foibcNbCaught) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(captures_seabirds_aggregate_OIL,"OIL summary/Tables/captures_seabirds_OIL.xlsx")

# sharks
captures_sharks_OIL <- captures_OIL %>%
  filter(!(speciesGroup=='Chimaeras' |  speciesGroup=='Deepwater sharks' |  speciesGroup=='Sharks and rays nei' ) & species3ACode %in% shark_species_CMM$FAOcode)

captures_sharks_aggregate_OIL <- captures_sharks_OIL %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) %>%
  rename("Captures (n)" = foibcNbCaught)

write_xlsx(captures_sharks_aggregate_OIL,"OIL summary/Tables/captures_sharks_OIL.xlsx")

### Analyze observations of seabirds and mammals
# Convert the observation dataframe to a spatial object. Note that the
# crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object
# and that missing values are removed
observations_spatial <- filter(observations, (!is.na(observations$Longitude)))
observations_spatial <- st_as_sf(observations_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 
class(observations_spatial)
st_crs(observations_spatial) <- 4326

observations_spatial_plot_OIL <- observations_spatial  %>%
  filter(!(speciesGroup=='NO BIRDS OBSERVED' |  speciesGroup=='NO OBSERVER')) %>%
  filter(source=='OBS') %>%
  filter(grepl('OIL', fishopTargetSpecies))

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = observations_spatial_plot_OIL, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA observations of seabirds and marine mammals
            in OIL/LEC target operations (2012-2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  scale_colour_discrete("Observations") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("OIL summary/SIOFAmap_observations_web_OIL.png", width = 10, height = 8, dpi = 150)

## analyze seabirds observations
observations_seabirds_aggregate_OIL <- observations %>%
  filter(grepl('OIL', fishopTargetSpecies)) %>%
  filter(!speciesGroup=='MAMMALS') %>%
  filter(!speciesGroup=='NO BIRDS OBSERVED') %>%
  filter(!speciesGroup=='NO OBSERVER') %>%
  group_by(Year, speciesEnglishName, speciesScientificName, Gear, speciesGroup) %>%
  summarise(Abundance = sum(Abundance)) %>%
  dplyr::select(-speciesGroup) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(observations_seabirds_aggregate_OIL,"OIL summary/Tables/observations_seabirds_OIL.xlsx")

## analyze negative seabirds observations
negative_observations_seabirds_aggregate_OIL <- observations %>%
  filter(grepl('OIL', fishopTargetSpecies)) %>%
  filter(speciesGroup=='NO BIRDS OBSERVED') %>%
  group_by(Year, Gear) %>%  
  summarise(n = n()) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Fishing events with no birds observed" = n)

write_xlsx(negative_observations_seabirds_aggregate_OIL,"OIL summary/Tables/negative_observations_seabirds_OIL.xlsx")

## analyze non-performed seabirds observations
no_observations_seabirds_aggregate_OIL <- observations %>%
  filter(grepl('OIL', fishopTargetSpecies)) %>%
  filter(speciesGroup=='NO OBSERVER') %>%
  group_by(Year, Gear) %>%  
  summarise(n = n()) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Fishing events not observed for bird presence" = n)

write_xlsx(no_observations_seabirds_aggregate_OIL,"OIL summary/Tables/no_observations_seabirds_OIL.xlsx")

## analyze marine mammals observations during fishing events
observations_mammals_aggregate_OIL <- observations %>%
  filter(speciesGroup=='MAMMALS') %>%
  filter(!speciesGroup=='NO BIRDS OBSERVED') %>%
  filter(!speciesGroup=='NO OBSERVER') %>%
  filter(!speciesGroup=='SEABIRDS') %>%
  filter(grepl('OIL', fishopTargetSpecies)) %>%
  group_by(Year, speciesEnglishName, speciesScientificName, Gear, speciesGroup) %>%
  summarise(Abundance = sum(Abundance)) %>%
  dplyr::select(-speciesGroup) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(observations_mammals_aggregate_OIL,"OIL summary/Tables/observations_mammals_OIL.xlsx")

## analyze countries that submitted observer data (measures of fish) in OIL fishery
countries_observing_OIL <- Observer_data %>%
  #filter(Year>=2013) %>%
  filter((species3ACode == "OIL") | (species3ACode == "LEC")) %>%
  group_by(Year,CountryISOCode) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::select(-n) %>%
  rename(Country = CountryISOCode) 

write_xlsx(countries_observing_OIL,"OIL summary/Tables/countries_observing_OIL.xlsx")

# measures of maturity, sex and weight, and otoliths collected for OIL
measured_fish_OIL <- Observer_data %>%
  #filter(Year>=2013) %>%
  filter((species3ACode == "OIL") | (species3ACode == "LEC")) %>%
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

sums_measured_fish_OIL <- colSums(measured_fish_OIL)
measured_fish_OIL <- rbind(measured_fish_OIL,sums_measured_fish_OIL)

write_xlsx(measured_fish_OIL,"OIL summary/Tables/measured_fish_OIL.xlsx")

## analyze countries participating in OIL fishery
countries_fishing_OIL <- fishing %>%
  filter(ActivityID %in% yearly_operations_OIL_target$ActivityID 
         & datasetID %in% yearly_operations_OIL_target$datasetID |
           Target == "OIL/LEC"| Target == "OIL" | Target == "LEC" | Target == "LEC/OIL"
           ) %>%
  group_by(Year,CCPCode1,dbSource) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::select(-n) %>%
  rename(Country = CCPCode1) %>%
  rename(Database = dbSource) 
write_xlsx(countries_fishing_OIL,"OIL summary/Tables/countries_fishing_OIL.xlsx")
