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
fishing <- read_excel("SC8-catch-effort.xlsx", col_types = c("numeric", "text", "text", "numeric", "numeric", "numeric","numeric","numeric","text","text","text","text","numeric","numeric","numeric","text","text","text","text","numeric","numeric", "numeric","numeric","text"))

fishing_boundaries <- st_read('siofa_subareas_edited')

# drop 2022 data (still incomplete)
fishing <- fishing %>%
  filter(Year<=2021)

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

#subset for  tarakihi fishing activity only in order to map it
# subset the whole data for all species, need catch, effort and subarea aggregations
yearly_global_catches_species <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, Longitude=fishing$Longitude, Latitude=fishing$Latitude), FUN=sum, na.rm = TRUE)
yearly_longline_effort_species <- aggregate(fishing$NbTows, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, ActivityID=fishing$ActivityID, Longitude=fishing$Longitude, Latitude=fishing$Latitude), FUN=sum, na.rm = TRUE)

# subset for tarakihi only data 
# !!!!! double check how the longline effort is calculated when missing data


yearly_global_catches_tarakihi <- filter(yearly_global_catches_species, (Species == "TAK"))

yearly_global_catches_subarea_tarakihi <- filter(yearly_global_catches_subarea_species, (Species == "TAK"))

yearly_longline_effort_tarakihi_activity <- filter(yearly_longline_effort_species_activity, (Species == "TAK"))

# toggle off to disaggregate effort per subarea
#yearly_longline_effort_tarakihi <- aggregate(yearly_longline_effort_tarakihi_activity$x, by=list(Year=yearly_longline_effort_tarakihi_activity$Year), FUN=sum, na.rm = TRUE)
yearly_longline_effort_tarakihi <- aggregate(yearly_longline_effort_tarakihi_activity$x, by=list(Year=yearly_longline_effort_tarakihi_activity$Year), FUN=sum, na.rm = TRUE)

# transform effort so that it is at a scale comparable to the one of the catches
yearly_longline_T_effort_tarakihi <- yearly_longline_effort_tarakihi
yearly_longline_T_effort_tarakihi$x <- yearly_longline_T_effort_tarakihi$x/10000

## plot table of TAK catch and effort by year 
TAK_catch_table <- yearly_global_catches_tarakihi  %>%
  dplyr::select(-Species) %>%
  group_by(Year) %>%
  summarise_all(sum) %>%
  mutate_at(vars(x), funs(round(., 1))) %>%
  rename('Total catch (tonnes)' = x)
TAK_catch_effort_table <- full_join(TAK_catch_table, yearly_longline_T_effort_tarakihi)
TAK_catch_effort_table <- TAK_catch_effort_table %>% rename('Effort (10 thousand hooks)' = x)

write_xlsx(TAK_catch_effort_table,"TAK summary/Tables/TAK_catch_effort_table.xlsx")

## plot table of TAK catch by subarea 
TAK_catch_subarea_table <- yearly_global_catches_subarea_tarakihi  %>%
  dplyr::select(-Species) %>%
  group_by(Year, SubArea) %>%
  summarise_all(sum) %>%
  mutate_at(vars(x), funs(round(., 1))) %>%
  pivot_wider(names_from = SubArea, values_from = x) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(Year, everything())

write_xlsx(TAK_catch_subarea_table,"TAK summary/Tables/TAK_catch_subarea_table.xlsx")

# plotting the data for tarakihi
# first graph catch (bars) and effort (line) per year
# second graph catch by year and subarea

ggplot() +
  geom_bar(data=yearly_global_catches_tarakihi, aes(x = Year, y = x, fill="Catch"), stat="identity") +
  #geom_bar(data=yearly_global_catches_subarea_tarakihi, stat="identity", fill="darkolivegreen2") +
  geom_line(data=yearly_longline_T_effort_tarakihi,  aes(x=Year,y=x, color="Effort"), inherit.aes = FALSE, size = 1) +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2022)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Longline effort (10 000 hooks)")) +
  labs(title="Yearly tarakihi catch in the SIOFA area", x="Year", y="Catch (tonnes)") +
  theme_bw() +
  theme( axis.line.y.right = element_line(color = "blue"), 
         axis.ticks.y.right = element_line(color = "blue"),
         axis.text.y.right = element_text(color = "blue"), 
         axis.title.y.right = element_text(color = "blue")) +
  scale_fill_manual(values = c("Catch" = "deeppink"), 
                    guide = guide_legend(title = "Fill", override.aes = list(linetype = "blank", shape = NA))) +
  scale_colour_manual(values = c("Effort" = "blue"),
                      guide = guide_legend(title = "Line", override.aes = list(linetype = "solid", shape = NA))) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("TAK summary/SIOFAcatches_effort_TAK_web.png", width = 10, height = 4, dpi = 150)  

ggplot(yearly_global_catches_subarea_tarakihi, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly tarakihi catch by SIOFA subarea", x="Year", y="Catch (tonnes)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2022)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("TAK summary/SIOFAcatches_TAK_subarea_web.png", width = 10, height = 4, dpi = 150)


# turn the longline effort locations in a spatial dataframe
# eliminate data points outside of the SIOFA area

yearly_longline_effort_tarakihi_activity <- filter(yearly_longline_effort_species, (Species == "TAK"))

fishing_spatial_tarakihi <- st_as_sf(yearly_longline_effort_tarakihi_activity, coords = c( "Longitude", "Latitude"), crs = 4326)

fishing_spatial_tarakihi <- fishing_spatial_tarakihi[fishing_boundaries, ]


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

tarakihi_poly5 <- st_join(fishing_spatial_tarakihi, grid5)
tarakihi_count5 <- tarakihi_poly5 %>% group_by(id) %>% count()
tarakihi_count5 <- st_drop_geometry(tarakihi_count5)
tarakihi_poly5 <- left_join(grid5, tarakihi_count5, by="id")
tarakihi_poly5 <- filter(tarakihi_poly5, (!is.na(tarakihi_poly5$n)))

# calculate number of fishing events per 1 degree grid cell

tarakihi_poly1 <- st_join(fishing_spatial_tarakihi, grid1)
tarakihi_count1 <- tarakihi_poly1 %>% group_by(id) %>% count()
tarakihi_count1 <- st_drop_geometry(tarakihi_count1)
tarakihi_poly1 <- left_join(grid1, tarakihi_count1, by="id")
tarakihi_poly1 <- filter(tarakihi_poly1, (!is.na(tarakihi_poly1$n)))


# calculate number of fishing events per 30 minutes grid cell

tarakihi_poly30 <- st_join(fishing_spatial_tarakihi, grid30)
tarakihi_count30 <- tarakihi_poly30 %>% group_by(id) %>% count()
tarakihi_count30 <- st_drop_geometry(tarakihi_count30)
tarakihi_poly30 <- left_join(grid30, tarakihi_count30, by="id")
tarakihi_poly30 <- filter(tarakihi_poly30, (!is.na(tarakihi_poly30$n)))


# calculate number of fishing events per 20 minutes grid cell

tarakihi_poly20 <- st_join(fishing_spatial_tarakihi, grid20)
tarakihi_count20 <- tarakihi_poly20 %>% group_by(id) %>% count()
tarakihi_count20 <- st_drop_geometry(tarakihi_count20)
tarakihi_poly20 <- left_join(grid20, tarakihi_count20, by="id")
tarakihi_poly20 <- filter(tarakihi_poly20, (!is.na(tarakihi_poly20$n)))


# create hybrid map, delete all squares without fishing activity, clip out of boundaries

tarakihi_footprint_hybrid <- st_join(tarakihi_poly30, tarakihi_poly20, left = T)
tarakihi_footprint_hybrid <- filter(tarakihi_footprint_hybrid, (!is.na(tarakihi_footprint_hybrid$n.x)))
tarakihi_footprint_hybrid <- filter(tarakihi_footprint_hybrid, (!is.na(tarakihi_footprint_hybrid$n.y)))
tarakihi_footprint_hybrid <- tarakihi_footprint_hybrid[fishing_boundaries,]


# classify interval of gridline cells for plotting

tarakihi_breaks_qt5 <- classIntervals(c(min(tarakihi_poly5$n) - .00001, tarakihi_poly5$n), n = 7, style = "pretty")
tarakihi_poly5 <- mutate(tarakihi_poly5, n.events5 = cut(n, tarakihi_breaks_qt5$brks))

tarakihi_breaks_qt1 <- classIntervals(c(min(tarakihi_poly1$n) - .00001, tarakihi_poly1$n), n = 7, style = "pretty")
tarakihi_poly1 <- mutate(tarakihi_poly1, n.events1 = cut(n, tarakihi_breaks_qt1$brks))

tarakihi_breaks_qt30 <- classIntervals(c(min(tarakihi_poly30$n) - .00001, tarakihi_poly30$n), n = 7, style = "pretty")
tarakihi_poly30 <- mutate(tarakihi_poly30, n.events30 = cut(n, tarakihi_breaks_qt30$brks))

tarakihi_breaks_qt20 <- classIntervals(c(min(tarakihi_poly20$n) - .00001, tarakihi_poly20$n), n = 7, style = "pretty")
tarakihi_poly20 <- mutate(tarakihi_poly20, n.events20 = cut(n, tarakihi_breaks_qt20$brks))


# crop layers on SIOFA agreement area boundaries

tarakihi_poly5 <- st_intersection(tarakihi_poly5, fishing_boundaries)
tarakihi_poly1 <- st_intersection(tarakihi_poly1, fishing_boundaries)
tarakihi_poly30 <- st_intersection(tarakihi_poly30, fishing_boundaries)
tarakihi_poly20 <- st_intersection(tarakihi_poly20, fishing_boundaries)

# plot world map and add SIOFA subareas + hybrid resolution SIOFA TAK fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = tarakihi_poly20, fill = "orchid3") +
  geom_sf(data = tarakihi_poly30, fill = "orchid3") +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA TAK fishing activities (hybrid 20'+30', 2013–2021)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 


ggsave("TAK summary/SIOFAmap_tarakihi_hybrid_web.png", width = 10, height = 10, dpi = 150)


# plot plot world map and add SIOFA subareas + 5 degree resolution SIOFA TAK fishing events on world map

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf(data = tarakihi_poly5, aes(fill=n.events5)) +
  scale_fill_viridis(discrete = TRUE) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(1,3,3.5, -1, 0, -2, 0, -6), nudge_x=c(0,-2.5,0, 6, 0, 0, 0, -6)) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA TAK fishing activities (5 degrees, 2013–2021)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("TAK summary/SIOFAmap_tarakihi_heatmap_5_web.png", width = 10, height = 7, dpi = 150)



# plot plot world map and add SIOFA subareas + 1 degree resolution siofa TAK fishing events on world map

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = tarakihi_poly1, aes(fill=n.events1)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA TAK fishing activities (1 degree, 2013–2021)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("TAK summary/SIOFAmap_tarakihi_heatmap_1_web.png", width = 10, height = 7, dpi = 150)

# plot plot world map and add SIOFA subareas + 30' resolution siofa TAK fishing events on world map

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = tarakihi_poly30, aes(fill=n.events30)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA TAK fishing activities (30', 2013–2021)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("TAK summary/SIOFAmap_tarakihi_heatmap_30_web.png", width = 10, height = 7, dpi = 150)


# plot plot world map and add SIOFA subareas + 20' resolution siofa TAK fishing events on world map

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = tarakihi_poly20, aes(fill=n.events20)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA TAK fishing activities (20', 2013–2021)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("TAK summary/SIOFAmap_tarakihi_heatmap_20_web.png", width = 10, height = 7, dpi = 150)




###catch/bycatch ratio TAK fisheries
#get the data ready to plot
# tarakihi is a non-target species

yearly_global_catches_tarakihi #total target catch per year in TAK target fisheries

# non-target catch in TAK fisheries
#yearly_nontarget_catches_tarakihi <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, TargetSpecies=fishing$Target, Species=fishing$SpeciesCode, ActivityID=fishing$ActivityID), FUN=sum, na.rm = TRUE)
#yearly_nontarget_catches_tarakihi <- yearly_nontarget_catches_tarakihi %>%
#  filter(TargetSpecies == "TAK") %>%
#  filter(Species != "TAK") %>%

#yearly_nontarget_catches_tarakihi <- aggregate(yearly_nontarget_catches_tarakihi$x, by=list(Year=yearly_nontarget_catches_tarakihi$Year, Species=yearly_nontarget_catches_tarakihi$Species), FUN=sum, na.rm = TRUE)

# plot a graph of non-target catch by species
# sort
#sort_bycatch_TAK <- aggregate(yearly_nontarget_catches_tarakihi$x, by=list(Species=yearly_nontarget_catches_tarakihi$Species), FUN=sum, na.rm = TRUE)
#sort_bycatch_TAK <- arrange(sort_bycatch_TAK, desc(x)) 
#TAK5_species <- sort_bycatch_TAK %>% slice(1:5)
#other_species <- sort_bycatch_TAK %>% slice(6:38)
# plot
#ggplot(data= subset(yearly_nontarget_catches_tarakihi, Species %in% TAK5_species$Species), aes(x = Year, y = x)) +
#  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  #geom_bar(data= subset(yearly_nontarget_catches_tarakihi, Species %in% other_species$Species), aes(x = Year, y = x, fill="'Other species'"), position="stack", stat="identity") +
#  theme_bw() +
#  scale_x_continuous(limits=c(2012, 2022)) +
#  labs(title="Yearly non-target catch by species in the fisheries targeting TAK in the SIOFA area", x="Year", y="Non-target catch (tonnes)") +
#  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

#ggsave("TAK summary/SIOFAcatches_nontarget_TAK_web.png", width = 10, height = 6, dpi = 150)

# Calculate an overall target/non-target catch ratio
# tarakihi is a non-target species

#yearly_nontarget_catches_tarakihi <- aggregate(yearly_nontarget_catches_tarakihi$x, by=list(Year=yearly_nontarget_catches_tarakihi$Year), FUN=sum, na.rm = TRUE)
#names(yearly_nontarget_catches_tarakihi)[names(yearly_nontarget_catches_tarakihi) == "x"] <- "NonTargetCatch"

#yearly_target_catches_tarakihi <- aggregate(yearly_global_catches_tarakihi$x, by=list(Year=yearly_global_catches_tarakihi$Year), FUN=sum, na.rm = TRUE)

#catch_bycatch_TAK <- full_join(yearly_target_catches_tarakihi, yearly_nontarget_catches_tarakihi)
#names(catch_bycatch_TAK)[names(catch_bycatch_TAK) == "x"] <- "TargetCatch"


#transform data for easier plotting
#catch_bycatch_TAK <- catch_bycatch_TAK %>% pivot_longer(!Year, names_to = "Catch", values_to = "Tonnes")


# plot target/non-target catch totals

#ggplot(catch_bycatch_TAK, aes(x = Year, y = Tonnes, fill=Catch)) +
#  geom_bar(position="stack", stat="identity") +
#  labs(title="Yearly total/non-target catch in TAK fisheries in the SIOFA area (absolute)", x="Year", y="Total catch (tonnes)") +
#  theme_bw() +
#  scale_x_continuous(limits=c(2018, 2022)) +
#  scale_fill_hue(labels = c("Non-target Catch", "Target Catch")) +
#  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#ggsave("TAK summary/SIOFAcatch_nontargetcatch_TAK_web.png", width = 10, height = 4, dpi = 150)

# plot target/non-target catch percentage

#ggplot(catch_bycatch_TAK, aes(x = Year, y = Tonnes, fill=Catch)) +
#  geom_bar(position="fill", stat="identity") +
#  labs(title="Yearly total/non-target catch in TAK fisheries in the SIOFA area (relative)", x="Year", y="Total catch (%)") +
#  theme_bw() +
#  scale_x_continuous(limits=c(2018, 2022)) +
#  scale_fill_hue(labels = c("Non-target Catch", "Target Catch")) +
#  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#ggsave("TAK summary/SIOFAcatch_nontargetcatch_TAK_fill_web.png", width = 10, height = 4, dpi = 150)



#calculate total target and non-target catch per year and subarea in TAK fisheries
# there are no target fisheries for tarakihi
#yearly_global_catches_subarea_species_TAK <- aggregate(fishing_within$CatchTon, by=list(Year=fishing_within$Year,SubArea=fishing_within$SubAreaNo, Target=fishing_within$Target, Species=fishing_within$SpeciesCode), FUN=sum, na.rm = TRUE)
#yearly_global_catches_subarea_target_TAK <- filter(yearly_global_catches_subarea_species_TAK,(Target == "TAK"))
#yearly_global_catches_subarea_nontarget_TAK <- filter(yearly_global_catches_subarea_target_TAK,((Species != "TAK") | (Species != "LEC")))
#yearly_global_catches_subarea_target_TAK <- filter(yearly_global_catches_subarea_target_TAK,((Species == "TAK") | (Species == "LEC")))

#names(yearly_global_catches_subarea_nontarget_TAK)[names(yearly_global_catches_subarea_nontarget_TAK) == "x"] <- "NonTargetCatch"
#yearly_global_catches_subarea_nontarget_TAK$Species <- NULL
#yearly_global_catches_subarea_nontarget_TAK$Target <- NULL
#names(yearly_global_catches_subarea_target_TAK)[names(yearly_global_catches_subarea_target_TAK) == "x"] <- "TargetCatch"
#yearly_global_catches_subarea_target_TAK$Species <- NULL
#yearly_global_catches_subarea_target_TAK$Target <- NULL

#catch_bycatch_subarea_TAK <- full_join(yearly_global_catches_subarea_nontarget_TAK, yearly_global_catches_subarea_target_TAK)


# plot histograms of target and non-target catch in TAK fisheries by year and subarea

#ggplot(catch_bycatch_subarea_TAK, aes(x = Year, y = Tonnes)) +
#  geom_bar(aes(fill=SubArea, y=TargetCatch), position="stack", stat="identity") +
#  labs(title="Yearly targetcatch in fisheries targeting TAK by SIOFA subareas (absolute)", x="Year", y="Target catch (tonnes)") +
#  scale_fill_brewer(type = "seq", palette = "Set3") +
# theme_bw() +
#  scale_x_continuous(limits=c(2018, 2022)) +
#  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#ggsave("TAK summary/SIOFAtargetcatch_subarea_TAK_web.png", width = 10, height = 4, dpi = 150)

#ggplot(catch_bycatch_subarea_TAK, aes(x = Year, y = Tonnes)) +
#  geom_bar(aes(fill=SubArea, y=NonTargetCatch), position="stack", stat="identity") +
#  labs(title="Yearly non-target catch in fisheries targeting TAK by SIOFA subareas (absolute)", x="Year", y="Non-target catch (tonnes)") +
#  scale_fill_brewer(type = "seq", palette = "Set3") +
#  theme_bw() +
#  scale_x_continuous(limits=c(2018, 2022)) +
#  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#ggsave("TAK summary/SIOFAnontargetcatch_subarea_TAK_web.png", width = 10, height = 4, dpi = 150)


# plot tables of target and non-target catch in TAK fisheries by year and subarea

#catch_bycatch_subarea_TAK_table_subarea <- catch_bycatch_subarea_TAK %>%
#  group_by(Year, SubArea) %>%
#  summarise_all(sum) %>%
#  mutate_at(vars(TargetCatch, NonTargetCatch), funs(round(., 1))) %>%
#  rename('Target catch (tonnes)' = TargetCatch,
#         'Non-target catch (tonnes)' = NonTargetCatch)

#write_xlsx(catch_bycatch_subarea_TAK_table_subarea,"TAK summary/Tables/catch_bycatch_subarea_TAK_table_subarea.xlsx")



## report captures of sharks in the TAK fisheries
# there are no target fisheries for tarakihi
#shark_species <- read_excel("sharks.xlsx")


#yearly_global_catches_species_TAK <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, Target=fishing$Target, Species=fishing$SpeciesCode), FUN=sum, na.rm = TRUE)
#yearly_global_catches_sharks_TAK <- filter(yearly_global_catches_species_TAK,(Target == "TAK"))
#yearly_global_catches_sharks_TAK <- filter(yearly_global_catches_sharks_TAK, Species %in% shark_species$Code)

# it appears that there are no sharks caught when TAK was declared as a target



