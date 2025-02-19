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
require(viridis)
require(wesanderson)
require(dplyr)
require(rclipboard)
require(classInt)
require(spatstat)
require(geosphere)
require(lubridate)
require(ggstats)
require(see)
require(ggokabeito)

## load main fishing data
setwd("D:/SIOFA/Data")   #### !!!check that this points to the parent folder!!!
fishing <- read_excel("Catch-effort-2024.xlsx", col_types = c("numeric","text","numeric", "text",  "numeric", "numeric", "numeric","numeric","numeric","text","text","text","text","numeric","numeric","numeric","numeric","numeric","text","text","text","text","numeric","numeric", "numeric","numeric","text"))
captures <- read_excel("Accidental-captures-2024.xlsx")
observations <- read_excel("Bird-observations-2024.xlsx")
VME_catch <- read_excel("SIOFA-VME_bycatch-2024.xlsx")
Observer_data <- read_excel("qry_overview_full_bio_sampling_data_2024.xlsx")
ALL_tags <- read_excel("SIOFA-all-tagging-data-2024.xlsx", col_types = c("numeric", "text", "text", "text", "numeric", "numeric", "numeric","numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","text","text","numeric","text","text","text","numeric","text"))
toothfish_tags <- read_excel("CCAMLR_Tags-2024.xlsx")
observer_coverage <- read_excel("Observers-coverage-2024.xlsx", col_types = c("numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "numeric", "numeric","numeric", "numeric", "text", "text", "text","numeric","numeric","numeric","numeric","numeric","text","text"))
observer_mitigation <- read_excel("Bird-bycatch-mitigation-measures-data-2024.xlsx")
observed_catches <- read_excel("Observer-reported-catch-2024.xlsx")

#load lists of species
shark_species <- read_excel("sharks_2024.xlsx")
shark_species_CMM <- read_excel("protected_sharks.xlsx") # this is a combined list of CMM sharks
shark_species_CMM_2024 <- read_excel("protected_sharks_2024.xlsx")
shark_species_CMM_2023 <- read_excel("protected_sharks_2023.xlsx")
shark_species_CMM_2019 <- read_excel("protected_sharks_2019.xlsx")
target_species <- read_excel("targets.xlsx")
target_species_revised <- read_excel("targets_primary_secondary.xlsx")

# drop 2024 data (still incomplete)
fishing <- fishing %>%
  filter(Year<=2023) %>%
  filter(Year>=2014)

Observer_data <- Observer_data %>%
  filter(Year<=2023) %>%
  filter(Year>=2004)

captures <- captures%>%
  filter(Year<=2023) %>%
  filter(Year>=2004)

observations <- observations%>%
  filter(Year<=2023) %>%
  filter(Year>=2004)

VME_catch <- VME_catch%>%
  filter(Year<=2023) %>%
  filter(Year>=2004)

ALL_tags <- ALL_tags %>%
  filter(Year<=2023) %>%
  filter(Year>=2004)

toothfish_tags <- toothfish_tags %>%
  filter(year_recapture<=2024)

observer_coverage <- observer_coverage%>%
  filter(Year<=2023) %>%
  filter(Year>=2018)

observer_mitigation <- observer_mitigation%>%
  filter(Year<=2023) %>%
  filter(Year>=2014)

observed_catches <- observed_catches%>%
  filter(Year<=2023) %>%
  filter(Year>=2014)

# load a number of base spatial layers
land <- st_read('natural_earth/land')
ocean <- st_read('natural_earth/ocean')
bathy <- st_read('natural_earth/bathy')
grid <- st_read('natural_earth/grid/ne_50m_graticules_10.shp')
islands <- st_read('natural_earth/islands')
CCAMLR <- st_read('CCAMLR_stat_areas')
ORY_MUs <- st_read('ORY_management_areas')
TOP_MUs <- st_read('TOP_management_areas_2023')
st_crs(TOP_MUs) <- 4326
BPAs <- st_read('BPAs')
fishing_boundaries <- st_read('siofa_subareas_edited')

# Convert the fishing dataframe to a spatial object. Note that the
# crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object
# and that missing values are removed
fishing_spatial <- filter(fishing, (!is.na(fishing$Longitude)))
fishing_spatial <- filter(fishing, (!is.na(fishing$Latitude)))
fishing_spatial <- st_as_sf(fishing_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 
class(fishing_spatial)
st_crs(fishing_boundaries) <- 4326

#set colorblind friendly palette (when not automatic)
colors_named_vector <- c(
  "5"="#D55E00",
  "1"="#E69F00",
  "6"="#CC79A7",
  "2"="#56B4E9", 
  "4"="#0072B2",
  "8"="#000000",
  "3a"="#009E73",
  "7"="#999999",
  "3b"="#F5C710"
)

#set a colorblind friendly palette for sets with more than 12 categories
pal <- c("#000000","#004949","#009292","#ff6db6","#ffb6db",
         "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
         "#920000","#924900","#db6d00","#24ff24","#ffff6d")

# set general parameters for flextables
set_flextable_defaults(
  font.size = 10, 
  font.family = 'Calibri',
  theme_fun = theme_box,
  digits = 2,
  decimal.mark = ".",
  big.mark = " ",
  padding = 2,
  background.color = "#ffffff",
  table.layout = "autofit",
)

# Overlay points and extract just the subarea column 
fishing_within <- st_join(fishing_spatial, left = FALSE, fishing_boundaries["SubAreaNo"])

#subset for  wreckfishes fishing activity only in order to map it
# subset the whole data for all species, need catch, effort and subarea aggregations
yearly_global_catches_species <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, Longitude=fishing$Longitude, Latitude=fishing$Latitude), FUN=sum, na.rm = TRUE)
yearly_longline_effort_species <- aggregate(fishing$NbTows, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, ActivityID=fishing$ActivityID, Longitude=fishing$Longitude, Latitude=fishing$Latitude), FUN=sum, na.rm = TRUE)
yearly_global_catches_subarea_species <- aggregate(fishing_within$CatchTon, by=list(Year=fishing_within$Year,SubArea=fishing_within$SubAreaNo, Species=fishing_within$SpeciesCode), FUN=sum, na.rm = TRUE)
yearly_longline_effort_species_activity <- aggregate(fishing$NbHooks, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, activityID=fishing$ActivityID), FUN=sum, na.rm = TRUE)

# subset for wreckfishes only data 
# !!!!! double check how the longline effort is calculated when missing data
yearly_global_catches_wreckfishes <- filter(yearly_global_catches_species, (Species == "WHA") | (Species == "WRF")  | (Species == "HAU"))
yearly_global_catches_subarea_wreckfishes <- filter(yearly_global_catches_subarea_species, (Species == "WHA") | (Species == "WRF")  | (Species == "HAU"))
yearly_longline_effort_wreckfishes_activity <- filter(yearly_longline_effort_species_activity, (Species == "WHA") | (Species == "WRF")  | (Species == "HAU"))

# toggle off to disaggregate effort per subarea
#yearly_longline_effort_wreckfishes <- aggregate(yearly_longline_effort_wreckfishes_activity$x, by=list(Year=yearly_longline_effort_wreckfishes_activity$Year), FUN=sum, na.rm = TRUE)
yearly_longline_effort_wreckfishes <- aggregate(yearly_longline_effort_wreckfishes_activity$x, by=list(Year=yearly_longline_effort_wreckfishes_activity$Year), FUN=sum, na.rm = TRUE)

# transform effort so that it is at a scale comparable to the one of the catches
yearly_longline_T_effort_wreckfishes <- yearly_longline_effort_wreckfishes
yearly_longline_T_effort_wreckfishes$x <- yearly_longline_T_effort_wreckfishes$x/10000

## plot table of HAU catch and effort by year 
HAU_catch_table <- yearly_global_catches_wreckfishes  %>%
  dplyr::select(-Species) %>%
  group_by(Year) %>%
  summarise_all(sum) %>%
  mutate_at(vars(x), funs(round(., 1))) %>%
  rename("Total catch (t)" = x)
HAU_catch_effort_table <- full_join(HAU_catch_table, yearly_longline_T_effort_wreckfishes)
HAU_catch_effort_table <- HAU_catch_effort_table %>% rename('Effort (10 thousand hooks)' = x)

write_xlsx(HAU_catch_effort_table,"HAU summary/Tables/HAU_catch_effort_table.xlsx")

## plot table of HAU catch by subarea 
HAU_catch_subarea_table <- yearly_global_catches_subarea_wreckfishes  %>%
  dplyr::select(-Species) %>%
  group_by(Year, SubArea) %>%
  summarise_all(sum) %>%
  mutate_at(vars(x), funs(round(., 1))) %>%
  pivot_wider(names_from = SubArea, values_from = x) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(Year, everything())

write_xlsx(HAU_catch_subarea_table,"HAU summary/Tables/HAU_catch_subarea_table.xlsx")

# plotting the data for wreckfishes
# first graph catch (bars) and effort (line) per year
# second graph catch by year and subarea

ggplot() +
  geom_bar(data=yearly_global_catches_wreckfishes, aes(x = Year, y = x, fill=Species), stat="identity") +
  #geom_bar(data=yearly_global_catches_subarea_wreckfishes, stat="identity", fill="darkolivegreen2") +
  geom_line(data=yearly_longline_T_effort_wreckfishes,  aes(x=Year,y=x, color="Effort"), inherit.aes = FALSE, size = 1) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Longline effort (10 000 hooks)")) +
  labs(title="Yearly wreckfishes catch in the SIOFA area", x="Year", y="Catch (t)") +
  theme_bw() +
  theme( axis.line.y.right = element_line(color = "blue"), 
         axis.ticks.y.right = element_line(color = "blue"),
         axis.text.y.right = element_text(color = "blue"), 
         axis.title.y.right = element_text(color = "blue")) +
  #scale_fill_manual(values = c("Catch" = "darkolivegreen2"), 
  #                  guide = guide_legend(title = "Fill", override.aes = list(linetype = "blank", shape = NA))) +
  scale_colour_manual(values = c("Effort" = "blue"),
                      guide = guide_legend(title = "Line", override.aes = list(linetype = "solid", shape = NA))) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("HAU summary/SIOFAcatches_effort_HAU_web.png", width = 10, height = 4, dpi = 150)  

ggplot(yearly_global_catches_subarea_wreckfishes, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly HAU catch by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("HAU summary/SIOFAcatches_HAU_subarea_web.png", width = 10, height = 4, dpi = 150)


# turn the longline effort locations in a spatial dataframe
# eliminate data points outside of the SIOFA area

yearly_longline_effort_wreckfishes_activity <- filter(yearly_longline_effort_species, (Species == "WHA") | (Species == "WRF")  | (Species == "HAU"))

fishing_spatial_wreckfishes <- st_as_sf(yearly_longline_effort_wreckfishes_activity, coords = c( "Longitude", "Latitude"), crs = 4326)

fishing_spatial_wreckfishes <- fishing_spatial_wreckfishes[fishing_boundaries, ]


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

wreckfishes_poly5 <- st_join(fishing_spatial_wreckfishes, grid5)
wreckfishes_count5 <- wreckfishes_poly5 %>% group_by(id) %>% count()
wreckfishes_count5 <- st_drop_geometry(wreckfishes_count5)
wreckfishes_poly5 <- left_join(grid5, wreckfishes_count5, by="id")
wreckfishes_poly5 <- filter(wreckfishes_poly5, (!is.na(wreckfishes_poly5$n)))

# calculate number of fishing events per 1 degree grid cell

wreckfishes_poly1 <- st_join(fishing_spatial_wreckfishes, grid1)
wreckfishes_count1 <- wreckfishes_poly1 %>% group_by(id) %>% count()
wreckfishes_count1 <- st_drop_geometry(wreckfishes_count1)
wreckfishes_poly1 <- left_join(grid1, wreckfishes_count1, by="id")
wreckfishes_poly1 <- filter(wreckfishes_poly1, (!is.na(wreckfishes_poly1$n)))


# calculate number of fishing events per 30 minutes grid cell

wreckfishes_poly30 <- st_join(fishing_spatial_wreckfishes, grid30)
wreckfishes_count30 <- wreckfishes_poly30 %>% group_by(id) %>% count()
wreckfishes_count30 <- st_drop_geometry(wreckfishes_count30)
wreckfishes_poly30 <- left_join(grid30, wreckfishes_count30, by="id")
wreckfishes_poly30 <- filter(wreckfishes_poly30, (!is.na(wreckfishes_poly30$n)))


# calculate number of fishing events per 20 minutes grid cell

wreckfishes_poly20 <- st_join(fishing_spatial_wreckfishes, grid20)
wreckfishes_count20 <- wreckfishes_poly20 %>% group_by(id) %>% count()
wreckfishes_count20 <- st_drop_geometry(wreckfishes_count20)
wreckfishes_poly20 <- left_join(grid20, wreckfishes_count20, by="id")
wreckfishes_poly20 <- filter(wreckfishes_poly20, (!is.na(wreckfishes_poly20$n)))


# create hybrid map, delete all squares without fishing activity, clip out of boundaries

wreckfishes_footprint_hybrid <- st_join(wreckfishes_poly30, wreckfishes_poly20, left = T)
wreckfishes_footprint_hybrid <- filter(wreckfishes_footprint_hybrid, (!is.na(wreckfishes_footprint_hybrid$n.x)))
wreckfishes_footprint_hybrid <- filter(wreckfishes_footprint_hybrid, (!is.na(wreckfishes_footprint_hybrid$n.y)))
wreckfishes_footprint_hybrid <- wreckfishes_footprint_hybrid[fishing_boundaries,]


# classify interval of gridline cells for plotting

wreckfishes_breaks_qt5 <- classIntervals(c(min(wreckfishes_poly5$n) - .00001, wreckfishes_poly5$n), n = 7, style = "pretty")
wreckfishes_poly5 <- mutate(wreckfishes_poly5, n.events5 = cut(n, wreckfishes_breaks_qt5$brks))

wreckfishes_breaks_qt1 <- classIntervals(c(min(wreckfishes_poly1$n) - .00001, wreckfishes_poly1$n), n = 7, style = "pretty")
wreckfishes_poly1 <- mutate(wreckfishes_poly1, n.events1 = cut(n, wreckfishes_breaks_qt1$brks))

wreckfishes_breaks_qt30 <- classIntervals(c(min(wreckfishes_poly30$n) - .00001, wreckfishes_poly30$n), n = 7, style = "pretty")
wreckfishes_poly30 <- mutate(wreckfishes_poly30, n.events30 = cut(n, wreckfishes_breaks_qt30$brks))

wreckfishes_breaks_qt20 <- classIntervals(c(min(wreckfishes_poly20$n) - .00001, wreckfishes_poly20$n), n = 7, style = "pretty")
wreckfishes_poly20 <- mutate(wreckfishes_poly20, n.events20 = cut(n, wreckfishes_breaks_qt20$brks))


# crop layers on SIOFA agreement area boundaries

wreckfishes_poly5 <- st_intersection(wreckfishes_poly5, fishing_boundaries)
wreckfishes_poly1 <- st_intersection(wreckfishes_poly1, fishing_boundaries)
wreckfishes_poly30 <- st_intersection(wreckfishes_poly30, fishing_boundaries)
wreckfishes_poly20 <- st_intersection(wreckfishes_poly20, fishing_boundaries)

# plot world map and add SIOFA subareas + hybrid resolution SIOFA HAU fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = wreckfishes_poly20, fill = "orchid3") +
  geom_sf(data = wreckfishes_poly30, fill = "orchid3") +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA HAU fishing activities (hybrid 20'+30', 2014–2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 


ggsave("HAU summary/SIOFAmap_wreckfishes_hybrid_web.png", width = 10, height = 10, dpi = 150)


# plot plot world map and add SIOFA subareas + 5 degree resolution SIOFA HAU fishing events on world map

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf(data = wreckfishes_poly5, aes(fill=n.events5)) +
  scale_fill_viridis(discrete = TRUE) +
  geom_sf_label(data = fishing_boundaries, aes(label = SubAreaNo), 
                size = 2.5, 
                color = colors_named_vector[as.character(fishing_boundaries$SubAreaNo)], 
                label.size = 0, 
                nudge_y = c(0, 2, 2.5, 0, 0, 1.5, 2, -6), 
                nudge_x = c(0, -2.5, 0, 3, 0, 0, -2, -6)) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA HAU fishing activities (5 degrees, 2014–2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("HAU summary/SIOFAmap_wreckfishes_heatmap_5_web.png", width = 10, height = 7, dpi = 150)

# plot world map and add SIOFA subareas + 1 degree resolution siofa HAU fishing events on world map

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = wreckfishes_poly1, aes(fill=n.events1)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA HAU fishing activities (1 degree, 2014–2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("HAU summary/SIOFAmap_wreckfishes_heatmap_1_web.png", width = 10, height = 7, dpi = 150)

# plot plot world map and add SIOFA subareas + 30' resolution siofa HAU fishing events on world map

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = wreckfishes_poly30, aes(fill=n.events30)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA HAU fishing activities (30', 2014–2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("HAU summary/SIOFAmap_wreckfishes_heatmap_30_web.png", width = 10, height = 7, dpi = 150)


# plot plot world map and add SIOFA subareas + 20' resolution siofa HAU fishing events on world map

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = wreckfishes_poly20, aes(fill=n.events20)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA HAU fishing activities (20', 2014–2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("HAU summary/SIOFAmap_wreckfishes_heatmap_20_web.png", width = 10, height = 7, dpi = 150)


###catch/bycatch ratio HAU fisheries
## Better definition of target/non-target catch: 
## if 20% of the catch in an operation
## is HAU, then that operation had HAU as a target 
## redo all analyses with this definition, for those events that didn't declare an HAU target

# select only operations that caught HAU (ignore all others)
# identify any operations that caught hapuka in all the dataset, 
# whenever targets were not declared
# find total HAU catch by ActivityID
# need to retain year

yearly_operations_HAU <- fishing %>%
  #filter(Year==2014 | Year==2015 |Year==2016 |Year==2017 |Year==2018) %>%
  group_by(Year, ActivityID, datasetID, Gear) %>% 
  filter(SpeciesCode == "HAU"| SpeciesCode== "WHA" | SpeciesCode== "WRF") %>%
  filter(is.na(Target)) %>%
  summarize(CatchTonHAU = sum(CatchTon))

# find total catch by operation = sum weights of all species caught by 
# an ActivityID that caught HAU
# in years where target was not declared 
yearly_operations_HAU_total <- fishing %>%
  filter(ActivityID %in% yearly_operations_HAU$ActivityID & datasetID %in% yearly_operations_HAU$datasetID) %>%
  group_by(Year, ActivityID, Gear) %>% 
  summarize(TotalCatch = sum(CatchTon))

# if HAU catch >20% in an acitivtyID then retain those ActivityIDs as operations targeting HAU
# target/non-target ratio is actually already defined  
yearly_operations_HAU_target <- full_join(yearly_operations_HAU, yearly_operations_HAU_total)
yearly_operations_HAU_target <- yearly_operations_HAU_target %>% 
  mutate(Ratio = CatchTonHAU/TotalCatch) %>%
  filter(Ratio >= 0.2) 

## non-target catch by species in HAU target operations in 2014-15-16-17-18
yearly_operations_HAU_species <- fishing %>%
  filter(ActivityID%in%yearly_operations_HAU_target$ActivityID & datasetID%in%yearly_operations_HAU_target$datasetID) %>%
  filter(!SpeciesCode == "HAU" | !SpeciesCode== "TOA") %>%
  group_by(Year,SpeciesCode) %>%
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  rename('Species' = SpeciesCode)

# join data in years where HAU targets were declared
# calculate target/non-target catches hapuka 
yearly_target_catches_hapuka <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, TargetSpecies=fishing$Target, Species=fishing$SpeciesCode, ActivityID=fishing$ActivityID), FUN=sum, na.rm = TRUE)
yearly_target_catches_hapuka <- filter(yearly_target_catches_hapuka,(TargetSpecies == "HAU"| TargetSpecies == "WRF"))
yearly_nontarget_catches_hapuka <- filter(yearly_target_catches_hapuka,(Species != "HAU" | Species != "WHA"| Species != "WRF"))
yearly_nontarget_catches_hapuka <- aggregate(yearly_nontarget_catches_hapuka$x, by=list(Year=yearly_nontarget_catches_hapuka$Year, Species=yearly_nontarget_catches_hapuka$Species), FUN=sum, na.rm = TRUE)
yearly_nontarget_catches_hapuka <- yearly_nontarget_catches_hapuka %>%  
  rename('NonTargetCatch' = x) 

# join non-target data from both years when catch was declared and not declared
yearly_nontarget_catches_hapuka <- rbind(yearly_nontarget_catches_hapuka,yearly_operations_HAU_species)

# sort bycatch data for plotting
sort_bycatch_HAU <- aggregate(yearly_nontarget_catches_hapuka$NonTargetCatch, by=list(Species=yearly_nontarget_catches_hapuka$Species), FUN=sum, na.rm = TRUE)
sort_bycatch_HAU <- arrange(sort_bycatch_HAU, desc(x)) 
HAU5_species <- sort_bycatch_HAU %>% slice(1:5)
other_species <- sort_bycatch_HAU %>% slice(6:72)
# plot
ggplot(data= subset(yearly_nontarget_catches_hapuka, Species %in% HAU5_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_nontarget_catches_hapuka, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly bycatch by species in HAU fisheries in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("HAU summary/SIOFAcatches_nontarget_HAU_web.png", width = 10, height = 5, dpi = 150)

# calculate target/non-target catch fraction in 2014-15-17-18
yearly_operations_HAU_target <- yearly_operations_HAU_target %>%
  mutate(NonTargetCatch = TotalCatch-CatchTonHAU) %>%
  mutate(TargetCatch = CatchTonHAU)

## sharks non-target catch in all HAU target operations 
shark_species <- read_excel("sharks.xlsx")
yearly_sharks_catches_HAU <- yearly_nontarget_catches_hapuka %>%
  filter(Species %in% shark_species$Code) 

# sort shark bycatch data for plotting
sort_shark_bycatch_HAU <- yearly_sharks_catches_HAU %>% 
  group_by(Species) %>%
  summarize(x = sum(NonTargetCatch))
sort_shark_bycatch_HAU <- arrange(sort_shark_bycatch_HAU, desc(x)) 
HAU5_shark_species <- sort_shark_bycatch_HAU %>% slice(1:5)
other_shark_species <- sort_shark_bycatch_HAU %>% slice(6:28)
# plot sharks catch 
ggplot(data= subset(yearly_sharks_catches_HAU, Species %in% HAU5_shark_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_sharks_catches_HAU, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly bycatch of sharks in HAU fisheries in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("HAU summary/SIOFAcatches_sharks_HAU_web.png", width = 10, height = 6, dpi = 150)

## target/nontarget catch in non-declared events, spatial
# subareas
# non target
yearly_nontarget_HAU_spatial <- fishing_within %>%
  filter(ActivityID %in% yearly_operations_HAU_target$ActivityID & datasetID %in% yearly_operations_HAU_target$datasetID) %>%
  filter(!SpeciesCode == "HAU") %>%
  filter(!SpeciesCode == "WHA") %>%
  filter(!SpeciesCode == "WRF") %>%
  group_by(Year, SubAreaNo) %>%
  summarize(NonTargetCatch = sum(CatchTon)) 

# target
yearly_target_HAU_spatial <- fishing_within %>%
  filter(ActivityID %in% yearly_operations_HAU_target$ActivityID & datasetID %in% yearly_operations_HAU_target$datasetID) %>%
  filter(SpeciesCode == "HAU"| SpeciesCode== "WHA"| SpeciesCode== "WRF") %>%
  group_by(Year, SubAreaNo) %>%
  summarize(TargetCatch = sum(CatchTon)) 

# drop geometry 
yearly_nontarget_HAU_spatial$geometry <- NULL
yearly_target_HAU_spatial$geometry <- NULL
# join target and non-target 2014-15-17-18 catches, by subarea
target_nontarget_HAU <- left_join(yearly_target_HAU_spatial, yearly_nontarget_HAU_spatial)

# calculate target and non-target catch in years when target declarations were made
catch_target_subarea_HAU <- fishing_within %>%
  filter(Target == "HAU" & (SpeciesCode== "HAU"| SpeciesCode== "WHA"| SpeciesCode== "WRF")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year, SubAreaNo) %>% 
  summarize(TargetCatch = sum(CatchTon)) %>%
  filter(TargetCatch>0)

catch_nontarget_subarea_HAU <- fishing_within %>%
  filter(Target == "HAU" & (SpeciesCode!= "HAU"&SpeciesCode!= "WHA"&SpeciesCode!= "WRF")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year, SubAreaNo) %>% 
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  filter(NonTargetCatch>0)

# drop geometry 
catch_target_subarea_HAU$geometry <- NULL
catch_nontarget_subarea_HAU$geometry <- NULL

# join target and nontarget catch in years when targets were declared
catch_bycatch_subarea_HAU <- left_join(catch_target_subarea_HAU, catch_nontarget_subarea_HAU)

# join non declared with HAU declared targets operations
catch_bycatch_subarea_HAU <- rbind(catch_bycatch_subarea_HAU, target_nontarget_HAU)

# plot histograms of target and non-target catch in HAU fisheries by year and subarea
ggplot(catch_bycatch_subarea_HAU, aes(x = Year, y = TargetCatch)) +
  geom_bar(aes(fill=SubAreaNo), position="fill", stat="identity") +
  labs(title="Yearly target catch in HAU fisheries by SIOFA subareas (relative)", x="Year", y="Target catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(catch_bycatch_subarea_HAU, aes(x = Year, y = TargetCatch)) +
  geom_bar(aes(fill=SubAreaNo), position="fill", stat="identity") +
  labs(title="Yearly target catch in HAU fisheries by SIOFA subareas (relative)", x="Year", y="Target catch proportion") +
  scale_fill_oi() +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("HAU summary/SIOFAtargetcatch_subarea_HAU_web.png", width = 10, height = 4, dpi = 150)

ggplot(catch_bycatch_subarea_HAU, aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=SubAreaNo), position="fill", stat="identity") +
  labs(title="Yearly bycatch in HAU fisheries by SIOFA subareas (relative)", x="Year", y="Bycatch proportion") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(catch_bycatch_subarea_HAU, aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=SubAreaNo), position="fill", stat="identity") +
  labs(title="Yearly bycatch in HAU fisheries by SIOFA subareas (relative)", x="Year", y="Bycatch proportion") +
  scale_fill_oi() +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("HAU summary/SIOFAnontargetcatch_subarea_HAU_web.png", width = 10, height = 4, dpi = 150)

# transform data for easier plotting
catch_bycatch_HAU <- catch_bycatch_subarea_HAU %>% 
  dplyr::select(-SubAreaNo) %>%
  pivot_longer(!Year, names_to = "Catch", values_to = "t")

# plot target/non-target catch totals
ggplot(catch_bycatch_HAU, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly target catch/bycatch in HAU fisheries in the SIOFA area (absolute)", x="Year", y="Total catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_hue(labels = c("Bycatch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(catch_bycatch_HAU, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly target catch/bycatch in HAU fisheries in the SIOFA area (absolute)", x="Year", y="Total catch (t)") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 4:5) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_manual(values = c("TargetCatch" = "#0072B2", "NonTargetCatch" = "#F5C710"), 
                    labels=c("Bycatch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("HAU summary/SIOFAcatch_nontargetcatch_HAU_web.png", width = 10, height = 4, dpi = 150)

# plot target/non-target catch percentage
ggplot(catch_bycatch_HAU, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly target catch/bycatch in HAU fisheries in the SIOFA area (relative)", x="Year", y="Total catch proportion") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_hue(labels = c("Bycatch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(catch_bycatch_HAU, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly target catch/bycatch in HAU fisheries in the SIOFA area (relative)", x="Year", y="Total catch proportion") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 4:5) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_manual(values = c("TargetCatch" = "#0072B2", "NonTargetCatch" = "#F5C710"), 
                    labels=c("Bycatch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("HAU summary/SIOFAcatch_nontargetcatch_HAU_fill_web.png", width = 10, height = 4, dpi = 150)

# check again bycatch by species
# declared targets
bycatch_declared_HAU <- fishing_within %>%
  filter(Target == "HAU" & (SpeciesCode!= "HAU"&SpeciesCode!= "WHA"&SpeciesCode!= "WRF")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year, SpeciesCode) %>% 
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  filter(NonTargetCatch>0)
bycatch_declared_HAU$geometry <- NULL
# non declared targets
bycatch_nondeclared_HAU <- fishing_within %>%
  filter(ActivityID %in% yearly_operations_HAU_target$ActivityID & datasetID %in% yearly_operations_HAU_target$datasetID) %>%
  filter(!SpeciesCode == "HAU") %>%
  filter(!SpeciesCode == "WHA") %>%
  filter(!SpeciesCode == "WRF") %>%
  group_by(Year, SpeciesCode) %>%
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  filter(NonTargetCatch>0)
bycatch_nondeclared_HAU$geometry <- NULL
# join them
bycatch_HAU_species <- rbind(bycatch_declared_HAU, bycatch_nondeclared_HAU)
bycatch_HAU_species <- bycatch_HAU_species %>%
  rename(Species = SpeciesCode)
# sort bycatch data for plotting
sort_bycatch_HAU <- aggregate(bycatch_HAU_species$NonTargetCatch, by=list(Species=bycatch_HAU_species$Species), FUN=sum, na.rm = TRUE)
sort_bycatch_HAU <- arrange(sort_bycatch_HAU, desc(x)) 
HAU5_species <- sort_bycatch_HAU %>% slice(1:5)
other_species <- sort_bycatch_HAU %>% slice(6:51)
# plot
ggplot(data= subset(bycatch_HAU_species, Species %in% HAU5_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(bycatch_HAU_species, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly bycatch by species in the fisheries targeting HAU in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

#colorblind friendly version
ggplot(data= subset(bycatch_HAU_species, Species %in% HAU5_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(bycatch_HAU_species, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 8:1) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly bycatch by species in the fisheries targeting HAU in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("HAU summary/SIOFAcatches_nontarget_HAU_web.png", width = 10, height = 5, dpi = 150)

# check again bycatch by shark species
yearly_sharks_catches_HAU <- bycatch_HAU_species %>%
  filter(Species %in% shark_species$Code)

# sort shark bycatch data for plotting
sort_shark_bycatch_HAU <- yearly_sharks_catches_HAU %>% 
  group_by(Species) %>%
  summarize(x = sum(NonTargetCatch))
sort_shark_bycatch_HAU <- arrange(sort_shark_bycatch_HAU, desc(x)) 
HAU5_shark_species <- sort_shark_bycatch_HAU %>% slice(1:5)
other_shark_species <- sort_shark_bycatch_HAU %>% slice(6:15)
# plot sharks catch top 5
ggplot(data= subset(yearly_sharks_catches_HAU, Species %in% HAU5_shark_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_sharks_catches_HAU, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly bycatch of sharks in HAU fisheries in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("HAU summary/SIOFAcatches_sharks_HAU_web.png", width = 10, height = 6, dpi = 150)

# plot HAU shark bycatch full
ggplot(yearly_sharks_catches_HAU, aes(x = Year, y = NonTargetCatch, fill=Species)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly bycatch of sharks in HAU fisheries in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  #scale_fill_hue(labels = c("Non-target Catch", "Target Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(yearly_sharks_catches_HAU, aes(x = Year, y = NonTargetCatch, fill=Species)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly bycatch of sharks in HAU fisheries in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_manual(values = pal) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("HAU summary/SIOFAcatches_sharks_HAU_full_web.png", width = 10, height = 4, dpi = 150)

## catch/bycatch in management units
# load HAU management units boundaries
# there are no management units or assessment areas defined for HAU

##discards
# filter all HAU target operations (with full data)
HAU_target_operations <- fishing %>%
  filter(ActivityID %in% yearly_operations_HAU_target$ActivityID 
         & datasetID %in% yearly_operations_HAU_target$datasetID |
           (Target == "HAU" & (SpeciesCode== "HAU"| SpeciesCode== "WHA"| SpeciesCode== "WRF")))
# load discards data
discards_species_HAU <- filter(HAU_target_operations, catchFate == "0")
# plot discards
# sort discards data for plotting
sort_discards_species_HAU <- aggregate(discards_species_HAU$CatchTon, by=list(Species=discards_species_HAU$SpeciesCode), FUN=sum, na.rm = TRUE)
sort_discards_species_HAU <- arrange(sort_discards_species_HAU, desc(x)) 
top5_discards_species_HAU <- sort_discards_species_HAU %>% slice(1:5)
other_discards_species_HAU <- sort_discards_species_HAU %>% slice(6:52)

ggplot(data= subset(discards_species_HAU, SpeciesCode %in% top5_discards_species_HAU$Species), aes(x = Year, y = CatchTon)) +
  geom_bar(aes(fill=SpeciesCode), position="stack", stat="identity") +
  geom_bar(data= subset(discards_species_HAU, SpeciesCode %in% other_discards_species_HAU$Species), aes(x = Year, y = CatchTon, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2002, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(data= subset(discards_species_HAU, SpeciesCode %in% top5_discards_species_HAU$Species), aes(x = Year, y = CatchTon)) +
  geom_bar(aes(fill=SpeciesCode), position="stack", stat="identity") +
  geom_bar(data= subset(discards_species_HAU, SpeciesCode %in% other_discards_species_HAU$Species), aes(x = Year, y = CatchTon, fill="'Other species'"), position="stack", stat="identity") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 8:1) +
  theme_bw() +
  labs(title="Yearly discards by species in HAU fisheries in the SIOFA area", x="Year", y="Discards (t)") +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("HAU summary/discards_species_HAU_web.png", width = 10, height = 4, dpi = 150)

# calculate discard rates
# total catch per year
catch_discards_HAU <- catch_bycatch_subarea_HAU %>% 
  dplyr::select(-SubAreaNo) %>%
  replace(is.na(.), 0) %>%
  group_by(Year) %>%
  summarise(TotCatch=sum(TargetCatch, NonTargetCatch))
# total discards per year
discards_HAU <- discards_species_HAU %>%
  group_by(Year) %>%
  summarise(TotDiscards=sum(CatchTon))
# join and correct for discards double counting
catch_discards_HAU <- full_join(catch_discards_HAU, discards_HAU)
catch_discards_HAU <- catch_discards_HAU %>%
  replace(is.na(.), 0) %>%
  mutate(RetainedCatch=TotCatch-TotDiscards) %>% # this is needed otherwise discards are double counted
  rename(Discards=TotDiscards) %>%
  dplyr::select(-TotCatch)
# transform for plotting
catch_discards_HAU <- catch_discards_HAU %>%
  pivot_longer(!Year, names_to = "Catch", values_to = "t")
# plot total catch /discards percentage
# colorblind friendly version
ggplot(catch_discards_HAU, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly retained catch/discards in HAU fisheries in the SIOFA area (relative)", x="Year", y="Total catch proportion") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 5:6) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_manual(values = c("Discards" = "#D55E00", "RetainedCatch" = "#0072B2"), 
                    labels=c("Discards", "Retained catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("HAU summary/SIOFAcatch_discards_HAU_fill_web.png", width = 10, height = 4, dpi = 150)

#colorblind friendly version
ggplot(catch_discards_HAU, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly retained catch/discards in HAU fisheries in the SIOFA area (absolute)", x="Year", y="Total catch (t)") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 4:5) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_manual(values = c("Discards" = "#D55E00", "RetainedCatch" = "#0072B2"), 
                    labels=c("Discards", "Retained catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("HAU summary/SIOFAcatch_discards_HAU_web.png", width = 10, height = 4, dpi = 150)


## VME catches in HAU fisheries
# filter only HAU target reports 
VME_catch_HAU <- filter(VME_catch, 
                        grepl('HAU|WHA|WRF', TargetSpecies)) 

# aggregate by year and taxon
yearly_HAU_VME_catch <- VME_catch_HAU %>%
  group_by(Year, FAOcode) %>%
  summarise_at(.vars = c("Weight"),
               .funs = "sum")
# plot HAU VME bycatch
ggplot(yearly_HAU_VME_catch, aes(x = Year, y = Weight, fill=FAOcode)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly incidental catch of VME indicator taxa in HAU fisheries in the SIOFA area", x="Year", y="Total weight (kg)") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  #scale_fill_hue(labels = c("Non-target Catch", "Target Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(yearly_HAU_VME_catch, aes(x = Year, y = Weight, fill=FAOcode)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly incidental catch of VME indicator taxa in HAU fisheries in the SIOFA area", x="Year", y="Total weight (kg)") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("HAU summary/VME_captures_HAU_full_web.png", width = 10, height = 5, dpi = 150)

# plot HAU VME bycatch, HAU 5 species only
# sort bycatch data for plotting
sort_VME_bycatch_HAU <- aggregate(yearly_HAU_VME_catch$Weight, by=list(Species=yearly_HAU_VME_catch$FAOcode), FUN=sum, na.rm = TRUE)
sort_VME_bycatch_HAU <- arrange(sort_VME_bycatch_HAU, desc(x)) 
HAU5_species_VME <- sort_VME_bycatch_HAU %>% slice(1:5)
other_species_VME <- sort_VME_bycatch_HAU %>% slice(6:29)

ggplot(data= subset(yearly_HAU_VME_catch, FAOcode %in% HAU5_species_VME$Species), aes(x = Year, y = Weight)) +
  geom_bar(aes(fill=FAOcode), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_HAU_VME_catch, FAOcode %in% other_species_VME$Species), aes(x = Year, y = Weight, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

# colorblind friendly version
ggplot(data= subset(yearly_HAU_VME_catch, FAOcode %in% HAU5_species_VME$Species), aes(x = Year, y = Weight)) +
  geom_bar(aes(fill=FAOcode), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_HAU_VME_catch, FAOcode %in% other_species_VME$Species), aes(x = Year, y = Weight, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 8:1) +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("HAU summary/VME_captures_HAU_web.png", width = 10, height = 4, dpi = 150)

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
captures_spatial_plot_HAU <- captures_spatial  %>%
  filter(!(speciesGroup=='Chimaeras' |  speciesGroup=='Deepwater sharks' |  speciesGroup=='Sharks and rays nei' ) & species3ACode %in% shark_species_CMM$FAOcode
         | (speciesGroup=='Seabirds'| speciesGroup=='Cetaceans'| speciesGroup=='Turtles')) %>%
  filter(source=='OBS') %>%
  filter(grepl('HAU|WHA|WRF', fishopTargetSpecies))

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = captures_spatial_plot_HAU, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA incidental captures of seabirds, marine mammals, turtles and 
          sharks considered to be at high risk and/or of concern in HAU target operations (2004-2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  scale_colour_discrete("Captures") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

#colorblind friendly version
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = SubAreaNo, label = SubAreaNo), size = 2.5, 
                col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 1.5, 2, -6), 
                nudge_x=c(0,-2.5,0, 3, 0, 0, -2, -6)) +
  scale_fill_manual(values = colors_named_vector, 
                    guide = "none") +
  geom_sf(data = captures_spatial_plot_HAU, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA incidental captures of seabirds, marine mammals, turtles and 
          sharks considered to be at high risk and/or of concern in HAU target operations (2004-2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  scale_colour_discrete("Captures") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("HAU summary/SIOFAmap_accidental_captures_web_HAU.png", width = 10, height = 8, dpi = 150)

# aggregate captures in a table that can be used in reports
# filter only operations that observers recorded as targeting HAU
captures_HAU <-   captures%>%
  filter(grepl('HAU|WHA|WRF', fishopTargetSpecies))

# turtles
captures_turtles <- filter(captures_HAU,(speciesGroup == 'Turtles'))

#captures_turtles_aggregate1 <- captures_turtles %>%
#  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
#  summarise(n = n()) 
captures_turtles_aggregate_HAU <- captures_turtles %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  relocate(speciesEnglishName, .after = Year) %>%
  relocate(speciesScientificName, .after = speciesEnglishName) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Captures" = foibcNbCaught) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(captures_turtles_aggregate_HAU,"HAU summary/Tables/captures_turtles_HAU.xlsx")

# marine mammals
captures_mammals <- filter(captures_HAU,(speciesGroup == 'Cetaceans'))

captures_mammals_aggregate_HAU <- captures_mammals %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) 

write_xlsx(captures_mammals_aggregate_HAU,"HAU summary/Tables/captures_mammals_HAU.xlsx")

# seabirds
captures_seabirds <- filter(captures_HAU,(speciesGroup == 'Seabirds'))

captures_seabirds_aggregate_HAU <- captures_seabirds %>%
  group_by(Year, speciesEnglishName, speciesScientificName, Gear) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Captures" = foibcNbCaught) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(captures_seabirds_aggregate_HAU,"HAU summary/Tables/captures_seabirds_HAU.xlsx")

# as flextable

FT.captures_seabirds_aggregate_HAU <- captures_seabirds_aggregate_HAU %>%
  dplyr::select(1:5) %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  fit_to_width(max_width = 18, unit="cm") %>%
  flextable::align(j = 1:4, align = "left", part = "all") %>%
  flextable::align(j = 5, align = "center", part = "all") %>%
  italic(j = 3, italic = TRUE, part = "body") %>%
  add_header_lines("Observed captures of seabirds in SIOFA hapuka fisheries") 

save_as_image(FT.captures_seabirds_aggregate_HAU, path = "HAU Summary/Tables/captures_seabirds_HAU.png")               


# sharks
captures_sharks_HAU <- captures_HAU %>%
  filter(!(speciesGroup=='Chimaeras' |  speciesGroup=='Deepwater sharks' |  speciesGroup=='Sharks and rays nei' ) & species3ACode %in% shark_species_CMM$FAOcode)

captures_sharks_aggregate_HAU <- captures_sharks_HAU %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) %>%
  rename("Captures (n)" = foibcNbCaught)

write_xlsx(captures_sharks_aggregate_HAU,"HAU summary/Tables/captures_sharks_HAU.xlsx")

### Analyze observations of seabirds and mammals
# Convert the observation dataframe to a spatial object. Note that the
# crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object
# and that missing values are removed
observations_spatial <- filter(observations, (!is.na(observations$Longitude)))
observations_spatial <- st_as_sf(observations_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 
class(observations_spatial)
st_crs(observations_spatial) <- 4326

observations_spatial_plot_HAU <- observations_spatial  %>%
  filter(!(speciesGroup=='NO BIRDS OBSERVED' |  speciesGroup=='NO OBSERVER' | speciesGroup=='NO MAMMALS OBSERVED')) %>%
  filter(source=='OBS') %>%
  filter(grepl('HAU|WHA|WRF', fishopTargetSpecies))

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = observations_spatial_plot_HAU, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA observations of seabirds and marine mammals
            in HAU target operations (2004-2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  scale_colour_discrete("Observations") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

#colorblind friendly version
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = SubAreaNo, label = SubAreaNo), size = 2.5, 
                col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 1.5, 2, -6), 
                nudge_x=c(0,-2.5,0, 3, 0, 0, -2, -6)) +
  scale_fill_manual(values = colors_named_vector, 
                    guide = "none") +
  geom_sf(data = observations_spatial_plot_HAU, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA observations of seabirds and marine mammals
            in HAU target operations (2004-2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  scale_colour_discrete("Observations") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)

ggsave("HAU summary/SIOFAmap_observations_web_HAU.png", width = 10, height = 8, dpi = 150)

## analyze seabirds observations
observations_seabirds_aggregate_HAU <- observations %>%
  filter(grepl('HAU|WHA|WRF', fishopTargetSpecies)) %>%
  filter(!speciesGroup=='MAMMALS') %>%
  filter(!speciesGroup=='NO BIRDS OBSERVED') %>%
  filter(!speciesGroup=='NO MAMMALS OBSERVED') %>%
  filter(!speciesGroup=='NO OBSERVER') %>%
  group_by(Year, speciesEnglishName, speciesScientificName, Gear, speciesGroup) %>%
  summarise(Abundance = sum(Abundance)) %>%
  dplyr::select(-speciesGroup) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(observations_seabirds_aggregate_HAU,"HAU summary/Tables/observations_seabirds_HAU.xlsx")

## analyze negative seabirds observations
negative_observations_seabirds_aggregate_HAU <- observations %>%
  filter(grepl('HAU|WHA|WRF', fishopTargetSpecies)) %>%
  filter(speciesGroup=='NO BIRDS OBSERVED') %>%
  group_by(Year, Gear) %>%  
  summarise(n = n()) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Fishing events with no birds observed" = n)

write_xlsx(negative_observations_seabirds_aggregate_HAU,"HAU summary/Tables/negative_observations_seabirds_HAU.xlsx")

## analyze non-performed seabirds observations
no_observations_seabirds_aggregate_HAU <- observations %>%
  filter(grepl('HAU|WHA|WRF', fishopTargetSpecies)) %>%
  filter(speciesGroup=='NO OBSERVER') %>%
  group_by(Year, Gear) %>%  
  summarise(n = n()) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Fishing events not observed for bird presence" = n)

write_xlsx(no_observations_seabirds_aggregate_HAU,"HAU summary/Tables/no_observations_seabirds_HAU.xlsx")

## analyze marine mammals observations during fishing events
observations_mammals_aggregate_HAU <- observations %>%
  filter(speciesGroup=='MAMMALS') %>%
  filter(!speciesGroup=='NO BIRDS OBSERVED') %>%
  filter(!speciesGroup=='NO OBSERVER') %>%
  filter(!speciesGroup=='SEABIRDS') %>%
  filter(grepl('HAU|WHA|WRF', fishopTargetSpecies)) %>%
  group_by(Year, speciesEnglishName, speciesScientificName, Gear, speciesGroup) %>%
  summarise(Abundance = sum(Abundance)) %>%
  dplyr::select(-speciesGroup) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(observations_mammals_aggregate_HAU,"HAU summary/Tables/observations_mammals_HAU.xlsx")

## analyze countries that submitted observer data (measures of fish) in HAU fishery
countries_observing_HAU <- Observer_data %>%
  #filter(Year>=2013) %>%
  filter(grepl('HAU|WHA|WRF', species3ACode)) %>%
  group_by(Year,CountryISOCode) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::select(-n) %>%
  rename(Country = CountryISOCode) 

write_xlsx(countries_observing_HAU,"HAU summary/Tables/countries_observing_HAU.xlsx")

# as flextable
FT.countries_observing_HAU <- countries_observing_HAU %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  fit_to_width(max_width = 18, unit="cm") %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  flextable::align(j = 2, align = "center", part = "all") %>%
  add_header_lines("Hapuka observer data submitted by different SIOFA CCPs")

save_as_image(FT.countries_observing_HAU, path = "HAU summary/Tables/countries_observing_HAU.png")  

# measures of maturity, sex and weight, and otoliths collected for HAU
measured_fish_HAU <- Observer_data %>%
  #filter(Year>=2013) %>%
  filter(grepl('HAU', species3ACode)) %>%
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

sums_measured_fish_HAU <- colSums(measured_fish_HAU)
measured_fish_HAU <- rbind(measured_fish_HAU,sums_measured_fish_HAU)

write_xlsx(measured_fish_HAU,"HAU summary/Tables/measured_fish_HAU.xlsx")

# as flextable
FT.measured_fish_HAU <- measured_fish_HAU %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  fit_to_width(max_width = 23, unit="cm") %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  flextable::align(j = 2:8, align = "center", part = "all") %>%
  compose(j = 1, i = nrow(measured_fish_HAU), as_paragraph(as_chunk('Total'))) %>%
  bold(i = nrow(measured_fish_HAU), j = 1) %>%
  add_header_lines("Hapuka observer data measurements")

save_as_image(FT.measured_fish_HAU, path = "HAU summary/Tables/measured_fish_HAU.png")  


# measures of maturity, sex and weight, and otoliths collected for WHA
measured_fish_WHA <- Observer_data %>%
  #filter(Year>=2013) %>%
  filter(grepl('WHA', species3ACode)) %>%
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

sums_measured_fish_WHA <- colSums(measured_fish_WHA)
measured_fish_WHA <- rbind(measured_fish_WHA,sums_measured_fish_WHA)

write_xlsx(measured_fish_WHA,"HAU summary/Tables/measured_fish_WHA.xlsx")

# as flextable
FT.measured_fish_WHA <- measured_fish_WHA %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  fit_to_width(max_width = 23, unit="cm") %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  flextable::align(j = 2:8, align = "center", part = "all") %>%
  compose(j = 1, i = nrow(measured_fish_WHA), as_paragraph(as_chunk('Total'))) %>%
  bold(i = nrow(measured_fish_WHA), j = 1) %>%
  add_header_lines("Hapuku wreckfish observer data measurements")

save_as_image(FT.measured_fish_WHA, path = "HAU summary/Tables/measured_fish_WHA.png")  

# measures of maturity, sex and weight, and otoliths collected for WRF
measured_fish_WRF <- Observer_data %>%
  #filter(Year>=2013) %>%
  filter(grepl('WRF', species3ACode)) %>%
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

sums_measured_fish_WRF <- colSums(measured_fish_WRF)
measured_fish_WRF <- rbind(measured_fish_HAU,sums_measured_fish_WRF)

write_xlsx(measured_fish_WRF,"HAU summary/Tables/measured_fish_WRF.xlsx")

# as flextable
FT.measured_fish_WRF <- measured_fish_WRF %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  fit_to_width(max_width = 23, unit="cm") %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  flextable::align(j = 2:8, align = "center", part = "all") %>%
  compose(j = 1, i = nrow(measured_fish_WRF), as_paragraph(as_chunk('Total'))) %>%
  bold(i = nrow(measured_fish_WRF), j = 1) %>%
  add_header_lines("Wreckfish observer data measurements")

save_as_image(FT.measured_fish_WRF, path = "HAU summary/Tables/measured_fish_WRF.png")  

## analyze countries participating in HAU fishery
countries_fishing_HAU <- fishing %>%
  filter(ActivityID %in% yearly_operations_HAU_target$ActivityID 
         & datasetID %in% yearly_operations_HAU_target$datasetID |
           grepl('HAU|WHA|WRF', Target)) %>%
  group_by(Year,CCPCode1,dbSource) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::select(-n) %>%
  rename(Country = CCPCode1) %>%
  rename(Database = dbSource) 
write_xlsx(countries_fishing_HAU,"HAU summary/Tables/countries_fishing_HAU.xlsx")

# as flextable
FT.countries_fishing_HAU <- countries_fishing_HAU %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  fit_to_width(max_width = 18, unit="cm") %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  flextable::align(j = 2:3, align = "center", part = "all") %>%
  add_header_lines("Hapuka catch and effort data submitted by different SIOFA CCPs")

save_as_image(FT.countries_fishing_HAU, path = "HAU summary/Tables/countries_fishing_HAU.png")  

## analyze average number of vessels in the last 5 years
vessels_fishing_HAU <- fishing %>%
  filter(ActivityID %in% yearly_operations_HAU_target$ActivityID 
         & datasetID %in% yearly_operations_HAU_target$datasetID |
           grepl('HAU|WHA|WRF', Target)) %>%
  filter(Year>(2023-5)) %>% # need to change this every year
  distinct(vesselName, Year) %>%
  group_by(Year) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::select(n) %>%
  sapply(mean, na.rm = TRUE)



