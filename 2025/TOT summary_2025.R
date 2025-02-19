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

#subset for  toothfish fishing activity only in order to map it
# subset the whole data for all species, need catch, effort and subarea aggregations
yearly_global_catches_species <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, Longitude=fishing$Longitude, Latitude=fishing$Latitude), FUN=sum, na.rm = TRUE)
yearly_longline_effort_species <- aggregate(fishing$NbHooks, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, ActivityID=fishing$ActivityID, Longitude=fishing$Longitude, Latitude=fishing$Latitude), FUN=sum, na.rm = TRUE)
yearly_global_catches_subarea_species <- aggregate(fishing_within$CatchTon, by=list(Year=fishing_within$Year,SubArea=fishing_within$SubAreaNo, Species=fishing_within$SpeciesCode), FUN=sum, na.rm = TRUE)

# subset for toothfish only data 
# !!!!! double check how the longline effort is calculated when missing data
yearly_global_catches_toothfish <- filter(yearly_global_catches_species, (Species == "TOP") | (Species == "TOT") | (Species == "TOA"))
yearly_global_catches_subarea_toothfish <- filter(yearly_global_catches_subarea_species, (Species == "TOP") | (Species == "TOT")| (Species == "TOA"))
yearly_longline_effort_toothfish_activity <- filter(yearly_longline_effort_species, (Species == "TOP") | (Species == "TOT")| (Species == "TOA"))

# toggle off to disaggregate effort per subarea
#yearly_longline_effort_toothfish <- aggregate(yearly_longline_effort_toothfish_activity$x, by=list(Year=yearly_longline_effort_toothfish_activity$Year), FUN=sum, na.rm = TRUE)
yearly_longline_effort_toothfish <- aggregate(yearly_longline_effort_toothfish_activity$x, by=list(Year=yearly_longline_effort_toothfish_activity$Year), FUN=sum, na.rm = TRUE)

# transform effort so that it is at a scale comparable to the one of the catches
yearly_longline_T_effort_toothfish <- yearly_longline_effort_toothfish
yearly_longline_T_effort_toothfish$x <- yearly_longline_T_effort_toothfish$x/10000

## plot table of TOP catch and effort by year 
TOP_catch_table <- yearly_global_catches_toothfish  %>%
  dplyr::select(-Species) %>%
  group_by(Year) %>%
  summarise_all(sum) %>%
  mutate_at(vars(x), funs(round(., 1))) %>%
  rename('Total catch (t)' = x)
TOP_catch_effort_table <- full_join(TOP_catch_table, yearly_longline_T_effort_toothfish)
TOP_catch_effort_table <- TOP_catch_effort_table %>% rename('Effort (10 thousand hooks)' = x)

write_xlsx(TOP_catch_effort_table,"TOP summary/Tables/TOP_catch_effort_table.xlsx")

## plot table of TOP catch by subarea 
TOP_catch_subarea_table <- yearly_global_catches_subarea_toothfish  %>%
  dplyr::select(-Species) %>%
  group_by(Year, SubArea) %>%
  summarise_all(sum) %>%
  mutate_at(vars(x), funs(round(., 1))) %>%
  pivot_wider(names_from = SubArea, values_from = x) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(Year, everything())

write_xlsx(TOP_catch_subarea_table,"TOP summary/Tables/TOP_catch_subarea_table.xlsx")

# plotting the data for toothfish
# first graph catch (bars) and effort (line) per year
# second graph catch by year and subarea
ggplot() +
  geom_bar(data=yearly_global_catches_toothfish, aes(x = Year, y = x, fill=Species), stat="identity") +
  #geom_bar(data=yearly_global_catches_subarea_toothfish, stat="identity", fill="darkolivegreen2") +
  geom_line(data=yearly_longline_T_effort_toothfish,  aes(x=Year,y=x, color="Effort"), inherit.aes = FALSE, size = 1) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Longline effort (10 000 hooks)")) +
  labs(title="Yearly toothfish catch in the SIOFA area", x="Year", y="Catch (t)") +
  theme_bw() +
  theme( axis.line.y.right = element_line(color = "blue"), 
         axis.ticks.y.right = element_line(color = "blue"),
         axis.text.y.right = element_text(color = "blue"), 
         axis.title.y.right = element_text(color = "blue")) +
  scale_fill_manual(values = c(TOP = "tomato3", TOA = "royalblue1"), 
                    guide = guide_legend(title = "Fill", override.aes = list(linetype = "blank", shape = NA))) +
  scale_colour_manual(values = c("Effort" = "blue"),
                      guide = guide_legend(title = "Line", override.aes = list(linetype = "solid", shape = NA))) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("TOP summary/SIOFAcatches_effort_TOP_web.png", width = 10, height = 4, dpi = 150)  

ggplot(yearly_global_catches_subarea_toothfish, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly toothfish catch by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(yearly_global_catches_subarea_toothfish, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly toothfish catch by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 4:5) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("TOP summary/SIOFAcatches_TOP_subarea_web.png", width = 10, height = 4, dpi = 150)


# turn the longline effort locations in a spatial dataframe
# eliminate data points outside of the SIOFA area

yearly_longline_effort_toothfish_activity <- filter(yearly_longline_effort_species, (Species == "TOP") | (Species == "TOT") | (Species == "TOA"))

fishing_spatial_toothfish <- st_as_sf(yearly_longline_effort_toothfish_activity, coords = c( "Longitude", "Latitude"), crs = 4326)

fishing_spatial_toothfish <- fishing_spatial_toothfish[fishing_boundaries, ]


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

toothfish_poly5 <- st_join(fishing_spatial_toothfish, grid5)
toothfish_count5 <- toothfish_poly5 %>% group_by(id) %>% count()
toothfish_count5 <- st_drop_geometry(toothfish_count5)
toothfish_poly5 <- left_join(grid5, toothfish_count5, by="id")
toothfish_poly5 <- filter(toothfish_poly5, (!is.na(toothfish_poly5$n)))

# calculate number of fishing events per 1 degree grid cell

toothfish_poly1 <- st_join(fishing_spatial_toothfish, grid1)
toothfish_count1 <- toothfish_poly1 %>% group_by(id) %>% count()
toothfish_count1 <- st_drop_geometry(toothfish_count1)
toothfish_poly1 <- left_join(grid1, toothfish_count1, by="id")
toothfish_poly1 <- filter(toothfish_poly1, (!is.na(toothfish_poly1$n)))


# calculate number of fishing events per 30 minutes grid cell

toothfish_poly30 <- st_join(fishing_spatial_toothfish, grid30)
toothfish_count30 <- toothfish_poly30 %>% group_by(id) %>% count()
toothfish_count30 <- st_drop_geometry(toothfish_count30)
toothfish_poly30 <- left_join(grid30, toothfish_count30, by="id")
toothfish_poly30 <- filter(toothfish_poly30, (!is.na(toothfish_poly30$n)))


# calculate number of fishing events per 20 minutes grid cell

toothfish_poly20 <- st_join(fishing_spatial_toothfish, grid20)
toothfish_count20 <- toothfish_poly20 %>% group_by(id) %>% count()
toothfish_count20 <- st_drop_geometry(toothfish_count20)
toothfish_poly20 <- left_join(grid20, toothfish_count20, by="id")
toothfish_poly20 <- filter(toothfish_poly20, (!is.na(toothfish_poly20$n)))


# create hybrid map, delete all squares without fishing activity, clip out of boundaries

toothfish_footprint_hybrid <- st_join(toothfish_poly30, toothfish_poly20, left = T)
toothfish_footprint_hybrid <- filter(toothfish_footprint_hybrid, (!is.na(toothfish_footprint_hybrid$n.x)))
toothfish_footprint_hybrid <- filter(toothfish_footprint_hybrid, (!is.na(toothfish_footprint_hybrid$n.y)))
toothfish_footprint_hybrid <- toothfish_footprint_hybrid[fishing_boundaries,]


# classify interval of gridline cells for plotting

toothfish_breaks_qt5 <- classIntervals(c(min(toothfish_poly5$n) - .00001, toothfish_poly5$n), n = 7, style = "pretty")
toothfish_poly5 <- mutate(toothfish_poly5, n.events5 = cut(n, toothfish_breaks_qt5$brks))

toothfish_breaks_qt1 <- classIntervals(c(min(toothfish_poly1$n) - .00001, toothfish_poly1$n), n = 7, style = "pretty")
toothfish_poly1 <- mutate(toothfish_poly1, n.events1 = cut(n, toothfish_breaks_qt1$brks))

toothfish_breaks_qt30 <- classIntervals(c(min(toothfish_poly30$n) - .00001, toothfish_poly30$n), n = 7, style = "pretty")
toothfish_poly30 <- mutate(toothfish_poly30, n.events30 = cut(n, toothfish_breaks_qt30$brks))

toothfish_breaks_qt20 <- classIntervals(c(min(toothfish_poly20$n) - .00001, toothfish_poly20$n), n = 7, style = "pretty")
toothfish_poly20 <- mutate(toothfish_poly20, n.events20 = cut(n, toothfish_breaks_qt20$brks))

# crop layers on SIOFA agreement area boundaries
toothfish_poly5 <- st_intersection(toothfish_poly5, fishing_boundaries)
toothfish_poly1 <- st_intersection(toothfish_poly1, fishing_boundaries)
toothfish_poly30 <- st_intersection(toothfish_poly30, fishing_boundaries)
toothfish_poly20 <- st_intersection(toothfish_poly20, fishing_boundaries)

# plot world map and add SIOFA subareas + hybrid resolution SIOFA TOP fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = toothfish_poly20, fill = "orchid3") +
  geom_sf(data = toothfish_poly30, fill = "orchid3") +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA TOT fishing activities (hybrid 20'+30', 2014–2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 


ggsave("TOP summary/SIOFAmap_toothfish_hybrid_web.png", width = 10, height = 10, dpi = 150)

# plot plot world map and add SIOFA subareas + 5 degree resolution SIOFA TOP fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf(data = toothfish_poly5, aes(fill=n.events5)) +
  geom_sf(data = TOP_MUs, fill = NA, color = "black") +
  scale_fill_viridis(discrete = TRUE) +
  geom_sf_label(data = fishing_boundaries, aes(label = SubAreaNo), 
                size = 2.5, 
                color = colors_named_vector[as.character(fishing_boundaries$SubAreaNo)], 
                label.size = 0, 
                nudge_y = c(0, 2, 2.5, 0, 0, 1.5, 2, -6), 
                nudge_x = c(0, -2.5, 0, 3, 0, 0, -2, -6)) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA TOT fishing activities (5 degrees, 2014–2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("TOP summary/SIOFAmap_toothfish_heatmap_5_web.png", width = 10, height = 7, dpi = 150)

# plot plot world map and add SIOFA subareas + 1 degree resolution siofa TOP fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = toothfish_poly1, aes(fill=n.events1)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA TOP fishing activities (1 degree, 2014–2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("TOP summary/SIOFAmap_toothfish_heatmap_1_web.png", width = 10, height = 7, dpi = 150)

# plot plot world map and add SIOFA subareas + 30' resolution siofa TOP fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = toothfish_poly30, aes(fill=n.events30)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA TOP fishing activities (30', 2014–2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("TOP summary/SIOFAmap_toothfish_heatmap_30_web.png", width = 10, height = 7, dpi = 150)

# plot plot world map and add SIOFA subareas + 20' resolution siofa TOP fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,2,2, 0, 0, 5, 0, -6), nudge_x=c(0,0,0,3,0,0,0,0)) +
  geom_sf(data = toothfish_poly20, aes(fill=n.events20)) +
  scale_fill_viridis(discrete = TRUE) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA TOP fishing activities (20', 2014–2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("TOP summary/SIOFAmap_toothfish_heatmap_20_web.png", width = 10, height = 7, dpi = 150)

# plot world map and add SIOFA MUs + siofa fishing events just to check

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf(data = TOP_MUs, fill = "orchid2", color = "black") +
  #geom_sf(data = fishing_spatial, color = 'black', cex = .1) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,0,0, 0, -5, 0, 0,-3), nudge_x=c(0,0,0,0,0,0,0,-11)) +
  geom_sf_label(data = TOP_MUs, aes(fill = NULL, label = area_name), size = 2.5, col = "black", label.size = 0, nudge_y=c(2,2)) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA TOP Management/Assessment Areas") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("TOP summary/SIOFA_TOP_MUs_web.png", width = 10, height = 7, dpi = 150)

###catch/bycatch ratio TOP fisheries
## Better definition of target/non-target catch: 
## if 20% of the catch in an operation
## is TOP, then that operation had TOP as a target 
## redo all analyses with this definition, for those events that didn't declare an TOP target

# select only operations that caught TOP (ignore all others)
# identify any operations that caught toothfish in all the dataset, 
# whenever targets were not declared
# find total TOP catch by ActivityID
# need to retain year

yearly_operations_TOP <- fishing %>%
  #filter(Year==2014 | Year==2015 |Year==2016 |Year==2017 |Year==2018) %>%
  group_by(Year, ActivityID, datasetID, Gear) %>% 
  filter(SpeciesCode == "TOP"| SpeciesCode== "TOA") %>%
  filter(is.na(Target)) %>%
  summarize(CatchTonTOP = sum(CatchTon))

# find total catch by operation = sum weights of all species caught by 
# an ActivityID that caught TOP
# in years where target was not declared (in 2014-15-16-17-18)
yearly_operations_TOP_total <- fishing %>%
  filter(ActivityID %in% yearly_operations_TOP$ActivityID & datasetID %in% yearly_operations_TOP$datasetID) %>%
  group_by(Year, ActivityID, Gear) %>% 
  summarize(TotalCatch = sum(CatchTon))

# if TOP catch >20% in an acitivtyID then retain those ActivityIDs as operations targeting TOP
# target/non-target ratio is actually already defined  
yearly_operations_TOP_target <- full_join(yearly_operations_TOP, yearly_operations_TOP_total)
yearly_operations_TOP_target <- yearly_operations_TOP_target %>% 
  mutate(Ratio = CatchTonTOP/TotalCatch) %>%
  filter(Ratio >= 0.2) 

## non-target catch by species in TOP target operations in 2014-15-16-17-18
yearly_operations_TOP_species <- fishing %>%
  filter(ActivityID%in%yearly_operations_TOP_target$ActivityID & datasetID%in%yearly_operations_TOP_target$datasetID) %>%
  filter(!SpeciesCode == "TOP" | !SpeciesCode== "TOA") %>%
  group_by(Year,SpeciesCode) %>%
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  rename('Species' = SpeciesCode)

# join data in years where TOP targets were declared
# calculate target/non-target catches toothfish 
yearly_target_catches_toothfish <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, TargetSpecies=fishing$Target, Species=fishing$SpeciesCode, ActivityID=fishing$ActivityID), FUN=sum, na.rm = TRUE)
yearly_target_catches_toothfish <- filter(yearly_target_catches_toothfish,(TargetSpecies == "TOP"))
yearly_nontarget_catches_toothfish <- filter(yearly_target_catches_toothfish,(Species != "TOP" | Species != "TOA"))
yearly_nontarget_catches_toothfish <- aggregate(yearly_nontarget_catches_toothfish$x, by=list(Year=yearly_nontarget_catches_toothfish$Year, Species=yearly_nontarget_catches_toothfish$Species), FUN=sum, na.rm = TRUE)
yearly_nontarget_catches_toothfish <- yearly_nontarget_catches_toothfish %>%  
  rename('NonTargetCatch' = x) 

# join non-target data from both years when catch was declared and not declared
yearly_nontarget_catches_toothfish <- rbind(yearly_nontarget_catches_toothfish,yearly_operations_TOP_species)

# sort bycatch data for plotting
sort_bycatch_TOP <- aggregate(yearly_nontarget_catches_toothfish$NonTargetCatch, by=list(Species=yearly_nontarget_catches_toothfish$Species), FUN=sum, na.rm = TRUE)
sort_bycatch_TOP <- arrange(sort_bycatch_TOP, desc(x)) 
top5_species <- sort_bycatch_TOP %>% slice(1:5)
other_species <- sort_bycatch_TOP %>% slice(6:72)
# plot
ggplot(data= subset(yearly_nontarget_catches_toothfish, Species %in% top5_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_nontarget_catches_toothfish, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly bycatch by species in TOT fisheries in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

#colorblind friendly version
ggplot(data= subset(yearly_nontarget_catches_toothfish, Species %in% top5_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_nontarget_catches_toothfish, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 8:1) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly bycatch by species in TOT fisheries in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("TOP summary/SIOFAcatches_nontarget_TOP_web.png", width = 10, height = 5, dpi = 150)

# calculate target/non-target catch fraction in 2014-15-17-18
yearly_operations_TOP_target <- yearly_operations_TOP_target %>%
  mutate(NonTargetCatch = TotalCatch-CatchTonTOP) %>%
  mutate(TargetCatch = CatchTonTOP)

## sharks non-target catch in all TOP target operations 
shark_species <- read_excel("sharks.xlsx")
yearly_sharks_catches_TOP <- yearly_nontarget_catches_toothfish %>%
  filter(Species %in% shark_species$Code) 

# sort shark bycatch data for plotting
sort_shark_bycatch_TOP <- yearly_sharks_catches_TOP %>% 
  group_by(Species) %>%
  summarize(x = sum(NonTargetCatch))
sort_shark_bycatch_TOP <- arrange(sort_shark_bycatch_TOP, desc(x)) 
top5_shark_species <- sort_shark_bycatch_TOP %>% slice(1:5)
other_shark_species <- sort_shark_bycatch_TOP %>% slice(6:28)
# plot sharks catch 
ggplot(data= subset(yearly_sharks_catches_TOP, Species %in% top5_shark_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_sharks_catches_TOP, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly bycatch of sharks in TOT fisheries in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

#colorblind friendly version
ggplot(data= subset(yearly_sharks_catches_TOP, Species %in% top5_shark_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_sharks_catches_TOP, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 8:1) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly bycatch of sharks in TOT fisheries in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("TOP summary/SIOFAcatches_sharks_TOP_web.png", width = 10, height = 6, dpi = 150)

## target/nontarget catch in non-declared events, spatial
# subareas
# non target
yearly_nontarget_TOP_spatial <- fishing_within %>%
  filter(ActivityID %in% yearly_operations_TOP_target$ActivityID & datasetID %in% yearly_operations_TOP_target$datasetID) %>%
  filter(!SpeciesCode == "TOP") %>%
  filter(!SpeciesCode== "TOA") %>%
  group_by(Year, SubAreaNo) %>%
  summarize(NonTargetCatch = sum(CatchTon)) 

# target
yearly_target_TOP_spatial <- fishing_within %>%
  filter(ActivityID %in% yearly_operations_TOP_target$ActivityID & datasetID %in% yearly_operations_TOP_target$datasetID) %>%
  filter(SpeciesCode == "TOP"| SpeciesCode== "TOA") %>%
  group_by(Year, SubAreaNo) %>%
  summarize(TargetCatch = sum(CatchTon)) 

# drop geometry 
yearly_nontarget_TOP_spatial$geometry <- NULL
yearly_target_TOP_spatial$geometry <- NULL
# join target and non-target 2014-15-17-18 catches, by subarea
target_nontarget_TOP <- left_join(yearly_target_TOP_spatial, yearly_nontarget_TOP_spatial)

# calculate target and non-target catch in years when target declarations were made
catch_target_subarea_TOP <- fishing_within %>%
  filter(Target == "TOP" & (SpeciesCode== "TOP"| SpeciesCode== "TOA")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year, SubAreaNo) %>% 
  summarize(TargetCatch = sum(CatchTon)) %>%
  filter(TargetCatch>0)

catch_nontarget_subarea_TOP <- fishing_within %>%
  filter(Target == "TOP" & (SpeciesCode!= "TOP"& SpeciesCode!= "TOA")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year, SubAreaNo) %>% 
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  filter(NonTargetCatch>0)

# drop geometry 
catch_target_subarea_TOP$geometry <- NULL
catch_nontarget_subarea_TOP$geometry <- NULL

# join target and nontarget catch in years when targets were declared
catch_bycatch_subarea_TOP <- left_join(catch_target_subarea_TOP, catch_nontarget_subarea_TOP)

# join non declared with TOP declared targets operations
catch_bycatch_subarea_TOP <- rbind(catch_bycatch_subarea_TOP, target_nontarget_TOP)

# plot histograms of target and non-target catch in TOP fisheries by year and subarea
ggplot(catch_bycatch_subarea_TOP, aes(x = Year, y = TargetCatch)) +
  geom_bar(aes(fill=SubAreaNo), position="fill", stat="identity") +
  labs(title="Yearly target catch in TOT fisheries by SIOFA subareas (relative)", x="Year", y="Target catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(catch_bycatch_subarea_TOP, aes(x = Year, y = TargetCatch)) +
  geom_bar(aes(fill=SubAreaNo), position="fill", stat="identity") +
  labs(title="Yearly target catch in TOT fisheries by SIOFA subareas (relative)", x="Year", y="Target catch proportion") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 4:5) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("TOP summary/SIOFAtargetcatch_subarea_TOP_web.png", width = 10, height = 4, dpi = 150)

ggplot(catch_bycatch_subarea_TOP, aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=SubAreaNo), position="fill", stat="identity") +
  labs(title="Yearly bycatch in TOT fisheries by SIOFA subareas (relative)", x="Year", y="Bycatch proportion") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(catch_bycatch_subarea_TOP, aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=SubAreaNo), position="fill", stat="identity") +
  labs(title="Yearly bycatch in TOT fisheries by SIOFA subareas (relative)", x="Year", y="Bycatch proportion") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 4:5) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("TOP summary/SIOFAnontargetcatch_subarea_TOP_web.png", width = 10, height = 4, dpi = 150)

# transform data for easier plotting
catch_bycatch_TOP <- catch_bycatch_subarea_TOP %>% 
  dplyr::select(-SubAreaNo) %>%
  pivot_longer(!Year, names_to = "Catch", values_to = "t")

# plot target/non-target catch totals
ggplot(catch_bycatch_TOP, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly target catch/bycatch in TOT fisheries in the SIOFA area (absolute)", x="Year", y="Total catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_hue(labels = c("Non-target Catch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(catch_bycatch_TOP, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly target catch/bycatch in TOT fisheries in the SIOFA area (absolute)", x="Year", y="Total catch (t)") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 4:5) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_manual(values = c("TargetCatch" = "#0072B2", "NonTargetCatch" = "#F5C710"), 
                    labels=c("Bycatch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("TOP summary/SIOFAcatch_nontargetcatch_TOP_web.png", width = 10, height = 4, dpi = 150)

# plot target/non-target catch percentage
ggplot(catch_bycatch_TOP, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly target catch/bycatch in TOT fisheries in the SIOFA area (relative)", x="Year", y="Total catch proportion") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_hue(labels = c("Non-target Catch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(catch_bycatch_TOP, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly target catch/bycatch in TOT fisheries in the SIOFA area (relative)", x="Year", y="Total catch proportion") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 4:5) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_manual(values = c("TargetCatch" = "#0072B2", "NonTargetCatch" = "#F5C710"), 
                    labels=c("Bycatch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("TOP summary/SIOFAcatch_nontargetcatch_TOP_fill_web.png", width = 10, height = 4, dpi = 150)

# check again bycatch by species
# declared targets
bycatch_declared_TOP <- fishing_within %>%
  filter(Target == "TOP" & (SpeciesCode!= "TOP"& SpeciesCode!= "TOA")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year, SpeciesCode) %>% 
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  filter(NonTargetCatch>0)
bycatch_declared_TOP$geometry <- NULL
# non declared targets
bycatch_nondeclared_TOP <- fishing_within %>%
  filter(ActivityID %in% yearly_operations_TOP_target$ActivityID & datasetID %in% yearly_operations_TOP_target$datasetID) %>%
  filter(!SpeciesCode == "TOP") %>%
  filter(!SpeciesCode== "TOA") %>%
  group_by(Year, SpeciesCode) %>%
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  filter(NonTargetCatch>0)
bycatch_nondeclared_TOP$geometry <- NULL
# join them
bycatch_TOP_species <- rbind(bycatch_declared_TOP, bycatch_nondeclared_TOP)
bycatch_TOP_species <- bycatch_TOP_species %>%
  rename(Species = SpeciesCode)
# sort bycatch data for plotting
sort_bycatch_TOP <- aggregate(bycatch_TOP_species$NonTargetCatch, by=list(Species=bycatch_TOP_species$Species), FUN=sum, na.rm = TRUE)
sort_bycatch_TOP <- arrange(sort_bycatch_TOP, desc(x)) 
top5_species <- sort_bycatch_TOP %>% slice(1:5)
other_species <- sort_bycatch_TOP %>% slice(6:69)
# plot
ggplot(data= subset(bycatch_TOP_species, Species %in% top5_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(bycatch_TOP_species, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly bycatch by species in the fisheries targeting TOT in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("TOP summary/SIOFAcatches_nontarget_TOP_web.png", width = 10, height = 5, dpi = 150)

# check again bycatch by shark species
yearly_sharks_catches_TOP <- bycatch_TOP_species %>%
  filter(Species %in% shark_species$Code)

# sort shark bycatch data for plotting
sort_shark_bycatch_TOP <- yearly_sharks_catches_TOP %>% 
  group_by(Species) %>%
  summarize(x = sum(NonTargetCatch))
sort_shark_bycatch_TOP <- arrange(sort_shark_bycatch_TOP, desc(x)) 
top5_shark_species <- sort_shark_bycatch_TOP %>% slice(1:5)
other_shark_species <- sort_shark_bycatch_TOP %>% slice(6:16)
# plot sharks catch 
ggplot(data= subset(yearly_sharks_catches_TOP, Species %in% top5_shark_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_sharks_catches_TOP, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly bycatch of sharks in TOT fisheries in the SIOFA area", x="Year", y="Bycatch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("TOP summary/SIOFAcatches_sharks_TOP_web.png", width = 10, height = 6, dpi = 150)

## catch/bycatch in management areas
# load TOP management areas boundaries

#visually verify that areas correspond to those in the CMM
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf(data = CCAMLR, fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, -1.7, -14, 0, 0, -12), nudge_x=c(0,-2.5,0, 1, 0, 0, -1, -15)) +
  geom_sf(data = TOP_MUs, fill = "orchid2", color = "black") +
  geom_sf_label(data = TOP_MUs, aes(fill = NULL, label = area_name), size = 2.5, col = "black", label.size = 0, nudge_y=c(1,2)) +
  annotate("text", x = 50, y = -47, label = c("Afred Faure & Ile de l'Est"), size = 2.5) +
  annotate("text", x = 70, y = -50, label = c("Kerguelen"), size = 2.5) +
  annotate("text", x = 72, y = -54, label = c("Heard and McDonald"), size = 2.5) +
  geom_sf_text(data = CCAMLR, aes(fill = NULL, label = GAR_Name), size = 2.5, col = "black", label.size = 0,  nudge_y=c(0,0,0,0,0,0,0,0,0.5,0,0,0,0,0,0,0,-1,1,0), nudge_x=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA toothfish Management Areas") +
  coord_sf(xlim = c(35, 90), ylim = c(-40, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) 


ggsave("TOP summary/SIOFAtoothfish_MUs_web.png", width = 10, height = 7, dpi = 150)

# filter data within management areas
fishing_within_TOP_MUs <- st_join(fishing_within, TOP_MUs, left = FALSE)

# calculate target and non-target catch in years when target declarations were made
catch_target_TOP_MUs_dec <- fishing_within_TOP_MUs %>%
  filter(Target == "TOP" & (SpeciesCode== "TOP"| SpeciesCode== "TOA")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year, area_name) %>% 
  summarize(TargetCatch = sum(CatchTon)) %>%
  filter(TargetCatch>0)

catch_nontarget_TOP_MUs_dec <- fishing_within_TOP_MUs %>%
  filter(Target == "TOP" & (SpeciesCode!= "TOP"& SpeciesCode!= "TOA")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year, area_name) %>% 
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  filter(NonTargetCatch>0)

# calculate target and non-target catch in non declared fishing operations
# keep the two areas separated
catch_target_TOP_MUs_nondec <- fishing_within_TOP_MUs %>%
  filter(ActivityID %in% yearly_operations_TOP_target$ActivityID & datasetID %in% yearly_operations_TOP_target$datasetID) %>%
  filter(SpeciesCode == "TOP"| SpeciesCode== "TOA") %>%
  group_by(Year, area_name) %>%
  summarize(TargetCatch = sum(CatchTon)) 

catch_nontarget_TOP_MUs_nondec <- fishing_within_TOP_MUs %>%
  filter(ActivityID %in% yearly_operations_TOP_target$ActivityID & datasetID %in% yearly_operations_TOP_target$datasetID) %>%
  filter(!SpeciesCode == "TOP") %>%
  filter(!SpeciesCode == "TOA") %>%
  group_by(Year, area_name) %>%
  summarize(NonTargetCatch = sum(CatchTon)) 

# join declared and non declared target and non target catches by area
catch_target_TOP_MUs <- rbind(catch_target_TOP_MUs_dec, catch_target_TOP_MUs_nondec)
catch_nontarget_TOP_MUs <- rbind(catch_nontarget_TOP_MUs_dec, catch_nontarget_TOP_MUs_nondec)
# drop geometry
catch_target_TOP_MUs <- st_drop_geometry(catch_target_TOP_MUs)
catch_nontarget_TOP_MUs <- st_drop_geometry(catch_nontarget_TOP_MUs)
# summarize
catch_target_TOP_MUs <- catch_target_TOP_MUs %>%
  group_by(Year, area_name) %>%
  summarize(TargetCatch = sum(TargetCatch))
catch_nontarget_TOP_MUs <- catch_nontarget_TOP_MUs %>%
  group_by(Year, area_name) %>%
  summarize(NonTargetCatch = sum(NonTargetCatch))
# join all in a single dataset # add column east/west
catch_TOP_MUs <- left_join(catch_target_TOP_MUs, catch_nontarget_TOP_MUs)
#transform data for easier plotting
catch_TOP_MUs <- catch_TOP_MUs %>% rename(Management_unit=area_name)
## plot histograms of target and non-target catch in MUs per year
#totals target
ggplot(catch_TOP_MUs, aes(x = Year, y = t)) +
  geom_bar(aes(fill=Management_unit, y=TargetCatch), position="stack", stat="identity") +
  labs(title="Yearly TOT catch in SIOFA toothfish management areas (absolute)", x="Year", y="Target catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(catch_TOP_MUs, aes(x = Year, y = t)) +
  geom_bar(aes(fill=Management_unit, y=TargetCatch), position="stack", stat="identity") +
  labs(title="Yearly TOT catch in SIOFA toothfish management areas (absolute)", x="Year", y="Target catch (t)") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 6:1) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("TOP summary/SIOFAtargetcatch_TOP_MUs_web.png", width = 10, height = 4, dpi = 150)

#totals target table
catch_bycatch_by_TOP_MU_table <- catch_TOP_MUs %>%
  dplyr::select(-NonTargetCatch) %>%
  mutate_at(vars(TargetCatch), funs(round(., 2))) %>%
  pivot_wider(names_from = Management_unit, values_from = c(TargetCatch),  names_sep = ' in ' )
write_xlsx(catch_bycatch_by_TOP_MU_table,"TOP summary/Tables/catch_bycatch_by_TOP_MU_table.xlsx")

#percentage of target catch in different MUs
ggplot(catch_TOP_MUs, aes(x = Year, y = t)) +
  geom_bar(aes(fill=Management_unit, y=TargetCatch), position="fill", stat="identity") +
  labs(title="Yearly TOP catch in SIOFA toothfish management areas (relative)", x="Year", y="Catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("TOP summary/SIOFAtargetcatch_TOP_MUs_fill_web.png", width = 10, height = 4, dpi = 150)

#totals non-target
ggplot(catch_TOP_MUs, aes(x = Year, y = t)) +
  geom_bar(aes(fill=Management_unit, y=NonTargetCatch), position="stack", stat="identity") +
  labs(title="Yearly bycatch of all other species in SIOFA toothfish management areas (absolute)", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(catch_TOP_MUs, aes(x = Year, y = t)) +
  geom_bar(aes(fill=Management_unit, y=NonTargetCatch), position="stack", stat="identity") +
  labs(title="Yearly bycatch of all other species in SIOFA toothfish management areas (absolute)", x="Year", y="Catch (t)") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 6:1) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("TOP summary/SIOFAnontargetcatch_TOP_MUs_web.png", width = 10, height = 4, dpi = 150)

#percentage of non-target catch in different MUs
ggplot(catch_TOP_MUs, aes(x = Year, y = t)) +
  geom_bar(aes(fill=Management_unit, y=NonTargetCatch), position="fill", stat="identity") +
  labs(title="Yearly bycatch in SIOFA toothfish management areas (relative)", x="Year", y="Catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("TOP summary/SIOFAnontargetcatch_TOP_MUs_fill_web.png", width = 10, height = 4, dpi = 150)

# target/non-target across all MUs
catch_bycatch_by_TOP_MU_across <- catch_TOP_MUs %>%
  drop_na(TargetCatch) %>%
  group_by(Year) %>%
  summarise_at(.vars = c("TargetCatch","NonTargetCatch"),
               .funs = "sum") %>%
  rename(TOP_catch = TargetCatch) %>%
  rename(Other_catch = NonTargetCatch)

#transform data for easier plotting
catch_bycatch_TOP_MU_across <- catch_bycatch_by_TOP_MU_across %>% 
  pivot_longer(!Year, names_to = "Catch", values_to = "t") 

#percentage of non-target catch in different MUs
ggplot(catch_bycatch_TOP_MU_across, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly TOP target catch/bycatch in SIOFA toothfish management areas (relative)", x="Year", y="Catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("TOP summary/SIOFAnontargetcatch_TOP_MUs_across_fill_web.png", width = 10, height = 4, dpi = 150)

##discards
# filter all TOP target operations (with full data)
TOP_target_operations <- fishing %>%
  filter(ActivityID %in% yearly_operations_TOP_target$ActivityID 
         & datasetID %in% yearly_operations_TOP_target$datasetID |
           Target == "TOP")
# load discards data
discards_species_TOP <- filter(TOP_target_operations, catchFate == "0")
# plot discards
# sort discards data for plotting
sort_discards_species_TOP <- aggregate(discards_species_TOP$CatchTon, by=list(Species=discards_species_TOP$SpeciesCode), FUN=sum, na.rm = TRUE)
sort_discards_species_TOP <- arrange(sort_discards_species_TOP, desc(x)) 
top5_discards_species_TOP <- sort_discards_species_TOP %>% slice(1:5)
other_discards_species_TOP <- sort_discards_species_TOP %>% slice(6:52)

ggplot(data= subset(discards_species_TOP, SpeciesCode %in% top5_discards_species_TOP$Species), aes(x = Year, y = CatchTon)) +
  geom_bar(aes(fill=SpeciesCode), position="stack", stat="identity") +
  geom_bar(data= subset(discards_species_TOP, SpeciesCode %in% other_discards_species_TOP$Species), aes(x = Year, y = CatchTon, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2002, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(data= subset(discards_species_TOP, SpeciesCode %in% top5_discards_species_TOP$Species), aes(x = Year, y = CatchTon)) +
  geom_bar(aes(fill=SpeciesCode), position="stack", stat="identity") +
  geom_bar(data= subset(discards_species_TOP, SpeciesCode %in% other_discards_species_TOP$Species), aes(x = Year, y = CatchTon, fill="'Other species'"), position="stack", stat="identity") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 8:1) +
  theme_bw() +
  labs(title="Yearly discards by species in TOT fisheries in the SIOFA area", x="Year", y="Discards (t)") +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("TOP summary/discards_species_TOP_web.png", width = 10, height = 4, dpi = 150)

# calculate discard rates
# total catch per year
catch_discards_TOP <- catch_bycatch_subarea_TOP %>% 
  dplyr::select(-SubAreaNo) %>%
  group_by(Year) %>%
  summarise(TotCatch=sum(TargetCatch, NonTargetCatch))
# total discards per year
discards_TOP <- discards_species_TOP %>%
  group_by(Year) %>%
  summarise(TotDiscards=sum(CatchTon))
# join and correct for discards double counting
catch_discards_TOP <- full_join(catch_discards_TOP, discards_TOP)
catch_discards_TOP <- catch_discards_TOP %>%
  replace(is.na(.), 0) %>%
  mutate(RetainedCatch=TotCatch-TotDiscards) %>% # this is needed otherwise discards are double counted
  rename(Discards=TotDiscards) %>%
  dplyr::select(-TotCatch)
# transform for plotting
catch_discards_TOP <- catch_discards_TOP %>%
  pivot_longer(!Year, names_to = "Catch", values_to = "t")
# plot total catch /discards percentage
# colorblind friendly version
ggplot(catch_discards_TOP, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly retained catch/discards in TOT fisheries in the SIOFA area (relative)", x="Year", y="Total catch proportion") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 5:6) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_manual(values = c("Discards" = "#D55E00", "RetainedCatch" = "#0072B2"), 
                    labels=c("Discards", "Retained catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("TOP summary/SIOFAcatch_discards_TOP_fill_web.png", width = 10, height = 4, dpi = 150)

#colorblind friendly version
ggplot(catch_discards_TOP, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly retained catch/discards in TOT fisheries in the SIOFA area (absolute)", x="Year", y="Total catch (t)") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 4:5) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_manual(values = c("Discards" = "#D55E00", "RetainedCatch" = "#0072B2"), 
                    labels=c("Discards", "Retained catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("TOP summary/SIOFAcatch_discards_TOP_web.png", width = 10, height = 4, dpi = 150)


## VME catches in TOP fisheries

# filter only TOP target reports 
VME_catch_TOP <- filter(VME_catch, grepl('TO', TargetSpecies))

# aggregate by year and taxon
yearly_TOP_VME_catch <- VME_catch_TOP %>%
  group_by(Year, FAOcode) %>%
  summarise_at(.vars = c("Weight"),
               .funs = "sum")
# plot TOP VME bycatch
ggplot(yearly_TOP_VME_catch, aes(x = Year, y = Weight, fill=FAOcode)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly incidental catch of VME indicator taxa in TOT fisheries in the SIOFA area", x="Year", y="Total weight (kg)") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  #scale_fill_hue(labels = c("Non-target Catch", "Target Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("TOP summary/VME_captures_TOP_full_web.png", width = 10, height = 4, dpi = 150)

# plot TOP VME bycatch, top 5 species only
# sort bycatch data for plotting
sort_VME_bycatch_TOP <- aggregate(yearly_TOP_VME_catch$Weight, by=list(Species=yearly_TOP_VME_catch$FAOcode), FUN=sum, na.rm = TRUE)
sort_VME_bycatch_TOP <- arrange(sort_VME_bycatch_TOP, desc(x)) 
top5_species_VME <- sort_VME_bycatch_TOP %>% slice(1:5)
other_species_VME <- sort_VME_bycatch_TOP %>% slice(6:29)

ggplot(data= subset(yearly_TOP_VME_catch, FAOcode %in% top5_species_VME$Species), aes(x = Year, y = Weight)) +
  geom_bar(aes(fill=FAOcode), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_TOP_VME_catch, FAOcode %in% other_species_VME$Species), aes(x = Year, y = Weight, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2002, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(data= subset(yearly_TOP_VME_catch, FAOcode %in% top5_species_VME$Species), aes(x = Year, y = Weight)) +
  geom_bar(aes(fill=FAOcode), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_TOP_VME_catch, FAOcode %in% other_species_VME$Species), aes(x = Year, y = Weight, fill="'Other species'"), position="stack", stat="identity") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 8:1) +
  theme_bw() +
  labs(title="Yearly incidental catch of VME indicator taxa in TOT fisheries in the SIOFA area", x="Year", y="Total weight (kg)") +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("TOP summary/VME_captures_TOP_web.png", width = 10, height = 4, dpi = 150)

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
captures_spatial_plot_TOP <- captures_spatial  %>%
  filter(!(speciesGroup=='Chimaeras' |  speciesGroup=='Deepwater sharks' |  speciesGroup=='Sharks and rays nei' ) & species3ACode %in% shark_species_CMM$FAOcode
         | (speciesGroup=='Seabirds'| speciesGroup=='Cetaceans'| speciesGroup=='Turtles')) %>%
  filter(source=='OBS') %>%
  filter((fishopTargetSpecies == "TOP"))

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = captures_spatial_plot_TOP, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA incidental captures of seabirds, marine mammals, turtles and 
          sharks considered to be at high risk and/or of concern in TOT target operations (2014-2023)") +
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
  geom_sf(data = captures_spatial_plot_TOP, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA incidental captures of seabirds, marine mammals, turtles and 
          sharks considered to be at high risk and/or of concern in TOT target operations (2012-2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  scale_colour_discrete("Captures") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("TOP summary/SIOFAmap_accidental_captures_web_TOP.png", width = 10, height = 8, dpi = 150)

# aggregate captures in a table that can be used in reports
# filter only operations that observers recorded as targeting TOP
captures_TOP <-   captures%>%
  filter(grepl('TOP', fishopTargetSpecies))

# plot a heatmap of captures
# calculate number of captures per 5 degree grid cell
toothfish_captures_poly5 <- st_join(captures_spatial_plot_TOP, grid5)
toothfish_captures_count5 <- toothfish_captures_poly5 %>% group_by(id) %>% count()
toothfish_captures_count5 <- st_drop_geometry(toothfish_captures_count5)
toothfish_captures_poly5 <- left_join(grid5, toothfish_captures_count5, by="id")
toothfish_captures_poly5 <- filter(toothfish_captures_poly5, (!is.na(toothfish_captures_poly5$n)))
# classify interval of gridline cells for plotting
toothfish_captures_breaks_qt5 <- classIntervals(c(min(toothfish_captures_poly5$n) - .00001, toothfish_captures_poly5$n), n = 7, style = "pretty")
toothfish_captures_poly5 <- mutate(toothfish_captures_poly5, n.events5 = cut(n, toothfish_captures_breaks_qt5$brks))
# crop grid squares with the SIOFA boundaries
toothfish_captures_poly5 <- st_intersection(toothfish_captures_poly5, fishing_boundaries)
# plot captures heatmap 5 degrees
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf(data = toothfish_captures_poly5, aes(fill=n.events5)) +
  scale_fill_viridis(discrete = TRUE) +
  geom_sf_label(data = fishing_boundaries, aes(label = SubAreaNo), 
                size = 2.5, 
                color = colors_named_vector[as.character(fishing_boundaries$SubAreaNo)], 
                label.size = 0, 
                nudge_y = c(0, 2, 2.5, 0, 0, 1.5, 2, -6), 
                nudge_x = c(0, -2.5, 0, 3, 0, 0, -2, -6)) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA incidental captures of seabirds, marine mammals, turtles and 
          sharks considered to be at high risk and/or of concern in TOT target operations (5 degrees, 2012-2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. captures") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("TOP summary/SIOFAmap_toothfish_captures_heatmap_5_web.png", width = 10, height = 7, dpi = 150)

# turtles
captures_turtles <- filter(captures_TOP,(speciesGroup == 'Turtles'))

#captures_turtles_aggregate1 <- captures_turtles %>%
#  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
#  summarise(n = n()) 
captures_turtles_aggregate_TOP <- captures_turtles %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  relocate(speciesEnglishName, .after = Year) %>%
  relocate(speciesScientificName, .after = speciesEnglishName) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Captures" = foibcNbCaught) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(captures_turtles_aggregate_TOP,"TOP summary/Tables/captures_turtles_TOP.xlsx")

# marine mammals
captures_mammals <- filter(captures_TOP,(speciesGroup == 'Cetaceans'))

captures_mammals_aggregate_TOP <- captures_mammals %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) 

write_xlsx(captures_mammals_aggregate_TOP,"TOP summary/Tables/captures_mammals_TOP.xlsx")

# seabirds
captures_seabirds <- filter(captures_TOP,(speciesGroup == 'Seabirds'))

captures_seabirds_aggregate_TOP <- captures_seabirds %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Captures" = foibcNbCaught) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(captures_seabirds_aggregate_TOP,"TOP summary/Tables/captures_seabirds_TOP.xlsx")

# sharks
captures_sharks_TOP <- captures_TOP %>%
  filter(!(speciesGroup=='Chimaeras' |  speciesGroup=='Deepwater sharks' |  speciesGroup=='Sharks and rays nei' ) & species3ACode %in% shark_species_CMM$FAOcode)

captures_sharks_aggregate_TOP <- captures_sharks_TOP %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) %>%
  rename("Captures (n)" = foibcNbCaught)

write_xlsx(captures_sharks_aggregate_TOP,"TOP summary/Tables/captures_sharks_TOP.xlsx")

### Analyze observations of seabirds and mammals
# Convert the observation dataframe to a spatial object. Note that the
# crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object
# and that missing values are removed
observations_spatial <- filter(observations, (!is.na(observations$Longitude)))
observations_spatial <- st_as_sf(observations_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 
class(observations_spatial)
st_crs(observations_spatial) <- 4326

observations_spatial_plot_TOP <- observations_spatial  %>%
  filter(!(speciesGroup=='NO BIRDS OBSERVED' |  speciesGroup=='NO OBSERVER'|  speciesGroup=='NO MAMMALS OBSERVED')) %>%
  filter(source=='OBS') %>%
  filter((fishopTargetSpecies == "TOP"))

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = observations_spatial_plot_TOP, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA observations of seabirds and marine mammals
            in TOT target operations (2012-2023)") +
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
  geom_sf(data = observations_spatial_plot_TOP, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA observations of seabirds and marine mammals
            in TOT target operations (2012-2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  scale_colour_discrete("Observations") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("TOP summary/SIOFAmap_observations_web_TOP.png", width = 10, height = 8, dpi = 150)

# plot a heatmap of seabirds observations
# calculate number of observations per 5 degree grid cell
toothfish_observations_poly5 <- st_join(observations_spatial_plot_TOP, grid5)
toothfish_observations_count5 <- toothfish_observations_poly5 %>% group_by(id) %>% count()
toothfish_observations_count5 <- st_drop_geometry(toothfish_observations_count5)
toothfish_observations_poly5 <- left_join(grid5, toothfish_observations_count5, by="id")
toothfish_observations_poly5 <- filter(toothfish_observations_poly5, (!is.na(toothfish_observations_poly5$n)))
# classify interval of gridline cells for plotting
toothfish_observations_breaks_qt5 <- classIntervals(c(min(toothfish_observations_poly5$n) - .00001, toothfish_observations_poly5$n), n = 7, style = "pretty")
toothfish_observations_poly5 <- mutate(toothfish_observations_poly5, n.events5 = cut(n, toothfish_observations_breaks_qt5$brks))
# crop grid squares with the SIOFA boundaries
toothfish_observations_poly5 <- st_intersection(toothfish_observations_poly5, fishing_boundaries)
# plot observations heatmap 5 degrees
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf(data = toothfish_observations_poly5, aes(fill=n.events5)) +
  scale_fill_viridis(discrete = TRUE) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(1,3,3.5, -1, 0, -2, 0, -6), nudge_x=c(0,-2.5,0, 6, 0, 0, 0, -6)) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA observations of seabirds, marine mammals, turtles and 
          sharks considered to be at high risk and/or of concern in TOT target operations (5 degrees, 2012-2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. observations") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("TOP summary/SIOFAmap_toothfish_observations_heatmap_5_web.png", width = 10, height = 7, dpi = 150)

## analyze seabirds observations
observations_seabirds_aggregate_TOP <- observations %>%
  filter(grepl('TOP', fishopTargetSpecies)) %>%
  filter(!speciesGroup=='MARINE MAMMALS') %>%
  filter(!speciesGroup=='NO BIRDS OBSERVED') %>%
  filter(!speciesGroup=='NO MAMMALS OBSERVED') %>%
  filter(!speciesGroup=='NO OBSERVER') %>%
  group_by(Year, speciesEnglishName, speciesScientificName, Gear, speciesGroup) %>%
  summarise(Abundance = sum(Abundance)) %>%
  dplyr::select(-speciesGroup) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(observations_seabirds_aggregate_TOP,"TOP summary/Tables/observations_seabirds_TOP.xlsx")

## analyze negative seabirds observations
negative_observations_seabirds_aggregate_TOP <- observations %>%
  filter(grepl('TOP', fishopTargetSpecies)) %>%
  filter(speciesGroup=='NO BIRDS OBSERVED') %>%
  group_by(Year, Gear) %>%  
  summarise(n = n()) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Fishing events with no birds observed" = n)

write_xlsx(negative_observations_seabirds_aggregate_TOP,"TOP summary/Tables/negative_observations_seabirds_TOP.xlsx")

## analyze non-performed seabirds observations
no_observations_seabirds_aggregate_TOP <- observations %>%
  filter(grepl('TOP', fishopTargetSpecies)) %>%
  filter(speciesGroup=='NO OBSERVER') %>%
  group_by(Year, Gear) %>%  
  summarise(n = n()) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Fishing events not observed for bird presence" = n)

write_xlsx(no_observations_seabirds_aggregate_TOP,"TOP summary/Tables/no_observations_seabirds_TOP.xlsx")

## analyze marine mammals observations during fishing events
observations_mammals_aggregate_TOP <- observations %>%
  filter(grepl('TOP', fishopTargetSpecies)) %>%
  group_by(Year, speciesEnglishName, speciesScientificName, Gear, speciesGroup) %>%
  filter(speciesGroup=='MARINE MAMMALS') %>%
  filter(!speciesGroup=='NO BIRDS OBSERVED') %>%
  filter(!speciesGroup=='NO MAMMALS OBSERVED') %>%
  filter(!speciesGroup=='NO OBSERVER') %>%
  filter(!speciesGroup=='SEABIRDS') %>%
  summarise(Abundance = sum(Abundance)) %>%
  dplyr::select(-speciesGroup) %>%
  mutate(speciesEnglishName = replace_na(speciesEnglishName, "-")) %>%
  mutate(speciesScientificName = replace_na(speciesScientificName, "-")) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Maximum abundance" = Abundance) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(observations_mammals_aggregate_TOP,"TOP summary/Tables/observations_mammals_TOP.xlsx")

# as flextable

FT.observations_mammals_aggregate_TOP <- observations_mammals_aggregate_TOP %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  fit_to_width(max_width = 18, unit="cm") %>%
  flextable::align(j = 1:4, align = "left", part = "all") %>%
  flextable::align(j = 5, align = "center", part = "all") %>%
  italic(j = 3, italic = TRUE, part = "body") %>%
  add_header_lines("Observations of marine mammals in SIOFA toothfish fisheries") 

save_as_image(FT.observations_mammals_aggregate_TOP, path = "TOP summary/Tables/observations_mammals_TOP.png")               

## analyze countries that submitted observer data (measures of fish) in TOP fishery
countries_observing_TOP <- Observer_data %>%
  #filter(Year>=2013) %>%
  filter((species3ACode == "TOP") | (species3ACode == "TOA")) %>%
  group_by(Year,CountryISOCode) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::select(-n) %>%
  rename(Country = CountryISOCode) 

write_xlsx(countries_observing_TOP,"TOP summary/Tables/countries_observing_TOP.xlsx")

# as flextable
FT.countries_observing_TOP <- countries_observing_TOP %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  fit_to_width(max_width = 18, unit="cm") %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  flextable::align(j = 2, align = "center", part = "all") %>%
  add_header_lines("Toothfish observer data submitted by different SIOFA CCPs")

save_as_image(FT.countries_observing_TOP, path = "TOP summary/Tables/countries_observing_TOP.png")  

# measures of maturity, sex and weight, and otoliths collected for TOP
measured_fish_TOP <- Observer_data %>%
  #filter(Year>=2013) %>%
  filter(species3ACode == "TOP") %>%
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

sums_measured_fish_TOP <- colSums(measured_fish_TOP)
measured_fish_TOP <- rbind(measured_fish_TOP,sums_measured_fish_TOP)

write_xlsx(measured_fish_TOP,"TOP summary/Tables/measured_fish_TOP.xlsx")

# as flextable
FT.measured_fish_TOP <- measured_fish_TOP %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  fit_to_width(max_width = 23, unit="cm") %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  flextable::align(j = 2:8, align = "center", part = "all") %>%
  compose(j = 1, i = nrow(measured_fish_TOP), as_paragraph(as_chunk('Total'))) %>%
  bold(i = nrow(measured_fish_TOP), j = 1) %>%
  add_header_lines("Patagonian toothfish observer data measurements")

save_as_image(FT.measured_fish_TOP, path = "TOP summary/Tables/measured_fish_TOP.png")  

# measures of maturity, sex and weight, and otoliths collected for TOA
measured_fish_TOA <- Observer_data %>%
  #filter(Year>=2013) %>%
  filter(species3ACode == "TOA") %>%
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

sums_measured_fish_TOA <- colSums(measured_fish_TOA)
measured_fish_TOA <- rbind(measured_fish_TOA,sums_measured_fish_TOA)

write_xlsx(measured_fish_TOA,"TOP summary/Tables/measured_fish_TOA.xlsx")

# as flextable
FT.measured_fish_TOA <- measured_fish_TOA %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  fit_to_width(max_width = 23, unit="cm") %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  flextable::align(j = 2:8, align = "center", part = "all") %>%
  compose(j = 1, i = nrow(measured_fish_TOA), as_paragraph(as_chunk('Total'))) %>%
  bold(i = nrow(measured_fish_TOA), j = 1) %>%
  add_header_lines("Antarctic toothfish observer data measurements")

save_as_image(FT.measured_fish_TOA, path = "TOP summary/Tables/measured_fish_TOA.png")  

## analyze countries participating in TOP fishery
countries_fishing_TOT <- fishing %>%
  filter(ActivityID %in% yearly_operations_TOP_target$ActivityID 
         & datasetID %in% yearly_operations_TOP_target$datasetID |
           Target == "TOP") %>%
  group_by(Year,CCPCode1,dbSource) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::select(-n) %>%
  rename(Country = CCPCode1) %>%
  rename(Database = dbSource) 

write_xlsx(countries_fishing_TOT,"TOP summary/Tables/countries_fishing_TOP.xlsx")

# as flextable
FT.countries_fishing_TOT <- countries_fishing_TOT %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  fit_to_width(max_width = 18, unit="cm") %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  flextable::align(j = 2:3, align = "center", part = "all") %>%
  add_header_lines("Toothfish catch and effort data submitted by different SIOFA CCPs")

save_as_image(FT.countries_fishing_TOT, path = "TOP summary/Tables/countries_fishing_TOP.png")  

## analyze average number of vessels in the last 5 years
vessels_fishing_TOT <- fishing %>%
  filter(ActivityID %in% yearly_operations_TOP_target$ActivityID 
         & datasetID %in% yearly_operations_TOP_target$datasetID |
           Target == "TOP") %>%
  filter(Year>(2023-5)) %>% # need to change this every year
  distinct(vesselName, Year) %>%
  group_by(Year) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::select(n) %>%
  sapply(mean, na.rm = TRUE)
