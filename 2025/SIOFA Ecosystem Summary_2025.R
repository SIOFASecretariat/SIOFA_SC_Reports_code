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

# plot world map and add SIOFA subareas + siofa fishing events on world map
# standard palette
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  #geom_sf(data = fishing_spatial, color = 'black', cex = .1) +
  geom_sf(data = BPAs, fill = "purple", color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,0,0, 0, 0, 0, 0, -6)) +
  geom_sf_label(data = BPAs, aes(fill = NULL, label = value), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,0,0, 0, 0, 0, 0, -6)) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA SubAreas") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 
# colorblind palette
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries, aes(fill = SubArea)) +
  geom_sf(data = BPAs, fill = "purple", color = "black") +
  scale_fill_okabeito(order=c(6,2,7,1,5,9,3,8,4), palette = "full") +
  #geom_sf(data = fishing_spatial, color = 'black', cex = .1) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 3.5, col = "black", label.size = 0, nudge_x=c(0,0,0,-9,0,0,0,0), nudge_y=c(0,1.5,1.5,1,0,0,0,-6)) +
  geom_sf_label(data = BPAs, aes(fill = NULL, label = value), size = 3.5, col = "black", label.size = 0, nudge_x=c(0,0,0,0,0), nudge_y=c(-2,-2,-2,-2,-2)) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA SubAreas and Interim Protected Areas") +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(title = "SubArea names", override.aes = aes(label = ""))) +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("Ecosystem Summary/SIOFAmap_web.png", width = 10, height = 10, dpi = 150)

## analyze observer coverage

# old way to analyze  (using only the OBS database)
observer_coverage_table <- observer_coverage %>%
  group_by(Year, Gear) %>%
  summarise(Observed= sum(foObserved=='Y',na.rm=T), Total = n()) %>%
  mutate(OBSratio = Observed/Total) %>%
  mutate_at(vars(OBSratio), funs(round(., 3))) %>%
  dplyr::select(-Observed) %>%
  rename('Observer ratio'= OBSratio, 'Total events' = Total) %>%
  pivot_wider(names_from = Gear, values_from = c('Total events', 'Observer ratio'),  names_sep = ' in ' ) 
  
# new way to analyze observer coverage (using CatchEffort and OBS databases)
# the gear do not match across datasets
# manually harmonize codes across datasets

# filter only OBS observed events
observed_fishing_events <- observer_coverage %>%
  filter(foObserved=='Y')

# retrieve all events from the CatchEffort database matching those in the OBS database
CE_observed_fishing_events <- fishing %>%
  filter(ActivityID %in% observed_fishing_events$ActivityID & datasetID %in% observed_fishing_events$CEdatasetID) %>%
  distinct(ActivityID, dbSource, .keep_all = TRUE) %>%
  dplyr::select(ActivityID, datasetID, CCPCode1, Year, Gear) 

# find matching events across the two datasets
matching_fishing_events <- observed_fishing_events %>%
    left_join(CE_observed_fishing_events, 
            by = c('ActivityID' = 'ActivityID',  'CountryISOCode'='CCPCode1', 'Year'='Year')) 

# inspect mismatch instances, harmonize values
# need to have a single Gear field
matching_fishing_events <- matching_fishing_events %>%
  mutate(Gear = case_when(
    Gear.y == Gear.x ~ Gear.x,       # If they match, use Gear.x
    is.na(Gear.y) ~ Gear.x,          # If Gear.y is NA, use Gear.x
    TRUE ~ Gear.y                     # Otherwise, use Gear.x
  ))

# replace harmonized gear instances in observed events
# summarize observed events
harmonized_fishing_events <- matching_fishing_events %>%
  dplyr::select(Year, Gear, foObserved) %>%
  group_by(Year, Gear) %>%
  summarise(Observed_events = sum(foObserved=='Y',na.rm=T)) 

# calculate catch effort total fishing events by gear

all_fishing_events <- fishing %>%
  filter(Year>=2018) %>% #ensures we only compare years when OBS coverage data available
  distinct(ActivityID, dbSource, .keep_all = TRUE) %>% #ensures we don't count duplicates of fishing events
  group_by(Year,Gear) %>%
  summarise(Fishing_events_trawls_AGG = sum(NbTows[dbSource=="AGG"], na.rm = TRUE),
            Fishing_events_sets_AGG = sum(NbSets[dbSource=="AGG"], na.rm = TRUE),
            Fishing_events_NA = sum(is.na(NbTows[dbSource=="AGG"])), 
            Fishing_events_HBH = n_distinct(ActivityID[dbSource=="HBH"]) 
  ) %>% #need to account for aggregated data and ND data
  mutate(Fishing_events = 
           Fishing_events_trawls_AGG + Fishing_events_sets_AGG + Fishing_events_NA + Fishing_events_HBH) %>%
  dplyr::select(-Fishing_events_trawls_AGG,-Fishing_events_sets_AGG, -Fishing_events_NA, -Fishing_events_HBH)  %>%
  replace(is.na(.), 0) 

# combine the two dataframes for the final analysis
observed_fishing_events <- full_join(harmonized_fishing_events,all_fishing_events, by = c("Year", "Gear"))
 
observed_fishing_events <- observed_fishing_events %>%
  replace(is.na(.), 0) %>%
  mutate(observed_ratio = Observed_events/Fishing_events) %>%
  mutate(observed_percentage = Observed_events/Fishing_events*100) %>%
  mutate_at(vars(observed_percentage, observed_ratio), funs(round(., 1)))

# clean up and cluster
is.na(observed_fishing_events) <- sapply(observed_fishing_events, is.infinite)

observed_fishing_events <- observed_fishing_events %>%
  dplyr::arrange(Gear, Year) %>%
  dplyr::select(Gear, everything())

# as flextable for output 
observer.coverage <- observed_fishing_events %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  fit_to_width(max_width = 18, unit="cm") %>%
  flextable::align(j = "Gear", align = "left") %>%
  flextable::align(j = 2:6, align = "center", part = "all") %>%
  merge_v(j = "Gear") %>%
  bg(i = ~ is.na(observed_ratio), 
    j = ~ observed_ratio, 
    bg="red") %>%
  bg(i = ~ observed_ratio>1, 
     j = ~ observed_ratio, 
     bg="yellow") %>%
  bg(i = ~ is.na(observed_percentage), 
     j = ~ observed_percentage, 
     bg="red") %>%
  bg(i = ~ observed_percentage>100, 
     j = ~ observed_percentage, 
     bg="yellow") %>%
  #bg(j = observed_ratio, i = ~ observed_ratio=is.na, bg = "red") %>%
  #bg(j = observed_percentage, i = ~ observed_percentage>100, bg = "red") %>%
  set_header_labels(values = list(
     Observed_events="Observed events"  , 
    Fishing_events= "CatchEffort events" ,
     observed_ratio="Observed events (ratio)"  ,
    observed_percentage="Observed events (%)" 
                    )) %>%
  add_header_lines("Observer coverage in SIOFA fisheries (2018-2023)") 

save_as_image(observer.coverage, path = "Ecosystem Summary/Tables/observer_coverage.png")               

## Fishing in BPAs
# intersect fishing events with BPAs
# Overlay points and extract just the subarea column 
fishing_within_BPAs <- st_join(fishing_spatial, left = FALSE, BPAs)

# create a data frame with only total catches in BPAs
yearly_global_catches_BPAs <- aggregate(fishing_within_BPAs$CatchTon, by=list(Year=fishing_within_BPAs$Year, Species=fishing_within_BPAs$SpeciesCode, Gear=fishing_within_BPAs$GearCode), FUN=sum, na.rm = TRUE)

# plot histogram of total catches per year in BPAs
sort_BPA <- aggregate(yearly_global_catches_BPAs$x, by=list(Species=yearly_global_catches_BPAs$Species), FUN=sum)

sort_BPA <- arrange(sort_BPA, desc(x)) 

top5_BPA <- sort_BPA %>% slice(1:5)
other_BPA <- sort_BPA %>% slice(6:67)

ggplot(data= subset(yearly_global_catches_BPAs, Species %in% top5_BPA$Species), aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_global_catches_BPAs, Species %in% other_BPA$Species), aes(x = Year, y = x, fill="'Other species'"), position="stack", stat="identity") +
  labs(title="Yearly catch in SIOFA IPAs by species", x="Year", y="Catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_BPAs_web.png", width = 10, height = 5, dpi = 150)

ggplot(yearly_global_catches_BPAs, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  labs(title="Yearly catch in SIOFA IPAs by species", x="Year", y="Catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_BPAs_full_web.png", width = 10, height = 5, dpi = 150)

global_catches_BPAs_table <- yearly_global_catches_BPAs %>%
  group_by(Species) %>%
  summarise('Total weight (t)' = sum(x)) %>%
  mutate_at(vars('Total weight (t)'), funs(round(., 1)))
  
write_xlsx(global_catches_BPAs_table,"Ecosystem Summary/Tables/global_catches_BPAs_table.xlsx")


## plot histogram of gear used per year in BPAs
# count fishing events by gear
yearly_gear_used_BPAs <- fishing_within_BPAs

yearly_gear_used_BPAs <- yearly_gear_used_BPAs %>%
  group_by(ActivityID, Year, Gear) %>%
  summarise(n = n())

yearly_gear_used_BPAs <- yearly_gear_used_BPAs %>%
  group_by(Year, Gear) %>%
  summarise(n = n())

sum(yearly_gear_used_BPAs$n)

# plot
ggplot(yearly_gear_used_BPAs, aes(x = Year, y = n)) +
  geom_bar(aes(fill=Gear), position="stack", stat="identity") +
  labs(title="Yearly fishing events in SIOFA IPAs by gear used", x="Year", y="Fishing events (number)") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(yearly_gear_used_BPAs, aes(x = Year, y = n)) +
  geom_bar(aes(fill=Gear), position="stack", stat="identity") +
  labs(title="Yearly fishing events in SIOFA IPAs by gear used", x="Year", y="Fishing events (number)") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 8:1) +  
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  geom_vline(xintercept=c(2018.5), linetype="dotted") +
  geom_text(x=2018.5, y=60, label="CMM 2018/01") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAgear_BPAs_web.png", width = 10, height = 4, dpi = 150)


## analyze VME bycatch data

#groom data for duplicates
VME_bycatch_groomed <- VME_catch %>% distinct(Longitude, Latitude, FAOcode, speciesScientificName, Weight, .keep_all = TRUE)

# plotting data for VME
# first graph catch (bars) per species and year, top5 species, all others grouped
# second graph catch by year and subarea

sort_VME <- aggregate(VME_bycatch_groomed$Weight, by=list(Species=VME_bycatch_groomed$FAOcode), FUN=sum, na.rm = TRUE)

sort_VME <- arrange(sort_VME, desc(x)) 

top5_VME <- sort_VME %>% slice(1:5)
other_VME <- sort_VME %>% slice(6:30)

#plot VME bycatch weight by main gear
VME_bycatch_gear <- aggregate(VME_bycatch_groomed$Weight, by=list(Year=VME_bycatch_groomed$Year, Gear=VME_bycatch_groomed$Gear), FUN=sum, na.rm = TRUE)  

VME_bycatch_gear <-  VME_bycatch_gear  %>%
  mutate_at(vars(x), funs(round(., 1))) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(Year, everything()) %>%
  arrange(Year)

ggplot(VME_bycatch_gear, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Gear), position="stack", stat="identity") +
  labs(title="Yearly VME taxa catch in SIOFA fisheries by gear", x="Year", y="VME catch weight (kg)") +
  theme_bw() +
  scale_x_continuous(limits=c(2003, 2024)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly(er) version
ggplot(VME_bycatch_gear, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Gear), position="stack", stat="identity") +
  scale_fill_brewer(type = "seq", palette = "Paired") +
  labs(title="Yearly VME taxa catch in SIOFA fisheries by gear", x="Year", y="VME catch weight (kg)") +
  theme_bw() +
  scale_x_continuous(limits=c(2003, 2024),breaks = seq(2003, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAgear_VMEs_web.png", width = 10, height = 4, dpi = 150)

#plot total VME bycatch by year
VME_bycatch_year <- aggregate(VME_bycatch_groomed$Weight, by=list(Year=VME_bycatch_groomed$Year, Group=VME_bycatch_groomed$FAOcode), FUN=sum, na.rm = TRUE)  

ggplot(data= subset(VME_bycatch_year, Group %in% top5_VME$Species), aes(x = Year, y = x)) +
  geom_bar(aes(fill=Group), position="stack", stat="identity") +
  geom_bar(data= subset(VME_bycatch_year, Group %in% other_VME$Species), aes(x = Year, y = x, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  labs(title="Yearly VME taxa catch in SIOFA fisheries by species group", x="Year", y="VME catch weight (kg)") +
  theme_bw() +
  scale_x_continuous(limits=c(2003, 2024),breaks = seq(2003, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(data= subset(VME_bycatch_year, Group %in% top5_VME$Species), aes(x = Year, y = x)) +
  geom_bar(aes(fill=Group), position="stack", stat="identity") +
  geom_bar(data= subset(VME_bycatch_year, Group %in% other_VME$Species), aes(x = Year, y = x, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  labs(title="Yearly VME taxa catch in SIOFA fisheries by species group", x="Year", y="VME catch weight (kg)") +
  theme_bw() +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 8:1) +
  scale_x_continuous(limits=c(2003, 2024),breaks = seq(2003, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_VMEs_web.png", width = 10, height = 4, dpi = 150)

# full figure
ggplot(VME_bycatch_year, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Group), position="stack", stat="identity") +
  theme_bw() +
  labs(title="Yearly VME taxa catch in SIOFA fisheries by species group", x="Year", y="VME catch weight (kg)") +
  theme_bw() +
  scale_x_continuous(limits=c(2003, 2024),breaks = seq(2003, 2024, by = 5))+
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_VMEs_cropped_web.png", width = 10, height = 4, dpi = 150)

VME_bycatch_year_table <- VME_bycatch_year %>%
  group_by(Group) %>%
  summarise('Total weight (kg)' = sum(x))  %>%
  rename(Taxon=Group) %>%
  mutate_at(vars('Total weight (kg)'), funs(round(., 2)))

write_xlsx(VME_bycatch_year_table,"Ecosystem Summary/Tables/VME_bycatch_year_table.xlsx")

#map of VME bycatch by taxon and weight, need to do it by subarea
VME_spatial <- filter(VME_bycatch_groomed, (!is.na(VME_bycatch_groomed$Longitude)))
VME_spatial <- filter(VME_spatial, (!is.na(VME_spatial$Latitude)))
VME_spatial <- st_as_sf(VME_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 
VME_within <- st_join(VME_spatial, left = FALSE, fishing_boundaries["SubAreaNo"])

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf(data = subset(VME_within, FAOcode %in% top5_VME$Species), aes(color = speciesScientificName), cex = 1.5) +
  #geom_sf(data = subset(VME_within, groupFAOCode %in% other_VME$Species), aes(color = "other taxa"), cex = 1.5) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA VME incidental catches (2014-2023)") +
  scale_color_discrete(name = "Taxonomic group") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

#colorblind friendly version
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf(data = subset(VME_within, FAOcode %in% top5_VME$Species), aes(color = speciesScientificName), cex = 1.5) +
  #geom_sf(data = subset(VME_within, groupFAOCode %in% other_VME$Species), aes(color = "other taxa"), cex = 1.5) +
  geom_sf_label(data = fishing_boundaries, aes(fill = SubAreaNo, label = SubAreaNo), size = 2.5, 
                col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 1.5, 2, -6), 
                nudge_x=c(0,-2.5,0, 3, 0, 0, -2, -6)) +
  scale_fill_manual(values = colors_named_vector, 
                    guide = "none") + 
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA VME incidental catches (2014-2023)") +
  scale_color_discrete(name = "Taxonomic group") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("Ecosystem Summary/SIOFAmap_VMEa_web.png", width = 10, height = 7, dpi = 150)

# full version
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf(data = subset(VME_within), aes(color = speciesScientificName), cex = 1.5) +
  #geom_sf(data = subset(VME_within, groupFAOCode %in% other_VME$Species), aes(color = "other taxa"), cex = 1.5) +
  geom_sf_label(data = fishing_boundaries, aes(fill = SubAreaNo, label = SubAreaNo), size = 2.5, 
                col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 1.5, 2, -6), 
                nudge_x=c(0,-2.5,0, 3, 0, 0, -2, -6)) +
  scale_fill_manual(values = colors_named_vector, 
                    guide = "none") + 
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA VME incidental catches (2014-2023)") +
  scale_color_discrete(name = "Taxonomic group") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("Ecosystem Summary/SIOFAmap_VMEb_web.png", width = 10, height = 7, dpi = 150)

# table of VME bycatch by species and gear
#Species/Gear/Total weight (kg)
VME_bycatch_table <- aggregate(VME_bycatch_groomed$Weight, by=list(Taxon=VME_bycatch_groomed$speciesScientificName, Gear=VME_bycatch_groomed$Gear), FUN=sum, na.rm = TRUE)  

VME_bycatch_table <-  VME_bycatch_table  %>%
  pivot_wider(names_from = Gear, values_from = x) 

write_xlsx(VME_bycatch_table,"Ecosystem Summary/Tables/VME_bycatch_table.xlsx")

##discards
# subset the whole data for all species, need catch, effort and subarea aggregations
fishing_within <- st_join(fishing_spatial, left = FALSE, fishing_boundaries["SubAreaNo"])
yearly_global_catches <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year), FUN=sum, na.rm = TRUE)
yearly_global_catches_species <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, Species=fishing$SpeciesCode), FUN=sum, na.rm = TRUE)
yearly_global_catches_subarea <- aggregate(fishing_within$CatchTon, by=list(Year=fishing_within$Year,SubArea=fishing_within$SubAreaNo), FUN=sum, na.rm = TRUE)
yearly_global_catches_subarea_species <- aggregate(fishing_within$CatchTon, by=list(Year=fishing_within$Year,SubArea=fishing_within$SubAreaNo, Species=fishing_within$SpeciesCode), FUN=sum, na.rm = TRUE)
#calculate total target catch per year
yearly_global_catches_target <- filter(yearly_global_catches_species, Species %in% target_species_revised$"FAO Code")
yearly_global_catches_target <- aggregate(yearly_global_catches_target$x, by=list(Year=yearly_global_catches_target$Year), FUN=sum, na.rm = TRUE)
#calculate total target catch per year and subarea
yearly_global_catches_subarea_target <- filter(yearly_global_catches_subarea_species, Species %in% target_species_revised$"FAO Code")
yearly_global_catches_subarea_target <- aggregate(yearly_global_catches_subarea_target$x, by=list(Year=yearly_global_catches_subarea_target$Year,SubArea=yearly_global_catches_subarea_target$SubArea), FUN=sum, na.rm = TRUE)
#subtract total target catch from total catch per subarea
names(yearly_global_catches_target)[names(yearly_global_catches_target) == "x"] <- "TargetCatch"
names(yearly_global_catches)[names(yearly_global_catches) == "x"] <- "TotalCatch"
catch_bycatch <- full_join(yearly_global_catches_target, yearly_global_catches)
catch_bycatch <- catch_bycatch%>% mutate(NonTargetCatch = TotalCatch-TargetCatch)
catch_bycatch$TotalCatch <- NULL
# load discards data
discards_species <- filter(fishing, catchFate == "0")
#aggregate discards data, by species and globally
yearly_global_catches_discards_species <- aggregate(discards_species$CatchTon, by=list(Year=discards_species$Year, Species=discards_species$SpeciesCode), FUN=sum, na.rm = TRUE)
yearly_global_catches_discards <- aggregate(discards_species$CatchTon, by=list(Year=discards_species$Year), FUN=sum, na.rm = TRUE)
#convert kgs to t
#names(yearly_global_catches_discards)[names(yearly_global_catches_discards) == "x"] <- "Discards"
#yearly_global_catches_discards <- yearly_global_catches_discards%>% mutate(Discards = Discards/1000)
#yearly_global_catches_discards_species <- yearly_global_catches_discards_species%>% mutate(x = x/1000)
#join aggregated data with bycatch data
#catch_bycatch <- pivot_wider(catch_bycatch, names_from = "Catch", values_from = "t")
catch_bycatch_discards <- full_join(catch_bycatch, yearly_global_catches_discards)
catch_bycatch_discards <- rename(catch_bycatch_discards, Discards = x)
#transform data for easier plotting
#catch_bycatch_discards <- catch_bycatch_discards %>% pivot_longer(!Year, names_to = "Catch", values_to = "t")
#catch_bycatch <- catch_bycatch %>% pivot_longer(!Year, names_to = "Catch", values_to = "t")
#catch_bycatch_subarea <- catch_bycatch_subarea %>% pivot_longer(cols = "TargetCatch":"NonTargetCatch", names_to = "Catch", values_to = "t")
#catch_bycatch_sharks <- catch_bycatch_sharks %>% pivot_longer(cols = "TargetCatch":"NonSharkBycatch", names_to = "Catch", values_to = "t")
#sharks vs other bycatch
yearly_global_catches_sharks <- filter(yearly_global_catches_species, Species %in% shark_species$Code)
yearly_global_catches_subarea_sharks <- filter(yearly_global_catches_subarea_species, Species %in% shark_species$Code)
yearly_global_catches_sharks_aggregated <- aggregate(yearly_global_catches_sharks$x, by=list(Year=yearly_global_catches_sharks$Year), FUN=sum, na.rm = TRUE)
names(yearly_global_catches_sharks_aggregated)[names(yearly_global_catches_sharks_aggregated) == "x"] <- "SharkBycatch"
catch_bycatch_sharks <- full_join(catch_bycatch, yearly_global_catches_sharks_aggregated)
catch_bycatch_sharks <- catch_bycatch_sharks%>% mutate(NonSharkBycatch = NonTargetCatch-SharkBycatch)
catch_bycatch_sharks$NonTargetCatch <- NULL
#subtract total target catch from total catch per subarea
names(yearly_global_catches_subarea_target)[names(yearly_global_catches_subarea_target) == "x"] <- "TargetCatch"
names(yearly_global_catches_subarea)[names(yearly_global_catches_subarea) == "x"] <- "TotalCatch"
catch_bycatch_subarea <- full_join(yearly_global_catches_subarea_target, yearly_global_catches_subarea)
catch_bycatch_subarea <- catch_bycatch_subarea%>% mutate(NonTargetCatch = TotalCatch-TargetCatch)
catch_bycatch_subarea$TotalCatch <- NULL
#transform data for easier plotting
catch_bycatch_discards <- catch_bycatch_discards %>% pivot_longer(!Year, names_to = "Catch", values_to = "t")
catch_bycatch <- catch_bycatch %>% pivot_longer(!Year, names_to = "Catch", values_to = "t")
catch_bycatch_subarea <- catch_bycatch_subarea %>% pivot_longer(cols = "TargetCatch":"NonTargetCatch", names_to = "Catch", values_to = "t")
catch_bycatch_sharks <- catch_bycatch_sharks %>% pivot_longer(cols = "TargetCatch":"NonSharkBycatch", names_to = "Catch", values_to = "t")

## plot tables of target and non-target catch, sharks highlighted 
catch_bycatch_sharks_table <- catch_bycatch_sharks  %>%
  pivot_wider(names_from = Catch, values_from = t) %>%
  mutate_at(vars(TargetCatch, SharkBycatch, NonSharkBycatch), funs(round(., 1))) %>%
  rename('Catch (t)' = TargetCatch,
         'Bycatch (t)' = NonSharkBycatch,
         'Shark catch (target/non-target, t)' = SharkBycatch) 

write_xlsx(catch_bycatch_sharks_table,"Ecosystem Summary/Tables/catch_bycatch_sharks_table.xlsx")

## plot histogram of target catch and bycatch per year 
#totals
ggplot(catch_bycatch, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly target/non-target catch in the SIOFA area (absolute)", x="Year", y="Total catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_hue(labels = c("Non-target Catch", "Target Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_nontargetcatch_web.png", width = 10, height = 4, dpi = 150)

#percentage
ggplot(catch_bycatch, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly target/non-target catch in the SIOFA area (relative)", x="Year", y="Total catch proportion") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_hue(labels = c("Non-target Catch", "Target Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_nontargetcatch_fill_web.png", width = 10, height = 4, dpi = 150)

## plot histogram of bycatch per year and subarea
#totals
bycatch_subarea <- catch_bycatch_subarea %>%
  filter(Catch == "NonTargetCatch")

ggplot(bycatch_subarea, aes(x = Year, y = t, fill=SubArea)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly non-target catch in SIOFA subareas (absolute)", x="Year", y="Non-target catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAnontargetcatch_subarea_web.png", width = 10, height = 4, dpi = 150)

#percentage
ggplot(bycatch_subarea, aes(x = Year, y = t, fill=SubArea)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly non-target catch in SIOFA subareas (relative)", x="Year", y="Non-target catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAnontargetcatch_subarea__fill_web.png", width = 10, height = 4, dpi = 150)

## plot histogram of shark bycatch and non-target catch per year 
#totals
ggplot(catch_bycatch_sharks, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly target/non-target catch in the SIOFA area (absolute), sharks highlighted", x="Year", y="Total catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_hue(labels = c("Non-target catch", "Shark target/non-target catch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_nontargetcatch_sharks_web.png", width = 10, height = 4, dpi = 150)

#percentage
ggplot(catch_bycatch_sharks, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly target/non-target catch in the SIOFA area (relative), sharks highlighted", x="Year", y="Total catch proportion") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_hue(labels = c("Bycatch", "Shark catch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_nontargetcatch_sharks_fill_web.png", width = 10, height = 4, dpi = 150)

## plot  target catch by subarea
target_catch_subarea <- catch_bycatch_subarea %>%
  filter(Catch == "TargetCatch")

ggplot(target_catch_subarea, aes(x = Year, y = t, fill=SubArea)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly target catch in SIOFA subareas", x="Year", y="Target catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAtargetcatch_subarea_web.png", width = 10, height = 4, dpi = 150)

## plot non target catch by subarea
ggplot(bycatch_subarea, aes(x = Year, y = t, fill = SubArea)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly non-target catch in SIOFA subareas", x="Year", y="Non-target catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAnontargetcatch_subarea_web.png", width = 10, height = 4, dpi = 150)

## plot histogram of bycatch and discards per year 
#totals
ggplot(catch_bycatch_discards, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly discarded catch as a fraction of non-target catch in the SIOFA area (absolute)", x="Year", y="Total non-target catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_hue(labels = c("Discarded catch", "Non-target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_nontargetcatch_discards_web.png", width = 10, height = 4, dpi = 150)

#percentage
ggplot(catch_bycatch_discards, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly discarded catch as a fraction of non-target catch in the SIOFA area (relative)", x="Year", y="Total non-target catch proportion") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  scale_fill_hue(labels = c("Discarded catch", "Non-target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_nontargetcatch_discards_fill_web.png", width = 10, height = 4, dpi = 150)

#discards by species
ggplot(data=yearly_global_catches_discards_species, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly discards by species in the SIOFA area", x="Year", y="Discards (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.6)

ggsave("Ecosystem Summary/SIOFAdiscards_species_web.png", width = 15, height = 6, dpi = 150) 

discards_species_table <- yearly_global_catches_discards_species %>%
  group_by(Species) %>%
  summarise('Total weight (t)' = sum(x)) %>%
  mutate_at(vars('Total weight (t)'), funs(round(., 1)))

write_xlsx(discards_species_table,"Ecosystem Summary/Tables/discards_species_table.xlsx")

#discards by species, top5 species + others aggregated
sort_discards <- aggregate(yearly_global_catches_discards_species$x, by=list(Species=yearly_global_catches_discards_species$Species), FUN=sum, na.rm = TRUE)

sort_discards <- arrange(sort_discards, desc(x)) 

top5_discards <- sort_discards %>% slice(1:5)
other_discards <- sort_discards %>% slice(6:133)

ggplot(data= subset(yearly_global_catches_discards_species, Species %in% top5_discards$Species), aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_global_catches_discards_species, Species %in% other_discards$Species), aes(x = Year, y = x, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly discards by species in the SIOFA area", x="Year", y="Discards (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.8)

ggsave("Ecosystem Summary/SIOFAdiscards_species_top5_web.png", width = 10, height = 6, dpi = 150) 

## plot table of target catch by subarea 
targetcatch_subarea_table <- catch_bycatch_subarea  %>%
  pivot_wider(names_from = Catch, values_from = t) %>%
  group_by(Year, SubArea) %>%
  dplyr::summarise_all(sum) %>%
  mutate_at(vars(TargetCatch, NonTargetCatch), funs(round(., 1))) %>%
  dplyr::select(-NonTargetCatch) %>%
  pivot_wider(names_from = SubArea, values_from = TargetCatch) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(Year, everything())

write_xlsx(targetcatch_subarea_table,"Ecosystem Summary/Tables/targetcatch_subarea_table.xlsx")

## plot table of non-target target catch by subarea 
nontargetcatch_subarea_table <- catch_bycatch_subarea  %>%
  pivot_wider(names_from = Catch, values_from = t) %>%
  group_by(Year, SubArea) %>%
  summarise_all(sum) %>%
  mutate_at(vars(TargetCatch, NonTargetCatch), funs(round(., 1))) %>%
  dplyr::select(-TargetCatch) %>%
  pivot_wider(names_from = SubArea, values_from = NonTargetCatch) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(Year, everything())

write_xlsx(nontargetcatch_subarea_table,"Ecosystem Summary/Tables/nontargetcatch_subarea_table.xlsx")

## subset for shark only data 
# !!!!! here I use a new list of all sharks ever reported in the database
yearly_global_catches_sharks <- filter(yearly_global_catches_species, Species %in% shark_species$Code)

yearly_global_catches_subarea_sharks <- filter(yearly_global_catches_subarea_species, Species %in% shark_species$Code)

## plot table of target catch by subarea 
shark_catch_subarea_table <- yearly_global_catches_subarea_sharks  %>%
  group_by(Year, SubArea) %>%
  dplyr::select(-Species) %>%
  summarise_all(sum) %>%
  mutate_at(vars(x), funs(round(., 1))) %>%
  pivot_wider(names_from = SubArea, values_from = x) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(Year, everything()) %>%
  replace(is.na(.), 0) 

write.xlsx(shark_catch_subarea_table,"Ecosystem Summary/Tables/shark_catch_subarea_table.xlsx", overwrite = TRUE)

# plotting the data for sharks
# all species
ggplot(yearly_global_catches_sharks, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly shark catch by species in the SIOFA area", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.8)

ggsave("Ecosystem Summary/SIOFAcatches_effort_SHA_web.png", width = 10, height = 6, dpi = 150)  

# first graph catch (bars) per species and year, top5 species, all others grouped
# second graph catch by year and subarea
sort_sharks <- aggregate(yearly_global_catches_sharks$x, by=list(Species=yearly_global_catches_sharks$Species), FUN=sum, na.rm = TRUE)

sort_sharks <- arrange(sort_sharks, desc(x)) 

top5_sharks <- sort_sharks %>% slice(1:5)
other_sharks <- sort_sharks %>% slice(6:58)

ggplot(data= subset(yearly_global_catches_sharks, Species %in% top5_sharks$Species), aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_global_catches_sharks, Species %in% other_sharks$Species), aes(x = Year, y = x, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly shark catch by species in the SIOFA area", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.8)

ggsave("Ecosystem Summary/SIOFAcatches_effort_SHA_top5_web.png", width = 10, height = 6, dpi = 150)  

global_catches_sharks_table <- yearly_global_catches_sharks %>%
  group_by(Species) %>%
  summarise('Total weight (t)' = sum(x)) %>%
  mutate_at(vars('Total weight (t)'), funs(round(., 1)))

write_xlsx(global_catches_sharks_table,"Ecosystem Summary/Tables/global_catches_sharks_table.xlsx")


ggplot(yearly_global_catches_subarea_sharks, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly shark catch by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatches_SHA_subarea_web.png", width = 10, height = 4, dpi = 150)



## subset for CMM 12 shark only data 
# !!!!! here I use the Annex 1 of CMM 12 from both 2019 and 2023 as a list of sharks
yearly_global_catches_sharks_CMM <- yearly_global_catches_species %>%
  filter(Species %in% shark_species_CMM$FAOcode)

yearly_global_catches_subarea_sharks_CMM <- filter(yearly_global_catches_subarea_species, Species %in% shark_species_CMM$FAOcode)

# plotting the data for sharks
# first graph catch (bars) per species and year
# second graph catch by year and subarea
ggplot(data=yearly_global_catches_sharks_CMM, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  geom_vline(xintercept=c(2019.5,2021.5, 2022.5), linetype="dotted") +
  geom_text(x=2019.5, y=1750, label="CMM 2019/12") +
  geom_text(x=2021.5, y=1500, label="CMM 2022/12") +
  geom_text(x=2022.5, y=1750, label="CMM 12(2023)") +
  labs(title="Yearly catch by shark species in CMM 12 Annex 1 in the SIOFA area", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Ecosystem Summary/SIOFAcatches_effort_DWS_web.png", width = 10, height = 6, dpi = 150)  

ggplot(yearly_global_catches_subarea_sharks_CMM, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly catch by SIOFA subarea of shark species in CMM 12 Annex 1", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  geom_vline(xintercept=c(2019.5,2021.5, 2022.5), linetype="dotted") +
  geom_text(x=2019.5, y=1750, label="CMM 2019/12") +
  geom_text(x=2021.5, y=1500, label="CMM 2022/12") +
  geom_text(x=2022.5, y=1750, label="CMM 12(2023)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatches_DWS_subarea_web.png", width = 10, height = 4, dpi = 150)

# shark catches by gear
yearly_global_catches_species_gear <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, Gear=fishing$Gear), FUN=sum, na.rm = TRUE)
yearly_global_catches_sharks_gear <- filter(yearly_global_catches_species_gear, Species %in% shark_species$Code)

ggplot(data=yearly_global_catches_sharks_gear, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Gear), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly shark catch in the SIOFA area, by gear", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.8)

#colorblind friendly version
ggplot(data=yearly_global_catches_sharks_gear, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Gear), position="stack", stat="identity") +
  scale_fill_brewer(type = "seq", palette = "Paired") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly shark catch in the SIOFA area, by gear", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.8)

ggsave("Ecosystem Summary/SIOFAcatches_effort_SHA_gear_web.png", width = 10, height = 6, dpi = 150) 

# !!!!! here I use the Annex 1 of CMM 12 from 2019 as a list of sharks

yearly_global_catches_sharks_CMM_2019 <- yearly_global_catches_species %>%
  filter(Species %in% shark_species_CMM_2019$FAOcode)
yearly_global_catches_subarea_sharks_CMM_2019 <- filter(yearly_global_catches_subarea_species, Species %in% shark_species_CMM_2019$FAOcode)
ggplot(data=yearly_global_catches_sharks_CMM_2019, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly catch by shark species in CMM 2019/12 Annex 1 in the SIOFA area", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)
ggsave("Ecosystem Summary/SIOFAcatches_effort_DWS_2019_web.png", width = 10, height = 6, dpi = 150) 

# !!!!! here I use the Annex 1 of CMM 12 from 2024 as a list of sharks

yearly_global_catches_sharks_CMM_2024 <- yearly_global_catches_species %>%
  filter(Species %in% shark_species_CMM_2024$FAOcode)
yearly_global_catches_subarea_sharks_CMM_2024 <- filter(yearly_global_catches_subarea_species, Species %in% shark_species_CMM_2024$FAOcode)
ggplot(data=yearly_global_catches_sharks_CMM_2024, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly catch by shark species in CMM 12(2023) Annex 1 in the SIOFA area", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)
ggsave("Ecosystem Summary/SIOFAcatches_effort_DWS_2023_web.png", width = 10, height = 6, dpi = 150)  

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
captures_spatial_plot <- captures_spatial  %>%
  filter(!(speciesGroup=='Chimaeras' |  speciesGroup=='Deepwater sharks' |  speciesGroup=='Sharks and rays nei' ) & species3ACode %in% shark_species_CMM$FAOcode
         | (speciesGroup=='Seabirds'| speciesGroup=='Cetaceans'| speciesGroup=='Turtles')) %>%
  filter(source=='OBS')

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = captures_spatial_plot, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA incidental captures of seabirds, marine mammals, turtles and 
          sharks considered to be at high risk and/or of concern (2014-2023)") +
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
  geom_sf(data = captures_spatial_plot, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA incidental captures of seabirds, marine mammals, turtles and 
          sharks considered to be at high risk and/or of concern (2014-2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  scale_colour_discrete("Captures") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("Ecosystem summary/SIOFAmap_accidental_captures_web.png", width = 10, height = 8, dpi = 150)

# aggregate captures in a table that can be used in reports

# turtles
captures_turtles <- filter(captures,(speciesGroup == 'Turtles'))

#captures_turtles_aggregate1 <- captures_turtles %>%
#  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
#  summarise(n = n()) 
captures_turtles_aggregate <- captures_turtles %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  dplyr::select(Year, speciesEnglishName, speciesScientificName, Gear,
                foibcNbCaught, foibcNbReleasedAlive ) %>%
  mutate(foibcNbReleasedAlive = case_when(
    foibcNbReleasedAlive > 0 ~ "Alive",
    is.na(foibcNbReleasedAlive) ~ "Unknown",)) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Captures" = foibcNbCaught) %>%
  rename("Status at release" = foibcNbReleasedAlive) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(captures_turtles_aggregate,"Ecosystem summary/Tables/captures_turtles.xlsx")

# as flextable
FT.captures_turtles_aggregate <- captures_turtles_aggregate %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  fit_to_width(max_width = 18, unit="cm") %>%
  flextable::align(j = 1:4, align = "left", part = "all") %>%
  flextable::align(j = 5, align = "center", part = "all") %>%
  italic(j = 3, italic = TRUE, part = "body") %>%
  add_header_lines("Captures of marine turtles in SIOFA fisheries (2004-2023)") 

save_as_image(FT.captures_turtles_aggregate, path = "Ecosystem Summary/Tables/captures_turtles.png")               

# marine mammals
captures_mammals <- filter(captures,(speciesGroup == 'Cetaceans'))

captures_mammals_aggregate <- captures_mammals %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  dplyr::select(Year, speciesEnglishName, speciesScientificName, Gear,
                foibcNbCaught, foibcNbReleasedAlive ) %>%
  mutate(foibcNbReleasedAlive = case_when(
    foibcNbReleasedAlive == 1 ~ "Alive")) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Captures" = foibcNbCaught) %>%
  rename("Status at release" = foibcNbReleasedAlive) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(captures_mammals_aggregate,"Ecosystem summary/Tables/captures_mammals.xlsx")

# as flextable
FT.captures_mammals_aggregate <- captures_mammals_aggregate %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  fit_to_width(max_width = 18, unit="cm") %>%
  flextable::align(j = 1:4, align = "left", part = "all") %>%
  flextable::align(j = 5, align = "center", part = "all") %>%
  italic(j = 3, italic = TRUE, part = "body") %>%
  add_header_lines("Captures of marine mammals in SIOFA fisheries (2004-2023)") 

save_as_image(FT.captures_mammals_aggregate, path = "Ecosystem Summary/Tables/captures_mammals.png")               

# seabirds
captures_seabirds <- filter(captures,(speciesGroup == 'Seabirds'))

captures_seabirds_aggregate <- captures_seabirds %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  dplyr::select(Year, speciesEnglishName, speciesScientificName, Gear,
                foibcNbCaught) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Captures" = foibcNbCaught) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(captures_seabirds_aggregate,"Ecosystem summary/Tables/captures_seabirds.xlsx")

# as flextable
FT.captures_seabirds_aggregate <- captures_seabirds_aggregate %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  fit_to_width(max_width = 18, unit="cm") %>%
  flextable::align(j = 1:4, align = "left", part = "all") %>%
  flextable::align(j = 5, align = "center", part = "all") %>%
  italic(j = 3, italic = TRUE, part = "body") %>%
  add_header_lines("Captures of seabirds in SIOFA fisheries (2004-2023)") 

save_as_image(FT.captures_seabirds_aggregate, path = "Ecosystem Summary/Tables/captures_seabirds.png")               

# protected sharks
captures_sharks <- captures %>%
  filter(!(speciesGroup=='Chimaeras' |  speciesGroup=='Deepwater sharks' |  speciesGroup=='Sharks and rays nei' ) & species3ACode %in% shark_species_CMM_2024$FAOcode)
   
captures_sharks_aggregate <- captures_sharks %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  dplyr::select(Year, speciesEnglishName, speciesScientificName, Gear,
                foibcNbCaught) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) %>%
  rename("Captures (n)" = foibcNbCaught)

write_xlsx(captures_sharks_aggregate,"Ecosystem summary/Tables/captures_sharks.xlsx")

# as flextable
FT.captures_sharks_aggregate <- captures_sharks_aggregate %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  fit_to_width(max_width = 18, unit="cm") %>%
  flextable::align(j = 1:4, align = "left", part = "all") %>%
  flextable::align(j = 5, align = "center", part = "all") %>%
  italic(j = 3, italic = TRUE, part = "body") %>%
  add_header_lines("Captures of sharks 'at risk' or 'of concern' sharks in SIOFA fisheries (2004-2023)") 

save_as_image(FT.captures_sharks_aggregate, path = "Ecosystem Summary/Tables/captures_sharks.png")               

### Analyze observations of seabirds and mammals
# Convert the observation dataframe to a spatial object. Note that the
# crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object
# and that missing values are removed
observations_spatial <- filter(observations, (!is.na(observations$Longitude)))
observations_spatial <- st_as_sf(observations_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 
class(observations_spatial)
st_crs(observations_spatial) <- 4326

observations_spatial_plot <- observations_spatial  %>%
  filter(!(speciesGroup=='NO BIRDS OBSERVED' |  speciesGroup=='NO OBSERVER' |  speciesGroup=='NO MAMMALS OBSERVED')) %>%
  filter(source=='OBS') 

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = observations_spatial_plot, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA observations of seabirds and marine mammals
            around fishing operations (2014-2023)") +
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
  geom_sf(data = observations_spatial_plot, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA observations of seabirds and marine mammals
            in fishing operations (2014-2023)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  scale_colour_discrete("Observations") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)

ggsave("Ecosystem summary/SIOFAmap_observations_web.png", width = 10, height = 8, dpi = 150)

## analyze seabirds observations
observations_seabirds_aggregate <- observations %>%
  group_by(Year, speciesEnglishName, speciesScientificName, Gear, speciesGroup) %>%
  filter(!speciesGroup=='MARINE MAMMALS') %>%
  filter(!speciesGroup=='NO BIRDS OBSERVED') %>%
  filter(!speciesGroup=='NO MAMMALS OBSERVED') %>%
  filter(!speciesGroup=='NO OBSERVER') %>%
  summarise(Abundance = sum(Abundance)) %>%
  dplyr::select(-speciesGroup) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  mutate(speciesScientificName = replace_na(speciesScientificName, "-")) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(observations_seabirds_aggregate,"Ecosystem summary/Tables/observations_seabirds.xlsx")

## analyze negative seabirds observations
negative_observations_seabirds_aggregate <- observations %>%
  filter(speciesGroup=='NO BIRDS OBSERVED') %>%
  group_by(Year, Gear) %>%  
  summarise(n = n()) %>%
  rename("Fishing gear" = Gear) %>%
  rename(FishEventsNoSeabirdsDetected = n)

positive_observations_seabirds_aggregate <- observations %>%
  filter(speciesGroup=='SEABIRDS') %>%
  filter(Year>=2020) %>%
  group_by(Year, Gear) %>%  
  summarise(n = n()) %>%
  rename("Fishing gear" = Gear) %>%
  rename(FishEventsSeabirdsDetected = n)

negative_observations_seabirds_table <- merge(negative_observations_seabirds_aggregate, positive_observations_seabirds_aggregate, all=TRUE)

negative_observations_seabirds_table <- negative_observations_seabirds_table %>%
  mutate(across(starts_with('FishEvents'), ~replace_na(., 0))) %>%
  mutate(Share= FishEventsNoSeabirdsDetected/(FishEventsSeabirdsDetected+FishEventsNoSeabirdsDetected)*100) %>%
  mutate_at(vars(Share), funs(round(., 1))) %>%
  rename("Fishing events observed with no seabirds reported" = FishEventsNoSeabirdsDetected) %>%
  rename("Fishing events observed with seabirds reported" = FishEventsSeabirdsDetected) %>%
  rename("Share of events observed with no seabirds reported (%)" = Share)

write_xlsx(negative_observations_seabirds_table,"Ecosystem summary/Tables/negative_observations_seabirds.xlsx")

# as flextable
FT.negative_observations_seabirds_table <- negative_observations_seabirds_table %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  fit_to_width(max_width = 18, unit="cm") %>%
  flextable::align(j = 1:2, align = "left", part = "all") %>%
  flextable::align(j = 3:5, align = "center", part = "all") %>%
  add_header_lines("Observations of seabirds in SIOFA fisheries (2004-2023)") 

save_as_image(FT.negative_observations_seabirds_table, path = "Ecosystem Summary/Tables/negative_seabirds_observations.png")               

## analyze non-performed seabirds observations
no_observations_seabirds_aggregate <- observations %>%
  filter(speciesGroup=='NO OBSERVER') %>%
  group_by(Year, Gear) %>%  
  summarise(n = n()) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Fishing events not observed for seabirds presence" = n)

write_xlsx(no_observations_seabirds_aggregate,"Ecosystem summary/Tables/no_observations_seabirds.xlsx")

# as flextable
FT.no_observations_seabirds_aggregate <- no_observations_seabirds_aggregate %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  fit_to_width(max_width = 18, unit="cm") %>%
  flextable::align(j = 1:2, align = "left", part = "all") %>%
  flextable::align(j = 3, align = "center", part = "all") %>%
  add_header_lines("Fishing events not observerd for seabirds in SIOFA fisheries (2004-2023)") 

save_as_image(FT.no_observations_seabirds_aggregate, path = "Ecosystem Summary/Tables/no_observations_seabirds.png")               

## analyze marine mammals observations during fishing events
observations_mammals_aggregate <- observations %>%
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
  
write_xlsx(observations_mammals_aggregate,"Ecosystem summary/Tables/observations_mammals.xlsx")

# as flextable

FT.observations_mammals_aggregate <- observations_mammals_aggregate %>%
  flextable() %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  fit_to_width(max_width = 18, unit="cm") %>%
  flextable::align(j = 1:4, align = "left", part = "all") %>%
  flextable::align(j = 5, align = "center", part = "all") %>%
  italic(j = 3, italic = TRUE, part = "body") %>%
  add_header_lines("Observations of marine mammals in SIOFA fisheries (2004-2023)") 

save_as_image(FT.observations_mammals_aggregate, path = "Ecosystem Summary/Tables/observations_mammals.png")               

## analysis of observer reported catches
# need to assign subarea to reported locations
# Convert the dataframe to a spatial object. Note that the
# crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object
# and that missing values are removed
observed_catches_spatial <- filter(observed_catches, (!is.na(observed_catches$Longitude)))
observed_catches_spatial <- filter(observed_catches_spatial, (!is.na(observed_catches_spatial$Latitude)))
observed_catches_spatial <- st_as_sf(observed_catches_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 
st_crs(observed_catches_spatial) <- 4326
# Overlay points and extract just the subarea column 
observed_catches_spatial <- st_join(observed_catches_spatial, left = FALSE, fishing_boundaries["SubAreaNo"])
#summarize data for plotting
observer_reported_catch <- observed_catches_spatial %>%
  group_by(Year, FAOCode, CommonName, ScientificName, Gear, SpGroup, SubAreaNo) %>%
  filter(FAOCode %in% shark_species$Code) %>%
  summarize(Catch = sum(catchWeight, na.rm=TRUE)) 
#sort for top 5 species
sort_sharks2 <- aggregate(observer_reported_catch$Catch, by=list(FAOCode=observer_reported_catch$FAOCode), FUN=sum, na.rm = TRUE)
sort_sharks2 <- arrange(sort_sharks2, desc(x))  
top5_sharks2 <- sort_sharks2 %>% slice(1:5)
other_sharks2 <- sort_sharks2 %>% slice(6:213)
#plot
ggplot(subset(observer_reported_catch, FAOCode %in% top5_sharks2$FAOCode), aes(x = Year, y = Catch)) +
  geom_bar(aes(fill=FAOCode), position="stack", stat="identity") +
  geom_bar(data= subset(observer_reported_catch, FAOCode %in% other_sharks2$FAOCode), aes(x = Year, y = Catch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly observer-reported catch of sharks, by species", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

#colorblind friendly version
ggplot(subset(observer_reported_catch, FAOCode %in% top5_sharks2$FAOCode), aes(x = Year, y = Catch)) +
  geom_bar(aes(fill=FAOCode), position="stack", stat="identity") +
  geom_bar(data= subset(observer_reported_catch, FAOCode %in% other_sharks2$FAOCode), aes(x = Year, y = Catch, fill="'Other species'"), position="stack", stat="identity") +
  scale_fill_oi(
    palette = "full",
    reverse = FALSE,
    order = 8:1) +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  labs(title="Yearly observer-reported catch of sharks, by species", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Ecosystem Summary/SIOFA_observer_shark_catches.png", width = 10, height = 6, dpi = 150)  

ggplot(observer_reported_catch, aes(x = Year, y = Catch)) +
  geom_bar(aes(fill=SubAreaNo), position="stack", stat="identity") +
  labs(title="Yearly observer-reported catch of sharks by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

#colorblind friendly version
ggplot(observer_reported_catch, aes(x = Year, y = Catch)) +
  geom_bar(aes(fill=SubAreaNo), position="stack", stat="identity") +
  labs(title="Yearly observer-reported catch of sharks by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_oi() +
  theme_bw() +
  scale_x_continuous(limits=c(2013, 2024),breaks = seq(2013, 2024, by = 5)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFA_observer_shark_catches_subarea.png", width = 10, height = 4, dpi = 150)

## observer-reported application of seabird bycatch mitigation measures

observed_mitigation <- observer_mitigation %>%
  filter(Year>=2020) %>%
  filter(Year<=2022) %>%
  dplyr::select(CCP:trawlBirdBafflersUsed) %>%
  #filter(!grepl("trawl", Gear)) %>%
  group_by(Year, Gear) %>%
  summarise(StreamerDeployed= sum(StreamerLineVesselEquipped=='Y',na.rm=T), 
            StreamerNotDeployed= sum(StreamerLineVesselEquipped=='N',na.rm=T), 
            Total = n())

write_xlsx(observed_mitigation,"Ecosystem summary/Tables/observed_mitigation.xlsx")

