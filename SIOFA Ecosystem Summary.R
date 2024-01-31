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

# load main fishing data
setwd("D:/SIOFA/Data")   #### !!!check that this points to the parent folder!!!
fishing <- read_excel("Catch-effort-2023.xlsx", col_types = c("numeric", "text", "text", "numeric", "numeric", "numeric","numeric","numeric","text","text","text","text","numeric","numeric","numeric","text","text","text","text","numeric","numeric", "numeric","numeric","text"))
captures <- read_excel("Accidental-captures-2023.xlsx")
observations <- read_excel("Bird-observations-2023.xlsx")
observed_catches <- read_excel("Observer-reported-catch-2023.xlsx")
fishing_boundaries <- st_read('siofa_subareas_edited')

# Convert the fishing dataframe to a spatial object. Note that the
# crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object
# and that missing values are removed
fishing_spatial <- filter(fishing, (!is.na(fishing$Longitude)))
fishing_spatial <- filter(fishing, (!is.na(fishing$Latitude)))
fishing_spatial <- st_as_sf(fishing_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 
class(fishing_spatial)
st_crs(fishing_boundaries) <- 4326

# drop 2023 data (still incomplete)
fishing <- fishing %>%
  filter(Year<=2022)

# load a number of base layers
land <- st_read('natural_earth/land')
ocean <- st_read('natural_earth/ocean')
bathy <- st_read('natural_earth/bathy')
grid <- st_read('natural_earth/grid/ne_50m_graticules_10.shp')
islands <- st_read('natural_earth/islands')

## analyze observer coverage
observer_coverage <- read_excel("Observers-coverage-2023.xlsx", col_types = c("numeric", "numeric", "text", "text", "numeric", "numeric","numeric", "numeric", "text", "text", "numeric","numeric","numeric","numeric","numeric","text"))

observer_coverage <- observer_coverage %>%
  filter(Year<=2022)

observer_coverage_table <- observer_coverage %>%
  group_by(Year, Gear) %>%
  summarise(Observed= sum(foObserved=='Y',na.rm=T), Total = n()) %>%
  mutate(OBSratio = Observed/Total) %>%
  mutate_at(vars(OBSratio), funs(round(., 3))) %>%
  dplyr::select(-Observed) %>%
  rename('Observer ratio'= OBSratio, 'Total events' = Total) %>%
  pivot_wider(names_from = Gear, values_from = c('Total events', 'Observer ratio'),  names_sep = ' in ' ) 
  
write_xlsx(observer_coverage_table,"Ecosystem Summary/Tables/observer_coverage.xlsx")

## Fishing in BPAs
# intersect fishing events with BPAs
# Overlay points and extract just the subarea column 
## read in the protected areas
BPAs <- st_read('BPAs')
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
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_BPAs_web.png", width = 10, height = 5, dpi = 150)

ggplot(yearly_global_catches_BPAs, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  labs(title="Yearly catch in SIOFA IPAs by species", x="Year", y="Catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_BPAs_full_web.png", width = 10, height = 5, dpi = 150)

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
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAgear_BPAs_web.png", width = 10, height = 4, dpi = 150)


## analyze VME bycatch data
#read data
VME_bycatch <- read_excel("SIOFA-VME_bycatch-2023.xlsx")
#groom data for duplicates
VME_bycatch_groomed <- VME_bycatch %>% distinct(Longitude, Latitude, FAOcode, speciesScientificName, Weight, .keep_all = TRUE)

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
  scale_x_continuous(limits=c(2002, 2023)) +
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
  scale_x_continuous(limits=c(2002, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_VMEs_web.png", width = 10, height = 4, dpi = 150)

# full figure
ggplot(VME_bycatch_year, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Group), position="stack", stat="identity") +
  theme_bw() +
  labs(title="Yearly VME taxa catch in SIOFA fisheries by species group", x="Year", y="VME catch weight (kg)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_VMEs_cropped_web.png", width = 10, height = 4, dpi = 150)


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
  ggtitle("SIOFA VME incidental catches (2013-2022)") +
  scale_color_discrete(name = "Taxonomic group") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("Ecosystem Summary/SIOFAmap_VMEa_web.png", width = 10, height = 7, dpi = 150)

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
target_species <- read_excel("targets.xlsx")
yearly_global_catches_target <- filter(yearly_global_catches_species, Species %in% target_species$"FAO Code")
yearly_global_catches_target <- aggregate(yearly_global_catches_target$x, by=list(Year=yearly_global_catches_target$Year), FUN=sum, na.rm = TRUE)
#calculate total target catch per year and subarea
yearly_global_catches_subarea_target <- filter(yearly_global_catches_subarea_species, Species %in% target_species$"FAO Code")
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
shark_species <- read_excel("sharks.xlsx")
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
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Non-target Catch", "Target Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_nontargetcatch_web.png", width = 10, height = 4, dpi = 150)

#percentage
ggplot(catch_bycatch, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly target/non-target catch in the SIOFA area (relative)", x="Year", y="Total catch (%)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
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
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAnontargetcatch_subarea_web.png", width = 10, height = 4, dpi = 150)

#percentage
ggplot(bycatch_subarea, aes(x = Year, y = t, fill=SubArea)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly non-target catch in SIOFA subareas (relative)", x="Year", y="Non-target catch (%)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAnontargetcatch_subarea__fill_web.png", width = 10, height = 4, dpi = 150)

## plot histogram of shark bycatch and non-target catch per year 
#totals
ggplot(catch_bycatch_sharks, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly target/non-target catch in the SIOFA area (absolute), sharks highlighted", x="Year", y="Total catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Non-target catch", "Shark target/non-target catch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_nontargetcatch_sharks_web.png", width = 10, height = 4, dpi = 150)

#percentage
ggplot(catch_bycatch_sharks, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly target/non-target catch in the SIOFA area (relative), sharks highlighted", x="Year", y="Total catch (%)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Non-target catch", "Shark target/non-target catch", "Target catch")) +
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
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAtargetcatch_subarea_web.png", width = 10, height = 4, dpi = 150)

## plot non target catch by subarea
ggplot(bycatch_subarea, aes(x = Year, y = t, fill = SubArea)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly non-target catch in SIOFA subareas", x="Year", y="Non-target catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAnontargetcatch_subarea_web.png", width = 10, height = 4, dpi = 150)

## plot histogram of bycatch and discards per year 
#totals
ggplot(catch_bycatch_discards, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly discarded catch as a fraction of non-target catch in the SIOFA area (absolute)", x="Year", y="Total non-target catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Discarded catch", "Non-target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_nontargetcatch_discards_web.png", width = 10, height = 4, dpi = 150)

#percentage
ggplot(catch_bycatch_discards, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly discarded catch as a fraction of non-target catch in the SIOFA area (relative)", x="Year", y="Total non-target catch (%)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Discarded catch", "Non-target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatch_nontargetcatch_discards_fill_web.png", width = 10, height = 4, dpi = 150)

#discards by species
ggplot(data=yearly_global_catches_discards_species, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly discards by species in the SIOFA area", x="Year", y="Discards (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.6)

ggsave("Ecosystem Summary/SIOFAdiscards_species_web.png", width = 15, height = 6, dpi = 150) 

#discards by species, top5 species + others aggregated
sort_discards <- aggregate(yearly_global_catches_discards_species$x, by=list(Species=yearly_global_catches_discards_species$Species), FUN=sum, na.rm = TRUE)

sort_discards <- arrange(sort_discards, desc(x)) 

top5_discards <- sort_discards %>% slice(1:5)
other_discards <- sort_discards %>% slice(6:133)

ggplot(data= subset(yearly_global_catches_discards_species, Species %in% top5_discards$Species), aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_global_catches_discards_species, Species %in% other_discards$Species), aes(x = Year, y = x, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
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
shark_species <- read_excel("sharks.xlsx")

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
  scale_x_continuous(limits=c(2012, 2023)) +
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
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly shark catch by species in the SIOFA area", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.8)

ggsave("Ecosystem Summary/SIOFAcatches_effort_SHA_top5_web.png", width = 10, height = 6, dpi = 150)  

ggplot(yearly_global_catches_subarea_sharks, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly shark catch by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFAcatches_SHA_subarea_web.png", width = 10, height = 4, dpi = 150)

## subset for CMM 12 shark only data 
# !!!!! here I use the Annex 1 of CMM 12 from both 2019 and 2023 as a list of sharks
shark_species_CMM <- read_excel("protected_sharks.xlsx")

yearly_global_catches_sharks_CMM <- yearly_global_catches_species %>%
  filter(Species %in% shark_species_CMM$FAOcode)

yearly_global_catches_subarea_sharks_CMM <- filter(yearly_global_catches_subarea_species, Species %in% shark_species_CMM$FAOcode)

# plotting the data for sharks
# first graph catch (bars) per species and year
# second graph catch by year and subarea
ggplot(data=yearly_global_catches_sharks_CMM, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
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
  scale_x_continuous(limits=c(2012, 2023)) +
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
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly shark catch in the SIOFA area, by gear", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.8)

ggsave("Ecosystem Summary/SIOFAcatches_effort_SHA_gear_web.png", width = 10, height = 6, dpi = 150) 

# !!!!! here I use the Annex 1 of CMM 12 from 2019 as a list of sharks
shark_species_CMM_2019 <- read_excel("protected_sharks_2019.xlsx")
yearly_global_catches_sharks_CMM_2019 <- yearly_global_catches_species %>%
  filter(Species %in% shark_species_CMM_2019$FAOcode)
yearly_global_catches_subarea_sharks_CMM_2019 <- filter(yearly_global_catches_subarea_species, Species %in% shark_species_CMM_2019$FAOcode)
ggplot(data=yearly_global_catches_sharks_CMM_2019, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly catch by shark species in CMM 2019/12 Annex 1 in the SIOFA area", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)
ggsave("Ecosystem Summary/SIOFAcatches_effort_DWS_2019_web.png", width = 10, height = 6, dpi = 150) 
# !!!!! here I use the Annex 1 of CMM 12 from 2023 as a list of sharks
shark_species_CMM_2023 <- read_excel("protected_sharks_2023.xlsx")
yearly_global_catches_sharks_CMM_2023 <- yearly_global_catches_species %>%
  filter(Species %in% shark_species_CMM_2023$FAOcode)
yearly_global_catches_subarea_sharks_CMM_2023 <- filter(yearly_global_catches_subarea_species, Species %in% shark_species_CMM_2023$FAOcode)
ggplot(data=yearly_global_catches_sharks_CMM_2023, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
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
          sharks considered to be at high risk and/or of concern (2012-2022)") +
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
  relocate(speciesEnglishName, .after = Year) %>%
  relocate(speciesScientificName, .after = speciesEnglishName) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Captures" = foibcNbCaught) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(captures_turtles_aggregate,"Ecosystem summary/Tables/captures_turtles.xlsx")

# marine mammals
captures_mammals <- filter(captures,(speciesGroup == 'marine mammals'))

captures_mammals_aggregate <- captures_mammals %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) 

write_xlsx(captures_mammals_aggregate,"Ecosystem summary/Tables/captures_mammals.xlsx")

# seabirds
captures_seabirds <- filter(captures,(speciesGroup == 'Seabirds'))

captures_seabirds_aggregate <- captures_seabirds %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) 

write_xlsx(captures_seabirds_aggregate,"Ecosystem summary/Tables/captures_seabirds.xlsx")

# sharks
captures_sharks <- captures %>%
  filter(!(speciesGroup=='Chimaeras' |  speciesGroup=='Deepwater sharks' |  speciesGroup=='Sharks and rays nei' ) & species3ACode %in% shark_species_CMM$FAOcode)
   
captures_sharks_aggregate <- captures_sharks %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) %>%
  rename("Captures" = foibcNbCaught)

write_xlsx(captures_sharks_aggregate,"Ecosystem summary/Tables/captures_sharks.xlsx")

### Analyze observations of seabirds and mammals
# Convert the observation dataframe to a spatial object. Note that the
# crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object
# and that missing values are removed
observations_spatial <- filter(observations, (!is.na(observations$Longitude)))
observations_spatial <- st_as_sf(observations_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 
class(observations_spatial)
st_crs(observations_spatial) <- 4326

observations_spatial_plot <- observations_spatial  %>%
  filter(!(speciesGroup=='NO BIRDS OBSERVED' |  speciesGroup=='NO OBSERVER')) %>%
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
            in fishing operations (2012-2022)") +
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
  filter(!speciesGroup=='MAMMALS') %>%
  filter(!speciesGroup=='NO BIRDS OBSERVED') %>%
  filter(!speciesGroup=='NO OBSERVER') %>%
  summarise(Abundance = sum(Abundance)) %>%
  dplyr::select(-speciesGroup) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
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

## analyze non-performed seabirds observations
no_observations_seabirds_aggregate <- observations %>%
  filter(speciesGroup=='NO OBSERVER') %>%
  group_by(Year, Gear) %>%  
  summarise(n = n()) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Fishing events not observed for seabirds presence" = n)

write_xlsx(no_observations_seabirds_aggregate,"Ecosystem summary/Tables/no_observations_seabirds.xlsx")

## analyze marine mammals observations during fishing events
observations_mammals_aggregate <- observations %>%
  group_by(Year, speciesEnglishName, speciesScientificName, Gear, speciesGroup) %>%
  filter(speciesGroup=='MAMMALS') %>%
  filter(!speciesGroup=='NO BIRDS OBSERVED') %>%
  filter(!speciesGroup=='NO OBSERVER') %>%
  filter(!speciesGroup=='SEABIRDS') %>%
  summarise(Abundance = sum(Abundance)) %>%
  dplyr::select(-speciesGroup) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) 
  
write_xlsx(observations_mammals_aggregate,"Ecosystem summary/Tables/observations_mammals.xlsx")

## analyze observer coverage
observer_coverage <- read_excel("Observers-coverage-2023.xlsx", col_types = c("numeric", "numeric", "text", "text", "numeric", "numeric","numeric", "numeric", "text", "text", "numeric","numeric","numeric","numeric","numeric","text"))

observer_coverage_table <- observer_coverage %>%
  group_by(Gear, Year) %>%
  summarise(observed= sum(foObserved=='Y',na.rm=T), total = n()) %>%
  mutate(OBSratio = observed/total*100) %>%
  mutate_at(vars(OBSratio), funs(round(., 2))) %>%
  #dplyr::select(-observed, - total) %>%
  rename("Fishing gear" = Gear) %>%
  rename("% observed" = OBSratio) %>%
  rename("Observed events" = observed) %>%
  rename("Total events" = total) %>%
  dplyr::select(Year, "Fishing gear", "Total events", "Observed events", "% observed")
  
write_xlsx(observer_coverage_table,"Ecosystem summary/Tables/observer_coverage.xlsx")

## analysis of observer reported catches
# need to assign subarea to reported locations
# Convert the dataframe to a spatial object. Note that the
# crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object
# and that missing values are removed
observed_catches_spatial <- filter(observed_catches, (!is.na(observed_catches$Longitude)))
observed_catches_spatial <- filter(observed_catches, (!is.na(observed_catches$Latitude)))
observed_catches_spatial <- st_as_sf(observed_catches_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 
st_crs(observed_catches_spatial) <- 4326
# Overlay points and extract just the subarea column 
observed_catches_spatial <- st_join(observed_catches_spatial, left = FALSE, fishing_boundaries["SubAreaNo"])
#summarize data for plotting
observer_reported_catch <- observed_catches_spatial %>%
  group_by(Year, FAOCode, CommonName, ScientificName, Gear, SpGroup, SubAreaNo) %>%
  filter(FAOCode %in% shark_species$Code) %>%
  summarize(Catch = sum(catchWeight)) 
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
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly observer-reported catch of sharks, by species", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Ecosystem Summary/SIOFA_observer_shark_catches.png", width = 10, height = 6, dpi = 150)  

ggplot(observer_reported_catch, aes(x = Year, y = Catch)) +
  geom_bar(aes(fill=SubAreaNo), position="stack", stat="identity") +
  labs(title="Yearly observer-reported catch of sharks by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Ecosystem Summary/SIOFA_observer_shark_catches_subarea.png", width = 10, height = 4, dpi = 150)

## observer-reported application of seabird bycatch mitigation measures
observer_mitigation <- read_excel("Bird-bycatch-mitigation-measures-data.xlsx")

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

