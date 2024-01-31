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

# drop 2023 data, if any, as it is still incomplete
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

#subset for common mora fishing activity only in order to map it
# subset the whole data for all species, need catch, effort and subarea aggregations
yearly_global_catches_species <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, Longitude=fishing$Longitude, Latitude=fishing$Latitude), FUN=sum, na.rm = TRUE)
yearly_longline_effort_species <- aggregate(fishing$NbHooks, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, ActivityID=fishing$ActivityID, Longitude=fishing$Longitude, Latitude=fishing$Latitude), FUN=sum, na.rm = TRUE)
yearly_global_catches_subarea_species <- aggregate(fishing_within$CatchTon, by=list(Year=fishing_within$Year,SubArea=fishing_within$SubAreaNo, Species=fishing_within$SpeciesCode), FUN=sum, na.rm = TRUE)

# it's not possible to calculate gillnet or dropline effort (e.g. km nets/soaking time) 
# as no data is reported for either gear type
# as a consequence we might have to split catches between gears with/without reports

# subset for common mora only data 
# !!!!! double check how the longline effort is calculated when missing data
yearly_global_catches_mora <- filter(yearly_global_catches_species, (Species == "RIB"))
yearly_global_catches_subarea_mora <- filter(yearly_global_catches_subarea_species, (Species == "RIB"))
yearly_longline_effort_mora_activity <- filter(yearly_longline_effort_species, (Species == "RIB"))

# toggle off to disaggregate effort per subarea
yearly_longline_effort_mora <- aggregate(yearly_longline_effort_mora_activity$x, by=list(Year=yearly_longline_effort_mora_activity$Year), FUN=sum, na.rm = TRUE)
yearly_longline_effort_mora <- aggregate(yearly_longline_effort_mora_activity$x, by=list(Year=yearly_longline_effort_mora_activity$Year), FUN=sum, na.rm = TRUE)

# transform effort so that it is at a scale comparable to the one of the catches
yearly_longline_T_effort_mora <- yearly_longline_effort_mora
yearly_longline_T_effort_mora$x <- yearly_longline_T_effort_mora$x/10000

## plot table of RIB catch and effort by year 
mora_catch_table <- yearly_global_catches_mora  %>%
  dplyr::select(-Species) %>%
  group_by(Year) %>%
  summarise_all(sum) %>%
  mutate_at(vars(x), funs(round(., 1))) %>%
  rename('Total catch (tonnes)' = x)
RIB_catch_effort_table <- full_join(mora_catch_table, yearly_longline_T_effort_mora)
RIB_catch_effort_table <- RIB_catch_effort_table %>% rename('Effort (10 thousand hooks)' = x)

write_xlsx(RIB_catch_effort_table,"RIB summary/Tables/RIB_catch_effort_table.xlsx")

## plot table of RIB catch by subarea 
RIB_catch_subarea_table <- yearly_global_catches_subarea_mora  %>%
  dplyr::select(-Species) %>%
  group_by(Year, SubArea) %>%
  summarise_all(sum) %>%
  mutate_at(vars(x), funs(round(., 1))) %>%
  pivot_wider(names_from = SubArea, values_from = x) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(Year, everything())

write_xlsx(RIB_catch_subarea_table,"RIB summary/Tables/RIB_catch_subarea_table.xlsx")

# plotting the data for mora
# first graph catch (bars) and effort (line) per year
# second graph catch by year and subarea

ggplot() +
  geom_bar(data=yearly_global_catches_mora, aes(x = Year, y = x, fill="Catch"), stat="identity") +
   geom_line(data=yearly_longline_T_effort_mora,  aes(x=Year,y=x, color="Effort"), inherit.aes = FALSE, size = 1) +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Longline effort (10 000 hooks)")) +
  labs(title="Yearly common mora catch in the SIOFA area", x="Year", y="Catch (t)") +
  theme_bw() +
  theme( axis.line.y.right = element_line(color = "blue"), 
         axis.ticks.y.right = element_line(color = "blue"),
         axis.text.y.right = element_text(color = "blue"), 
         axis.title.y.right = element_text(color = "blue")) +
  scale_fill_manual(values = c("Catch" = "cornflowerblue"), 
                    guide = guide_legend(title = "Fill", override.aes = list(linetype = "blank", shape = NA))) +
  scale_colour_manual(values = c("Effort" = "blue"),
                      guide = guide_legend(title = "Line", override.aes = list(linetype = "solid", shape = NA))) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("RIB summary/SIOFAcatches_effort_RIB_web.png", width = 10, height = 4, dpi = 150)  

ggplot(yearly_global_catches_subarea_mora, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly common mora catch by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("RIB summary/SIOFAcatches_RIB_subarea_web.png", width = 10, height = 4, dpi = 150)

# calculate average annual catches during the recent period
yearly_global_catches_species_special_RIB <- yearly_global_catches_species %>%
  filter(Species == "RIB") %>%
  dplyr::select(-Species) %>%
  group_by(Year) %>%
  dplyr::summarise_all(sum) %>%
  mutate(Species=c("RIB"), .after=Year) #%>%
avg_RIB_2018_2022 <- filter(yearly_global_catches_species_special_RIB, (Year >= 2018))
avg_RIB_2018_2022 <- sum(avg_RIB_2018_2022$x)/5

## mapping fisheries in space
# turn the longline effort locations in a spatial dataframe
# eliminate data points outside of the SIOFA area
yearly_longline_effort_mora_activity <- filter(yearly_longline_effort_species, (Species == "RIB"))
fishing_spatial_mora <- st_as_sf(yearly_longline_effort_mora_activity, coords = c( "Longitude", "Latitude"), crs = 4326)
fishing_spatial_mora <- fishing_spatial_mora[fishing_boundaries, ]

# load 5 degree square grid, subset it for the SIOFA area only
grid5 <- st_read('5degree_squares')
st_crs(grid5) <- 4326

# calculate number of fishing events per 5 degree grid cell
mora_poly5 <- st_join(fishing_spatial_mora, grid5)
mora_count5 <- mora_poly5 %>% group_by(id) %>% count()
mora_count5 <- st_drop_geometry(mora_count5)
mora_poly5 <- left_join(grid5, mora_count5, by="id")
mora_poly5 <- filter(mora_poly5, (!is.na(mora_poly5$n)))

# classify interval of gridline cells for plotting
mora_breaks_qt5 <- classIntervals(c(min(mora_poly5$n) - .00001, mora_poly5$n), n = 7, style = "pretty")
mora_poly5 <- mutate(mora_poly5, n.events5 = cut(n, mora_breaks_qt5$brks))

# crop fishing effort layer using fishing boundaries
mora_poly5 <- st_intersection(mora_poly5, fishing_boundaries)

# plot world map and add SIOFA subareas + 5 degree resolution SIOFA RIB fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf(data = mora_poly5, aes(fill=n.events5)) +
  scale_fill_viridis(discrete = TRUE) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(1,3,3.5, -1, 0, -2, 0, -6), nudge_x=c(0,-2.5,0, 6, 0, 0, 0, -6)) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA RIB fishing activities (5 degrees, 2013–2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  labs(fill="N. fishing events") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("RIB summary/SIOFAmap_mora_heatmap_5_web.png", width = 10, height = 7, dpi = 150)

## UNSTANDARDIZED CPUEs
#get the data ready to plot
yearly_global_catches_mora <- aggregate(yearly_global_catches_mora$x, by=list(Year=yearly_global_catches_mora$Year), FUN=sum, na.rm = TRUE)
names(yearly_global_catches_mora)[names(yearly_global_catches_mora) == "x"] <- "Catch"
yearly_longline_T_effort_mora$x <- yearly_longline_effort_mora$x/10000
names(yearly_longline_T_effort_mora)[names(yearly_longline_T_effort_mora) == "x"] <- "Effort"
CPUE_mora <- full_join(yearly_global_catches_mora, yearly_longline_T_effort_mora)
#calculate CPUE
CPUE_mora <- CPUE_mora %>% 
  mutate(CPUE = Catch/Effort) %>% 
  dplyr::select(-Catch) %>% 
  dplyr::select(-Effort) %>% 
  filter(!row_number() %in% c(1, 2))

#plot CPUE
ggplot(data=CPUE_mora, aes(x=Year,y=CPUE), inherit.aes = FALSE, size = 1) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous(limits=c(2014, 2023)) +
  scale_y_continuous(limits=c(0, 3)) +
  labs(title="Unstandardised CPUE for common mora in the SIOFA area", x="Year", y="CPUE (t/10 000 hooks)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("RIB summary/SIOFA_CPUEs_RIB_web.png", width = 10, height = 4, dpi = 150)  
write_xlsx(CPUE_mora,"RIB summary/Tables/CPUE_mora.xlsx")


###catch/bycatch ratio RIB fisheries
## Better definition of target/non-target catch: 
## if 20% of the catch in an operation
## is RIB, then that operation had RIB as a target 
## redo all analyses with this definition, for those events that didn't declare an RIB target

# select only operations that caught RIB (ignore all others)
# identify any operations that caught common mora in all the dataset, 
# whenever targets were not declared
# find total RIB catch by ActivityID
# need to retain year

yearly_operations_RIB <- fishing %>%
  #filter(Year==2014 | Year==2015 |Year==2016 |Year==2017 |Year==2018) %>%
  group_by(Year, ActivityID, datasetID, Gear) %>% 
  filter(SpeciesCode == "RIB") %>%
  filter(is.na(Target)) %>%
  summarize(CatchTonRIB = sum(CatchTon))

# find total catch by operation = sum weights of all species caught by 
# an ActivityID that caught RIB
# in years where target was not declared (in 2014-15-16-17-18)
yearly_operations_RIB_total <- fishing %>%
  filter(ActivityID %in% yearly_operations_RIB$ActivityID & datasetID %in% yearly_operations_RIB$datasetID) %>%
  group_by(Year, ActivityID, Gear) %>% 
  summarize(TotalCatch = sum(CatchTon))

# if RIB catch >20% in an acitivtyID then retain those ActivityIDs as operations targeting RIB
# target/non-target ratio is actually already defined  
yearly_operations_RIB_target <- full_join(yearly_operations_RIB, yearly_operations_RIB_total)
yearly_operations_RIB_target <- yearly_operations_RIB_target %>% 
  mutate(Ratio = CatchTonRIB/TotalCatch) %>%
  filter(Ratio >= 0.2) 

## non-target catch by species in RIB target operations with undeclared targets
yearly_operations_RIB_species <- fishing %>%
  filter(ActivityID%in%yearly_operations_RIB_target$ActivityID & datasetID%in%yearly_operations_RIB_target$datasetID) %>%
  filter(!SpeciesCode == "RIB") %>%
  group_by(Year,SpeciesCode) %>%
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  rename('Species' = SpeciesCode)

# join data in years where RIB targets were declared
# calculate target/non-target catches common mora 

### NOTE: RIB was never declared as a target!

# join non-target data from both years when catch was declared and not declared
yearly_nontarget_catches_commonmora <- yearly_operations_RIB_species

# sort bycatch data for plotting
sort_bycatch_RIB <- aggregate(yearly_nontarget_catches_commonmora$NonTargetCatch, by=list(Species=yearly_nontarget_catches_commonmora$Species), FUN=sum, na.rm = TRUE)
sort_bycatch_RIB <- arrange(sort_bycatch_RIB, desc(x)) 
RIB5_species <- sort_bycatch_RIB %>% slice(1:5)
other_species <- sort_bycatch_RIB %>% slice(6:25)
# plot
ggplot(data= subset(yearly_nontarget_catches_commonmora, Species %in% RIB5_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_nontarget_catches_commonmora, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly bycatch by species in RIB fisheries in the SIOFA area", x="Year", y="Non-target catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("RIB summary/SIOFAcatches_nontarget_RIB_web.png", width = 10, height = 5, dpi = 150)

# calculate target/non-target catch fraction in undeclared operations
yearly_operations_RIB_target <- yearly_operations_RIB_target %>%
  mutate(NonTargetCatch = TotalCatch-CatchTonRIB) %>%
  mutate(TargetCatch = CatchTonRIB)

## sharks non-target catch in all RIB target operations 
shark_species <- read_excel("sharks.xlsx")
yearly_sharks_catches_RIB <- yearly_nontarget_catches_commonmora %>%
  filter(Species %in% shark_species$Code) 

# sort shark bycatch data for plotting
sort_shark_bycatch_RIB <- yearly_sharks_catches_RIB %>% 
  group_by(Species) %>%
  summarize(x = sum(NonTargetCatch))
sort_shark_bycatch_RIB <- arrange(sort_shark_bycatch_RIB, desc(x)) 
RIB5_shark_species <- sort_shark_bycatch_RIB %>% slice(1:5)
other_shark_species <- sort_shark_bycatch_RIB %>% slice(6:28)
# plot sharks catch 
ggplot(data= subset(yearly_sharks_catches_RIB, Species %in% RIB5_shark_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_sharks_catches_RIB, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly bycatch of sharks in RIB fisheries in the SIOFA area", x="Year", y="Non-target catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("RIB summary/SIOFAcatches_sharks_RIB_web.png", width = 10, height = 6, dpi = 150)

## target/nontarget catch in non-declared events, spatial
# subareas
# non target
yearly_nontarget_RIB_spatial <- fishing_within %>%
  filter(ActivityID %in% yearly_operations_RIB_target$ActivityID & datasetID %in% yearly_operations_RIB_target$datasetID) %>%
  filter(!SpeciesCode == "RIB") %>%
  group_by(Year, SubAreaNo) %>%
  summarize(NonTargetCatch = sum(CatchTon)) 

# target
yearly_target_RIB_spatial <- fishing_within %>%
  filter(ActivityID %in% yearly_operations_RIB_target$ActivityID & datasetID %in% yearly_operations_RIB_target$datasetID) %>%
  filter(SpeciesCode == "RIB") %>%
  group_by(Year, SubAreaNo) %>%
  summarize(TargetCatch = sum(CatchTon)) 

# drop geometry 
yearly_nontarget_RIB_spatial$geometry <- NULL
yearly_target_RIB_spatial$geometry <- NULL
# join target and non-target 2014-15-17-18 catches, by subarea
target_nontarget_RIB <- left_join(yearly_target_RIB_spatial, yearly_nontarget_RIB_spatial)

# calculate target and non-target catch in years when target declarations were made
catch_target_subarea_RIB <- fishing_within %>%
  filter(Target == "RIB" & (SpeciesCode== "RIB")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year, SubAreaNo) %>% 
  summarize(TargetCatch = sum(CatchTon)) %>%
  filter(TargetCatch>0)

catch_nontarget_subarea_RIB <- fishing_within %>%
  filter(Target == "RIB" & (SpeciesCode!= "RIB")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year, SubAreaNo) %>% 
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  filter(NonTargetCatch>0)

# drop geometry 
catch_target_subarea_RIB$geometry <- NULL
catch_nontarget_subarea_RIB$geometry <- NULL

# join target and nontarget catch in years when targets were declared
catch_bycatch_subarea_RIB <- left_join(catch_target_subarea_RIB, catch_nontarget_subarea_RIB)

# join non declared with RIB declared targets operations
catch_bycatch_subarea_RIB <- rbind(catch_bycatch_subarea_RIB, target_nontarget_RIB)

# plot histograms of target and non-target catch in RIB fisheries by year and subarea
ggplot(catch_bycatch_subarea_RIB, aes(x = Year, y = TargetCatch)) +
  geom_bar(aes(fill=SubAreaNo), position="fill", stat="identity") +
  labs(title="Yearly target catch in RIB fisheries by SIOFA subareas (relative)", x="Year", y="Target catch (%)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("RIB summary/SIOFAtargetcatch_subarea_RIB_web.png", width = 10, height = 4, dpi = 150)

ggplot(catch_bycatch_subarea_RIB, aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=SubAreaNo), position="fill", stat="identity") +
  labs(title="Yearly bycatch in RIB fisheries by SIOFA subareas (relative)", x="Year", y="Non-target catch (%)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("RIB summary/SIOFAnontargetcatch_subarea_RIB_web.png", width = 10, height = 4, dpi = 150)

# transform data for easier plotting
catch_bycatch_RIB <- catch_bycatch_subarea_RIB %>% 
  dplyr::select(-SubAreaNo) %>%
  pivot_longer(!Year, names_to = "Catch", values_to = "t")

# plot target/non-target catch totals
ggplot(catch_bycatch_RIB, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly catch/bycatch in RIB fisheries in the SIOFA area (absolute)", x="Year", y="Total catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Non-target Catch", "Target Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("RIB summary/SIOFAcatch_nontargetcatch_RIB_web.png", width = 10, height = 4, dpi = 150)

# plot target/non-target catch percentage
ggplot(catch_bycatch_RIB, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly catch/bycatch in RIB fisheries in the SIOFA area (relative)", x="Year", y="Total catch (%)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Non-target Catch", "Target Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("RIB summary/SIOFAcatch_nontargetcatch_RIB_fill_web.png", width = 10, height = 4, dpi = 150)

# check again bycatch by species
# declared targets
bycatch_declared_RIB <- fishing_within %>%
  filter(Target == "RIB" & (SpeciesCode!= "RIB")) %>%
  mutate(CatchTon = ifelse(is.na(CatchTon), 0, CatchTon)) %>%
  group_by(Year, SpeciesCode) %>% 
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  filter(NonTargetCatch>0)
bycatch_declared_RIB$geometry <- NULL
# non declared targets
bycatch_nondeclared_RIB <- fishing_within %>%
  filter(ActivityID %in% yearly_operations_RIB_target$ActivityID & datasetID %in% yearly_operations_RIB_target$datasetID) %>%
  filter(!SpeciesCode == "RIB") %>%
  group_by(Year, SpeciesCode) %>%
  summarize(NonTargetCatch = sum(CatchTon)) %>%
  filter(NonTargetCatch>0)
bycatch_nondeclared_RIB$geometry <- NULL
# join them
bycatch_RIB_species <- rbind(bycatch_declared_RIB, bycatch_nondeclared_RIB)
bycatch_RIB_species <- bycatch_RIB_species %>%
  rename(Species = SpeciesCode)
# sort bycatch data for plotting
sort_bycatch_RIB <- aggregate(bycatch_RIB_species$NonTargetCatch, by=list(Species=bycatch_RIB_species$Species), FUN=sum, na.rm = TRUE)
sort_bycatch_RIB <- arrange(sort_bycatch_RIB, desc(x)) 
RIB5_species <- sort_bycatch_RIB %>% slice(1:5)
other_species <- sort_bycatch_RIB %>% slice(6:24)
# plot
ggplot(data= subset(bycatch_RIB_species, Species %in% RIB5_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(bycatch_RIB_species, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly non-target catch by species in the fisheries targeting RIB in the SIOFA area", x="Year", y="Non-target catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("RIB summary/SIOFAcatches_nontarget_RIB_web.png", width = 10, height = 5, dpi = 150)

# check again bycatch by shark species
yearly_sharks_catches_RIB <- bycatch_RIB_species %>%
  filter(Species %in% shark_species$Code) 
# sort shark bycatch data for plotting
sort_shark_bycatch_RIB <- yearly_sharks_catches_RIB %>% 
  group_by(Species) %>%
  summarize(x = sum(NonTargetCatch))
sort_shark_bycatch_RIB <- arrange(sort_shark_bycatch_RIB, desc(x)) 
RIB5_shark_species <- sort_shark_bycatch_RIB %>% slice(1:5)
other_shark_species <- sort_shark_bycatch_RIB %>% slice(6:16)
# plot sharks catch 
ggplot(data= subset(yearly_sharks_catches_RIB, Species %in% RIB5_shark_species$Species), aes(x = Year, y = NonTargetCatch)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_sharks_catches_RIB, Species %in% other_species$Species), aes(x = Year, y = NonTargetCatch, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly bycatch of sharks in RIB fisheries in the SIOFA area", x="Year", y="Non-target catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("RIB summary/SIOFAcatches_sharks_RIB_web.png", width = 10, height = 6, dpi = 150)

## catch/bycatch in management units
# load RIB management units boundaries
## NOTE: there are no RIB management units or assessment areas

## VME catches in RIB fisheries

# filter only RIB target reports 
VME_catch_RIB <- filter(VME_catch, grepl('RIB', TargetSpecies))

# aggregate by year and taxon
yearly_RIB_VME_catch <- VME_catch_RIB %>%
  group_by(Year, FAOcode) %>%
  summarise_at(.vars = c("Weight"),
               .funs = "sum")
# plot RIB VME bycatch
ggplot(yearly_RIB_VME_catch, aes(x = Year, y = Weight, fill=FAOcode)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly incidental catch of VME indicator taxa in RIB fisheries in the SIOFA area", x="Year", y="Total weight (kg)") +
  theme_bw() +
  scale_x_continuous(limits=c(2002, 2023)) +
  #scale_fill_hue(labels = c("Non-target Catch", "Target Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("RIB summary/VME_captures_RIB_full_web.png", width = 10, height = 4, dpi = 150)

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
captures_spatial_plot_RIB <- captures_spatial  %>%
  filter(!(speciesGroup=='Chimaeras' |  speciesGroup=='Deepwater sharks' |  speciesGroup=='Sharks and rays nei' ) & species3ACode %in% shark_species_CMM$FAOcode
         | (speciesGroup=='Seabirds'| speciesGroup=='Cetaceans'| speciesGroup=='Turtles')) %>%
  filter(source=='OBS') %>%
  filter(grepl('RIB', fishopTargetSpecies))

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = captures_spatial_plot_RIB, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA incidental captures of seabirds, marine mammals, turtles and 
          sharks considered to be at high risk and/or of concern in RIB target operations (2012-2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  scale_colour_discrete("Captures") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("RIB summary/SIOFAmap_accidental_captures_web_RIB.png", width = 10, height = 8, dpi = 150)

# aggregate captures in a table that can be used in reports
# filter only operations that observers recorded as targeting RIB
captures_RIB <-   captures%>%
  filter(grepl('RIB', fishopTargetSpecies))

# turtles
captures_turtles <- filter(captures_RIB,(speciesGroup == 'Turtles'))

#captures_turtles_aggregate1 <- captures_turtles %>%
#  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
#  summarise(n = n()) 
captures_turtles_aggregate_RIB <- captures_turtles %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  relocate(speciesEnglishName, .after = Year) %>%
  relocate(speciesScientificName, .after = speciesEnglishName) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Captures" = foibcNbCaught) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(captures_turtles_aggregate_RIB,"RIB summary/Tables/captures_turtles_RIB.xlsx")

# marine mammals
captures_mammals <- filter(captures_RIB,(speciesGroup == 'Cetaceans'))

captures_mammals_aggregate_RIB <- captures_mammals %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) 

write_xlsx(captures_mammals_aggregate_RIB,"RIB summary/Tables/captures_mammals_RIB.xlsx")

# seabirds
captures_seabirds <- filter(captures_RIB,(speciesGroup == 'Seabirds'))

captures_seabirds_aggregate_RIB <- captures_seabirds %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Captures" = foibcNbCaught) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(captures_seabirds_aggregate_RIB,"RIB summary/Tables/captures_seabirds_RIB.xlsx")

# sharks
captures_sharks_RIB <- captures_RIB %>%
  filter(!(speciesGroup=='Chimaeras' |  speciesGroup=='Deepwater sharks' |  speciesGroup=='Sharks and rays nei' ) & species3ACode %in% shark_species_CMM$FAOcode)

captures_sharks_aggregate_RIB <- captures_sharks_RIB %>%
  group_by(Year, Gear, speciesEnglishName, speciesScientificName) %>%
  summarise(across(starts_with('foibc'), sum)) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) %>%
  rename("Captures (n)" = foibcNbCaught)

write_xlsx(captures_sharks_aggregate_RIB,"RIB summary/Tables/captures_sharks_RIB.xlsx")

### Analyze observations of seabirds and mammals
# Convert the observation dataframe to a spatial object. Note that the
# crs= 4326 parameter assigns a WGS84 coordinate system to the spatial object
# and that missing values are removed
observations_spatial <- filter(observations, (!is.na(observations$Longitude)))
observations_spatial <- st_as_sf(observations_spatial, coords = c("Longitude", "Latitude"), crs = 4326) 
class(observations_spatial)
st_crs(observations_spatial) <- 4326

observations_spatial_plot_RIB <- observations_spatial  %>%
  filter(!(speciesGroup=='NO BIRDS OBSERVED' |  speciesGroup=='NO OBSERVER')) %>%
  filter(source=='OBS') %>%
  filter(grepl('RIB', fishopTargetSpecies))

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 0, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, 0, -6)) +
  geom_sf(data = observations_spatial_plot_RIB, aes(color=speciesGroup), alpha = 1/5, cex = 5) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA observations of seabirds and marine mammals
            in RIB target operations (2012-2022)") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  scale_colour_discrete("Observations") +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("RIB summary/SIOFAmap_observations_web_RIB.png", width = 10, height = 8, dpi = 150)

## analyze seabirds observations
observations_seabirds_aggregate_RIB <- observations %>%
  filter(grepl('RIB', fishopTargetSpecies)) %>%
  filter(!speciesGroup=='MAMMALS') %>%
  filter(!speciesGroup=='NO BIRDS OBSERVED') %>%
  filter(!speciesGroup=='NO OBSERVER') %>%
  group_by(Year, speciesEnglishName, speciesScientificName, Gear, speciesGroup) %>%
  summarise(Abundance = sum(Abundance)) %>%
  dplyr::select(-speciesGroup) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(observations_seabirds_aggregate_RIB,"RIB summary/Tables/observations_seabirds_RIB.xlsx")

## analyze negative seabirds observations
negative_observations_seabirds_aggregate_RIB <- observations %>%
  filter(grepl('RIB', fishopTargetSpecies)) %>%
  filter(speciesGroup=='NO BIRDS OBSERVED') %>%
  group_by(Year, Gear) %>%  
  summarise(n = n()) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Fishing events with no birds observed" = n)

write_xlsx(negative_observations_seabirds_aggregate_RIB,"RIB summary/Tables/negative_observations_seabirds_RIB.xlsx")

## analyze non-performed seabirds observations
no_observations_seabirds_aggregate_RIB <- observations %>%
  filter(grepl('RIB', fishopTargetSpecies)) %>%
  filter(speciesGroup=='NO OBSERVER') %>%
  group_by(Year, Gear) %>%  
  summarise(n = n()) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Fishing events not observed for bird presence" = n)

write_xlsx(no_observations_seabirds_aggregate_RIB,"RIB summary/Tables/no_observations_seabirds_RIB.xlsx")

## analyze marine mammals observations during fishing events
observations_mammals_aggregate_RIB <- observations %>%
  filter(speciesGroup=='MAMMALS') %>%
  filter(!speciesGroup=='NO BIRDS OBSERVED') %>%
  filter(!speciesGroup=='NO OBSERVER') %>%
  filter(!speciesGroup=='SEABIRDS') %>%
  filter(grepl('RIB', fishopTargetSpecies)) %>%
  group_by(Year, speciesEnglishName, speciesScientificName, Gear, speciesGroup) %>%
  summarise(Abundance = sum(Abundance)) %>%
  dplyr::select(-speciesGroup) %>%
  rename("Common name" = speciesEnglishName) %>%
  rename("Fishing gear" = Gear) %>%
  rename("Scientific name" = speciesScientificName) 

write_xlsx(observations_mammals_aggregate_RIB,"RIB summary/Tables/observations_mammals_RIB.xlsx")

## analyze countries that submitted observer data (measures of fish) in RIB fishery
countries_observing_RIB <- Observer_data %>%
  #filter(Year>=2013) %>%
  filter((species3ACode == "RIB")) %>%
  group_by(Year,CountryISOCode) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::select(-n) %>%
  rename(Country = CountryISOCode) 

write_xlsx(countries_observing_RIB,"RIB summary/Tables/countries_observing_RIB.xlsx")

# measures of maturity, sex and weight, and otoliths collected for RIB
measured_fish_RIB <- Observer_data %>%
  #filter(Year>=2013) %>%
  filter((species3ACode == "RIB")) %>%
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

sums_measured_fish_RIB <- colSums(measured_fish_RIB)
measured_fish_RIB <- rbind(measured_fish_RIB,sums_measured_fish_RIB)

write_xlsx(measured_fish_RIB,"RIB summary/Tables/measured_fish_RIB.xlsx")

## analyze countries participating in RIB fishery
countries_fishing_RIB <- fishing %>%
  filter(ActivityID %in% yearly_operations_RIB_target$ActivityID 
         & datasetID %in% yearly_operations_RIB_target$datasetID |
           Target == "RIB") %>%
  group_by(Year,CCPCode1,dbSource) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::select(-n) %>%
  rename(Country = CCPCode1) %>%
  rename(Database = dbSource) 
write_xlsx(countries_fishing_RIB,"RIB summary/Tables/countries_fishing_RIB.xlsx")
