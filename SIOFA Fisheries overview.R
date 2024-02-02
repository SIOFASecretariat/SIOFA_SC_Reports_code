
# install packages
install.packages("sp")
install.packages("sf")
install.packages("OpenStreetMap")
install.packages("DT")
install.packages("RColorBrewer")
install.packages("mapproj")
install.packages("RgoogleMaps")
install.packages("scales")
install.packages("rworldmap")
install.packages("maps")
install.packages("tidyverse")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("rgeos")
install.packages("ggspatial")
install.packages("maptools")
install.packages("leaflet")
install.packages("tmap")
install.packages("here")
install.packages("rgdal")
install.packages("scales")
install.packages("flextable")
install.packages('spatialEco')
install.packages('openxlsx')
install.packages("writexl")
install.packages("spData")
install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")
install.packages("cowplot")
install.packages('googleway')
install.packages('ggplot2')
install.packages('ggrepel')
install.packages('ggspatial')
install.packages("RColorBrewer")
install.packages("wesanderson")
install.packages("dplyr")
install.packages('spatstat')
install.packages("geosphere")
install.packages("lubridate")
install.packages('ggstats')
# install package from github
devtools::install_github("dkahle/ggmap", ref = "tidyup")
# install klippy for copy-to-clipboard button in code chunks
remotes::install_github("rlesur/klippy")


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
require(spatstat)
require(geosphere)
require(lubridate)
require(ggstats)
## load main fishing data
setwd("D:/SIOFA/Data")   #### !!!check that this points to the parent folder!!!
fishing <- read_excel("Catch-effort-2023.xlsx", col_types = c("numeric", "text", "text", "numeric", "numeric", "numeric","numeric","numeric","text","text","text","text","numeric","numeric","numeric","text","text","text","text","numeric","numeric", "numeric","numeric","text"))

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
CCAMLR <- st_read('CCAMLR_stat_areas')

#check that layers are viable
class(land)
class(ocean)
class(fishing_boundaries)

# plot world map and add SIOFA subareas + siofa fishing events on world map
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
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

ggsave("Overview/SIOFAmap_web.png", width = 10, height = 10, dpi = 150)

# Overlay points and extract just the subarea column 
fishing_within <- st_join(fishing_spatial, left = FALSE, fishing_boundaries["SubAreaNo"])

# go from a spatial layer back to a data frame

# export as xlsx if needed
#write_xlsx(fishing_subareas,"subareas.xlsx")

# double check points that were unassigned in subarea 3b (and others)
# load data
#fishing_unassigned <- read_excel("Unassigned.xlsx")
#fishing_boundaries3b <- st_read('subarea_3b')
#fishing_spatial_unassigned <- st_as_sf(fishing_unassigned, coords = c("Longitude", "Latitude"), crs = 4326) 

# plot these points to check where they lay compared to SIOFA subareas 3b
#ggplot() +
#geom_sf(data = ocean, fill = "lightblue", color = "black") +
#geom_sf(data = grid, color = "white") +
#geom_sf(data = land, fill = "antiquewhite", color = "black") +
#geom_sf(data = islands, fill = "antiquewhite", color = "black") +
#geom_sf(data = fishing_boundaries3b, fill = "red") +
#geom_sf(data = fishing_spatial_unassigned, color = 'black', cex = .1) +
#xlab("Longitude") + ylab("Latitude") +
#ggtitle("SIOFA unassigned events") +
#coord_sf(xlim = c(25, 70), ylim = c(-30, -50), expand = TRUE) +
#annotation_scale(location = "bl", width_hint = 0.1) +
#annotation_north_arrow(location = "tl", which_north = "true", 
#pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
#style = north_arrow_fancy_orienteering)

#try again to overlay points to see if there is any difference
#fishing_within2 <- st_join(fishing_spatial_unassigned, fishing_boundaries3b, left = TRUE)

#it still doesn't work, it looks like the polygon is bent
#try again with tweaked subarea polygon


# create a data frame with only total catches per year
# plot histogram of total catches per year
#watch out for NAs
yearly_global_catches <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year), FUN=sum, na.rm = TRUE)

ggplot(yearly_global_catches, aes(x = Year, y = x)) +
  geom_bar(stat="identity", fill="#4393C3") +
  labs(title="Yearly total catch in the SIOFA area", x="Year", y="Catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatches_web.png", width = 10, height = 4, dpi = 150)
write_xlsx(yearly_global_catches,"Overview/Tables/yearly_global_catches.xlsx")

# create a data frame with only total catches per year and per subarea
# plot histogram of total catches per year and subarea
yearly_global_catches_subarea <- aggregate(fishing_within$CatchTon, by=list(Year=fishing_within$Year,SubArea=fishing_within$SubAreaNo), FUN=sum, na.rm = TRUE)

ggplot(yearly_global_catches_subarea, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly total catch by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatches_subarea_web.png", width = 10, height = 4, dpi = 150)
write_xlsx(yearly_global_catches_subarea,"Overview/Tables/yearly_global_catches_subarea.xlsx")

# subset the whole data for all species, need catch, effort and subarea aggregations

yearly_global_catches_species <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, Species=fishing$SpeciesCode), FUN=sum, na.rm = TRUE)
yearly_global_catches_subarea_species <- aggregate(fishing_within$CatchTon, by=list(Year=fishing_within$Year,SubArea=fishing_within$SubAreaNo, Species=fishing_within$SpeciesCode), FUN=sum, na.rm = TRUE)

yearly_trawl_effort_species_activity <- aggregate(fishing$NbTows, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, activityID=fishing$ActivityID), FUN=sum, na.rm = TRUE)
yearly_longline_effort_species_activity <- aggregate(fishing$NbHooks, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, activityID=fishing$ActivityID), FUN=sum, na.rm = TRUE)

# toggle on to select unique records based on activity
yearly_trawl_effort_species <- distinct(yearly_trawl_effort_species_activity, activityID, Species, .keep_all = TRUE)
#yearly_longline_effort_species <- distinct(yearly_longline_effort_species_activity, activityID, Species, .keep_all = TRUE)

# Effort by subarea, normally not needed
#yearly_trawl_effort_species <- aggregate(fishing_within$NbTows, by=list(Year=fishing_within$Year,SubArea=fishing_within$SubAreaNo, Species=fishing_within$SpeciesCode), FUN=sum, na.rm = TRUE)
#yearly_longline_effort_species <- aggregate(fishing_within$NbHooks, by=list(Year=fishing_within$Year,SubArea=fishing_within$SubAreaNo, Species=fishing_within$SpeciesCode), FUN=sum, na.rm = TRUE)

# subset for orange roughy only data 
# !!!!! check well how the trawl effort is calculated when missing data
yearly_global_catches_roughy <- filter(yearly_global_catches_species, (Species == "ORY") | (Species == "HPR") | (Species == "FSZ"))

yearly_global_catches_subarea_roughy <- filter(yearly_global_catches_subarea_species, (Species == "ORY") | (Species == "HPR") | (Species == "FSZ"))

yearly_trawl_effort_roughy_activity <- filter(yearly_trawl_effort_species, (Species == "ORY") | (Species == "HPR") | (Species == "FSZ"))
yearly_trawl_effort_roughy_activity_a <- filter(yearly_trawl_effort_roughy_activity, x > 0)
yearly_trawl_effort_roughy_activity_h <- filter(yearly_trawl_effort_roughy_activity, x == 0)

# toggle off to disaggregate effort per subarea
#yearly_trawl_effort_alfonsino <- aggregate(yearly_trawl_effort_alfonsino_activity$x, by=list(Year=yearly_trawl_effort_alfonsino_activity$Year), FUN=sum, na.rm = TRUE)
yearly_trawl_effort_roughy_a <- aggregate(yearly_trawl_effort_roughy_activity_a$x, by=list(Year=yearly_trawl_effort_roughy_activity_a$Year), FUN=sum, na.rm = TRUE)
yearly_trawl_effort_roughy_h <- yearly_trawl_effort_roughy_activity_h %>%
  group_by(Year) %>%
  summarise(n = n())

yearly_trawl_effort_roughy <- full_join(yearly_trawl_effort_roughy_a, yearly_trawl_effort_roughy_h)
yearly_trawl_effort_roughy <- yearly_trawl_effort_roughy %>%
  replace(is.na(.), 0) %>% 
  mutate(m = rowSums(across(c(x, n)))) %>%
  dplyr::select(-x,-n)  %>%
  rename(x = m)

## plot table of ORY catch and effort by year 
ORY_catch_table <- yearly_global_catches_roughy  %>%
  dplyr::select(-Species) %>%
  group_by(Year) %>%
  mutate_at(vars(x), funs(round(., 1))) %>%
  rename('Total catch (t)' = x)
ORY_catch_effort_table <- full_join(ORY_catch_table, yearly_trawl_effort_roughy)
ORY_catch_effort_table <- ORY_catch_effort_table %>% rename('Effort (tows)' = x)

write_xlsx(ORY_catch_effort_table,"Overview/Tables/ORY_catch_effort_table.xlsx")

## plot table of ORY catch by subarea 
ORY_catch_subarea_table <- yearly_global_catches_subarea_roughy  %>%
  dplyr::select(-Species) %>%
  group_by(Year, SubArea) %>%
  summarise_all(sum) %>%
  mutate_at(vars(x), funs(round(., 1))) %>%
  pivot_wider(names_from = SubArea, values_from = x) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(Year, everything())

write_xlsx(ORY_catch_subarea_table,"Overview/Tables/ORY_catch_subarea_table.xlsx")

# plotting the data for orange roughy
# first graph catch (bars) and effort (line) per year
# second graph catch by year and subarea
ggplot() +
  geom_bar(data=yearly_global_catches_roughy, aes(x = Year, y = x, fill="Catch"), stat="identity") +
  #geom_bar(data=yearly_global_catches_subarea_roughy, stat="identity", fill="lightblue") +
  geom_line(data=yearly_trawl_effort_roughy, aes(x=Year,y=x, color="Effort"), inherit.aes = FALSE, size = 1) +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Trawl effort (# tows)")) +
  labs(title="Yearly orange roughy catch in the SIOFA area", x="Year", y="Catch (t)") +
  theme_bw() +
  theme( axis.line.y.right = element_line(color = "red"), 
         axis.ticks.y.right = element_line(color = "red"),
         axis.text.y.right = element_text(color = "red"), 
         axis.title.y.right = element_text(color = "red"))+
  scale_fill_manual(values = c("Catch" = "lightblue"), 
                    guide = guide_legend(title = "Fill", override.aes = list(linetype = "blank", shape = NA))) +
  scale_colour_manual(values = c("Effort" = "red"),
                      guide = guide_legend(title = "Line", override.aes = list(linetype = "solid", shape = NA))) +
    theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Overview/SIOFAcatches_effort_ORY_web.png", width = 10, height = 4, dpi = 150)  
write_xlsx(yearly_trawl_effort_roughy,"Overview/Tables/ORY_catch_subarea_table.xlsx")

ggplot(yearly_global_catches_subarea_roughy, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly orange roughy catch by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatches_ORY_subarea_web.png", width = 10, height = 4, dpi = 150)

# subset for alfonsino only data 
# !!!!! check well how the trawl effort is calculated when missing data
yearly_global_catches_alfonsino <- filter(yearly_global_catches_species, (Species == "ALF") | (Species == "BYS")| (Species == "BRX")| (Species == "BYX")| (Species == "BXD"))
yearly_global_catches_alfonsino <- aggregate(yearly_global_catches_alfonsino$x, by=list(Year=yearly_global_catches_alfonsino$Year), FUN=sum, na.rm = TRUE)

yearly_global_catches_subarea_alfonsino <- filter(yearly_global_catches_subarea_species, (Species == "ALF") | (Species == "BYS")| (Species == "BRX")| (Species == "BYX")| (Species == "BXD"))

yearly_trawl_effort_alfonsino_activity <- filter(yearly_trawl_effort_species, (Species == "ALF") | (Species == "BYS")| (Species == "BRX")| (Species == "BYX")| (Species == "BXD"))

yearly_trawl_effort_alfonsino_activity_a <- filter(yearly_trawl_effort_alfonsino_activity, x > 0)
yearly_trawl_effort_alfonsino_activity_h <- filter(yearly_trawl_effort_alfonsino_activity, x == 0)

# toggle off to disaggregate effort per subarea
#yearly_trawl_effort_alfonsino <- aggregate(yearly_trawl_effort_alfonsino_activity$x, by=list(Year=yearly_trawl_effort_alfonsino_activity$Year), FUN=sum, na.rm = TRUE)
yearly_trawl_effort_alfonsino_a <- aggregate(yearly_trawl_effort_alfonsino_activity_a$x, by=list(Year=yearly_trawl_effort_alfonsino_activity_a$Year), FUN=sum, na.rm = TRUE)
yearly_trawl_effort_alfonsino_h <- yearly_trawl_effort_alfonsino_activity_h %>%
  group_by(Year) %>%
  summarise(n = n())

yearly_trawl_effort_alfonsino <- full_join(yearly_trawl_effort_alfonsino_a, yearly_trawl_effort_alfonsino_h)
yearly_trawl_effort_alfonsino <- yearly_trawl_effort_alfonsino %>%
  replace(is.na(.), 0) %>% 
  mutate(m = rowSums(across(c(x, n)))) %>%
  dplyr::select(-x,-n)  %>%
  rename(x = m)

## plot table of ALF catch and effort by year 
ALF_catch_table <- yearly_global_catches_alfonsino  %>%
  rename('Total catch (t)' = x)
ALF_catch_effort_table <- full_join(ALF_catch_table, yearly_trawl_effort_alfonsino)
ALF_catch_effort_table <- ALF_catch_effort_table %>% rename('Effort (tows)' = x)

write_xlsx(ALF_catch_effort_table,"Overview/Tables/ALF_catch_effort_table.xlsx")

## plot table of ALF catch by subarea 
ALF_catch_subarea_table <- yearly_global_catches_subarea_alfonsino  %>%
  dplyr::select(-Species) %>%
  group_by(Year, SubArea) %>%
  summarise_all(sum) %>%
  mutate_at(vars(x), funs(round(., 1))) %>%
  pivot_wider(names_from = SubArea, values_from = x) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(Year, everything())

write_xlsx(ALF_catch_subarea_table,"Overview/Tables/ALF_catch_subarea_table.xlsx")

# plotting the data for alfonsino
# first graph catch (bars) and effort (line) per year
# second graph catch by year and subarea

ggplot() +
  geom_bar(data=yearly_global_catches_alfonsino, aes(x = Year, y = x, fill="Catch"), stat="identity") +
  #geom_bar(data=yearly_global_catches_subarea_alfonsino, stat="identity", fill="cyan2") +
  geom_line(data=yearly_trawl_effort_alfonsino, aes(x=Year,y=x, color="Effort"), inherit.aes = FALSE, size = 1) +
  labs(title="Yearly alfonsino catch in the SIOFA area", x="Year", y="Catch (t)") +
  theme_bw() +
  theme( axis.line.y.right = element_line(color = "red"), 
         axis.ticks.y.right = element_line(color = "red"),
         axis.text.y.right = element_text(color = "red"), 
         axis.title.y.right = element_text(color = "red"))+
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Trawl effort (# tows)")) +
  scale_fill_manual(values = c("Catch" = "cyan2"), 
                    guide = guide_legend(title = "Fill", override.aes = list(linetype = "blank", shape = NA))) +
  scale_colour_manual(values = c("Effort" = "red"),
                      guide = guide_legend(title = "Line", override.aes = list(linetype = "solid", shape = NA))) +
    theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)


ggsave("Overview/SIOFAcatches_effort_ALF_web.png", width = 10, height = 4, dpi = 150)  

ggplot(yearly_global_catches_subarea_alfonsino, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly alfonsino catch by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatches_ALF_subarea_web.png", width = 10, height = 4, dpi = 150)

# subset for toothfish only data 
# !!!!! double check how the trawl effort is calculated when missing data
yearly_global_catches_toothfish <- filter(yearly_global_catches_species, (Species == "TOP") | (Species == "TOT") | (Species == "TOA"))

yearly_global_catches_subarea_toothfish <- filter(yearly_global_catches_subarea_species, (Species == "TOP") | (Species == "TOT")| (Species == "TOA"))

yearly_longline_effort_toothfish_activity <- filter(yearly_longline_effort_species_activity, (Species == "TOP") | (Species == "TOT")| (Species == "TOA"))

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

write_xlsx(TOP_catch_effort_table,"Overview/Tables/TOP_catch_effort_table.xlsx")

## plot table of TOP catch by subarea 
TOP_catch_subarea_table <- yearly_global_catches_subarea_toothfish  %>%
  dplyr::select(-Species) %>%
  group_by(Year, SubArea) %>%
  summarise_all(sum) %>%
  mutate_at(vars(x), funs(round(., 1))) %>%
  pivot_wider(names_from = SubArea, values_from = x) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(Year, everything())

write_xlsx(TOP_catch_subarea_table,"Overview/Tables/TOP_catch_subarea_table.xlsx")

# plotting the data for toothfish
# first graph catch (bars) and effort (line) per year
# second graph catch by year and subarea
ggplot() +
  geom_bar(data=yearly_global_catches_toothfish, aes(x = Year, y = x, fill=Species), stat="identity") +
  #geom_bar(data=yearly_global_catches_subarea_toothfish, stat="identity", fill="darkolivegreen2") +
  geom_line(data=yearly_longline_T_effort_toothfish,  aes(x=Year,y=x, color="Effort"), inherit.aes = FALSE, size = 1) +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Longline effort (10 000 hooks)")) +
  labs(title="Yearly toothfish catch in the SIOFA area", x="Year", y="Catch (t)") +
  theme_bw() +
  theme( axis.line.y.right = element_line(color = "blue"), 
         axis.ticks.y.right = element_line(color = "blue"),
         axis.text.y.right = element_text(color = "blue"), 
         axis.title.y.right = element_text(color = "blue")) +
  scale_fill_manual(values = c(TOP = "darkolivegreen2", TOA = "red"), 
                    guide = guide_legend(title = "Fill", override.aes = list(linetype = "blank", shape = NA))) +
  scale_colour_manual(values = c("Effort" = "blue"),
                      guide = guide_legend(title = "Line", override.aes = list(linetype = "solid", shape = NA))) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Overview/SIOFAcatches_effort_TOP_web.png", width = 10, height = 4, dpi = 150)  

ggplot(yearly_global_catches_subarea_toothfish, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly toothfish catch by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatches_TOP_subarea_web.png", width = 10, height = 4, dpi = 150)

# subset for oilfish only data 
# !!!!! check well how the longline effort is calculated when missing data
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
yearly_global_catches_oilfish_T <- yearly_global_catches_oilfish %>% 
  group_by(Year = Year) %>%
  summarize(Catch = sum(x))

OIL_catch_table <- yearly_global_catches_oilfish_T  %>%
  #dplyr::select(-Species) %>%
  #group_by(Year) %>%
  #summarise_all(sum) %>%
  rename('Total catch (t)' = Catch) %>%
  mutate_at(vars('Total catch (t)'), funs(round(., 1))) 
OIL_catch_effort_table <- full_join(OIL_catch_table, yearly_longline_T_effort_oilfish)
OIL_catch_effort_table <- OIL_catch_effort_table %>% rename('Effort (10 thousand hooks)' = x)

write_xlsx(OIL_catch_effort_table,"Overview/Tables/OIL_catch_effort_table.xlsx")

## plot table of OIL catch by subarea 
OIL_catch_subarea_table <- yearly_global_catches_subarea_oilfish  %>%
  dplyr::select(-Species) %>%
  group_by(Year, SubArea) %>%
  summarise_all(sum) %>%
  mutate_at(vars(x), funs(round(., 1))) %>%
  pivot_wider(names_from = SubArea, values_from = x) %>%
  dplyr::select(order(colnames(.))) %>%
  dplyr::select(Year, everything())

write_xlsx(OIL_catch_subarea_table,"Overview/Tables/OIL_catch_subarea_table.xlsx")

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
  labs(title="Yearly oilfish catch in the SIOFA area", x="Year", y="Catch (t)") +
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

ggsave("Overview/SIOFAcatches_effort_OIL_web.png", width = 10, height = 4, dpi = 150)  

ggplot(yearly_global_catches_subarea_oilfish, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly oilfish catch by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatches_OIL_subarea_web.png", width = 10, height = 4, dpi = 150)

# subset for hapuku wreckfish only data 
# !!!!! check well how the longline effort is calculated when missing data

yearly_global_catches_hapuku <- filter(yearly_global_catches_species, (Species == "WHA"))

yearly_global_catches_subarea_hapuku <- filter(yearly_global_catches_subarea_species, (Species == "WHA"))

yearly_longline_effort_hapuku_activity <- filter(yearly_longline_effort_species_activity, (Species == "WHA"))

# toggle off to disaggregate effort per subarea
#yearly_longline_effort_hapuku <- aggregate(yearly_longline_effort_hapuku_activity$x, by=list(Year=yearly_longline_effort_hapuku_activity$Year), FUN=sum, na.rm = TRUE)
yearly_longline_effort_hapuku <- aggregate(yearly_longline_effort_hapuku_activity$x, by=list(Year=yearly_longline_effort_hapuku_activity$Year), FUN=sum, na.rm = TRUE)

# transform effort so that it is at a scale comparable to the one of the catches
yearly_longline_T_effort_hapuku <- yearly_longline_effort_hapuku
yearly_longline_T_effort_hapuku$x <- yearly_longline_T_effort_hapuku$x/10000

# plotting the data for hapuku
# first graph catch (bars) and effort (line) per year
# second graph catch by year and subarea
ggplot() +
  geom_bar(data=yearly_global_catches_hapuku, aes(x = Year, y = x, fill="Catch"), stat="identity") +
  #geom_bar(data=yearly_global_catches_subarea_hapuku, stat="identity", fill="chocolate2") +
  geom_line(data=yearly_longline_T_effort_hapuku, aes(x=Year,y=x, color="Effort"), inherit.aes = FALSE, size = 1) +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_y_continuous(limits = c(0,40), sec.axis = sec_axis(~., name = "Longline effort (10 000 hooks)")) +
  labs(title="Yearly hapuku catch in the SIOFA area", x="Year", y="Catch (t)") +
  theme_bw() +
  theme( axis.line.y.right = element_line(color = "blue"), 
         axis.ticks.y.right = element_line(color = "blue"),
         axis.text.y.right = element_text(color = "blue"), 
         axis.title.y.right = element_text(color = "blue"))+
  scale_fill_manual(values = c("Catch" = "chocolate2"), 
                    guide = guide_legend(title = "Fill", override.aes = list(linetype = "blank", shape = NA))) +
  scale_colour_manual(values = c("Effort" = "blue"),
                      guide = guide_legend(title = "Line", override.aes = list(linetype = "solid", shape = NA))) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Overview/SIOFAcatches_effort_WHA_web.png", width = 10, height = 4, dpi = 150)  

ggplot(yearly_global_catches_subarea_hapuku, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly hapuku catch by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatches_WHA_subarea_web.png", width = 10, height = 4, dpi = 150)

# subset for wreckfish only data 
# !!!!! check well how the longline effort is calculated when missing data
yearly_global_catches_wreckfish <- filter(yearly_global_catches_species, (Species == "WRF"))

yearly_global_catches_subarea_wreckfish <- filter(yearly_global_catches_subarea_species, (Species == "WRF"))

yearly_longline_effort_wreckfish_activity <- filter(yearly_longline_effort_species_activity, (Species == "WRF"))

# toggle off to disaggregate effort per subarea
#yearly_longline_effort_wreckfish <- aggregate(yearly_longline_effort_wreckfish_activity$x, by=list(Year=yearly_longline_effort_wreckfish_activity$Year), FUN=sum, na.rm = TRUE)
yearly_longline_effort_wreckfish <- aggregate(yearly_longline_effort_wreckfish_activity$x, by=list(Year=yearly_longline_effort_wreckfish_activity$Year), FUN=sum, na.rm = TRUE)

# transform effort so that it is at a scale comparable to the one of the catches
yearly_longline_T_effort_wreckfish <- yearly_longline_effort_wreckfish
yearly_longline_T_effort_wreckfish$x <- yearly_longline_T_effort_wreckfish$x/10000


# plotting the data for wreckfish
# first graph catch (bars) and effort (line) per year
# second graph catch by year and subarea
ggplot() +
  geom_bar(data=yearly_global_catches_wreckfish, aes(x = Year, y = x, fill="Catch"), stat="identity") +
  #geom_bar(data=yearly_global_catches_subarea_wreckfish, stat="identity", fill="coral2") +
  geom_line(data=yearly_longline_T_effort_wreckfish, aes(x=Year,y=x, color="Effort"), inherit.aes = FALSE, size = 1) +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Longline effort (10 000 hooks)")) +
  labs(title="Yearly wreckfish catch in the SIOFA area", x="Year", y="Catch (t)") +
  theme_bw() +
  theme( axis.line.y.right = element_line(color = "blue"), 
         axis.ticks.y.right = element_line(color = "blue"),
         axis.text.y.right = element_text(color = "blue"), 
         axis.title.y.right = element_text(color = "blue"))+
  scale_fill_manual(values = c("Catch" = "coral2"), 
                    guide = guide_legend(title = "Fill", override.aes = list(linetype = "blank", shape = NA))) +
  scale_colour_manual(values = c("Effort" = "blue"),
                      guide = guide_legend(title = "Line", override.aes = list(linetype = "solid", shape = NA))) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)


ggsave("Overview/SIOFAcatches_effort_WRF_web.png", width = 10, height = 4, dpi = 150)  

ggplot(yearly_global_catches_subarea_wreckfish, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly wreckfish catch by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatches_WRF_subarea_web.png", width = 10, height = 4, dpi = 150)

# subset for tarakihi only data 
# !!!!! check well how the longline effort is calculated when missing data
yearly_global_catches_tarakihi <- filter(yearly_global_catches_species, (Species == "TAK"))

yearly_global_catches_subarea_tarakihi <- filter(yearly_global_catches_subarea_species, (Species == "TAK"))

yearly_longline_effort_tarakihi_activity <- filter(yearly_longline_effort_species_activity, (Species == "TAK"))

# toggle off to disaggregate effort per subarea
#yearly_longline_effort_tarakihi <- aggregate(yearly_longline_effort_tarakihi_activity$x, by=list(Year=yearly_longline_effort_tarakihi_activity$Year), FUN=sum, na.rm = TRUE)
yearly_longline_effort_tarakihi <- aggregate(yearly_longline_effort_tarakihi_activity$x, by=list(Year=yearly_longline_effort_tarakihi_activity$Year), FUN=sum, na.rm = TRUE)

# transform effort so that it is at a scale comparable to the one of the catches
yearly_longline_T_effort_tarakihi <- yearly_longline_effort_tarakihi
yearly_longline_T_effort_tarakihi$x <- yearly_longline_T_effort_tarakihi$x/10000

#yearly_longline_log_effort_tarakihi[,2] <- log10(yearly_longline_log_effort_tarakihi[2])
  
# plotting the data for tarakihi
# first graph catch (bars) and effort (line) per year
# second graph catch by year and subarea
ggplot() +
  geom_bar(data=yearly_global_catches_tarakihi, aes(x = Year, y = x, fill="Catch"), stat="identity") +
  #geom_bar(data=yearly_global_catches_subarea_tarakihi, stat="identity", fill="darkorange2") +
  geom_line(data=yearly_longline_T_effort_tarakihi, aes(x = Year, y = x, color="Effort"), inherit.aes = FALSE, size = 1) +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_y_continuous(limits = c(0,5.5), sec.axis = sec_axis(~., name = "Longline effort (10 000 hooks)")) +
  labs(title="Yearly tarakihi catch in the SIOFA area", x="Year", y="Catch (t)") +
  theme( axis.line.y.right = element_line(color = "blue"), 
         axis.ticks.y.right = element_line(color = "blue"),
         axis.text.y.right = element_text(color = "blue"), 
         axis.title.y.right = element_text(color = "blue"))+
  scale_fill_manual(values = c("Catch" = "darkorange2"), 
                    guide = guide_legend(title = "Fill", override.aes = list(linetype = "blank", shape = NA))) +
  scale_colour_manual(values = c("Effort" = "blue"),
                      guide = guide_legend(title = "Line", override.aes = list(linetype = "solid", shape = NA))) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)


ggsave("Overview/SIOFAcatches_effort_TAK_web.png", width = 10, height = 4, dpi = 150)  

ggplot(yearly_global_catches_subarea_tarakihi, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly tarakihi catch by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatches_TAK_subarea_web.png", width = 10, height = 4, dpi = 150)

## subset for shark only data 
# !!!!! here I use a new list of all sharks, screw coherence with previous editions
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

write.xlsx(shark_catch_subarea_table,"Overview/Tables/shark_catch_subarea_table.xlsx", overwrite = TRUE)

# plotting the data for sharks
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

ggsave("Overview/SIOFAcatches_effort_SHA_web.png", width = 10, height = 6, dpi = 150)  

ggplot(yearly_global_catches_subarea_sharks, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly shark catch by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatches_SHA_subarea_web.png", width = 10, height = 4, dpi = 150)

# subset for CMM 12 shark only data 
# !!!!! here I use Annex 1 of CMM 12 as a list of sharks

shark_species_CMM <- read_excel("protected_sharks.xlsx")

yearly_global_catches_sharks_CMM <- filter(yearly_global_catches_species, Species %in% shark_species_CMM$FAOcode)

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

ggsave("Overview/SIOFAcatches_effort_DWS_web.png", width = 10, height = 6, dpi = 150)  

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

ggsave("Overview/SIOFAcatches_DWS_subarea_web.png", width = 10, height = 4, dpi = 150)

# shark catches by gear
yearly_global_catches_species_gear <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year, Species=fishing$SpeciesCode, Gear=fishing$Gear), FUN=sum, na.rm = TRUE)
yearly_global_catches_sharks_gear <- filter(yearly_global_catches_species_gear, Species %in% shark_species$Code)

ggplot(data=yearly_global_catches_sharks_gear, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Gear), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly shark catch by species in the SIOFA area, by gear", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.8)

ggsave("Overview/SIOFAcatches_effort_SHA_gear_web.png", width = 10, height = 6, dpi = 150) 

# subset for all wreckfishes only data 
# !!!!! check well how the longline effort is calculated when missing data
yearly_global_catches_wreckfishes <- filter(yearly_global_catches_species, (Species == "WHA") | (Species == "WRF")  | (Species == "HAU"))

yearly_global_catches_subarea_wreckfishes <- filter(yearly_global_catches_subarea_species, (Species == "WHA") | (Species == "WRF") | (Species == "HAU"))

yearly_longline_effort_wreckfishes_activity <- filter(yearly_longline_effort_species_activity, (Species == "WHA") | (Species == "WRF") | (Species == "HAU"))

# toggle off to disaggregate effort per subarea
#yearly_longline_effort_wreckfishes <- aggregate(yearly_longline_effort_hapuku_activity$x, by=list(Year=yearly_longline_effort_hapuku_activity$Year), FUN=sum, na.rm = TRUE)
yearly_longline_effort_wreckfishes <- aggregate(yearly_longline_effort_wreckfishes_activity$x, by=list(Year=yearly_longline_effort_wreckfishes_activity$Year), FUN=sum, na.rm = TRUE)

# transform effort so that it is at a scale comparable to the one of the catches
yearly_longline_T_effort_wreckfishes <- yearly_longline_effort_wreckfishes
yearly_longline_T_effort_wreckfishes$x <- yearly_longline_T_effort_wreckfishes$x/10000

# plotting the data for wreckfishes
# first graph catch (bars) and effort (line) per year
# second graph catch by year and subarea

ggplot() +
  geom_bar(data=yearly_global_catches_wreckfishes, aes(x = Year, y = x, fill=Species), stat="identity") +
  #geom_bar(data=yearly_global_catches_subarea_hapuku, stat="identity", fill="chocolate2") +
  geom_line(data=yearly_longline_T_effort_wreckfishes, aes(x=Year,y=x, color="Effort"), inherit.aes = FALSE, size = 1) +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_y_continuous(limits = c(0,230), sec.axis = sec_axis(~., name = "Longline effort (10 000 hooks)")) +
  labs(title="Yearly hapuka catch in the SIOFA area", x="Year", y="Catch (t)") +
  theme_bw() +
  theme( axis.line.y.right = element_line(color = "blue"), 
         axis.ticks.y.right = element_line(color = "blue"),
         axis.text.y.right = element_text(color = "blue"), 
         axis.title.y.right = element_text(color = "blue"))+
  #scale_fill_manual(values = c("Catch" = "chocolate2"), 
  #                  guide = guide_legend(title = "Fill", override.aes = list(linetype = "blank", shape = NA))) +
  scale_colour_manual(values = c("Effort" = "blue"),
                      guide = guide_legend(title = "Line", override.aes = list(linetype = "solid", shape = NA))) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)


ggsave("Overview/SIOFAcatches_effort_HAU_web.png", width = 10, height = 4, dpi = 150)  

ggplot(yearly_global_catches_subarea_wreckfishes, aes(x = Year, y = x)) +
  geom_bar(aes(fill=SubArea), position="stack", stat="identity") +
  labs(title="Yearly hapuka catch by SIOFA subarea", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatches_HAU_subarea_web.png", width = 10, height = 4, dpi = 150)

# subset for catches by country data 
yearly_global_catches_country <- fishing
yearly_global_catches_country <- aggregate(fishing$CatchTon, by=list(Year=fishing$Year,CCP=fishing$CCPCode2), FUN=sum, na.rm = TRUE)

yearly_global_catches_country_order <- yearly_global_catches_country

# Change Chinese Taipei code position in graph
#yearly_global_catches_country_order$CCP <- gsub('CT', 'TPE', yearly_global_catches_country_order$CCP)
yearly_global_catches_country_order$CCP <- gsub('ESP', 'EU', yearly_global_catches_country_order$CCP)
yearly_global_catches_country_order$CCP <- gsub('FRA', 'EU', yearly_global_catches_country_order$CCP)
yearly_global_catches_country_order <- aggregate(yearly_global_catches_country_order$x, by=list(Year=yearly_global_catches_country_order$Year,CCP=yearly_global_catches_country_order$CCP), FUN=sum, na.rm = TRUE)

# Relevel group factor
yearly_global_catches_country_order <- yearly_global_catches_country_order [order(yearly_global_catches_country_order$CCP),]
yearly_global_catches_country_order$CCP <- factor(yearly_global_catches_country_order$CCP,  
                                                levels = c("AUS", "COK", "COM", "EU", "FR-OT", "JPN", "KOR", "MUS", "TPE", "THA"))

ggplot(yearly_global_catches_country_order, aes(x = Year, y = x)) +
  geom_bar(aes(fill=CCP), position="stack", stat="identity") +
  labs(title="Yearly total catch in the SIOFA area by CCP", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Overview/SIOFAcatches_CCPs_web.png", width = 10, height = 4, dpi = 150)


### UNSTANDARDIZED CPUEs
#orange roughy
#get the data ready to plot
names(yearly_global_catches_roughy)[names(yearly_global_catches_roughy) == "x"] <- "Catch"
names(yearly_trawl_effort_roughy)[names(yearly_trawl_effort_roughy) == "x"] <- "Effort"
CPUE_roughy <- full_join(yearly_global_catches_roughy, yearly_trawl_effort_roughy)
#calculate CPUE
CPUE_roughy <- CPUE_roughy %>% 
  mutate(CPUEs = Catch/Effort) %>% 
  dplyr::select(-Species) %>% 
  dplyr::select(-Catch) %>% 
  dplyr::select(-Effort)

#plot CPUEs
ggplot(data=CPUE_roughy, aes(x=Year,y=CPUEs), inherit.aes = FALSE, size = 1) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_y_continuous(limits=c(0, 8)) +
  labs(title="Unstandardised CPUEs for orange roughy in the SIOFA area", x="Year", y="CPUE (t/tow)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Overview/SIOFA_CPUEs_ORY_web.png", width = 10, height = 4, dpi = 150)  
write_xlsx(CPUE_roughy,"Overview/Tables/CPUE_roughy.xlsx")

#alfonsino
#get the data ready to plot
names(yearly_global_catches_alfonsino)[names(yearly_global_catches_alfonsino) == "x"] <- "Catch"
names(yearly_trawl_effort_alfonsino)[names(yearly_trawl_effort_alfonsino) == "x"] <- "Effort"
CPUE_alfonsino <- full_join(yearly_global_catches_alfonsino, yearly_trawl_effort_alfonsino)
#calculate CPUE
CPUE_alfonsino <- CPUE_alfonsino %>% 
  mutate(CPUEs = Catch/Effort) %>% 
  dplyr::select(-Catch) %>% 
  dplyr::select(-Effort)
#plot CPUEs
ggplot(data=CPUE_alfonsino, aes(x=Year,y=CPUEs), inherit.aes = FALSE, size = 1) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_y_continuous(limits=c(0, 7)) +
  labs(title="Unstandardised CPUEs for alfonsino in the SIOFA area", x="Year", y="CPUE (t/tow)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Overview/SIOFA_CPUEs_ALF_web.png", width = 10, height = 4, dpi = 150)  
write_xlsx(CPUE_alfonsino,"Overview/Tables/CPUE_alfonsino.xlsx")

#toothfish
#get the data ready to plot
names(yearly_global_catches_toothfish)[names(yearly_global_catches_toothfish) == "x"] <- "Catch"
names(yearly_longline_T_effort_toothfish)[names(yearly_longline_T_effort_toothfish) == "x"] <- "Effort"
yearly_global_catches_toothfish_T <- yearly_global_catches_toothfish %>% 
  group_by(Year = Year) %>%
  summarize(Catch = sum(Catch))
CPUE_toothfish <- full_join(yearly_global_catches_toothfish_T, yearly_longline_T_effort_toothfish)
#calculate CPUE
CPUE_toothfish <- CPUE_toothfish%>% 
  mutate(CPUEs = Catch/Effort) %>% 
  dplyr::select(-Catch) %>% 
  dplyr::select(-Effort)
#plot CPUEs
ggplot(data=CPUE_toothfish, aes(x=Year,y=CPUEs), inherit.aes = FALSE, size = 1) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_y_continuous(limits=c(0, 7)) +
  labs(title="Unstandardised CPUEs for toothfish in the SIOFA area", x="Year", y="CPUE (t/10 000 hooks)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Overview/SIOFA_CPUEs_TOP_web.png", width = 10, height = 4, dpi = 150)  
write_xlsx(CPUE_toothfish,"Overview/Tables/CPUE_toothfish.xlsx")

#oilfish
#get the data ready to plot
names(yearly_global_catches_oilfish)[names(yearly_global_catches_oilfish) == "x"] <- "Catch"
names(yearly_longline_T_effort_oilfish)[names(yearly_longline_T_effort_oilfish) == "x"] <- "Effort"
yearly_global_catches_oilfish_T <- yearly_global_catches_oilfish %>% 
  group_by(Year = Year) %>%
  summarize(Catch = sum(Catch))
CPUE_oilfish <- full_join(yearly_global_catches_oilfish_T, yearly_longline_T_effort_oilfish)
#calculate CPUE
CPUE_oilfish <- CPUE_oilfish%>% 
  mutate(CPUEs = Catch/Effort) %>% 
  dplyr::select(-Catch) %>% 
  dplyr::select(-Effort)
#remove inf and missing values
CPUE_oilfish <- CPUE_oilfish[is.finite((CPUE_oilfish$CPUEs)),]
#plot CPUEs
ggplot(data=CPUE_oilfish, aes(x=Year,y=CPUEs), inherit.aes = FALSE, size = 1) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_y_continuous(limits=c(0, 7)) +
  labs(title="Unstandardised CPUEs for oilfish in the SIOFA area", x="Year", y="CPUE (t/10 000 hooks)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Overview/SIOFA_CPUEs_OIL_web.png", width = 10, height = 4, dpi = 150)  
write_xlsx(CPUE_oilfish,"Overview/Tables/CPUE_oilfish.xlsx")

#tarakihi
#get the data ready to plot
names(yearly_global_catches_tarakihi)[names(yearly_global_catches_tarakihi) == "x"] <- "Catch"
names(yearly_longline_T_effort_tarakihi)[names(yearly_longline_T_effort_tarakihi) == "x"] <- "Effort"
CPUE_tarakihi <- full_join(yearly_global_catches_tarakihi, yearly_longline_T_effort_tarakihi)
#calculate CPUE
CPUE_tarakihi <- CPUE_tarakihi%>% mutate(CPUEs = Catch/Effort)
#plot CPUEs
ggplot(data=CPUE_tarakihi, aes(x=Year,y=CPUEs), inherit.aes = FALSE, size = 1) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_y_continuous(limits=c(0, 7)) +
  labs(title="Unstandardised CPUEs for tarakihi in the SIOFA area", x="Year", y="CPUE (t/10 000 hooks)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Overview/SIOFA_CPUEs_TAK_web.png", width = 10, height = 4, dpi = 150)  

#wreckfishes
#get the data ready to plot
yearly_global_catches_wreckfishes_aggr <- aggregate(yearly_global_catches_wreckfishes$x, by=list(Year=yearly_global_catches_wreckfishes$Year), FUN=sum, na.rm = TRUE)
names(yearly_global_catches_wreckfishes_aggr)[names(yearly_global_catches_wreckfishes_aggr) == "x"] <- "Catch"
names(yearly_longline_T_effort_wreckfishes)[names(yearly_longline_T_effort_wreckfishes) == "x"] <- "Effort"
CPUE_wreckfishes <- full_join(yearly_global_catches_wreckfishes_aggr, yearly_longline_T_effort_wreckfishes)
#calculate CPUE
CPUE_wreckfishes <- CPUE_wreckfishes%>% mutate(CPUEs = Catch/Effort)
#remove inf and missing values
CPUE_wreckfishes <- CPUE_wreckfishes[is.finite((CPUE_wreckfishes$CPUEs)),]
#plot CPUEs
ggplot(data=CPUE_wreckfishes, aes(x=Year,y=CPUEs), inherit.aes = FALSE, size = 1) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_y_continuous(limits=c(0, 7)) +
  labs(title="Unstandardised CPUEs for hapuka in the SIOFA area", x="Year", y="CPUE (t/10 000 hooks)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Overview/SIOFA_CPUEs_HAU_web.png", width = 10, height = 4, dpi = 150)  

#wreckfish
#get the data ready to plot
names(yearly_global_catches_wreckfish)[names(yearly_global_catches_wreckfish) == "x"] <- "Catch"
names(yearly_longline_T_effort_wreckfish)[names(yearly_longline_T_effort_wreckfish) == "x"] <- "Effort"
CPUE_wreckfish <- full_join(yearly_global_catches_wreckfish, yearly_longline_T_effort_wreckfish)
#calculate CPUE
CPUE_wreckfish <- CPUE_wreckfish%>% mutate(CPUEs = Catch/Effort)
#remove inf and missing values
CPUE_wreckfish <- CPUE_wreckfish[is.finite((CPUE_wreckfish$CPUEs)),]
#plot CPUEs
ggplot(data=CPUE_wreckfish, aes(x=Year,y=CPUEs), inherit.aes = FALSE, size = 1) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_y_continuous(limits=c(0, 7)) +
  labs(title="Unstandardised CPUEs for wreckfish in the SIOFA area", x="Year", y="CPUE (t/10 000 hooks)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Overview/SIOFA_CPUEs_WRF_web.png", width = 10, height = 4, dpi = 150)  

#hapuku
#get the data ready to plot
names(yearly_global_catches_hapuku)[names(yearly_global_catches_hapuku) == "x"] <- "Catch"
names(yearly_longline_T_effort_hapuku)[names(yearly_longline_T_effort_hapuku) == "x"] <- "Effort"
CPUE_hapuku <- full_join(yearly_global_catches_hapuku, yearly_longline_T_effort_hapuku)
#calculate CPUE
CPUE_hapuku <- CPUE_hapuku%>% mutate(CPUEs = Catch/Effort)
#plot CPUEs
ggplot(data=CPUE_hapuku, aes(x=Year,y=CPUEs), inherit.aes = FALSE, size = 1) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_y_continuous(limits=c(0, 7)) +
  labs(title="Unstandardised CPUEs for hapuku in the SIOFA area", x="Year", y="CPUE (t/10 000 hooks)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4)

ggsave("Overview/SIOFA_CPUEs_WHA_web.png", width = 10, height = 4, dpi = 150) 

###catch/bycatch ratio
#get the data ready to plot
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
#subtract total target catch from total catch per subarea
names(yearly_global_catches_subarea_target)[names(yearly_global_catches_subarea_target) == "x"] <- "TargetCatch"
names(yearly_global_catches_subarea)[names(yearly_global_catches_subarea) == "x"] <- "TotalCatch"
catch_bycatch_subarea <- full_join(yearly_global_catches_subarea_target, yearly_global_catches_subarea)
catch_bycatch_subarea <- catch_bycatch_subarea%>% mutate(NonTargetCatch = TotalCatch-TargetCatch)
catch_bycatch_subarea$TotalCatch <- NULL
#sharks vs other bycatch
yearly_global_catches_sharks_aggregated <- aggregate(yearly_global_catches_sharks$x, by=list(Year=yearly_global_catches_sharks$Year), FUN=sum, na.rm = TRUE)
names(yearly_global_catches_sharks_aggregated)[names(yearly_global_catches_sharks_aggregated) == "x"] <- "SharkBycatch"
catch_bycatch_sharks <- full_join(catch_bycatch, yearly_global_catches_sharks_aggregated)
catch_bycatch_sharks <- catch_bycatch_sharks%>% mutate(NonSharkBycatch = NonTargetCatch-SharkBycatch)
catch_bycatch_sharks$NonTargetCatch <- NULL

##discards
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
catch_bycatch_discards <- catch_bycatch_discards %>% pivot_longer(!Year, names_to = "Catch", values_to = "t")
catch_bycatch <- catch_bycatch %>% pivot_longer(!Year, names_to = "Catch", values_to = "t")
catch_bycatch_subarea <- catch_bycatch_subarea %>% pivot_longer(cols = "TargetCatch":"NonTargetCatch", names_to = "Catch", values_to = "t")
catch_bycatch_sharks <- catch_bycatch_sharks %>% pivot_longer(cols = "TargetCatch":"NonSharkBycatch", names_to = "Catch", values_to = "t")

## plot tables of target and non-target catch, sharks highlighted 
catch_bycatch_sharks_table <- catch_bycatch_sharks  %>%
  pivot_wider(names_from = Catch, values_from = t) %>%
  mutate_at(vars(TargetCatch, SharkBycatch, NonSharkBycatch), funs(round(., 1))) %>%
  rename('Target catch (t)' = TargetCatch,
         'Bycatch (t)' = NonSharkBycatch,
         'Shark catch (t)' = SharkBycatch) 

write_xlsx(catch_bycatch_sharks_table,"Overview/Tables/catch_bycatch_sharks_table.xlsx")

## plot histogram of target catch and bycatch per year 
#totals
ggplot(catch_bycatch, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly target catch/bycatch in the SIOFA area (absolute)", x="Year", y="Total catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatch_nontargetcatch_web.png", width = 10, height = 4, dpi = 150)

#percentage
ggplot(catch_bycatch, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly target catch/bycatch in the SIOFA area (relative)", x="Year", y="Total catch proportion") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatch_nontargetcatch_fill_web.png", width = 10, height = 4, dpi = 150)

## plot histogram of bycatch per year and subarea
#totals
bycatch_subarea <- catch_bycatch_subarea %>%
  filter(Catch == "NonTargetCatch")

ggplot(bycatch_subarea, aes(x = Year, y = t, fill=SubArea)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly bycatch in SIOFA subareas (absolute)", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAnontargetcatch_subarea_web.png", width = 10, height = 4, dpi = 150)

#percentage
ggplot(bycatch_subarea, aes(x = Year, y = t, fill=SubArea)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly bycatch in SIOFA subareas (relative)", x="Year", y="Catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAnontargetcatch_subarea__fill_web.png", width = 10, height = 4, dpi = 150)

## plot histogram of shark bycatch and non-target catch per year 
#totals
ggplot(catch_bycatch_sharks, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly target catch/bycatch in the SIOFA area (absolute), sharks highlighted", x="Year", y="Total catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Shark catch", "Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatch_nontargetcatch_sharks_web.png", width = 10, height = 4, dpi = 150)

#percentage
ggplot(catch_bycatch_sharks, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly target catch/bycatch in the SIOFA area (relative), sharks highlighted", x="Year", y="Total catch proportion") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Shark catch", "Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatch_nontargetcatch_sharks_fill_web.png", width = 10, height = 4, dpi = 150)

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

ggsave("Overview/SIOFAtargetcatch_subarea_web.png", width = 10, height = 4, dpi = 150)

## plot non target catch by subarea
ggplot(bycatch_subarea, aes(x = Year, y = t, fill = SubArea)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly bycatch in SIOFA subareas", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Set3") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAnontargetcatch_subarea_web.png", width = 10, height = 4, dpi = 150)


## plot histogram of bycatch and discards per year 
#totals
ggplot(catch_bycatch_discards, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly discarded catch as a fraction of bycatch in the SIOFA area (absolute)", x="Year", y="Total catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Discarded catch", "Bycatch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatch_nontargetcatch_discards_web.png", width = 10, height = 4, dpi = 150)

#percentage
ggplot(catch_bycatch_discards, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly discarded catch as a fraction of bycatch in the SIOFA area (relative)", x="Year", y="Catch proportion") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Discarded catch", "Bycatch", "Target catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatch_nontargetcatch_discards_fill_web.png", width = 10, height = 4, dpi = 150)

#discards by species
ggplot(data=yearly_global_catches_discards_species, aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  labs(title="Yearly discards by species in the SIOFA area", x="Year", y="Discards (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.6)

ggsave("Overview/SIOFAdiscards_species_web.png", width = 15, height = 6, dpi = 150) 

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

ggsave("Overview/SIOFAdiscards_species_top5_web.png", width = 10, height = 6, dpi = 150) 

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

write_xlsx(targetcatch_subarea_table,"OVerview/Tables/targetcatch_subarea_table.xlsx")

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

write_xlsx(nontargetcatch_subarea_table,"Overview/Tables/nontargetcatch_subarea_table.xlsx")
## catch/bycatch in management units

# load ORY management units boundaries
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
  geom_sf_label(data = ORY_MUs, aes(fill = NULL, label = name), size = 2.5, col = "black", label.size = 0) +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA ORY Assessment Areas") +
  coord_sf(xlim = c(25, 65), ylim = c(-25, -50), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("Overview/SIOFA_ORY_MUs_web.png", width = 10, height = 7, dpi = 150)

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
yearly_ORY_MUs_catches_target <- filter(yearly_MUs_global_catches_ORY_MUs, Species %in% target_species$"FAO Code")
yearly_ORY_MUs_catches_target <- aggregate(yearly_ORY_MUs_catches_target$x, by=list(Year=yearly_ORY_MUs_catches_target$Year), FUN=sum, na.rm = TRUE)

#calculate only target catch per year and MU
yearly_ORY_MUs_catches_MU_target <- filter(yearly_MUs_global_catches_ORY_MUs, Species %in% target_species$"FAO Code")
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
  labs(title="Yearly target catch/bycatch in all SIOFA ORY assessment areas (absolute)", x="Year", y="Total catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("All other species catch", "ORY catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatch_nontargetcatch_ORY_MUs_web.png", width = 10, height = 4, dpi = 150)

#percentage
ggplot(catch_bycatch_ORY_MUs, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly target catch/bycatch in all SIOFA ORY assessment areas (relative)", x="Year", y="Total catch proportion") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("All other species catch", "ORY catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatch_nontargetcatch_ORY_MUs_fill_web.png", width = 10, height = 4, dpi = 150)

## plot histograms of target and non-target catch in MUs per year

#totals target
ggplot(catch_bycatch_by_ORY_MU, aes(x = Year, y = t)) +
  geom_bar(aes(fill=MU, y=TargetCatch), position="stack", stat="identity") +
  labs(title="Yearly catch of target species in SIOFA ORY assessment areas (absolute)", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAtargetcatch_ORY_MUs_web.png", width = 10, height = 4, dpi = 150)

#percentage of target catch in different MUs
ggplot(catch_bycatch_by_ORY_MU, aes(x = Year, y = t)) +
  geom_bar(aes(fill=MU, y=TargetCatch), position="fill", stat="identity") +
  labs(title="Yearly catch of target species in SIOFA ORY assessment areas (relative)", x="Year", y="Catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAtargetcatch_ORY_MUs_fill_web.png", width = 10, height = 4, dpi = 150)


#totals non-target
ggplot(catch_bycatch_by_ORY_MU, aes(x = Year, y = t)) +
  geom_bar(aes(fill=MU, y=NonTargetCatch), position="stack", stat="identity") +
  labs(title="Yearly bycatch in SIOFA ORY assessment areas (absolute)", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAnontargetcatch_ORY_MUs_web.png", width = 10, height = 4, dpi = 150)

#percentage of non-target catch in different MUs
ggplot(catch_bycatch_by_ORY_MU, aes(x = Year, y = t)) +
  geom_bar(aes(fill=MU, y=NonTargetCatch), position="fill", stat="identity") +
  labs(title="Yearly bycatch in SIOFA ORY assessment areas (relative)", x="Year", y="Catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAnontargetcatch_ORY_MUs_fill_web.png", width = 10, height = 4, dpi = 150)

# load TOP management units boundaries
TOP_MUs <- st_read('TOP_management_areas_2023')
st_crs(TOP_MUs) <- 4326

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
  ggtitle("SIOFA TOP Management Units") +
  coord_sf(xlim = c(25, 85), ylim = c(-30, -55), expand = TRUE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("Overview/SIOFA_TOP_MUs_web.png", width = 10, height = 6, dpi = 150)

# intersect fishing events with MUs
# Overlay points and extract just the subarea column 

fishing_within_TOP_MUs <- st_join(fishing_spatial, left = FALSE, TOP_MUs)

# create a data frame with only total catches in MUs, retaining MUs details
yearly_MUs_global_catches_TOP_MUs <- aggregate(fishing_within_TOP_MUs$CatchTon, by=list(Year=fishing_within_TOP_MUs$Year, MU=fishing_within_TOP_MUs$area_name, Species=fishing_within_TOP_MUs$SpeciesCode), FUN=sum, na.rm = TRUE)

# create a data frame with only total catches in all MUs, aggregated by year
yearly_TOP_MUs_global_catches <- aggregate(fishing_within_TOP_MUs$CatchTon, by=list(Year=fishing_within_TOP_MUs$Year), FUN=sum, na.rm = TRUE)

# create a data frame with only total catches in all MUs, aggregated by year AND MU
yearly_TOP_MUs_total_catches_MU <- aggregate(fishing_within_TOP_MUs$CatchTon, by=list(Year=fishing_within_TOP_MUs$Year, MU=fishing_within_TOP_MUs$area_name), FUN=sum, na.rm = TRUE)

#calculate only target catch per year within all MUs
yearly_TOP_MUs_catches_target <- filter(yearly_MUs_global_catches_TOP_MUs, Species %in% target_species$"FAO Code")
yearly_TOP_MUs_catches_target <- aggregate(yearly_TOP_MUs_catches_target$x, by=list(Year=yearly_TOP_MUs_catches_target$Year), FUN=sum, na.rm = TRUE)

#calculate only target catch per year and MU
yearly_TOP_MUs_catches_MU_target <- filter(yearly_MUs_global_catches_TOP_MUs, Species %in% target_species$"FAO Code")
yearly_TOP_MUs_catches_MU_target <- aggregate(yearly_TOP_MUs_catches_MU_target$x, by=list(Year=yearly_TOP_MUs_catches_MU_target$Year,MU=yearly_TOP_MUs_catches_MU_target$MU), FUN=sum, na.rm = TRUE)

#subtract total target catch from total catch within MUs
names(yearly_TOP_MUs_catches_target)[names(yearly_TOP_MUs_catches_target) == "x"] <- "TargetCatch"
names(yearly_TOP_MUs_global_catches)[names(yearly_TOP_MUs_global_catches) == "x"] <- "TotalCatch"
catch_bycatch_TOP_MUs <- full_join(yearly_TOP_MUs_global_catches, yearly_TOP_MUs_catches_target)
catch_bycatch_TOP_MUs <- catch_bycatch_TOP_MUs %>%  mutate(NonTargetCatch = TotalCatch-TargetCatch)
catch_bycatch_TOP_MUs$TotalCatch <- NULL

#subtract total target catch from total catch within EACH MU
names(yearly_TOP_MUs_catches_MU_target)[names(yearly_TOP_MUs_catches_MU_target) == "x"] <- "TargetCatch"
names(yearly_TOP_MUs_total_catches_MU)[names(yearly_TOP_MUs_total_catches_MU) == "x"] <- "TotalCatch"
catch_bycatch_by_TOP_MU <- full_join(yearly_TOP_MUs_catches_MU_target, yearly_TOP_MUs_total_catches_MU)
catch_bycatch_by_TOP_MU <- catch_bycatch_by_TOP_MU%>% mutate(NonTargetCatch = TotalCatch-TargetCatch)
catch_bycatch_by_TOP_MU$TotalCatch <- NULL

#transform data for easier plotting
catch_bycatch_TOP_MUs <- catch_bycatch_TOP_MUs %>% pivot_longer(!Year, names_to = "Catch", values_to = "t")
#catch_bycatch_by_MU <- catch_bycatch_by_MU %>% pivot_longer(cols = "TargetCatch":"NonTargetCatch", names_to = "Catch", values_to = "t")

## plot histograms of target/non-target catch in MUs per year 
#totals
ggplot(catch_bycatch_TOP_MUs, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Yearly target catch/bycatch in all SIOFA TOP management units (absolute)", x="Year", y="Total catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Target Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatch_nontargetcatch_TOP_MUs_web.png", width = 10, height = 4, dpi = 150)

#percentage
ggplot(catch_bycatch_TOP_MUs, aes(x = Year, y = t, fill=Catch)) +
  geom_bar(position="fill", stat="identity") +
  labs(title="Yearly target catch/bycatch in all SIOFA TOP management units (relative)", x="Year", y="Catch proportion") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  scale_fill_hue(labels = c("Bycatch", "Catch")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatch_nontargetcatch_TOP_MUs_fill_web.png", width = 10, height = 4, dpi = 150)

## plot histograms of target and non-target catch in MUs per year

#totals target
ggplot(catch_bycatch_by_TOP_MU, aes(x = Year, y = t)) +
  geom_bar(aes(fill=MU, y=TargetCatch), position="stack", stat="identity") +
  labs(title="Yearly target catch in SIOFA TOP management units (absolute)", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAtargetcatch_TOP_MUs_web.png", width = 10, height = 4, dpi = 150)

#totals non-target
ggplot(catch_bycatch_by_TOP_MU, aes(x = Year, y = t)) +
  geom_bar(aes(fill=MU, y=NonTargetCatch), position="stack", stat="identity") +
  labs(title="Yearly bycatch in SIOFA TOP management units (absolute)", x="Year", y="Catch (t)") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAnontargetcatch_TOP_MUs_web.png", width = 10, height = 4, dpi = 150)

#percentage of non-target catch in different MUs
ggplot(catch_bycatch_by_TOP_MU, aes(x = Year, y = t)) +
  geom_bar(aes(fill=MU, y=NonTargetCatch), position="fill", stat="identity") +
  labs(title="Yearly bycatch in SIOFA TOP management units (relative)", x="Year", y="Catch proportion") +
  scale_fill_brewer(type = "seq", palette = "Spectral") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAnontargetcatch_TOP_MUs_fill_web.png", width = 10, height = 4, dpi = 150)


# check for issues in confidentiality (more than 1 vessel per year in a MUs)?

### Fishing activities in protected areas

## read in the protected areas

BPAs <- st_read('BPAs')

# plot world map and add SIOFA BPAs + siofa fishing events just to check
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf(data = BPAs, fill = "orchid2", color = "black") +
  #geom_sf(data = fishing_spatial, color = 'black', cex = .1) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 2, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, -2, -6)) +
  coord_sf(xlim = c(30, 100), ylim = c(-25, -50), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA Interim Protected Areas") +
  geom_sf_label(data = BPAs, aes(fill = NULL, label = value), size = 2.5, col = "black", label.size = 0, nudge_y=c(1.2,1.2,1.2,1.2,1.2)) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("Overview/SIOFA_BPAs_web.png", width = 7, height = 4, dpi = 150)

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = grid, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3")) +
  #geom_sf(data = fishing_spatial, color = 'black', cex = .1) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,1,2, 0, 0, 0, 0, -6)) +
  geom_sf(data = BPAs, fill = "orchid2", color = "black") +
  #geom_sf(data = fishing_spatial, color = 'black', cex = .1) +
  #geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, 0, 0, 0, 2, -6), nudge_x=c(0,-2.5,0, 3, 0, 0, -2, -6)) +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA Agreement area and Interim SIOFA Protected Areas") +
  geom_sf_label(data = BPAs, aes(fill = NULL, label = value), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2.5,-2.5,-2.5,-2.5,-2.5), nudge_x=c(0,0,0,0,6)) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("Overview/SIOFA_BPAs_full_web.png", width = 7, height = 5, dpi = 150)



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
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatch_BPAs_web.png", width = 10, height = 5, dpi = 150)

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

ggsave("Overview/SIOFAgear_BPAs_web.png", width = 10, height = 4, dpi = 150)


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

ggsave("Overview/SIOFAgear_VMEs_web.png", width = 10, height = 4, dpi = 150)

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

ggsave("Overview/SIOFAcatch_VMEs_web.png", width = 10, height = 4, dpi = 150)

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
  ggtitle("SIOFA VME incidental catches (2013-2020)") +
  scale_color_discrete(name = "Taxonomic group") +
  coord_sf(xlim = c(25, 125), ylim = c(15, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("Overview/SIOFAmap_VMEa_web.png", width = 10, height = 7, dpi = 150)

# table of VME bycatch by species and gear

#Species/Gear/Total weight (kg)
VME_bycatch_table <- aggregate(VME_bycatch_groomed$Weight, by=list(Taxon=VME_bycatch_groomed$speciesScientificName, Gear=VME_bycatch_groomed$Gear), FUN=sum, na.rm = TRUE)  

VME_bycatch_table <-  VME_bycatch_table  %>%
  pivot_wider(names_from = Gear, values_from = x) 
  
write_xlsx(VME_bycatch_table,"Overview/Tables/VME_bycatch_table.xlsx")

# first graph catch (bars) per species and year, top5 species, all others grouped
# second graph catch by year and subarea, last 5 years

sort_species <- aggregate(yearly_global_catches_species$x, by=list(Species=yearly_global_catches_species$Species), FUN=sum, na.rm = TRUE)

sort_species <- arrange(sort_species, desc(x)) 

top5_species <- sort_species %>% slice(1:10)
other_species <- sort_species %>% slice(11:295)


ggplot(data= subset(yearly_global_catches_species, Species %in% top5_species$Species), aes(x = Year, y = x)) +
  geom_bar(aes(fill=Species), position="stack", stat="identity") +
  geom_bar(data= subset(yearly_global_catches_species, Species %in% other_species$Species), aes(x = Year, y = x, fill="'Other species'"), position="stack", stat="identity") +
  theme_bw() +
  scale_x_continuous(limits=c(2016, 2023)) +
  labs(title="Yearly catch and bycatch by species in the SIOFA area, top 10 species", x="Year", y="Catch (t)") +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.8)

ggsave("Overview/SIOFAcatches_species_web.png", width = 10, height = 6, dpi = 150)  

# make a table with Total weight (kg)	and number of Occurrences
VME_table <- VME_bycatch %>%
  group_by(FAOcode,speciesScientificName) %>%
  summarise('Total weight (kg)' = sum(Weight),n = n()) %>%
  rename('Scientific name'=speciesScientificName) %>%
  rename('FAO code'=FAOcode) %>%
  rename('Occurrences'=n) 

write_xlsx(VME_table,"Overview/Tables/VME_bycatch_summary.xlsx")


## additional figure with total yearly catch for high value species and everything else lumped

yearly_global_catches_species_special_TOT <- yearly_global_catches_species %>%
  filter(Species == "TOP" | Species == "TOA" ) %>%
  dplyr::select(-Species) %>%
  group_by(Year) %>%
  dplyr::summarise_all(sum) %>%
  mutate(Species=c("TOT"), .after=Year) #%>%
 # mutate(Order=c("3"), .after=Species)
yearly_global_catches_species_special_ALF <- yearly_global_catches_species %>%
  filter(Species == "ALF" | Species == "BYS" | Species == "BRX" | Species == "BYX" | Species == "BXD" ) %>%
  dplyr::select(-Species) %>%
  group_by(Year) %>%
  dplyr::summarise_all(sum) %>%
  mutate(Species=c("ALF"), .after=Year) #%>%
 # mutate(Order=c("1"), .after=Species)
yearly_global_catches_species_special_ORY <- yearly_global_catches_species %>%
  filter(Species == "ORY" | Species == "HPR" | Species == "FSZ") %>%
  dplyr::select(-Species) %>%
  group_by(Year) %>%
  dplyr::summarise_all(sum) %>%
  mutate(Species=c("ORY"), .after=Year) #%>%
 # mutate(Order=c("2"), .after=Species)
yearly_global_catches_species_special_HAU <- yearly_global_catches_species %>%
  filter(Species == "WHA" | Species == "WRF"  | Species == "HAU") %>%
  dplyr::select(-Species) %>%
  group_by(Year) %>%
  dplyr::summarise_all(sum) %>%
  mutate(Species=c("HAU"), .after=Year)

yearly_global_catches_species_special <- 
  rbind(yearly_global_catches_species_special_TOT,
        yearly_global_catches_species_special_ALF, 
        yearly_global_catches_species_special_ORY)

yearly_global_catches_species_other <- yearly_global_catches_species %>%
  filter(Species != "TOP", Species != "TOA", 
           Species != "ALF", Species != "BYS", Species != "BRX", Species != "BYX", Species != "BXD",
           Species != "ORY", Species != "HPR", Species != "FSZ")
yearly_global_catches_species_other <- yearly_global_catches_species_other %>%
  dplyr::select(-Species) %>%
  group_by(Year) %>%
  dplyr::summarise_all(sum) %>%
  mutate(Species=c('Other species'),
         .after=Year) #%>%
 # mutate(Order=c("4"), .after=Species)

common_plot <-rbind(yearly_global_catches_species_special,
                    yearly_global_catches_species_other)
common_plot$Species <- factor(common_plot$Species,  
                      levels = c("ALF", "ORY", "TOT", "Other species"))

ggplot()+
  #geom_bar(data=common_plot, aes(x = Year, y = x), stat="identity", position="stack") +
  geom_bar(data= common_plot, aes(x = Year, y = x, fill=Species), stat="identity",  position="stack") +
  labs(title="Yearly total catch in the SIOFA area - high value species vs other species", x="Year", y="Catch (t)") +
  theme_bw() +
  scale_x_continuous(limits=c(2012, 2023)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("Overview/SIOFAcatches_special_web.png", width = 10, height = 4, dpi = 150)  

## Analysis of 5-year mean value of catches for HS limits
# !!!! need to change value of years from 4 to 5 in final version

avg_ORY_2018_2022 <- filter(yearly_global_catches_species_special_ORY, (Year >= 2018))
avg_ORY_2018_2022 <- sum(avg_ORY_2018_2022$x)/5
avg_ORY_2017_2022 <- filter(yearly_global_catches_species_special_ORY, (Year >= 2017))
avg_ORY_2017_2022 <- sum(avg_ORY_2017_2022$x)/6

avg_ALF_2018_2022 <- filter(yearly_global_catches_species_special_ALF, (Year >= 2018))
avg_ALF_2018_2022 <- sum(avg_ALF_2018_2022$x)/5

avg_TOT_2018_2022 <- filter(yearly_global_catches_species_special_TOT, (Year >= 2018))
avg_TOT_2018_2022 <- sum(avg_TOT_2018_2022$x)/5

avg_OIL_2018_2022 <- filter(yearly_global_catches_oilfish, (Year >= 2018) | (Species == "OIL"))
avg_OIL_2018_2022 <- sum(avg_OIL_2018_2022$Catch)/5

avg_LEC_2018_2022 <- filter(yearly_global_catches_oilfish, (Year >= 2018) | (Species == "LEC"))
avg_LEC_2018_2022 <- sum(avg_LEC_2018_2022$Catch)/5

avg_HAU_2018_2022 <- filter(yearly_global_catches_species_special_HAU, (Year >= 2018))
avg_HAU_2018_2022 <- sum(avg_HAU_2018_2022$x)/5

## toothfish tagging 

# load data matrix
TOP_tags <- read_excel("SIOFA-all-tagging-data-2023.xlsx", col_types = c("numeric", "text", "text", "text", "numeric", "numeric", "numeric","numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","text","text","numeric","text","text","text","numeric","text"))

# assign data to subareas
TOP_tags_spatial <- filter(TOP_tags, (!is.na(TOP_tags$follSettingStartLongitude)))
TOP_tags_spatial <- st_as_sf(TOP_tags_spatial, coords = c("follSettingStartLongitude", "follSettingStartLatitude"), crs = 4326) 
class(TOP_tags_spatial)

TOP_tags_spatial_subareas <- st_join(TOP_tags_spatial, left = FALSE, fishing_boundaries)

# output data by fate and subarea
yearly_TOP_tags <- TOP_tags_spatial_subareas %>%
  group_by(year, tagFate, SubAreaNo) %>%
  summarise(n = n())

write_xlsx(yearly_TOP_tags,"Overview/Tables/yearly_TOP_tags.xlsx")

# output data for tags released in SIOFA
yearly_TOP_tags_releases <- as.data.frame(yearly_TOP_tags)
yearly_TOP_tags_releases <- yearly_TOP_tags_releases %>% dplyr::select(-geometry)
yearly_TOP_tags_releases <- yearly_TOP_tags_releases %>%
  filter(tagFate == 'Released') %>%
  rename(Year=year) %>%
  dplyr::select(-tagFate) %>%
  pivot_wider(names_from = SubAreaNo, values_from = n) %>%
  replace(is.na(.), 0) %>%
  rename('SIOFA Subarea 3b'= '3b') %>%
  rename('SIOFA Subarea 7'= '7') 
yearly_TOP_tags_releases <-  yearly_TOP_tags_releases[, c(1, 3, 2)]
sums_releases_cols <- colSums(yearly_TOP_tags_releases)
yearly_TOP_tags_releases <- rbind(yearly_TOP_tags_releases,sums_releases_cols)
yearly_TOP_tags_releases <- yearly_TOP_tags_releases %>%
  mutate(Total=rowSums(dplyr::select(.,-Year))) 
write_xlsx(yearly_TOP_tags_releases,"Overview/Tables/yearly_TOP_tags_releases.xlsx")

## analyze toothfish tags release/recaptures in CCAMLR/SIOFA
# read data
toothfish_tags <- read_excel("CCAMLR_Tags-2023.xlsx")
toothfish_tags_spatial_release <- st_as_sf(toothfish_tags, coords = c("longitude_release", "latitude_release"), crs = 4326) 
toothfish_tags_spatial_recapture <- st_as_sf(toothfish_tags, coords = c("longitude_recapture", "latitude_recapture"), crs = 4326) 

# check for recapture locations within SIOFA subareas 
# individuals released in SIOFA and recaptured in SIOFA
toothfish_tags_release <- st_join(toothfish_tags_spatial_release, left = FALSE, fishing_boundaries["SubAreaNo"])
toothfish_tags_release <- toothfish_tags_release %>%
  group_by(year_recapture, SubAreaNo) %>%
  summarise(n = n())
# individuals released in CCAMLR and recaptured in SIOFA
toothfish_tags_recapture <- st_join(toothfish_tags_spatial_recapture, left = FALSE, fishing_boundaries["SubAreaNo"])
toothfish_tags_recapture <- toothfish_tags_recapture %>%
  group_by(year_recapture, SubAreaNo) %>%
  summarise(n = n())

# produce a table of recaptures, taking into account both releases in SIOFA and in CCAMLR
# need to get data from the other tags that were not recaptured across SIOFA/CCAMLR
# take out geometry
toothfish_tags_recapture <- as.data.frame(toothfish_tags_recapture)
toothfish_tags_recapture <- toothfish_tags_recapture %>% dplyr::select(-geometry)
toothfish_tags_release <- as.data.frame(toothfish_tags_release)
toothfish_tags_release <- toothfish_tags_release %>% dplyr::select(-geometry)
#reshape matrices
toothfish_tags_release <- toothfish_tags_release %>%
  pivot_wider(names_from = SubAreaNo, values_from = n) %>%
  replace(is.na(.), 0)  %>%
  rename('SIOFA Subarea 3b'= '3b') %>%
  rename('SIOFA Subarea 7'= '7') %>%
  rename(Year = year_recapture)
toothfish_tags_recapture <- toothfish_tags_recapture %>%
  pivot_wider(names_from = SubAreaNo, values_from = n) %>%
  replace(is.na(.), 0)  %>%
  rename('SIOFA Subarea 3b'= '3b') %>%
  rename('SIOFA Subarea 7'= '7') %>%
  rename(Year = year_recapture)

# prepare the tag recapture data within SIOFA
yearly_TOP_tags_recapture <- as.data.frame(yearly_TOP_tags)
yearly_TOP_tags_recapture <- yearly_TOP_tags_recapture %>% dplyr::select(-geometry)
yearly_TOP_tags_recapture <- yearly_TOP_tags_recapture %>%
  filter(tagFate == 'Recovered') %>%
  rename(Year=year) %>%
  dplyr::select(-tagFate) %>%
  pivot_wider(names_from = SubAreaNo, values_from = n) %>%
  replace(is.na(.), 0) %>%
  rename('SIOFA SubArea 3b'= '3b') %>%
  rename('SIOFA SubArea 7'= '7') 
yearly_TOP_tags_recapture <-  yearly_TOP_tags_recapture[, c(1, 3, 2)]

# now prepare the CCAMLR/SIOFA recaptures
# compare tags released in SIOFA from those recaptured in CCAMLR (get n of tags from CCAMLR)
yearly_TOP_tags_recapture_CCAMLR <- full_join(toothfish_tags_release, toothfish_tags_recapture ,by = c("Year"), all = TRUE)
yearly_TOP_tags_recapture_CCAMLR <- yearly_TOP_tags_recapture_CCAMLR %>%
  replace(is.na(.), 0) %>%
  arrange(Year)  
yearly_TOP_tags_recapture_CCAMLR[,6] <- yearly_TOP_tags_recapture_CCAMLR[,5]-yearly_TOP_tags_recapture_CCAMLR[,2] 
yearly_TOP_tags_recapture_CCAMLR[,7] <- yearly_TOP_tags_recapture_CCAMLR[,4]-yearly_TOP_tags_recapture_CCAMLR[,3] 
colnames(yearly_TOP_tags_recapture_CCAMLR)[6] = "CCAMLR tags recaptured in SIOFA Subarea 3b" 
colnames(yearly_TOP_tags_recapture_CCAMLR)[7] = "CCAMLR tags recaptured in SIOFA Subarea 7" 
yearly_TOP_tags_recapture_CCAMLR <- yearly_TOP_tags_recapture_CCAMLR %>%
  dplyr::select('Year','CCAMLR tags recaptured in SIOFA Subarea 3b', 'CCAMLR tags recaptured in SIOFA Subarea 7') 

# Join SIOFA tags and CCAMLR tags recaptures
# rename columns
yearly_TOP_tags_recapture <- yearly_TOP_tags_recapture %>%
  rename('All recaptures in SIOFA Subarea 3b'='SIOFA SubArea 3b') %>%
  rename('All recaptures in SIOFA Subarea 7'='SIOFA SubArea 7')
#join + write
yearly_TOP_tags_recaptures <- full_join(yearly_TOP_tags_recapture, yearly_TOP_tags_recapture_CCAMLR ,by = c("Year"), all = TRUE)
yearly_TOP_tags_recaptures <- yearly_TOP_tags_recaptures %>%
  replace(is.na(.), '0')

sums_recaptures_cols <- colSums(yearly_TOP_tags_recaptures)
yearly_TOP_tags_recaptures <- rbind(yearly_TOP_tags_recaptures,sums_recaptures_cols)
yearly_TOP_tags_recaptures <- yearly_TOP_tags_recaptures %>%
  mutate(Total=rowSums(dplyr::select(.,-Year))) 

write_xlsx(yearly_TOP_tags_recaptures,"Overview/Tables/yearly_TOP_tags_recaptures.xlsx")

# calculate growth between release and recapture
# calculate distances between release and recapture points

toothfish_tags <- toothfish_tags %>%
  mutate(growth = length_at_recapture-length_at_release)

distances <- st_distance(toothfish_tags_spatial_release, toothfish_tags_spatial_recapture)
distances <- diag(distances)
distances <- as.numeric(unlist(distances))
distances <- data.frame(distances)
distances$ID <- toothfish_tags$ID
distances <- distances[,c("ID","distances")]

# make a table for tags recaptured across SIOFA/CCAMLR
table <- left_join(toothfish_tags,distances)
table <- table %>%
  mutate('Distance (km)'=distances/1000) %>%
  rename('Tag link score' = taglink_score) %>%
  rename('Growth (cm)' = growth) %>%
  rename('Release year' = year_release) %>%
  rename('Length at release (cm)' = length_at_release) %>%
  mutate('Weight at release (kg)' = weight_at_release/1000) %>%
  rename('Recapture year' = year_recapture) %>%
  rename('Length at recapture (cm)' = length_at_recapture) %>%
  mutate('Weight at recapture (kg)' = weight_at_recapture/1000) %>%
  mutate(Date_at_release_decimal=decimal_date(as_date(Date_at_release))) %>%
  mutate(Date_at_recapture_decimal=decimal_date(as_date(Date_at_recapture))) %>%
  mutate('Years at liberty'=Date_at_recapture_decimal-Date_at_release_decimal) %>%
  mutate_at(vars('Growth (cm)', 'Length at release (cm)',
                 'Weight at release (kg)', 'Length at recapture (cm)',
                 'Weight at recapture (kg)', 'Distance (km)', 'Years at liberty')
            , funs(round(., 1))) %>%
  dplyr::select('Release year','Length at release (cm)','Weight at release (kg)',
                'Recapture year', 'Length at recapture (cm)', 'Weight at recapture (kg)',
                'Growth (cm)', 'Years at liberty', 'Distance (km)')  #need to extract only relevant columns and then output everything as a table
write_xlsx(table,"Overview/Tables/CCAMLR_tags.xlsx")

# plot lines between release and recapture points
lines <- st_sfc(mapply(function(a,b)
{st_cast(st_union(a,b),"LINESTRING")}, 
toothfish_tags_spatial_release$geometry, toothfish_tags_spatial_recapture$geometry, SIMPLIFY=FALSE))
lines <- st_as_sf(lines, crs = 4326)

CCAMLR$GAR_Name <- paste0("CCAMLR ", CCAMLR$GAR_Name)
fishing_boundaries$SubAreaNo <- paste0("SIOFA Subarea ", fishing_boundaries$SubAreaNo)

ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  #geom_sf(data = grid_subset, color = "white") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = NA, color = "black") +
  geom_sf(data = CCAMLR, fill = NA, color = "black") +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, fill= brewer.pal(n=9, name = "Set3"), col = "black", label.size = 0, nudge_y=c(0,2,2.5, -1.7, -14, 0, 0, -12), nudge_x=c(0,-2.5,0, 1, 0, 0, -1, -15)) +
  geom_sf(data = TOP_MUs, fill = "orchid2", color = "black") +
  geom_sf_label(data = TOP_MUs, aes(fill = NULL, label = area_name), size = 2.5, col = "black", label.size = 0, nudge_y=c(1.2,2), nudge_x=c(2,0)) +
  geom_sf(data = toothfish_tags_spatial_release, color = "green", cex=2) +
  geom_sf(data = toothfish_tags_spatial_recapture, color = "blue", cex=2) +
  geom_sf(data = lines, colour = 'black') +
  annotate("text", x = 50, y = -47, label = c("Afred Faure & Ile de l'Est"), size = 2.5) +
  annotate("text", x = 70, y = -50, label = c("Kerguelen"), size = 2.5) +
  annotate("text", x = 72, y = -54, label = c("Heard and McDonald"), size = 2.5) +
  geom_sf_text(data = CCAMLR, aes(fill = NULL, label = GAR_Name), size = 2.5, col = "black", label.size = 0,  nudge_y=c(0,0,0,0,0,0,0,0,0.5,0,0,0,0,0,0,0,-1,1,0), nudge_x=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Toothfish tag releases and recaptures SIOFA/CCAMLR (2008-2022)") +
  coord_sf(xlim = c(40, 85), ylim = c(-40, -60), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) 

ggsave("Overview/SIOFAmap_CCAMLR_tags_web.png", width = 8, height = 6, dpi = 150)

## analyze observer data
Observer_data <- read_excel("qry_overview_full_bio_sampling_data_2023.xlsx")
# calculate number of fish of SIOFA key species measured 
# for length by Scientific Observers 
observed_lengths_target_table <- Observer_data %>%
  group_by(species3ACode,speciesEnglishName,speciesScientificName,Year) %>%
  filter(species3ACode== "ORY"| species3ACode== "BXD"| species3ACode== "BYS"|  
         species3ACode== "OIL"| species3ACode== "LEC"|   species3ACode== "TOP"|  
         species3ACode== "TOA"| species3ACode== "WHA"| species3ACode== "WRF"| 
         species3ACode== "HAU") %>%
  filter(bsLength>=0) %>%
  summarise(n = n()) %>%
  filter(Year>=2013) %>%
  rename('Scientific name'=speciesScientificName) %>%
  arrange(Year,species3ACode, .by_group = FALSE) %>%
  rename('FAO code'=species3ACode) %>%
  rename('Common name'=speciesEnglishName) %>%
  #rename('Individuals measured'=n) %>%
  pivot_wider(names_from = Year, values_from = n) %>%
  replace(is.na(.), 0) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) 

write_xlsx(observed_lengths_target_table,"Overview/Tables/target_lengths_table.xlsx")

# calculate fraction of fish of catch measured for length 
# for SIOFA key species 
# need to get number of fish measured per year (from previous section)
observed_lengths_target_weight_table <- Observer_data %>%
  group_by(species3ACode,speciesEnglishName,speciesScientificName,Year) %>%
  filter(species3ACode== "ORY"| species3ACode== "BXD"| species3ACode== "BYS"|  
           species3ACode== "OIL"| species3ACode== "LEC"|   species3ACode== "TOP"|  
           species3ACode== "TOA"| species3ACode== "WHA"| species3ACode== "WRF"| 
           species3ACode== "HAU") %>%
  filter(bsWeight>=0) %>%
  summarise(AvgWeight = mean(bsWeight)) %>%
  filter(Year>=2013) %>%
  rename('Scientific name'=speciesScientificName) %>%
  arrange(Year,species3ACode, .by_group = FALSE) %>%
  rename('FAO code'=species3ACode) %>%
  rename('Common name'=speciesEnglishName) %>%
  #rename('Individuals measured'=n) %>%
  pivot_wider(names_from = Year, values_from = AvgWeight) %>%
  replace(is.na(.), 0) 
# then multiply this number by the average weight measured in that year
MW <- observed_lengths_target_weight_table
i <- 4
while(i <=14) {                  # Start while-loop
  MW[ , i] <- (observed_lengths_target_table[ , i] * MW[ ,i])/1000
  i <- i + 1
}
# then express this as a fraction of the catch for that species in that year
cumulative_catch_table <- fishing %>%
  group_by(SpeciesCode,SpeciesEngName,SpeciesScName,Year) %>%
  filter(SpeciesCode== "ORY"| SpeciesCode== "BXD"| SpeciesCode== "BYS"|  
           SpeciesCode== "OIL"| SpeciesCode== "LEC"| SpeciesCode== "TOP"|  
           SpeciesCode== "TOA"| SpeciesCode== "WHA"| SpeciesCode== "WRF"| 
           SpeciesCode== "HAU") %>%
  filter(CatchTon>=0) %>%
  summarise(TotalCatch = sum(CatchTon)) %>%
  filter(Year>=2013) %>%
  arrange(SpeciesCode, Year, .by_group = TRUE) %>%
  pivot_wider(names_from = Year, values_from = TotalCatch) %>%
  replace(is.na(.), 0) 
# since the two tables are not matching species, need to remove
# the species that was not measured in catches
species_cumulative_catch_table <- cumulative_catch_table %>%
  filter(SpeciesCode!= "HAU") %>%
  dplyr::select(SpeciesCode,SpeciesEngName,SpeciesScName,
         '2013','2014','2015','2016','2017','2018','2019',
         '2020', '2021', '2022') # need to manually add years
# finally calculate ratio
MWratio <- MW
i <- 4
while(i <=13) {                  # Start while-loop
  MWratio[ , i] <- (MWratio[ ,i]/species_cumulative_catch_table[ , i]*100)
  i <- i + 1
}
MWratio[sapply(MWratio, is.infinite)] <- NA
MWratio <- MWratio %>%
  replace(is.na(.), 0) %>%
  #rename('FAO code'=SpeciesCode) %>%
  mutate_if(is.numeric, ~round(., 3))

write_xlsx(MWratio,"Overview/Tables/MWratio.xlsx")

# calculate number of fish of SIOFA non key species measured 
# for length by Scientific Observers 
# select only when more than 40 individuals measured
observed_lengths_nontarget_table <- Observer_data %>%
  group_by(species3ACode,speciesEnglishName,speciesScientificName,Year) %>%
  filter(bsLength>=0) %>%
  summarise(n = n()) %>%
  filter(Year>=2013) %>%
  filter(species3ACode!= "ORY", species3ACode!= "BXD", species3ACode!= "BYS",  
           species3ACode!= "OIL", species3ACode!= "LEC", species3ACode!= "TOP",  
           species3ACode!= "TOA", species3ACode!= "WHA", species3ACode!= "WRF", 
           species3ACode!= "HAU") %>%
  rename('Scientific name'=speciesScientificName) %>%
  arrange(Year,species3ACode, .by_group = FALSE) %>%
  rename('FAO code'=species3ACode) %>%
  rename('Common name'=speciesEnglishName) %>%
  #rename('Individuals measured'=n) %>%
  pivot_wider(names_from = Year, values_from = n) %>%
  #mutate(is.na('Common name'), '-') %>%
  mutate_at(c(4:13), ~replace_na(.,0)) %>% 
  mutate_at(c(2), ~replace_na(.,' ')) %>% 
  filter_at(vars(starts_with('20')),  any_vars(. > 40)) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  filter(Total>=40) 

write_xlsx(observed_lengths_nontarget_table,"Overview/Tables/non_target_lengths_table.xlsx")

# measures of maturity, sex and weight in all fish species
observed_fish_other_table <- Observer_data %>%
  filter(Year>=2013) %>%
  group_by(species3ACode,speciesEnglishName,speciesScientificName) %>%
  summarise(Maturity = sum(bsMaturity>=0), 
            Sex = sum(bsSex>=0),
            Weight = sum(bsWeight>=0)) %>%
  rename('Scientific name'=speciesScientificName) %>%
  rename('FAO code'=species3ACode) %>%
  rename('Common name'=speciesEnglishName) %>%
  filter(Maturity>=40|Sex>=40|Weight>=40) %>%
  rename('Maturity (n)'=Maturity) %>%
  rename('Sex (n)'=Sex) %>%
  rename('Weight (n)'=Weight) %>%
  mutate_at(c(4:6), ~replace_na(.,0)) %>% 
  mutate_at(c(2), ~replace_na(.,' ')) 

write_xlsx(observed_fish_other_table,"Overview/Tables/observed_fish_other_table.xlsx")

# measures of sharks 
observed_lengths_sharks_table <- Observer_data %>%
  filter(bsLength>=0) %>%
  filter(Year>=2013) %>%
  filter(species3ACode %in% shark_species$Code) %>%
  group_by(Year) %>%
  summarise(n = n()) %>%
  arrange(Year) %>%
  rename('N. of sharks measured'=n) 

write_xlsx(observed_lengths_sharks_table,"Overview/Tables/observed_lengths_sharks_table.xlsx")

# measures of sharks included in CMM 12
# need to include maturity, sex and weight
observed_sharks_CMM_table <- Observer_data %>%
  filter(Year>=2013) %>%
  filter(species3ACode %in% shark_species_CMM$FAOcode) %>%
  group_by(species3ACode,speciesEnglishName,speciesScientificName) %>%
  summarise('Maturity (n)' = sum(bsMaturity>=0), 
            'Sex (n)' = sum(bsSex>=0),
            'Length (n)' = sum(bsLength>=0), 
            'Weight (n)' = sum(bsWeight>=0)) %>%
  rename('Scientific name'=speciesScientificName) %>%
  rename('FAO code'=species3ACode) %>%
  rename('Common name'=speciesEnglishName) %>%
  mutate_at(c(4:7), ~replace_na(.,0)) %>% 
  mutate_at(c(2), ~replace_na(.,' '))

write_xlsx(observed_sharks_CMM_table,"Overview/Tables/obs_sharks_CMM_table.xlsx")

