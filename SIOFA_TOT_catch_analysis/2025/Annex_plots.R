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
require(marmap)
require(geosphere)
require(interp)
require(fields)
require(ggpubr)
require(modelr)
require(see)
require(ggokabeito)
require(ggh4x)


### produce annex plots for toothfish catch paper

## load data
fishing <- read_excel("D:/SIOFA/Data/Catch-effort-2024.xlsx", col_types = c("numeric","text","numeric", "text",  "numeric", "numeric", "numeric","numeric","numeric","text","text","text","text","numeric","numeric","numeric","numeric","numeric","text","text","text","text","numeric","numeric", "numeric","numeric","text"))
Observer_data <- read_excel("D:/SIOFA/Data/qry_overview_full_bio_sampling_data_2024.xlsx")
ALL_tags <- read_excel("D:/SIOFA/Data/SIOFA-all-tagging-data-2024.xlsx", col_types = c("numeric", "text", "text", "text", "date", "numeric", "numeric","numeric","numeric","numeric", "numeric", "numeric","numeric","numeric","numeric","text","text","numeric","text","text","text","numeric","text"))
toothfish_tags <- read_excel("D:/SIOFA/Data/CCAMLR_Tags-2024.xlsx")
land <- st_read('D:/SIOFA/Data/natural_earth/land')
ocean <- st_read('D:/SIOFA/Data/natural_earth/ocean')
bathy <- st_read('D:/SIOFA/Data/natural_earth/bathy')
islands <- st_read('D:/SIOFA/Data/natural_earth/islands')
CCAMLR <- st_read('D:/SIOFA/Data/CCAMLR_stat_areas')
TOP_MUs <- st_read('D:/SIOFA/Data/TOP_management_areas_2023')
st_crs(TOP_MUs) <- 4326
BPAs <- st_read('D:/SIOFA/Data/BPAs')
fishing_boundaries <- st_read('D:/SIOFA/Data/siofa_subareas_edited')

## catch per year and per management area
fishing_TOP <- fishing %>%
  filter(Target == "TOP") %>%
  filter(SpeciesCode == "TOP") %>% # only patagonian toothfish catch
  filter(Year<=Est_Season & Year>=Min_Season) # takes parameters from the main routine
  #filter(Year<=2023 & Year>=2019) # sets parameters manually
fishing_spatial_TOP <- filter(fishing_TOP, (!is.na(fishing_TOP$Longitude)))
fishing_spatial_TOP <- filter(fishing_TOP, (!is.na(fishing_TOP$Latitude)))
fishing_spatial_TOP <- st_as_sf(fishing_spatial_TOP, coords = c("Longitude", "Latitude"), crs = 4326) 
fishing_TOP_within_SIR <- st_join(fishing_spatial_TOP, left = FALSE, SIR)
fishing_TOP_within_DC <- st_join(fishing_spatial_TOP, left = FALSE, DelCano)

# tags per year and management area
tags_TOP <- ALL_tags %>%
  filter(species3ACode == "TOP") %>% # only patagonian toothfish tags
  mutate(Year=as.numeric(format(follSettingStartDatetime,'%Y'))) %>%
  filter(Year<=Est_Season & Year>=Min_Season)

tags_spatial_TOP <- filter(tags_TOP, (!is.na(tags_TOP$follSettingStartLatitude)))
tags_spatial_TOP <- filter(tags_TOP, (!is.na(tags_TOP$follSettingStartLongitude)))
tags_spatial_TOP <- st_as_sf(tags_spatial_TOP, coords = c("follSettingStartLongitude", "follSettingStartLatitude"), crs = 4326) 
tags_TOP_within_SIR <- st_join(tags_spatial_TOP, left = FALSE, SIR)
tags_TOP_within_DC <- st_join(tags_spatial_TOP, left = FALSE, DelCano)

# plot world map and add SIOFA subareas + TOP MA and fishing events on world map
# standard palette
ggplot() +
  geom_sf(data = ocean, fill = "lightblue", color = "black") +
  geom_sf(data = land, fill = "antiquewhite", color = "black") +
  geom_sf(data = islands, fill = "antiquewhite", color = "black") +
  geom_sf(data = fishing_boundaries["SubArea"], fill = brewer.pal(n=9, name = "Set3"), alpha = 0.2) +
  geom_sf(data = TOP_MUs, fill = "orchid2", color = "black") +
  geom_sf(data = SIR, fill = "green", color = "black") +
  geom_sf(data = DelCano, fill = "green", color = "black") +
  geom_sf(data = fishing_TOP_within_SIR, color = '#56B4E9', cex = .1) +
  geom_sf(data = fishing_TOP_within_DC, color = '#E69F00', cex = .1) +
  #geom_sf(data = fishing_spatial_TOP, color = 'blue', cex = .1) +
  geom_sf_label(data = TOP_MUs, aes(fill = NULL, label = area_name), size = 2.5, col = "black", label.size = 0, nudge_y=c(1.2,2), nudge_x=c(2,0)) +
  geom_sf_label(data = fishing_boundaries, aes(fill = NULL, label = SubAreaNo), size = 2.5, col = "black", label.size = 0, nudge_y=c(-2,0,0, 0, 0, 0, 0, -6)) +
  geom_sf(data = CCAMLR, fill = NA, color = "black") +
  coord_sf(crs = "4326") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("SIOFA TOP fishing") +
  coord_sf(xlim = c(30, 60), ylim = c(-38, -48), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 

# summarize data for plotting
fishing_TOP_within_SIR_catch <- fishing_TOP_within_SIR %>%
  st_drop_geometry() %>%
  group_by(Year, name) %>%
  summarise_at(.vars = c("CatchTon"),
               .funs = "sum")
fishing_TOP_within_DC_catch <- fishing_TOP_within_DC %>%
  st_drop_geometry() %>%
  group_by(Year, name) %>%
  summarise_at(.vars = c("CatchTon"),
               .funs = "sum")
# plot data and compose image
fishing_TOP_within_DC_SIR_catch <- full_join(fishing_TOP_within_DC_catch, fishing_TOP_within_SIR_catch)
plot_TOP_SC_SIR_catch <- 
  ggplot() +
  geom_bar(data=fishing_TOP_within_DC_SIR_catch, aes(x = Year, y = CatchTon, fill=name), position="stack", stat="identity") +
  labs(title="Patagonian toothfish catch", x="Year", y="TOP catch (t)") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(limits=c(2018, 2024),breaks = seq(2018, 2024, by = 1)) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.8) +
  scale_fill_manual(values = c(SIR = "#56B4E9", DC ="#E69F00")) +
  facet_wrap(~name, ncol = 2)

ggsave("D:/SIOFA/Data/SIOFA_2025_Toothfish_catch_esitmate/Output/Annex1.png", width = 10, height = 4, dpi = 150)

## boxplots of unstandardized CPUEs per year and management area
# calculate effort by operation ID
fishing_TOP_within_SIR_effort <- fishing_TOP_within_SIR %>%
  st_drop_geometry() %>%
  group_by(Year, ActivityID, name) %>%
  summarise_at(.vars = c("NbHooks"),
               .funs = "sum")

fishing_TOP_within_DC_effort <-  fishing_TOP_within_DC %>%
  st_drop_geometry() %>%
  group_by(Year, ActivityID, name) %>%
  summarise_at(.vars = c("NbHooks"),
               .funs = "sum")

fishing_TOP_within_DC_SIR_effort <- full_join(fishing_TOP_within_DC_effort, fishing_TOP_within_SIR_effort)

# calculate catch by operation ID
fishing_TOP_within_SIR_catch_ID <- fishing_TOP_within_SIR %>%
  st_drop_geometry() %>%
  group_by(Year,ActivityID, name) %>%
  summarise_at(.vars = c("CatchKg"),
               .funs = "sum")
fishing_TOP_within_DC_catch_ID <- fishing_TOP_within_DC %>%
  st_drop_geometry() %>%
  group_by(Year, ActivityID,name) %>%
  summarise_at(.vars = c("CatchKg"),
               .funs = "sum")

fishing_TOP_within_DC_SIR_catch_ID <- full_join(fishing_TOP_within_DC_catch_ID, fishing_TOP_within_SIR_catch_ID)

#calculate CPUE
CPUE_toothfish_DC_SIR <- full_join(fishing_TOP_within_DC_SIR_catch_ID, fishing_TOP_within_DC_SIR_effort)

CPUE_toothfish_DC_SIR <- CPUE_toothfish_DC_SIR %>% 
  mutate(CPUEs = (CatchKg)/(NbHooks/1000)) 

#plot CPUEs
CPUE_toothfish_DC_SIR$Year <- as.factor(CPUE_toothfish_DC_SIR$Year)

ggplot(data=CPUE_toothfish_DC_SIR, aes(x=Year, y=CPUEs, fill = name), group = 2) +
  geom_boxplot(outliers = FALSE) +
  labs(title="Unstandardised CPUE for toothfish", x="Year", y="CPUE (kg/1000 hooks)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) +
  scale_fill_manual(values = c(SIR = "#56B4E9", DC ="#E69F00")) +
  theme(legend.position = "none") +
  facet_wrap(~name, ncol = 2)
  
ggsave("D:/SIOFA/Data/SIOFA_2025_Toothfish_catch_esitmate/Output/SIOFA_CPUEs_TOP_web.png", width = 10, height = 4, dpi = 150) 

## plot length weight graph with a regression curve
# filter DC and SIR data
Observer_data_TOP <- Observer_data %>%
  filter(species3ACode == "TOP") %>%
  filter(!is.na(Latitude)) %>%
  filter(!is.na(Longitude))
  
Observer_data_spatial_TOP <- st_as_sf(Observer_data_TOP, coords = c("Longitude", "Latitude"), crs = 4326) 
Observer_data_spatial_TOP_within_SIR <- st_join(Observer_data_spatial_TOP, left = FALSE, SIR)
Observer_data_spatial_TOP_within_DC <- st_join(Observer_data_spatial_TOP, left = FALSE, DelCano)

# calculate a regression for the data
fit_LW_SIR <- lm(log(Observer_data_spatial_TOP_within_SIR$bsWeight)~log(Observer_data_spatial_TOP_within_SIR$bsLength),data=Observer_data_spatial_TOP_within_SIR)
fit_LW_DC <- lm(log(Observer_data_spatial_TOP_within_DC$bsWeight)~log(Observer_data_spatial_TOP_within_DC$bsLength),data=Observer_data_spatial_TOP_within_DC)

summary(fit_LW_SIR)
summary(fit_LW_DC)

aug.LW_SIR <- Observer_data_spatial_TOP_within_SIR %>% 
  add_predictions(fit_LW_SIR) %>% 
  add_residuals(fit_LW_SIR) %>%
  st_drop_geometry()
aug.LW_DC <- Observer_data_spatial_TOP_within_DC %>% 
  add_predictions(fit_LW_DC) %>% 
  add_residuals(fit_LW_DC) %>%
  st_drop_geometry()

TOP_LW_DC_SIR <- full_join(aug.LW_SIR, aug.LW_DC)

# plot data
ggplot(data=TOP_LW_DC_SIR) + 
  geom_point(aes(x=bsLength, y=bsWeight, colour = factor(name), shape = factor(name))) +
  #geom_smooth(formula= y ~ , level= 0.9) +
  geom_line(aes(x=bsLength,y=exp(pred),colour = factor(name), shape = factor(name))) +
  labs(title="Lenght/weight relationship of TOP in DC and SIR", x="Lenght (cm)", y="Weight (kg)") +
  theme_bw() +
  scale_color_manual(values = c(SIR = "#56B4E9", DC ="#E69F00")) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio=0.4) 

ggsave("D:/SIOFA/Data/SIOFA_2025_Toothfish_catch_esitmate/Output/SIOFAspecies_TOP_LW_SIR_DC.png", width = 10, height = 4, dpi = 150)

## create a table with total catch and tag releases by area
tags_TOP_DC <- tags_TOP_within_DC %>%
  st_drop_geometry() %>%
  group_by(Year, name) %>%
  dplyr::summarise(Tags = n())

tags_TOP_SIR <- tags_TOP_within_SIR %>%
  st_drop_geometry() %>%
  group_by(Year, name) %>%
  dplyr::summarise(Tags = n())

tags_TOP_within_DC_SIR <- full_join(tags_TOP_DC,tags_TOP_SIR)
fishing_TOP_within_DC_SIR_catch # total catch per year
table <- full_join(fishing_TOP_within_DC_SIR_catch, tags_TOP_within_DC_SIR) 
table <- table[,c(2,1,3,4)]
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

FT.catch_tags_TOP_DC_SIR <- table %>%
  replace(is.na(.), 0) %>%
  dplyr::rename('Management Area'=name) %>%
  dplyr::rename('Catch (t)'=CatchTon) %>%
  dplyr::rename("Tags released (n)"=Tags) %>%
  flextable() %>%
  merge_v(j = c("Management Area")) %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  flextable::colformat_double(j = "Year", big.mark = "", digits = 0)  %>%
  flextable::colformat_double(j = 'Catch (t)', big.mark = "", digits = 1)  %>%
  fit_to_width(max_width = 18, unit="cm") %>%
  flextable::align(j = 1:2, align = "left", part = "all") %>%
  flextable::align(j = 3:4, align = "center", part = "all") %>%
  #italic(j = 3, italic = TRUE, part = "body") %>%
  add_header_lines("Annual catch and number of tags released
                    in SIOFA toothfish fisheries within subarea 3b") 

save_as_image(FT.catch_tags_TOP_DC_SIR, path = "D:/SIOFA/Data/SIOFA_2025_Toothfish_catch_esitmate/Output/catch_tags_DC_SIR.png")               

## create a table with SIOFA released tag recaptures by area
toothfish_tags_spatial_release <- toothfish_tags %>%
  filter(convention_area_release=="SIOFA") %>%
  filter(convention_area_recapture=="SIOFA") %>%
  st_as_sf(coords = c("longitude_release", "latitude_release"), crs = 4326) 
toothfish_tags_spatial_recapture <- toothfish_tags %>%
  filter(convention_area_release=="SIOFA") %>%
  filter(convention_area_recapture=="SIOFA") %>%
  filter(year_recapture>Min_Season) %>%
  filter(year_recapture<Est_Season) %>%
  st_as_sf(coords = c("longitude_recapture", "latitude_recapture"), crs = 4326)  
toothfish_tags_DC_release <- st_join(toothfish_tags_spatial_release, left = FALSE, DelCano)
toothfish_tags_SIR_release <- st_join(toothfish_tags_spatial_release, left = FALSE, SIR)
toothfish_tags_DC_recapture <- st_join(toothfish_tags_spatial_recapture, left = FALSE, DelCano)
toothfish_tags_SIR_recapture <- st_join(toothfish_tags_spatial_recapture, left = FALSE, SIR)

toothfish_tags_SIR_recapture <- toothfish_tags_SIR_recapture %>%
  st_drop_geometry() %>%
  group_by(name,year_release, year_recapture) %>%
  dplyr::summarise(Tags = n())

toothfish_tags_DC_recapture <- toothfish_tags_DC_recapture %>%
  st_drop_geometry() %>%
  group_by(name,year_release, year_recapture) %>%
  dplyr::summarise(Tags = n())

toothfish_tags_DC_SIR_recapture <- full_join(toothfish_tags_DC_recapture,toothfish_tags_SIR_recapture)

# make table
FT.toothfish_tags_DC_SIR_recapture <- toothfish_tags_DC_SIR_recapture %>%
  replace(is.na(.), 0) %>%
  dplyr::rename('Management Area'=name) %>%
  dplyr::rename('Release year'=year_release) %>%
  dplyr::rename("Recapture year"=year_recapture) %>%
    flextable() %>%
  merge_v(j = c("Management Area")) %>%
  bg(bg = "grey", part = "header") %>%
  autofit(part = c("body")) %>%
  fit_to_width(max_width = 18, unit="cm") %>%
  flextable::colformat_double(j = 'Release year', big.mark = "", digits = 0)  %>%
  flextable::colformat_double(j = 'Recapture year', big.mark = "", digits = 0)  %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  flextable::align(j = 2:4, align = "center", part = "all") %>%
  bg(i = ~ Tags>3, 
     j = ~ Tags, 
     bg="salmon") %>%
  #dplyr::rename("Recaptured tags (n)"=Tags) %>%
  #italic(j = 3, italic = TRUE, part = "body") %>%
  add_header_lines("Annual number of tags recaptured
                    in SIOFA toothfish fisheries within subarea 3b") 

save_as_image(FT.toothfish_tags_DC_SIR_recapture, path = "D:/SIOFA/Data/SIOFA_2025_Toothfish_catch_esitmate/Output/recapture_tags_DC_SIR.png")               


