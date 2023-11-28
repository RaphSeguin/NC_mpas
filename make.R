
#To do
#Add all seamounts/sensible ecosystems for whales on map to check if it's also where fishing occurs
#Add proposed MPAs to check if all of them were in an area without fishing



#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","units","tidymodels","parallel","cowplot","janitor","sf","RColorBrewer","ggridges","plotly","heatmaply","parsedate","birk","ggthemes","MASS","automap","pbmcapply","ggnewscale","CausalImpact","sjmisc","ggspatial","stringr",
          "harrypotter","wesanderson","ranger","ggpubr","data.table","xml2","XML","rnaturalearth","ggExtra","raster","exactextractr","gstat","magrittr","scales","grid","gridExtra","XML","gfwr","fasterize")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

sf_use_s2(FALSE)
#-----------------Loading all data---------------------

getwd()

#Load Rdata
path = (here::here("data"))
setwd(path)
files <- list.files(pattern="Rdata")
data_list = lapply(files, load, .GlobalEnv)

#-----------------Loading all functions---------------------

path = (here::here("R"))
setwd(path)
files.source = list.files(here::here("R"))
sapply(files.source, source)

#-----------------Loading all outputs---------------------

path = (here::here("output"))
setwd(path)
files <- list.files(pattern="Rdata")
data_list = lapply(files, load, .GlobalEnv)

#-----------ANALYSES---------

setwd(here())

#MPA new caledonia parc
#Download from https://georep-dtsi-sgt.opendata.arcgis.com/pages/2986a754fd574194b4633a629b87c61e
parc_corail <- sf::st_read("maps/limite_parc_corail/limite_parc_corail.shp") %>% st_transform(crs = 4326)
reserve_corail <- sf::st_read("maps/limite_reserve_corail/limite_reserve_corail.shp") %>% st_transform(crs = 4326)
reserve_corail_detail <- sf::st_read("maps/reserve_detail/reserve_detail.shp") %>% st_transform(crs = 4326)

angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}
new_reserves_NC <- list.files(path = "maps/shapefiles_reserves_new/",
                              pattern="*.csv",
                              full.names = T) %>%
  map_df(~read_csv(.))
# new_reserves_NC <- read.csv("maps/shapefiles_reserves_new/banc_capel.csv")

#Working on this DF
# new_reserves_sf <- new_reserves_NC %>%
#   #Cleaning DF 
#   dplyr::mutate(Latitude = str_squish(gsub("[^0-9.-]", " ", new_reserves_NC$Latitude)),
#          Longitude =str_squish(gsub("[^0-9.-]", " ", new_reserves_NC$Longitude))) %>%
#   dplyr::mutate(Latitude = angle2dec(Latitude),
#                 Longitude = angle2dec(Longitude)) %>%
#   dplyr::mutate(Latitude = -Latitude,
#                 Longitude = Longitude) %>%
#   dplyr::rename(name = "Name") %>%
#   st_as_sf(coords = c("Longitude","Latitude"),crs=4326) %>%
#   st_transform(crs = st_crs(parc_corail)) %>%
#   # Converting to polygon
#   group_by(name) %>%
#   summarise(geometry = st_combine(geometry)) %>%
#   st_cast("POLYGON") %>%
#   ungroup() %>%
#   mutate(PA_category = ifelse(name == "Banc Capel","Réserve intégrale","Réserve naturelle"))
# 
# st_write(new_reserves_sf,dsn = "maps/new_reserves_sf.shp")
new_reserves_sf <- st_read("maps/shapefiles_reserves_new/new_reserves_NC.shp")

#Map of new caledonia
#Download from https://maps.princeton.edu/catalog/stanford-qr598rg4243
NC <- sf::st_read("maps/qr598rg4243.shp") %>% st_transform(crs = st_crs(parc_corail))

#NC EBSAs
norfolk_ebsa <- st_read("maps/norfolk_ebsa.shp") %>% st_transform(crs = st_crs(parc_corail)) %>% st_intersection(parc_corail)  %>% mutate(Category = "EBSA")
northern_low_ridge_ebsa <- st_read("maps/northern_low_ridge.shp") %>% st_transform(crs = st_crs(parc_corail)) %>% st_intersection(parc_corail) %>% mutate(Category = "EBSA")
hebrides_ebsa <- st_read("maps/hebrides_ebsa.shp") %>% st_transform(crs = st_crs(parc_corail)) %>% st_intersection(parc_corail) %>% mutate(Category = "EBSA")

#IMMAs
#Contact IMMA for shapefiles
NC_imma <- st_read("maps/iucn-imma/iucn-imma.shp") %>% st_transform(crs = st_crs(parc_corail)) %>% st_intersection(parc_corail) %>% mutate(Category = "IMMA")

#IBAs
#Contact Birdlife for shapefiles
NC_IBA <- st_read("maps/New_Calendonia_IBA/New_Calendonia_IBA.shp") %>% st_transform(crs = st_crs(parc_corail)) %>% st_intersection(parc_corail) %>% mutate(Category = "IBA")
NC_ebsa <- bind_rows(norfolk_ebsa,northern_low_ridge_ebsa,hebrides_ebsa,NC_imma,NC_IBA)

#Seamounts
#Download from https://data.unep-wcmc.org/datasets/41
seamounts_NC <- st_read("maps/SeamountsBaseArea.shp") %>% st_transform(crs = st_crs(parc_corail)) %>% st_make_valid() %>% st_intersection(parc_corail) %>% mutate(Category = "Seamount")
knolls_NC <- st_read("maps/Knolls.shp") %>% st_transform(crs = st_crs(parc_corail)) %>% st_intersection(parc_corail) %>% mutate(Category = "Knoll")

NC_seamounts <- bind_rows(seamounts_NC, knolls_NC)

#All reserves
all_reserves = new_reserves_sf %>%
  bind_rows(reserve_corail) %>%
  mutate(name = ifelse(is.na(nom),name,nom)) %>%
  mutate(type = ifelse(is.na(nom),"Nouvelle réserve","Réserve existante"))

#--------------NEW CALEDONIA----------------------------
setwd(here())

fishing_effort_2021_sf <- fishing_effort_2021 %>%
  st_as_sf(coords = c("Lon","Lat"), crs = 4326) %>%
  mutate(Year = 2021)

fishing_effort_2022_sf <- fishing_effort_2022 %>%
  st_as_sf(coords = c("Lon","Lat"), crs = 4326) %>%
  mutate(Year = 2022)

fishing_effort_2023_sf <- fishing_effort_2023 %>%
  st_as_sf(coords = c("Lon","Lat"), crs = 4326) %>%
  mutate(Year = 2023)

fishing_effort <- rbind(fishing_effort_2021_sf,fishing_effort_2022_sf, fishing_effort_2023_sf)

#Associate detections
AIS_protected_area <- associate_detections_AIS(fishing_effort)

#Calculations
AIS_fishing_effort <- calculate_fishing(AIS_protected_area)

#Fishing effort plot
(fishing_effort_plot <- ggplot(data = AIS_fishing_effort,aes(time_range,fishing_effort,color = reserve)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    scale_color_hp_d(option = "Ravenclaw",labels = c("Hors réserves","Dans les futures réserves"),name = "Effort de pêche/km^2 entre 2021 et 2023") +
    labs(x = " ",
         y= "Effort de pêche par km^2",
         color = "Protection") +
    geom_vline(xintercept = as.numeric(as.Date("2023-07-27"))) +
    scale_x_date(date_breaks = "1 month",limits = as.Date(c('2021-01-01','2023-10-31'))) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position = "bottom"))

ggsave(fishing_effort_plot, file = "figures/fishing_effort_plot.pdf", width = 297, height = 210, units = "mm", dpi = 600)
ggsave(fishing_effort_plot, file = "figures/fishing_effort_plot.jpg", width = 297, height = 210, units = "mm", dpi = 600)

#Maps
reserve_legend = c("Banc Capel"="#3B9AB2",
                   "Ride d?Entrecasteaux" = "#78B7C5",
                   "Ile de Walpole" = "#EBCC2B",
                   "Nouvelles Hebrides"="#F21A01",
                   "Ride de Norfolk"="#E1AF02",
                   "Sud Bassin de Fairway"="#ECCBAE")

map <- ggplot() +
  geom_sf(data = NC, fill = "black")+
  geom_sf(data = parc_corail, fill = "lightblue")+
  geom_sf(data = new_reserves_sf, aes(fill = name),alpha = 0.8)+
  scale_fill_manual(values = reserve_legend,name = "Réserve",
                    labels = c("Banc Capel"="Banc Capel",
                               "Ride d?Entrecasteaux" = "Ride d'Entrecasteaux",
                               "Ile de Walpole" = "La Monique - Ile de Walpole",
                               "Nouvelles Hebrides" = "Fosse des Nouvelles-Hébrides",
                               "Sud Bassin de Fairway" = "Sud Bassin de Fairway"))+
  geom_sf(data = all_reserves, aes(color = type),pch = 21,linewidth = 0.8,alpha = 0) +
  scale_color_hp_d(option="Gryffindor",name = "Type") +
  geom_sf(data = fishing_effort, shape = ".") +
  theme_map() +
  ggspatial::annotation_scale(location = "tr") +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
    )
  ) +
  theme(legend.position = "bottom")

ggsave(map, file = "figures/map.pdf", width = 297, height = 210, units = "mm",dpi=600)
ggsave(map, file = "figures/map.jpeg", width = 297, height = 210, units = "mm",dpi=600)

#Heatmap
xy <- st_coordinates(fishing_effort)
de = MASS::kde2d(xy[,1], xy[,2],n=300)
raster_density = raster(de)

#   Convert to usable data
density_NC <- as(raster_density, "SpatialPixelsDataFrame")
density_NC <- as.data.frame(density_NC)
colnames(density_NC) <- c("value", "x", "y")

heatmap <- ggplot() + 
  geom_tile(data=density_NC, aes(x=x, y=y, fill=value)) +
  scale_fill_gradient2(low = "#1B3B5B", mid = "white", high = "#AC0A27", limits = c(0,0.07),
                       # breaks = c(-0.7,-0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2,0.3),
                       name = "Effort de pêche entre 2021 et 2023",
                       guide = guide_colorbar(
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(50, units = "mm"),
                         draw.ulim = F,
                         title.position = 'top',
                         title.vjust = 0.4
                       )) +
  new_scale_fill() +
  geom_sf(data = NC, fill = "black") +
  geom_sf(data = parc_corail, fill = "lightblue", alpha = 0, pch = 21, color = "black") +
  geom_sf(data = new_reserves_sf, aes(fill = name),alpha = 0.8)+
  scale_fill_manual(values = reserve_legend,name = "Réserve",
                    labels = c("Banc Capel"="Banc Capel",
                               "Ride d?Entrecasteaux" = "Ride d'Entrecasteaux",
                               "Ile de Walpole" = "La Monique - Ile de Walpole",
                               "Nouvelles Hebrides" = "Fosse des Nouvelles-Hébrides",
                               "Sud Bassin de Fairway" = "Sud Bassin de Fairway"))+
  geom_sf(data = all_reserves, aes(color = type),pch = 21,linewidth = 0.4,alpha = 0) +
  scale_color_hp_d(option = "Ravenclaw",name = "Type") +
  theme_map() +
  theme(legend.position = "bottom")+
  ggspatial::annotation_scale(location = "tr") +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
    )
  )

ggsave(heatmap, file = "figures/heatmap.pdf", width = 297, height = 210, units = "mm",dpi=600)
ggsave(heatmap, file = "figures/heatmap.jpg", width = 297, height = 210, units = "mm",dpi=600)

#Zone sensibles
(area_sensible <- ggplot() + 
    geom_sf(data = NC, fill = "black") +
    geom_sf(data = parc_corail, pch = 22, colour = "black",alpha = 0) +
    geom_sf(data = NC_ebsa, aes(fill = Category),alpha = 0.6) +
    scale_fill_hp_d(option = "Gryffindor",name = "Zone Sensible", labels = c(EBSA = "Ecologically or Biologically Significant Marine Areas",
                                                                             IMMA = "Important Marine Mammal Area",IBA = "Important Bird Areas")) +
    new_scale_fill() +
    geom_sf(data = all_reserves, aes(color = type,fill=type),pch = 21,linewidth = 1,alpha = 0.2) +
    scale_color_hp_d(option="Ravenclaw",name = "Type") +
    scale_fill_hp_d(option="Ravenclaw",name = "Type") +
    theme_map() +
    theme(legend.position = "bottom")+
    ggspatial::annotation_scale(location = "tr") +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
      style = ggspatial::north_arrow_nautical(
        fill = c("grey40", "white"),
        line_col = "grey20",
      )
    ))

ggsave(area_sensible, file = "figures/area_sensible.pdf",width = 297, height = 210, units = "mm",dpi=600)
ggsave(area_sensible, file = "figures/area_sensible.jpg",width = 297, height = 210, units = "mm",dpi=600)

#Seamounts and knolls
seamounts_and_knolls <- ggplot() + 
  geom_sf(data = NC, fill = "black") +
  geom_sf(data = parc_corail, pch = 22, colour = "black",alpha = 0) +
  geom_sf(data = all_reserves, aes(fill = type),alpha = 0.9) +
  scale_fill_hp_d(option="Ravenclaw",name = "Type") +
  new_scale_fill() +
  geom_sf(data = NC_seamounts, aes(fill = Category),alpha = 0.7) +
  scale_fill_hp_d(option = "Slytherin",name = "Seamounts and knolls") + 
  theme_map() +
  theme(legend.position = "bottom")+
  ggspatial::annotation_scale(location = "tr") +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
    )
  )

ggsave(seamounts_and_knolls, file = "figures/seamounts_and_knolls.pdf",width = 297, height = 210, units = "mm",dpi=600)
ggsave(seamounts_and_knolls, file = "figures/seamounts_and_knolls.jpg",width = 297, height = 210, units = "mm",dpi=600)

