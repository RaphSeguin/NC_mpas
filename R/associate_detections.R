#' Associate AIS detections to coral national park management

associate_detections_AIS <- function(AIS_NC){

  coords = st_coordinates(AIS_NC)
  
  #ASsociate unique ID to each observation
  AIS_unique_id <- AIS_NC %>% 
    clean_names() %>%
    st_transform(crs = st_crs(parc_corail)) %>%
    cbind(coords) %>%
    mutate(obs_id = paste0(X,Y,time_range,flag,geartype,apparent_fishing_hours)) 
  
  #Inside natural reserve
  AIS_natural_reserve <- st_intersection(AIS_unique_id, new_reserves_sf)
  save(AIS_natural_reserve, file = "output/AIS_natural_reserve.Rdata")
  
  #Keep dataframes with only points that are outside natural reserves
  AIS_outside_reserve <- AIS_unique_id %>%
    filter(!obs_id %in% AIS_natural_reserve$obs_id)
  
  #Inside national park
  AIS_national_park <- st_intersection(AIS_outside_reserve,parc_corail)
  
  #SAR data
  AIS_protected_area <- AIS_national_park %>%
    bind_rows(AIS_natural_reserve) %>%
    mutate(time_range = as.Date(time_range)) 
  
  save(AIS_protected_area, file = "output/AIS_protected_area.Rdata")
  
  return(AIS_protected_area)
  
  
}