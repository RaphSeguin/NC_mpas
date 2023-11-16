#' Associate AIS detections to coral national park management



calculate_fishing <- function(AIS_protected_area){
  
  #New reserves surface is 
  #110847587518
  #Coral park surface is
  area_parc_no_reserve <- set_units((st_area(parc_corail) - sum(st_area(reserve_corail)) - sum(st_area(new_reserves_sf))),km^2)
  area_reserves = set_units(sum(st_area(new_reserves_sf)),km^2)

  AIS_fishing_effort <- AIS_protected_area %>%
    mutate(MonthNum = as.factor(ifelse(str_detect(time_range,"-03-"),"03",
                                       ifelse(str_detect(time_range,"-04-"),"04",
                                              ifelse(str_detect(time_range,"-05-"),"05",
                                                     ifelse(str_detect(time_range,"-06-"),"06",
                                                            ifelse(str_detect(time_range,"-07-"),"07",
                                                                   ifelse(str_detect(time_range,"-08-"),"08",
                                                                          ifelse(str_detect(time_range,"-09-"),"09",
                                                                                 ifelse(str_detect(time_range,"-10-"),"10",
                                                                                        ifelse(str_detect(time_range,"-11-"),"11",
                                                                                               ifelse(str_detect(time_range,"-01-"),"01",
                                                                                                      ifelse(str_detect(time_range,"-12-"),"12","02"))))))))))))) %>%
    mutate(reserve = ifelse(is.na(name),"No_reserve","Reserve")) %>%
    group_by(MonthNum,reserve) %>%
    mutate(daily_fishing = sum(apparent_fishing_hours)) %>%
    ungroup() %>%
    arrange(time_range) %>%
    mutate(fishing_effort = ifelse(reserve == "No_reserve",
                                   daily_fishing/area_parc_no_reserve,
                                   daily_fishing/area_reserves))
  
  print("Mean fishing effort in Parc")
  print(mean(filter(AIS_fishing_effort, reserve == "No_reserve")$fishing_effort))
  print(sd(filter(AIS_fishing_effort, reserve == "No_reserve")$fishing_effort))
  print("Mean fishing effort in reserves")
  print(mean(filter(AIS_fishing_effort, reserve == "Reserve")$fishing_effort))
  print(sd(filter(AIS_fishing_effort, reserve == "Reserve")$fishing_effort))
  

  #Bits of code i don't need right now
  # area_banc_capel <- new_reserves_sf %>%
  #   filter(name == "Banc Capel") %>%
  #   st_area()
  # area_entrecasteaux <- new_reserves_sf %>%
  #   filter(name == "Ride Entrecasteaux") %>%
  #   st_area()
  # area_nord_walpole <- new_reserves_sf %>%
  #   filter(name == "Nord Walpole") %>%
  #   st_area()
  # area_hebrides <- new_reserves_sf %>%
  #   filter(name == "Nouvelle Hebrides") %>%
  #   st_area()
  # area_norfolk <- new_reserves_sf %>%
  #   filter(name == "Norfolk") %>%
  #   st_area()
  # area_fairway <- new_reserves_sf %>%
  #   filter(name == "Fairway") %>%
  #   st_area()
  
  # mutate(fishing_effort = ifelse(is.na(name),
  #                                 daily_fishing/parc_no_reserve,
  #                                ifelse(name == "Banc Capel",
  #                                daily_fishing/area_banc_capel,
  #                                ifelse(name == "Ride Entrecasteaux",
  #                                daily_fishing/area_entrecasteaux,
  #                                ifelse(name == "Nord Walpole",
  #                                 daily_fishing/area_nord_walpole,
  #                                 ifelse(name == "Nouvelle Hebrides",
  #                                 daily_fishing/area_hebrides,
  #                                 ifelse(name == "Norfolk",
  #                                 daily_fishing/area_norfolk,
  #                                 daily_fishing/area_fairway)))))))
  # 
  
  
}