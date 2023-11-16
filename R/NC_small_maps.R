NC_small_maps <- function(fishing_effort, new_reserves_sf){
  
  plots_maps <- mclapply(1:nrow(new_reserves_sf), function(i){
    
    temp <- new_reserves_sf[i,]
    
    temp_buffer <- st_buffer(temp,2)
    
    plot(temp_buffer)
    
    fishing_effort_temp <- st_crop(fishing_effort,temp_buffer)
    NC_temp <- st_crop(NC,temp_buffer)
    parc_corail_temp <- st_crop(NC,temp_buffer)
    
    plot_mpa_NC <- ggplot() +
      geom_sf(data = NC_temp, fill = "black") + 
      geom_sf(data = temp,aes(fill = name),alpha = 0.8) +
      geom_sf(data = parc_corail_temp, fill = "lightblue", alpha = 0, pch = 21, color = "black") +
      # geom_sf(data = all_reserves_temp %>% filter(type == "Réserve existante"), aes(color = type),alpha = 0,pch=21,linewidth = 1) +
      # scale_color_hp_d(option = "Gryffindor", name = "Réserve") + 
      geom_sf(data = fishing_effort_temp,size = 0.8,alpha = 0.8) +
      scale_fill_manual(values = reserve_legend,name = "Réserve") +
      theme_map() +
      theme(legend.position="none")
    
    ggsave(plot_mpa_NC, file = paste0("figures/plot_mpa_NC",temp$id,".png"), width = 297, height = 210, units = "mm", dpi=600)
    
  },mc.cores =1)
  
}