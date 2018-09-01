# VIC (ET and SWE) Maps


source("R/0_utilities.R")


# ---------------------------------------------------------------------
# Data processing

vic_maps_sum <- purrr::map(vic_maps, function(x){
  tidyr::gather(x,key="location",value="value",2:2046) %>% 
    dplyr::group_by(location) %>% 
    dplyr::summarise(mean_value = mean(value))
})

# Combine tibbles from list into one tibble then separate out coordinates and variable/gcm/period
vic_maps_tib <- vic_maps_sum %>% 
  bind_rows(.id="type_gcm_period") %>% 
  tidyr::separate(type_gcm_period,into = c("wy", "flux_type", "gcm", "period")) %>% 
  tidyr::separate(location,into = c("flux", "lat", "long"), sep="_")  %>% 
  dplyr::select(-c(wy, flux))

# Change lat and long from character to double
vic_maps_tib$lat <- as.double(vic_maps_tib$lat)
vic_maps_tib$long <- as.double(vic_maps_tib$long)

vic_maps_et <- vic_maps_tib %>% 
  dplyr::filter(flux_type=="et") %>% 
  mutate(value_interval = cut_interval(mean_value, n=11))

vic_maps_swe <- vic_maps_tib %>% 
  dplyr::filter(flux_type=="swe") %>% 
  mutate(value_interval = cut_interval(mean_value, n=11))


# vic_maps_tib %>% 
#   dplyr::group_by(flux_type, lat, long) %>% 
#   dplyr::summarise(happy = length(mean_value))
# 
# happy <- vic_maps_tib %>% 
#   dplyr::filter(period==45, )
#   dplyr::group_by(flux_type, lat, long) %>% 
#   nest()

# ***** Need to select periods, not just RCPs ********

# ***** Also, need approach to cut out watersheds or SS Region from full Sierra extent ********
  
# ----
# Plot ET

et_map <- function(tib, border, gcm_input){
  
  # Create labels for facetting
  period_id <- c(
    `historical` = "Historical",
    `mid_century` = "Mid Century",
    `z_end_of_century` = "Late Century"
  )
  
  rcp_id <- c(
    `45` = "RCP 4.5",
    `85` = "RCP 8.5"
  )
  
  x <- ggplot() +
    geom_raster(data=tib,aes(x=long,y=lat, fill=value_interval)) +
    geom_sf(data=border, fill=NA, col="white") +
    scale_fill_brewer(palette = "RdBu", direction = -1, name="ET") +
    #scale_fill_gradient(low="red", high="blue", name="ET") +
    scale_x_continuous(expand=c(0,0), limits = c(-119.8, -117.86)) +   # This eliminates margin buffer around plot
    scale_y_continuous(expand=c(0,0), limits = c(35.65, 37.85)) +   # This eliminates margin buffer around plot
    labs(title=paste("ET -", gcm_input), x="Longitude",y="Latitude", size=0.5) +
    theme_classic(base_size =12) +
    theme(axis.text.x = element_text(angle = 330, hjust=0)) +
    # geom_point(data = dplyr::filter(cities, name != 'Fresno'), aes(x = lon, y = lat), 
    #            shape = 19, color = "black", fill = "grey50", size = 1.2) +
    # geom_text(data = dplyr::filter(cities, name == 'Visalia'), 
    #           aes(x = lon, y = lat, label = paste("  ", as.character(name), sep="")), 
    #           size=3, angle = 0, vjust= -0.85, hjust = 0.95, color = "black") +
    # geom_text(data = dplyr::filter(cities, name == 'Porterville'), 
    #           aes(x = lon, y = lat, label = paste("  ", as.character(name), sep="")), 
    #           size=3, angle = 0, vjust= -0.85, hjust = 1.1, color = "black") +
    # geom_text(data = dplyr::filter(cities, name == 'Bishop'), 
    #           aes(x = lon, y = lat, label = paste("  ", as.character(name), sep="")), 
    #           size=3, angle = 0, vjust= -0.85, hjust = 0.95, color = "black") +
    # facet_grid(rcp~period, labeller = as_labeller(c(rcp_id, period_id))) +
    # theme(legend.position = "bottom") +
    NULL
  #plot(x)
  
  ggsave(paste("output/map_et_",gcm_input,".jpg",sep=""),plot=x, width = 7.5, height = 7)
}

et_map(tib=vic_maps_et, border=ss_border, gcm_input="CanESM2")
et_map(tib=vic_maps_et, border=ss_border, gcm_input="CNRMCM5")
et_map(tib=vic_maps_et, border=ss_border, gcm_input="HadGEM2ES")
et_map(tib=vic_maps_et, border=ss_border, gcm_input="MIROC5")




