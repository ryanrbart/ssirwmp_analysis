# VIC (ET and SWE) Maps


source("R/0_utilities.R")


# ---------------------------------------------------------------------
# Data processing

# Reshape the vic_map files so that all the coordinates are in a single column
# This procedure is done separately for each Flux Type, GCM and Period
vic_maps_sum <- purrr::map(vic_maps, function(x){
  tidyr::gather(x,key="location",value="value",2:2046)
})

# Combine tibbles from list into one tibble
# Separate out coordinates and variable/gcm/period
# Filter 4.5 and 8.5 scenarios for end-of-century years
# Summarize across WY to get average for each location
vic_maps_tib <- vic_maps_sum %>% 
  bind_rows(.id="type_gcm_period") %>% 
  tidyr::separate(type_gcm_period,into = c("wy", "flux_type", "gcm", "period")) %>% 
  tidyr::separate(location,into = c("flux", "lat", "long"), sep="_") %>% 
  dplyr::select(-c(wy, flux)) %>%     # These variables don't actually vary.
  dplyr::filter(period == "hist" |
                (period == 45 & WY >= 2070) |
                (period == 85 & WY >= 2070)) %>% 
  dplyr::group_by(flux_type,gcm,period,lat,long) %>% 
  dplyr::summarise(mean_value = mean(value))

# Change lat and long from character to double
vic_maps_tib$lat <- as.double(vic_maps_tib$lat)
vic_maps_tib$long <- as.double(vic_maps_tib$long)
period_levels <- c("hist", "45", "85")
vic_maps_tib$period <- factor(vic_maps_tib$period, levels = period_levels)


# ----
# ET Processing
vic_maps_et <- vic_maps_tib %>% 
  dplyr::filter(flux_type=="et") %>% 
  #mutate(value_interval = cut_interval(mean_value, n=11))
  dplyr::mutate(value_interval = cut_interval(mean_value,n=11,
                                              breaks = c(0,100,200,300,
                                                         350,400,450,500,
                                                         550,600,700,800),
                                              labels = c("0-100","100-200","200-300","300-350",
                                                        "350-400","400-450","450-500",
                                                         "500-550","550-600","600-700","700-800")))

# Plot ET map
vic_map <- function(tib, border, flux_type, flux_name, gcm_input){
  
  # Create labels for facetting
  period_id <- c(
    `hist` = "Historical",
    `45` = "End of Century (RCP4.5)",
    `85` = "End of Century (RCP8.5)"
  )
  
  tib_gcm <- dplyr::filter(tib, gcm == gcm_input)

  x <- ggplot(tib_gcm) +
    geom_raster(aes(x=long,y=lat, fill=value_interval)) +
    geom_sf(data=border, fill=NA, col="black") +
    scale_fill_brewer(palette = "RdBu", direction = 1, name="ET (mm/year)") +
    scale_x_continuous(expand=c(0,0), limits = c(-119.8, -117.86)) +   # This eliminates margin buffer around plot
    scale_y_continuous(expand=c(0,0), limits = c(35.65, 37.85)) +   # This eliminates margin buffer around plot
    labs(title=paste(flux_name), x="Longitude",y="Latitude", size=0.5) +
    theme_classic(base_size =10) +
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
    facet_grid(.~period, labeller = as_labeller(c(period_id))) +
    theme(legend.position = "bottom") +
    NULL
  #plot(x)
  
  ggsave(paste("output/map_",flux_type,"_",gcm_input,".jpg",sep=""),plot=x, width = 7.2, height = 5)
}

vic_map(tib=vic_maps_et, border=ss_border, flux_type = "et", flux_name = "Mean Annual Evapotranspiration", gcm_input="canesm2") 
vic_map(tib=vic_maps_et, border=ss_border, flux_type = "et", flux_name = "Mean Annual Evapotranspiration", gcm_input="ccsm4") 
vic_map(tib=vic_maps_et, border=ss_border, flux_type = "et", flux_name = "Mean Annual Evapotranspiration", gcm_input="cnrm") 
vic_map(tib=vic_maps_et, border=ss_border, flux_type = "et", flux_name = "Mean Annual Evapotranspiration", gcm_input="hadgemcc") 
vic_map(tib=vic_maps_et, border=ss_border, flux_type = "et", flux_name = "Mean Annual Evapotranspiration", gcm_input="hadgemec") 
vic_map(tib=vic_maps_et, border=ss_border, flux_type = "et", flux_name = "Mean Annual Evapotranspiration", gcm_input="miroc5") 



# ----
# SWE Processing

vic_maps_swe <- vic_maps_tib %>% 
  dplyr::filter(flux_type=="swe") %>% 
  #mutate(value_interval = cut_interval(mean_value, n=11))
  dplyr::mutate(value_interval = cut_interval(mean_value,n=10,
                                              breaks = c(0,200,400,600,
                                                         800,1000,1200,
                                                         1400,1600,2000),
                                              labels = c("0-200","200-400","400-600","600-800",
                                                         "800-1000","1000-1200","1200-1400",
                                                         "1400-1600","1600-2000")))


# Plot SWE map
vic_map <- function(tib, border, flux_type, flux_name, gcm_input){
  
  # Create labels for facetting
  period_id <- c(
    `hist` = "Historical",
    `45` = "End of Century (RCP4.5)",
    `85` = "End of Century (RCP8.5)"
  )
  
  tib_gcm <- dplyr::filter(tib, gcm == gcm_input)
  
  x <- ggplot(tib_gcm) +
    geom_raster(aes(x=long,y=lat, fill=value_interval)) +
    geom_sf(data=border, fill=NA, col="black") +
    scale_fill_brewer(palette = "Blues", direction = 1, name="Mean Annual\nPeak SWE (mm)") +
    scale_x_continuous(expand=c(0,0), limits = c(-119.8, -117.86)) +   # This eliminates margin buffer around plot
    scale_y_continuous(expand=c(0,0), limits = c(35.65, 37.85)) +   # This eliminates margin buffer around plot
    labs(title=paste(flux_name), x="Longitude",y="Latitude", size=0.5) +
    theme_classic(base_size =10) +
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
  facet_grid(.~period, labeller = as_labeller(c(period_id))) +
    theme(legend.position = "bottom") +
    NULL
  #plot(x)
  
  ggsave(paste("output/map_",flux_type,"_",gcm_input,".jpg",sep=""),plot=x, width = 7.2, height = 5)
}


vic_map(tib=vic_maps_swe, border=ss_border, flux_type = "swe", flux_name = "Mean Annual Peak Snow Water Equivalent", gcm_input="canesm2") 
vic_map(tib=vic_maps_swe, border=ss_border, flux_type = "swe", flux_name = "Mean Annual Peak Snow Water Equivalent", gcm_input="ccsm4") 
vic_map(tib=vic_maps_swe, border=ss_border, flux_type = "swe", flux_name = "Mean Annual Peak Snow Water Equivalent", gcm_input="cnrm") 
vic_map(tib=vic_maps_swe, border=ss_border, flux_type = "swe", flux_name = "Mean Annual Peak Snow Water Equivalent", gcm_input="hadgemcc") 
vic_map(tib=vic_maps_swe, border=ss_border, flux_type = "swe", flux_name = "Mean Annual Peak Snow Water Equivalent", gcm_input="hadgemec") 
vic_map(tib=vic_maps_swe, border=ss_border, flux_type = "swe", flux_name = "Mean Annual Peak Snow Water Equivalent", gcm_input="miroc5") 



