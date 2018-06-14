# Wildfire Analysis
# 

source("R/0_utilities.R")


# ---------------------------------------------------------------------
# Data processing


fire_stack


# ---------------------------------------------------------------------
# Spatial Plots

# ----
# Process temperature data

fire_files_names
newname

# Isolate only late century layers (2070-2099)
fire_map_1954_2005 <- purrr::map(fire_stack_map,function(x)raster::subset(x, 1:52))
fire_map_2040_2069 <- purrr::map(fire_stack_map,function(x)raster::subset(x, 87:116))
fire_map_2070_2099 <- purrr::map(fire_stack_map,function(x)raster::subset(x, 117:146))

# Calculate mean fire value for each pixel across time periods.
fire_map_list <- list(
  historical = purrr::map(fire_map_1954_2005, ~raster::calc(.x, mean)),
  mid_century = purrr::map(fire_map_2040_2069, ~raster::calc(.x, mean)),
  end_of_century = purrr::map(fire_map_2070_2099, ~raster::calc(.x, mean))
)

# Turn fire data into tibble that can be plotted using geom_sf
fire_map_tib <- purrr::map(fire_map_list,function(x)
  {
  x %>% 
    raster::stack() %>% 
    rasterToPoints() %>% 
    as_tibble() %>% 
    tidyr::gather(key="layer", value="area_burned_h", seq(3,26)) %>% 
    tidyr::separate(layer, 
                    into = c("gcm", "rcp", "junk1", "junk2", "lc_scen", "junk3", "junk4")) %>% 
    unite(rcp, lc_scen, col="rcp_lc", remove=FALSE) %>% 
    dplyr::select(-c(junk1,junk2,junk3,junk4))
  }
)

# Combine tibbles from three periods
fire_map_tib <- bind_rows(fire_map_tib, .id="future_period")

fire_map_tib <- mutate(fire_map_tib, 
                       fire_interval = cut(area_burned_h,
                                           breaks=c(0,5,10,15,25,35,55,85,135,200,300,420)))

  
# What would be best? Dimesions to deal with.
# 2 RCP
# 3 land cover
# 3 periods (hist, mid century, and late century)
# Maybe 2 rows by 4 columns. Rows = hist and late century. Column = (45,L), (45,H), (85,L), (85,H)



# Create labels for facetting
period_id <- c(
  `historical` = "Historical",
  `end_of_century` = "End of Century"
)

rcp_lc_id <- c(
  `45_L` = "RCP 4.5 - Low Land Cover",
  `85_L` = "RCP 8.5 - Low Land Cover",
  `45_H` = "RCP 4.5 - High Land Cover",
  `85_H` = "RCP 8.5 - High Land Cover"
)

rcp_id <- c(
  `45` = "RCP 4.5",
  `85` = "RCP 8.5"
)

lc_id <- c(
  `L` = "Low Land Cover",
  `H` = "High Land Cover"
)


# ----
# Plot fire figures

# All Temperatures - Continuous

fire_map_filter <- dplyr::filter(fire_map_tib, lc_scen != "bau", future_period != "mid_century")
# Average over gcm?
# Change area burnt to percent burnt

x <- ggplot() +
  geom_raster(data=fire_map_filter,aes(x=x,y=y, fill=fire_interval)) +
  geom_sf(data=ss_border, fill=NA, col="white") +
#  scale_fill_continuous(low="blue", high="red", name=expression('Temperature'~'('*degree*'C)')) +
  scale_fill_brewer(palette = "RdBu", direction=-1, name=expression('Temperature'~'('*degree*'C)')) +
  scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  labs(title="Average Annual Area Burnt", x="Longitude",y="Latitude", size=0.5) +
  theme_classic(base_size =12) +
  theme(axis.text.x = element_text(angle = 330, hjust=0)) +
  geom_point(data = dplyr::filter(cities, name != 'Fresno'), aes(x = lon, y = lat), 
             shape = 19, color = "black", fill = "grey50", size = 1.2) +
  geom_text(data = dplyr::filter(cities, name == 'Visalia'), 
            aes(x = lon, y = lat, label = paste("  ", as.character(name), sep="")), 
            size=3, angle = 0, vjust= -0.85, hjust = 0.95, color = "black") +
  geom_text(data = dplyr::filter(cities, name == 'Porterville'), 
            aes(x = lon, y = lat, label = paste("  ", as.character(name), sep="")), 
            size=3, angle = 0, vjust= -0.85, hjust = 1.1, color = "black") +
  geom_text(data = dplyr::filter(cities, name == 'Bishop'), 
            aes(x = lon, y = lat, label = paste("  ", as.character(name), sep="")), 
            size=3, angle = 0, vjust= -0.85, hjust = 0.95, color = "black") +
  facet_grid(future_period~rcp_lc, labeller = as_labeller(c(period_id, rcp_lc_id))) +
  theme(legend.position = "bottom")
#plot(x)

ggsave("output/map_wildfire.jpg",plot=x, width = 8, height = 9)

