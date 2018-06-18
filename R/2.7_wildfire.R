# Wildfire Analysis
# 

source("R/0_utilities.R")


# ---------------------------------------------------------------------
# Compare lumped fire between scenarios

# ----
# Process wildfire data

# Transfer from brick to tibble (and compute on mean value of region)
timeseries_mean <- function(brick){
  x <- cellStats(brick, stat=mean)
  y <- seq(1954,2100)
   tib <- tibble(year = y,
                 area_burned = x,
                 percent_area_burned = x/36)
  return(tib)
}

# Combine projection scenarios
fire_tib <- fire_stack %>% 
  purrr::map(timeseries_mean) %>% 
  bind_rows(.id="type") %>% 
  tidyr::separate(type, into = c("gcm", "rcp", "junk1", "junk2", "lc_scen", "junk3", "junk4")) %>% 
  unite(rcp, lc_scen, col="rcp_lc", remove=FALSE) %>% 
  dplyr::select(-c(junk1,junk2,junk3,junk4)) %>% 
  mutate(future_period = case_when(
    between(year,1954,2009) ~ "historical",
    between(year,2010,2039) ~ "2010-39",
    between(year,2040,2069) ~ "2040-69",
    between(year,2070,2100) ~ "2070-99"
  ))


# Change to percent

# ----
# Figures - Wildfire

historical_area_burned <- fire_tib %>% 
  dplyr::filter(future_period=="historical") %>% 
  summarize(historical_area_burned = mean(percent_area_burned))

fire_by_fp <- fire_tib %>% 
  dplyr::filter(future_period!="historical") %>% 
  group_by(gcm, rcp, future_period) %>% 
  summarise(mean_annual_percent_area_burned = mean(percent_area_burned))


ggplot() + 
  geom_boxplot(data=fire_by_fp, aes(future_period, mean_annual_percent_area_burned, fill=rcp),
               color="black") +
  geom_hline(data=historical_area_burned, aes(yintercept = historical_area_burned), color="gray20") +
  labs(title = "Projected Mean Annual Percent Area Burned", x = "Period", y = "Mean Annual Area Burned (%)") +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  ylim(0,3.25) +
  theme_bw()
ggsave("output/wildfire_box_annual.jpg", width = 4.5, height = 3.5)



# ---------------------------------------------------------------------
# Spatial Plots

# ----
# Process wildfire data

# Isolate only late century layers (2070-2099)
fire_map_1954_2005 <- purrr::map(fire_stack_map,function(x)raster::subset(x, 1:52))
fire_map_2040_2069 <- purrr::map(fire_stack_map,function(x)raster::subset(x, 87:116))
fire_map_2070_2099 <- purrr::map(fire_stack_map,function(x)raster::subset(x, 117:146))

# Calculate mean fire value for each pixel across time periods.
fire_map_list <- list(
  historical = purrr::map(fire_map_1954_2005, ~raster::calc(.x, mean)),
  mid_century = purrr::map(fire_map_2040_2069, ~raster::calc(.x, mean)),
  z_end_of_century = purrr::map(fire_map_2070_2099, ~raster::calc(.x, mean))
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
                       percent_area_burned = area_burned_h/36,  # Original data is hectares burned per pixel. 6x6km pixel is equal to 3600 ha. To get percent burned, dividing by 3600 and multipling by 100.
                       mean_annual_percent_area_burned = cut(percent_area_burned,
                                                             breaks=c(0,0.1,0.2,0.3,0.4,
                                                                      0.6,0.8,1.1,1.7,2.5,
                                                                      6,30)))

happy <- fire_map_tib %>% 
  #dplyr::filter(gcm=="CanESM2")
  dplyr::filter(gcm=="CNRMCM5")
  # dplyr::filter(gcm=="HadGEM2ES")
  # dplyr::filter(gcm=="MIROC5")
  summary(cut(happy$percent_area_burned,
              breaks=c(0,0.1,0.2,0.3,0.4,
                       0.6,0.8,1.1,1.7,2.5,
                       6,30)))

# Dimesions to deal with.
# 4 GCMs
# 2 RCP
# 3 land cover scenarios
# 3 periods (hist, mid century, and late century)
# Maybe 2 rows by 4 columns. Rows = hist and late century. Column = (45,L), (45,H), (85,L), (85,H)

# ----
# Plot wildfire figures

# Comparison of historical, mid and end of century by RCP/ medium land use
wildfire_map <- function(tib, border, gcm_input){
  
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
  
  fire_map_filter <- dplyr::filter(tib,
                                   lc_scen == "bau",
                                   #future_period != "mid_century",
                                   gcm==gcm_input)
  
  x <- ggplot() +
    geom_raster(data=fire_map_filter,aes(x=x,y=y, fill=mean_annual_percent_area_burned)) +
    geom_sf(data=border, fill=NA, col="white") +
    scale_fill_brewer(palette = "RdBu", direction=-1, name="Mean Annual\nArea Burned (%)") +
    scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
    scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
    labs(title=paste("Mean Annual Percent Area Burned"), x="Longitude",y="Latitude", size=0.5) +
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
    facet_grid(rcp~future_period, labeller = as_labeller(c(rcp_id, period_id))) +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = 'gray'))
  #plot(x)
  
  ggsave(paste("output/map_wildfire_",gcm_input,".jpg",sep=""),plot=x, width = 7, height = 7.8)
}

wildfire_map(tib=fire_map_tib, border=ss_border, gcm_input="CanESM2")
wildfire_map(tib=fire_map_tib, border=ss_border, gcm_input="CNRMCM5")
wildfire_map(tib=fire_map_tib, border=ss_border, gcm_input="HadGEM2ES")
wildfire_map(tib=fire_map_tib, border=ss_border, gcm_input="MIROC5")


# ----
# ----
# ----

# Comparison of historical and end of century by RCP/land use
wildfire2_map <- function(tib, border, gcm_input){

  # Create labels for facetting
  period_id <- c(
    `historical` = "Historical",
    `end_of_century` = "Late Century"
  )
  
  rcp_lc_id <- c(
    `45_L` = "RCP 4.5 - Low Land Cover",
    `85_L` = "RCP 8.5 - Low Land Cover",
    `45_H` = "RCP 4.5 - High Land Cover",
    `85_H` = "RCP 8.5 - High Land Cover"
  )
  
  fire_map_filter <- dplyr::filter(tib,
                                   lc_scen != "bau",
                                   future_period != "mid_century",
                                   gcm==gcm_input)
  
  x <- ggplot() +
    geom_raster(data=fire_map_filter,aes(x=x,y=y, fill=mean_annual_percent_area_burned)) +
    geom_sf(data=border, fill=NA, col="white") +
    scale_fill_brewer(palette = "RdBu", direction=-1, name="Mean Annual\nPercent Area Burned") +
    scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
    scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
    labs(title=paste("Mean Annual Percent Area Burned -", gcm_input), x="Longitude",y="Latitude", size=0.5) +
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
  
  ggsave(paste("output/map2_wildfire_",gcm_input,".jpg",sep=""),plot=x, width = 6, height = 6)
}

wildfire2_map(tib=fire_map_tib, border=ss_border, gcm_input="CanESM2")
wildfire2_map(tib=fire_map_tib, border=ss_border, gcm_input="CNRMCM5")
wildfire2_map(tib=fire_map_tib, border=ss_border, gcm_input="HadGEM2ES")
wildfire2_map(tib=fire_map_tib, border=ss_border, gcm_input="MIROC5")


