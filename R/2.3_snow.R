# Snow Analysis
# 

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Processing

# Snow data
# values(snow)
# cellStats(snow, stat=mean)

# Process snow data for ggplot
snow_tib <- snow %>% 
  rasterToPoints() %>% 
  as_tibble() %>% 
  dplyr::select(x,y,histdjf,rcp85.djf) %>% 
  tidyr::gather(key=period, value=snowdjf,-c(x,y)) %>% 
  mutate(snowdjf_groups = cut(snowdjf, breaks=c(0,0.1,0.2,0.3,0.4,
                                                0.5,0.6,0.7,0.8,0.9,1)))



# ---------------------------------------------------------------------
# Figures - map


# Create labels for facetting
snow_id <- c(
  `histdjf` = "Historical",
  `rcp85.djf` = "Mid-Century (RCP 8.5)"
)

x <- ggplot() +
  geom_raster(data=snow_tib, aes(x,y, fill=snowdjf)) +
  geom_sf(data=ss_border, fill=NA, col="gray50") +
  scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  scale_fill_gradient2(low="blue", mid="red", high="aliceblue", midpoint = 0.5,name="Probability") +
  labs(title="Probability of Snowfall", x="Longitude",y="Latitude", size=0.5) +
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
  facet_grid(.~period, labeller = as_labeller(snow_id)) +
  theme(legend.position = "right")
#plot(x)

ggsave("output/map_snowline.jpg", width = 7, height = 5)




