# Temperature Analysis
# 

source("R/0_utilities.R")


# ---------------------------------------------------------------------
# Time-series (processing)

# Guidelines from https://climate.northwestknowledge.net/MACA/MACAanalysis.php
# recommend that first average over years, then over study area
# Unclear if that will work for this analysis, since interest is region as whole


# Transfer from brick to tibble (and compute on mean value of region)
timeseries_mean <- function(brick){
  x <- cellStats(brick, stat=mean)
  y <- brick %>% 
    names() %>% 
    str_replace("X", "") %>%   # Remove extra 'X' from date 
    ymd()
  tib <- tibble(date = y,
                year = year(y),
                month = month(y),
                day = day(y),
                temp = x)
  return(tib)
}

# Combine projection scenarios
temp_hist_tib <- temp_hist %>% 
  purrr::map(timeseries_mean) %>% 
  bind_rows(.id="type") %>% 
  separate(type, into=c("temp_var", "rcp", "gcm")) %>% 
  mutate(season = case_when(
    (month==1)~"1",(month==2)~"1",(month==3)~"1",
    (month==4)~"2",(month==5)~"2",(month==6)~"2",
    (month==7)~"3",(month==8)~"3",(month==9)~"3",
    (month==10)~"4",(month==11)~"4",(month==12)~"4"
  ))


temp_proj_tib <- temp_proj %>% 
  purrr::map(timeseries_mean) %>% 
  bind_rows(.id="type") %>% 
  separate(type, into=c("temp_var", "rcp", "gcm")) %>% 
  mutate(season = case_when(
    (month==1)~"1",(month==2)~"1",(month==3)~"1",
    (month==4)~"2",(month==5)~"2",(month==6)~"2",
    (month==7)~"3",(month==8)~"3",(month==9)~"3",
    (month==10)~"4",(month==11)~"4",(month==12)~"4"
  ))

# Group historical by year so that they means can be joined to future scenarios
temp_hist_year <- temp_hist_tib %>% 
  group_by(temp_var, gcm) %>% 
  summarize(temp_hist_annual = mean(temp))

# Group historical by month so that they means can be joined to future scenarios
temp_hist_month <- temp_hist_tib %>% 
  group_by(temp_var, gcm, month) %>% 
  summarize(temp_hist_month = mean(temp))

# Group historical by season so that they means can be joined to future scenarios
temp_hist_season <- temp_hist_tib %>% 
  group_by(temp_var, gcm, season) %>% 
  summarize(temp_hist_season = mean(temp))

# Join historical scenarios to future projections and add future period groupings
temp_tib_final <- temp_proj_tib %>% 
  left_join(temp_hist_year, by = c("temp_var", "gcm")) %>% 
  left_join(temp_hist_month, by = c("temp_var", "gcm", "month")) %>% 
  left_join(temp_hist_season, by = c("temp_var", "gcm", "season")) %>% 
  dplyr::filter(year >= 2010) %>% 
  mutate(future_period = case_when(
    between(year,2010,2039) ~ "2010-39",
    between(year,2040,2069) ~ "2040-69",
    between(year,2070,2099) ~ "2070-99"
  ))

# ----

# Produce summarized annual data
temp_annual <- temp_tib_final %>% 
  group_by(temp_var, rcp, gcm, year, future_period) %>% 
  summarise(temp = mean(temp), temp_hist_annual=mean(temp_hist_annual)) %>% 
  mutate(temp_annual_diff = temp-temp_hist_annual)
# Produce summarized annual data by future period
temp_annual_by_fp <- temp_annual %>% 
  group_by(temp_var, rcp, gcm, future_period) %>% 
  summarise(temp = mean(temp), temp_hist_annual=mean(temp_hist_annual),
            temp_annual_diff=mean(temp_annual_diff))

temp_annual_max <- dplyr::filter(temp_annual, temp_var == "tmax")
temp_annual_min <- dplyr::filter(temp_annual, temp_var == "tmin")
temp_annual_by_fp_max <- dplyr::filter(temp_annual_by_fp, temp_var == "tmax")
temp_annual_by_fp_min <- dplyr::filter(temp_annual_by_fp, temp_var == "tmin")


# Produce summarized monthly data by year
temp_month <- temp_tib_final %>% 
  mutate(temp_monthly_diff = temp-temp_hist_month)
# Produce summarized monthly data by future period
temp_month_by_fp <- temp_month %>% 
  group_by(temp_var, rcp, gcm, month, future_period) %>% 
  summarise(temp = mean(temp), temp_hist_month=mean(temp_hist_month),
            temp_monthly_diff=mean(temp_monthly_diff))

temp_month_max <- dplyr::filter(temp_month, temp_var == "tmax")
temp_month_min <- dplyr::filter(temp_month, temp_var == "tmin")
temp_month_by_fp_max <- dplyr::filter(temp_month_by_fp, temp_var == "tmax")
temp_month_by_fp_min <- dplyr::filter(temp_month_by_fp, temp_var == "tmin")


# Produce summarized seasonal data by year
temp_season <- temp_tib_final %>% 
  group_by(temp_var, rcp, gcm, year, season, future_period) %>% 
  summarise(temp = mean(temp), temp_hist_season = mean(temp_hist_season)) %>% 
  mutate(temp_seasonal_diff = temp-temp_hist_season)
# Produce summarized seasonal data by future period
temp_season_by_fp <- temp_season %>% 
  group_by(temp_var, rcp, gcm, season, future_period) %>% 
  summarise(temp = mean(temp), temp_hist_season=mean(temp_hist_season),
            temp_seasonal_diff=mean(temp_seasonal_diff))

temp_season_max <- dplyr::filter(temp_season, temp_var == "tmax")
temp_season_min <- dplyr::filter(temp_season, temp_var == "tmin")
temp_season_by_fp_max <- dplyr::filter(temp_season_by_fp, temp_var == "tmax")
temp_season_by_fp_min <- dplyr::filter(temp_season_by_fp, temp_var == "tmin")


# ---------------------------------------------------------------------
# Time-series figures

# Annual
ggplot() +
  geom_line(data=temp_annual, aes(x=year,y=temp, col=rcp, linetype=temp_var)) +
  facet_grid(.~gcm)

# Annual Difference
ggplot() +
  geom_line(data=temp_annual,aes(x=year,y=temp_annual_diff, col=rcp)) +
  facet_grid(temp_var~gcm)


# ---------------------------------------------------------------------
# Box plot by time-periods (annual)

temp_id <- c(
  `tmax` = "Maximum Temperature",
  `tmin` = "Minimum Temperature"
)

# Actual temperatures - Max
ggplot() + 
  geom_boxplot(data=temp_annual_by_fp_max, aes(future_period, temp, fill=rcp),
               color="black") +
  labs(title = "Projected Maximum Annual Temperatures", x = "Period", y = expression('Temperature'~'('*degree*'C)')) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  theme_bw()
ggsave("output/box_annual_max.jpg", width = 4, height = 3)


# Actual temperatures - Min
ggplot() + 
  geom_boxplot(data=temp_annual_by_fp_min, aes(future_period, temp, fill=rcp),
               color="black") +
  labs(title = "Projected Minimum Annual Temperatures", x = "Period", y = expression('Temperature'~'('*degree*'C)')) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  theme_bw()
ggsave("output/box_annual_min.jpg", width = 4, height = 3)


# Changes in temperature
ggplot() + 
  geom_boxplot(data=temp_annual_by_fp, aes(future_period, temp_annual_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Change in Annual Temperatures", x = "Period", y = expression('Change'~'in'~'Temperature'~'('*degree*'C)')) +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_grid(.~temp_var, labeller = as_labeller(temp_id)) +
  theme_bw()
ggsave("output/box_annual_delta.jpg", width = 6, height = 4)


# Baseline annual temperature values
temp_annual_by_fp %>% 
  group_by(temp_var) %>% 
  summarise(mean = mean(temp_hist_annual))


# ---------------------------------------------------------------------
# Box plot by time-periods (monthly)

month_id <- c(
  `1` = "Jan", `2` = "Feb", `3` = "Mar",`4` = "Apr",
  `5` = "May",`6` = "Jun",`7` = "Jul",`8` = "Aug",
  `9` = "Sep",`10` = "Oct",`11` = "Nov",`12` = "Dec"
)

# Actual Monthly Temperatures - Max
ggplot() + 
  geom_boxplot(data=temp_month_by_fp_max, aes(future_period, temp, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Maximum Monthly Temperatures", x = "Period", y = expression('Temperature'~'('*degree*'C)')) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_wrap(~month, labeller = as_labeller(month_id)) +
  theme_bw()
ggsave("output/box_monthly_max.jpg", width = 8, height = 6)

# Actual Monthly Temperatures - Min
ggplot() + 
  geom_boxplot(data=temp_month_by_fp_min, aes(future_period, temp, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Minimum Monthly Temperatures", x = "Period", y = expression('Temperature'~'('*degree*'C)')) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_wrap(~month, labeller = as_labeller(month_id)) +
  theme_bw()
ggsave("output/box_monthly_min.jpg", width = 8, height = 6)

# Change in monthly temperature - Max 
ggplot() + 
  geom_boxplot(data=temp_month_by_fp_max, aes(future_period, temp_monthly_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Change in Maximum Monthly Temperatures", x = "Period", y = expression('Change'~'in'~'Temperature'~'('*degree*'C)')) +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_wrap(~month, labeller = as_labeller(month_id)) +
  theme_bw()
ggsave("output/box_monthly_max_delta.jpg", width = 8, height = 6)

# Change in monthly temperature - Min 
ggplot() + 
  geom_boxplot(data=temp_month_by_fp_min, aes(future_period, temp_monthly_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Change in Minimum Monthly Temperatures", x = "Period", y = expression('Change'~'in'~'Temperature'~'('*degree*'C)')) +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_wrap(~month, labeller = as_labeller(month_id)) +
  theme_bw()
ggsave("output/box_monthly_min_delta.jpg", width = 8, height = 6)

# ---------------------------------------------------------------------
# Box plot by time-periods (Seasonal)

season_id <- c(
  `1` = "Jan-Feb-Mar",
  `2` = "Apr-May-Jun",
  `3` = "Jul-Aug-Sep",
  `4` = "Oct-Nov-Dec"
)

# Actual Seasonal Temperatures - Max
ggplot() + 
  geom_boxplot(data=temp_season_by_fp_max, aes(future_period, temp, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Maximum Seasonal Temperatures", x = "Period", y = expression('Temperature'~'('*degree*'C)')) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_wrap(~season, labeller = as_labeller(season_id)) +
  theme_bw()
ggsave("output/box_seasonal_max.jpg", width = 5, height = 4)

# Actual Seasonal Temperatures - Min
ggplot() + 
  geom_boxplot(data=temp_season_by_fp_min, aes(future_period, temp, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Minimum Seasonal Temperatures", x = "Period", y = expression('Temperature'~'('*degree*'C)')) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_wrap(~season, labeller = as_labeller(season_id)) +
  theme_bw()
ggsave("output/box_seasonal_min.jpg", width = 5, height = 4)

# Change in seasonal temperature - Max 
ggplot() + 
  geom_boxplot(data=temp_season_by_fp_max, aes(future_period, temp_seasonal_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Change in Maximum Seasonal Temperatures", x = "Period", y = expression('Change'~'in'~'Temperature'~'('*degree*'C)')) +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_wrap(~season, labeller = as_labeller(season_id)) +
  theme_bw()
ggsave("output/box_seasonal_max_delta.jpg", width = 5, height = 4)

# Change in seasonal temperature - Min 
ggplot() + 
  geom_boxplot(data=temp_season_by_fp_min, aes(future_period, temp_seasonal_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Change in Minimum Seasonal Temperatures", x = "Period", y = expression('Change'~'in'~'Temperature'~'('*degree*'C)')) +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_wrap(~season, labeller = as_labeller(season_id)) +
  theme_bw()
ggsave("output/box_seasonal_min_delta.jpg", width = 5, height = 4)

# Baseline seasonal temperature values
temp_season_by_fp %>% 
  group_by(temp_var, season) %>% 
  summarise(mean = mean(temp_hist_season))

# ---------------------------------------------------------------------
# Mean monthly temperatures plot

# happy <- temp_month_by_fp_max %>% 
#   group_by(rcp,future_period,month) %>% 
#   summarize(month_temp=mean(temp))
# 
# ggplot() +
#   geom_line(data=happy, aes(x=month,y=month_temp, color=future_period)) +
#   facet_grid(rcp~.)



# ---------------------------------------------------------------------
# Table (Historical plus 30-year change groupings)

library(knitr)
library(kableExtra)

# 
temp_annual_by_fp %>% 
  group_by(temp_var,rcp,future_period) %>% 
  summarize(temp_annual_diff_period = mean(temp_annual_diff))


temp_month_by_fp %>% 
  group_by(temp_var,rcp,future_period, month) %>% 
  summarize(temp_month_diff_period = mean(temp_monthly_diff)) %>%
  unite(rcp_period, rcp, future_period) %>% 
  spread(key = rcp_period, value = temp_month_diff_period)
#rename columns

temp_season_by_fp %>% 
  group_by(temp_var,rcp,future_period, season) %>% 
  summarize(temp_season_diff_period = median(temp_seasonal_diff)) %>%
  unite(rcp_period, rcp, future_period) %>% 
  spread(key = rcp_period, value = temp_season_diff_period)
#rename columns



# ---------------------------------------------------------------------
# Spatial Plots

# ----
# Process temperature data

# Isolate only late century layers (2070-2099)
temp_proj_map_2070_2099 <- map(temp_proj_map,function(x)raster::subset(x, 769:1128))

# Generates mean temperatures
map_tmax_hist_ccsm4 <- calc(temp_hist_map$tmax_hist_ccsm4, mean)
map_tmax_45_ccsm4 <- calc(temp_proj_map_2070_2099$tmax_45_ccsm4, mean)
map_tmax_85_ccsm4 <- calc(temp_proj_map_2070_2099$tmax_85_ccsm4, mean)
map_tmin_hist_ccsm4 <- calc(temp_hist_map$tmin_hist_ccsm4, mean) 
map_tmin_45_ccsm4 <- calc(temp_proj_map_2070_2099$tmin_45_ccsm4, mean)
map_tmin_85_ccsm4 <- calc(temp_proj_map_2070_2099$tmin_85_ccsm4, mean)

# Maximum Temperature 
# Tidy the rasters so maps can be plotted
map_tmax_ccsm4_stack <- stack(map_tmax_hist_ccsm4,map_tmax_45_ccsm4,map_tmax_85_ccsm4)
names(map_tmax_ccsm4_stack) =  c("map_tmax_1hist_ccsm4","map_tmax_45_ccsm4","map_tmax_85_ccsm4")
map_tmax_ccsm4_p <- rasterToPoints(map_tmax_ccsm4_stack)
map_tmax_ccsm4_tib <- as_tibble(map_tmax_ccsm4_p)
map_tmax_ccsm4 <- gather(map_tmax_ccsm4_tib, key="layer", value="temp",
                         "map_tmax_1hist_ccsm4","map_tmax_45_ccsm4","map_tmax_85_ccsm4")
map_tmax_ccsm4 <- mutate(map_tmax_ccsm4, 
                         temp_interval = cut_width(temp, width=3,
                                                   center = .5))

# Minimum Temperature
# Tidy the rasters so maps can be plotted
map_tmin_ccsm4_stack <- stack(map_tmin_hist_ccsm4,map_tmin_45_ccsm4,map_tmin_85_ccsm4)
names(map_tmin_ccsm4_stack) =  c("map_tmin_1hist_ccsm4","map_tmin_45_ccsm4","map_tmin_85_ccsm4")
map_tmin_ccsm4_p <- rasterToPoints(map_tmin_ccsm4_stack)
map_tmin_ccsm4_tib <- as_tibble(map_tmin_ccsm4_p)
map_tmin_ccsm4 <- gather(map_tmin_ccsm4_tib, key="layer", value="temp",
                         "map_tmin_1hist_ccsm4","map_tmin_45_ccsm4","map_tmin_85_ccsm4")
map_tmin_ccsm4 <- mutate(map_tmin_ccsm4, 
                         temp_interval = cut_width(temp, width=3,
                                                   center = .5))

# Create labels for facetting
tmax_id <- c(
  `map_tmax_1hist_ccsm4` = "Historical",
  `map_tmax_45_ccsm4` = "End of Century (RCP4.5)",
  `map_tmax_85_ccsm4` = "End of Century (RCP8.5)"
)

tmin_id <- c(
  `map_tmin_1hist_ccsm4` = "Historical",
  `map_tmin_45_ccsm4` = "End of Century (RCP4.5)",
  `map_tmin_85_ccsm4` = "End of Century (RCP8.5)"
)


# ----
# Plot temperatures

# Maximum Temperature - Continuous
x <- ggplot() +
  geom_raster(data=map_tmax_ccsm4,aes(x=x,y=y, fill=temp)) +
  geom_sf(data=ss_border, fill=NA, col="white") +
  scale_fill_continuous(low="blue", high="red", name=expression('Temperature'~'('*degree*'C)')) +
  scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  labs(title="Mean Annual Maximum Air Temperature", x="Longitude",y="Latitude", size=0.5) +
  theme_classic(base_size =12) +
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
  facet_grid(.~layer, labeller = as_labeller(tmax_id)) +
  theme(legend.position = "bottom")
plot(x)


# Maximum Temperature - Discrete
x <- ggplot() +
  geom_raster(data=map_tmax_ccsm4,aes(x=x,y=y, fill=temp_interval)) +
  geom_sf(data=ss_border, fill=NA, col="white") +
  scale_fill_brewer(palette = "RdBu", direction=-1, name=expression('Temperature'~'('*degree*'C)')) +
  scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  labs(title="Mean Annual Maximum Air Temperature", x="Longitude",y="Latitude", size=0.5) +
  theme_classic(base_size =12) +
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
  facet_grid(.~layer, labeller = as_labeller(tmax_id)) +
  theme(legend.position = "bottom")
plot(x)
ggsave("output/map_maximum_temperature.jpg", width = 8, height = 5)


# Minimum Temperature - Continuous
x <- ggplot() +
  geom_raster(data=map_tmin_ccsm4,aes(x=x,y=y, fill=temp)) +
  geom_sf(data=ss_border, fill=NA, col="white") +
  scale_fill_continuous(low="blue", high="red", name=expression('Temperature'~'('*degree*'C)')) +
  scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  labs(title="Mean Annual Minimum Air Temperature", x="Longitude",y="Latitude", size=0.5) +
  theme_classic(base_size =12) +
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
  facet_grid(.~layer, labeller = as_labeller(tmin_id)) +
  theme(legend.position = "bottom")
plot(x)


# Minimum Temperature - Discrete
x <- ggplot() +
  geom_raster(data=map_tmin_ccsm4,aes(x=x,y=y, fill=temp_interval)) +
  geom_sf(data=ss_border, fill=NA, col="white") +
  scale_fill_brewer(palette = "RdBu", direction=-1, name=expression('Temperature'~'('*degree*'C)')) +
  scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  labs(title="Mean Annual Minimum Air Temperature", x="Longitude",y="Latitude", size=0.5) +
  theme_classic(base_size =12) +
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
  facet_grid(.~layer, labeller = as_labeller(tmin_id)) +
  theme(legend.position = "bottom")
plot(x)
ggsave("output/map_minimum_temperature.jpg", width = 8, height = 5)



# ---------------------------------------------------------------------
# 


