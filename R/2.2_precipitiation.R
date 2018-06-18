# Precipitation Analysis
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
                precip = x)
  return(tib)
}

# Combine projection scenarios
precip_hist_tib <- precip_hist %>% 
  purrr::map(timeseries_mean) %>% 
  bind_rows(.id="type") %>% 
  separate(type, into=c("precip_var", "rcp", "gcm")) %>% 
  mutate(season = case_when(
    (month==1)~"1",(month==2)~"1",(month==3)~"1",
    (month==4)~"2",(month==5)~"2",(month==6)~"2",
    (month==7)~"3",(month==8)~"3",(month==9)~"3",
    (month==10)~"4",(month==11)~"4",(month==12)~"4"
  ))


precip_proj_tib <- precip_proj %>% 
  purrr::map(timeseries_mean) %>% 
  bind_rows(.id="type") %>% 
  separate(type, into=c("precip_var", "rcp", "gcm")) %>% 
  mutate(season = case_when(
    (month==1)~"1",(month==2)~"1",(month==3)~"1",
    (month==4)~"2",(month==5)~"2",(month==6)~"2",
    (month==7)~"3",(month==8)~"3",(month==9)~"3",
    (month==10)~"4",(month==11)~"4",(month==12)~"4"
  ))


# Group historical by year so that they means can be joined to future scenarios
precip_hist_year <- precip_hist_tib %>% 
  group_by(precip_var, gcm) %>% 
  summarize(precip_hist_annual = mean(precip))

# Group historical by month so that they means can be joined to future scenarios
precip_hist_month <- precip_hist_tib %>% 
  group_by(precip_var, gcm, month) %>% 
  summarize(precip_hist_month = mean(precip))

# Group historical by season so that they means can be joined to future scenarios
precip_hist_season <- precip_hist_tib %>% 
  group_by(precip_var, gcm, season) %>% 
  summarize(precip_hist_season = mean(precip))

# Join historical scenarios to future projections and add future period groupings
precip_tib_final <- precip_proj_tib %>% 
  left_join(precip_hist_year, by = c("precip_var", "gcm")) %>% 
  left_join(precip_hist_month, by = c("precip_var", "gcm", "month")) %>% 
  left_join(precip_hist_season, by = c("precip_var", "gcm", "season")) %>% 
  dplyr::filter(year >= 2010) %>% 
  mutate(future_period = case_when(
    between(year,2010,2039) ~ "2010-39",
    between(year,2040,2069) ~ "2040-69",
    between(year,2070,2099) ~ "2070-99"
  ))
# Note: all precip values are monthly in mm

# ----

# Produce summarized annual data by year
precip_annual <- precip_tib_final %>% 
  group_by(precip_var, rcp, gcm, year, future_period) %>% 
  summarise(precip = sum(precip), precip_hist_annual = sum(precip_hist_annual)) %>% 
  mutate(precip_annual_diff = precip-precip_hist_annual)
# Produce summarized annual data by future period
precip_annual_by_fp <- precip_annual %>% 
  group_by(precip_var, rcp, gcm, future_period) %>% 
  summarise(precip = mean(precip), precip_hist_annual=mean(precip_hist_annual),
            precip_annual_diff=mean(precip_annual_diff))

# Produce summarized monthly data
precip_month <- precip_tib_final %>% 
  mutate(precip_monthly_diff = precip-precip_hist_month)
# Produce summarized monthly data by future period
precip_month_by_fp <- precip_month %>% 
  group_by(precip_var, rcp, gcm, month, future_period) %>% 
  summarise(precip = mean(precip), precip_hist_month=mean(precip_hist_month),
            precip_monthly_diff=mean(precip_monthly_diff))

# Produce summarized seasonal data
precip_season <- precip_tib_final %>% 
  group_by(precip_var, rcp, gcm, year, season, future_period) %>% 
  summarise(precip = sum(precip), precip_hist_season = sum(precip_hist_season)) %>% 
  mutate(precip_seasonal_diff = precip-precip_hist_season)
# Produce summarized seasonal data by future period
precip_season_by_fp <- precip_season %>% 
  group_by(precip_var, rcp, gcm, season, future_period) %>% 
  summarise(precip = mean(precip), precip_hist_season=mean(precip_hist_season),
            precip_seasonal_diff=mean(precip_seasonal_diff))

# ---------------------------------------------------------------------
# Time-series

# Annual
ggplot() +
  geom_line(data=precip_annual, aes(x=year,y=precip, col=rcp)) +
  facet_grid(gcm~.)

# Annual Difference
ggplot() +
  geom_line(data=precip_annual,aes(x=year,y=precip_annual_diff, col=rcp)) +
  facet_grid(gcm~.)


# ---------------------------------------------------------------------
# Box plot by time-periods (annual)

# Actual Precipitation
ggplot() + 
  geom_boxplot(data=precip_annual_by_fp, aes(future_period, precip, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Annual Precipitation", x = "Period", y = "Annual Precipitation (mm)") +
  geom_hline(data=precip_annual_by_fp, aes(yintercept = precip_hist_annual), color="gray20") +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  scale_y_continuous(sec.axis = sec_axis(~./25.4, name = "Annual Precipitation (in)")) +
  theme_bw()
ggsave("output/precip_box_annual.jpg", width = 5.5, height = 3.5)

# Changes in Precipitation
ggplot() + 
  geom_boxplot(data=precip_annual_by_fp, aes(future_period, precip_annual_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Change in Annual Precipitation", x = "Period", y = "Change in Annual Precipitation (mm)") +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  scale_y_continuous(sec.axis = sec_axis(~./25.4, name = "Change in Annual Precipitation (in)")) +
  theme_bw()
ggsave("output/precip_box_annual_delta.jpg", width = 5.5, height = 3.5)

# Baseline annual precipitation values
precip_annual_by_fp %>% 
  summarise(mean = mean(precip_hist_annual))


# ---------------------------------------------------------------------
# Box plot by time-periods (monthly)

month_id <- c(
  `1` = "Jan", `2` = "Feb", `3` = "Mar",`4` = "Apr",
  `5` = "May",`6` = "Jun",`7` = "Jul",`8` = "Aug",
  `9` = "Sep",`10` = "Oct",`11` = "Nov",`12` = "Dec"
)

# Actual Monthly Precipitation
ggplot() + 
  geom_boxplot(data=precip_month_by_fp, aes(future_period, precip, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Monthly Precipitation", x = "Period", y = "Monthly Precipitation (mm)") +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_wrap(~month, labeller = as_labeller(month_id)) +
  scale_y_continuous(sec.axis = sec_axis(~./25.4, name = "Annual Precipitation (in)")) +
  theme_bw()
ggsave("output/precip_box_monthly.jpg", width = 8, height = 6)

# Change in monthly precipitation
ggplot() + 
  geom_boxplot(data=precip_month_by_fp, aes(future_period, precip_monthly_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Change in Monthly Precipitation", x = "Period", y = "Change in Monthly Precipitation (mm)") +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_wrap(~month, labeller = as_labeller(month_id)) +
  scale_y_continuous(sec.axis = sec_axis(~./25.4, name = "Change in Annual Precipitation (in)")) +
  theme_bw()
ggsave("output/precip_box_monthly_delta.jpg", width = 8, height = 6)

# ---------------------------------------------------------------------
# Box plot by time-periods (Seasonal)

season_id <- c(
  `1` = "Jan-Feb-Mar",
  `2` = "Apr-May-Jun",
  `3` = "Jul-Aug-Sep",
  `4` = "Oct-Nov-Dec"
)

# Actual Seasonal Precipitation
ggplot() + 
  geom_boxplot(data=precip_season_by_fp, aes(future_period, precip, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Seasonal Precipitation", x = "Period", y = "Seasonal Precipitation (mm)") +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_wrap(~season, labeller = as_labeller(season_id)) +
  scale_y_continuous(sec.axis = sec_axis(~./25.4, name = "Annual Precipitation (in)")) +
  theme_bw()
ggsave("output/precip_box_seasonal.jpg", width = 5, height = 4)

# Change in Seasonal Precipitation 
ggplot() + 
  geom_boxplot(data=precip_season_by_fp, aes(future_period, precip_seasonal_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Change in Seasonal Precipitation", x = "Period", y = "Change in Seasonal Precipitation (mm)") +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_wrap(~season, labeller = as_labeller(season_id)) +
  scale_y_continuous(sec.axis = sec_axis(~./25.4, name = "Change in Annual Precipitation (in)")) +
  theme_bw()
ggsave("output/precip_box_seasonal_delta.jpg", width = 5, height = 4)


# ---------------------------------------------------------------------
# Mean monthly precipitation

happy <- precip_month %>% 
  group_by(rcp,future_period,month) %>% 
  summarize(month_precip=mean(precip))

ggplot() +
  geom_line(data=happy, aes(x=month,y=month_precip, color=future_period)) +
  facet_grid(rcp~.)


# ---------------------------------------------------------------------
# Table (Historical plus 30-year change groupings)

library(knitr)
library(kableExtra)

# 
precip_annual_by_fp %>% 
  group_by(precip_var,rcp,future_period) %>% 
  summarize(precip_annual_diff_period = mean(precip_annual_diff))


precip_month_by_fp %>% 
  group_by(precip_var,rcp,future_period, month) %>% 
  summarize(precip_month_diff_period = mean(precip_monthly_diff)) %>%
  unite(rcp_period, rcp, future_period) %>% 
  spread(key = rcp_period, value = precip_month_diff_period)
#rename columns

precip_season_by_fp %>% 
  group_by(precip_var,rcp,future_period, season) %>% 
  summarize(precip_season_diff_period = median(precip_seasonal_diff)) %>%
  unite(rcp_period, rcp, future_period) %>% 
  spread(key = rcp_period, value = precip_season_diff_period)
#rename columns


# ---------------------------------------------------------------------
# Spatial Plots

# ----
# Process precipitation data

# Isolate only late century layers (2070-2099)
precip_proj_map_2070_2099 <- map(precip_proj_map,function(x)raster::subset(x, 769:1128))

# Generates mean annual precipitation (mm) 
map_precip_hist_ccsm4 <- calc(precip_hist_map$p_hist_ccsm4, function(x)sum(x)/56) # Dividing by number of years in dataset
map_precip_45_ccsm4 <- calc(precip_proj_map_2070_2099$p_45_ccsm4, function(x)sum(x)/30)
map_precip_85_ccsm4 <- calc(precip_proj_map_2070_2099$p_85_ccsm4, function(x)sum(x)/30)

# Precipitation 
# Tidy the rasters so maps can be plotted
map_precip_ccsm4_stack <- stack(map_precip_hist_ccsm4,map_precip_45_ccsm4,map_precip_85_ccsm4)
names(map_precip_ccsm4_stack) =  c("map_precip_1hist_ccsm4","map_precip_45_ccsm4","map_precip_85_ccsm4")
map_precip_ccsm4_p <- rasterToPoints(map_precip_ccsm4_stack)
map_precip_ccsm4_tib <- as_tibble(map_precip_ccsm4_p)
map_precip_ccsm4 <- gather(map_precip_ccsm4_tib, key="layer", value="precip",
                         "map_precip_1hist_ccsm4","map_precip_45_ccsm4","map_precip_85_ccsm4")
map_precip_ccsm4 <- mutate(map_precip_ccsm4, 
                           precip_interval = cut_interval(precip,n=9,
                                                          breaks = c(100,300,500,700,900,
                                                                     1100,1300,1500,1700,1900),
                                                          labels = c("100-300","300-500","500-700",
                                                                     "700-900","900-1100","1100-1300",
                                                                     "1300-1500","1500-1700","1700-1900")))

# Create labels for facetting
precip_id <- c(
  `map_precip_1hist_ccsm4` = "Historical",
  `map_precip_45_ccsm4` = "End of Century (RCP4.5)",
  `map_precip_85_ccsm4` = "End of Century (RCP8.5)"
)


# ----
# Plot Precipitation

# Precipitation - Continuous
x <- ggplot() +
  geom_raster(data=map_precip_ccsm4,aes(x=x,y=y, fill=precip)) +
  geom_sf(data=ss_border, fill=NA, col="white") +
  scale_fill_continuous(low="red", high="blue", name="Precipitation (mm)") +
  scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  labs(title="Mean Annual Precipitation", x="Longitude",y="Latitude", size=0.5) +
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
  facet_grid(.~layer, labeller = as_labeller(precip_id)) +
  theme(legend.position = "bottom")
plot(x)


# Precipitation - Discrete
x <- ggplot() +
  geom_raster(data=map_precip_ccsm4,aes(x=x,y=y, fill=precip_interval)) +
  geom_sf(data=ss_border, fill=NA, col="white") +
  scale_fill_brewer(palette = "RdBu", name="Precipitation (mm)",
                    labels=scales::comma) +
  scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  labs(title="Mean Annual Precipitation", x="Longitude",y="Latitude", size=0.5) +
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
  facet_grid(.~layer, labeller = as_labeller(precip_id)) +
  theme(legend.position = "bottom")
plot(x)
ggsave("output/map_precipitation.jpg", width = 8, height = 5)




