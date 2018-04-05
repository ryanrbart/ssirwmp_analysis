# Temperature Analysis
# 

source("R/0_utilities.R")


# ---------------------------------------------------------------------
# Time-series (processing)

# Transfer from brick to tibble (and compute on mean value of region)
timeseries_mean <- function(brick){
  x <- cellStats(brick, stat=mean)
  y <- brick %>% 
    names() %>% 
    str_replace("X", "") %>% 
    ymd()
  tib <- tibble(date = y,
                year = year(y),
                month = month(y),
                day = day(y),
                temp = x)
  return(tib)
}

# Run function
tmax_hist_tib <- timeseries_mean(tmax_hist)
tmax_45_tib <- timeseries_mean(tmax_45)
tmax_85_tib <- timeseries_mean(tmax_85)
tmin_hist_tib <- timeseries_mean(tmin_hist)
tmin_45_tib <- timeseries_mean(tmin_45)
tmin_85_tib <- timeseries_mean(tmin_85)

# Combine scenarios
temp_tib <- bind_rows(tmax_45 = tmax_45_tib,
                      tmax_85 = tmax_85_tib,
                      tmin_45 = tmin_45_tib,
                      tmin_85 = tmin_85_tib,
                      .id = "type")

temp_tib <- separate(temp_tib, type, into=c("temp_type","rcp"))

# Attach historical mean in new column
tmax_hist_mean <- mean(tmax_hist_tib$temp)
tmax_hist_mean_v <- rep(tmax_hist_mean, length(tmax_45_tib$date) + length(tmax_85_tib$date))
tmin_hist_mean <- mean(tmin_hist_tib$temp)
tmin_hist_mean_v <- rep(tmin_hist_mean, length(tmin_45_tib$date) + length(tmin_85_tib$date))
temp_hist_mean_v <- c(tmax_hist_mean_v, tmin_hist_mean_v)
temp_tib <- bind_cols(temp_tib, temp_hist_mean = temp_hist_mean_v)

# Produce annual data
temp_annual <- temp_tib %>% 
  group_by(temp_type, rcp, year, temp_hist_mean) %>% 
  summarise(temp = mean(temp)) %>% 
  mutate(temp_diff = temp-temp_hist_mean)


# ---------------------------------------------------------------------
# Time-series (lumped region)

# Annual
ggplot() +
  geom_line(data=temp_annual, aes(x=year,y=temp, col=rcp, linetype=temp_type)) +
  geom_hline(yintercept = tmax_hist_mean) +
  geom_hline(yintercept = tmin_hist_mean)

# Annual Difference
ggplot() +
  geom_line(data=temp_annual,aes(x=year,y=temp_diff, col=rcp))+
  facet_grid(~temp_type)



# ---------------------------------------------------------------------
# Time-series (Sub-regions or sub-periods)
# Potential sub-regions: watersheds, elevation




# ---------------------------------------------------------------------
# Plots

par(mfrow=c(1,1)) 
happy <- calc(tmax_85, mean)   # Generates long-term tmax 

# Base
#plot(happy)
#plot(ss_border, add=TRUE, col=NA)

# ggplot
happy_p <- rasterToPoints(happy)
happy_tib <- as_tibble(happy_p)

ggplot() +
  geom_raster(data=happy_tib,aes(x,y, fill=layer)) +
  geom_sf(data=ss_border, fill=NA, col="white")





# ---------------------------------------------------------------------
# 
