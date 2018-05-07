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

# Produce summarized annual data
precip_annual <- precip_tib_final %>% 
  group_by(precip_var, rcp, gcm, year, future_period) %>% 
  summarise(precip = sum(precip), precip_hist_annual = sum(precip_hist_annual)) %>% 
  mutate(precip_annual_diff = precip-precip_hist_annual)
precip_annual_by_fp <- precip_annual %>% 
  group_by(precip_var, rcp, gcm, future_period) %>% 
  summarise(precip = mean(precip), precip_hist_annual=mean(precip_hist_annual),
            precip_annual_diff=mean(precip_annual_diff))

# Produce summarized monthly data
precip_month <- precip_tib_final %>% 
  mutate(precip_monthly_diff = precip-precip_hist_month)
precip_month_by_fp <- precip_month %>% 
  group_by(precip_var, rcp, gcm, month, future_period) %>% 
  summarise(precip = mean(precip), precip_hist_month=mean(precip_hist_month),
            precip_monthly_diff=mean(precip_monthly_diff))

# Produce summarized seasonal data
precip_season <- precip_tib_final %>% 
  group_by(precip_var, rcp, gcm, year, season, future_period) %>% 
  summarise(precip = sum(precip), precip_hist_season = sum(precip_hist_season)) %>% 
  mutate(precip_seasonal_diff = precip-precip_hist_season)
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
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  theme_bw()

# Changes in Precipitation
ggplot() + 
  geom_boxplot(data=precip_annual_by_fp, aes(future_period, precip_annual_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Changes in Annual Precipitation from Historical Baseline", x = "Period", y = "Change in Annual Precipitation (mm)") +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  theme_bw()



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
  theme_bw()


# Change in monthly precipitation
ggplot() + 
  geom_boxplot(data=precip_month_by_fp, aes(future_period, precip_monthly_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Changes in Monthly Precipitation from Historical Baseline", x = "Period", y = "Change in Monthly Precipitation (mm)") +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_wrap(~month, labeller = as_labeller(month_id)) +
  theme_bw()


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
  theme_bw()


# Change in Seasonal Precipitation 
ggplot() + 
  geom_boxplot(data=precip_season_by_fp, aes(future_period, precip_seasonal_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Changes in Seasonal Precipitation from Historical Baseline", x = "Period", y = "Change in Seasonal Precipitation (mm)") +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_wrap(~season, labeller = as_labeller(season_id)) +
  theme_bw()



# ---------------------------------------------------------------------
# Mean monthly precipitation

happy <- precip_month %>% 
  group_by(rcp,future_period,month) %>% 
  summarize(month_precip=mean(precip))

ggplot() +
  geom_line(data=happy, aes(x=month,y=month_precip, color=future_period)) +
  facet_grid(rcp~.)





