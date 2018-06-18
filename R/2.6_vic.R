# VIC (ET, Q and SWE) Analysis
# 

source("R/0_utilities.R")


# ---------------------------------------------------------------------
# Data processing

etqswe_kings_d <- etqswe_kings %>% 
  purrr::map(function(x)(mutate(x,year=year(Day), month=month(Day), day=day(Day)))) %>% 
  bind_rows(.id="type") %>% 
  separate(type, into=c("watershed","gcm", "rcp")) %>% 
  mutate(season = case_when(
    (month==1)~"1",(month==2)~"1",(month==3)~"1",
    (month==4)~"2",(month==5)~"2",(month==6)~"2",
    (month==7)~"3",(month==8)~"3",(month==9)~"3",
    (month==10)~"4",(month==11)~"4",(month==12)~"4"
  ))

# Group historical by year so that they means can be joined to future scenarios
etqswe_hist_year <- etqswe_kings_d %>% 
  dplyr::filter(rcp=="hist") %>% 
  group_by(watershed, gcm, year) %>% 
  summarize(Q_hist_annual = sum(Q),
            ET_hist_annual = sum(ET),
            SWE_hist_annual = max(SWE)) %>% 
  group_by(watershed, gcm) %>% 
  summarize(Q_hist_annual = mean(Q_hist_annual),
            ET_hist_annual = mean(ET_hist_annual),
            SWE_hist_annual = mean(SWE_hist_annual))
etqswe_hist_year2 <- etqswe_hist_year %>% 
  group_by(watershed) %>% 
  summarize(Q_hist_annual2 = mean(Q_hist_annual),    # For plotting mean line
            ET_hist_annual2 = mean(ET_hist_annual),
            SWE_hist_annual2 = mean(SWE_hist_annual))

# Group historical by month so that they means can be joined to future scenarios
etqswe_hist_month <- etqswe_kings_d %>% 
  dplyr::filter(rcp=="hist") %>% 
  group_by(watershed, gcm, year, month) %>% 
  summarize(Q_hist_month = sum(Q),
            ET_hist_month = sum(ET),
            SWE_hist_month = max(SWE)) %>% 
  group_by(watershed, gcm, month) %>% 
  summarize(Q_hist_month = mean(Q_hist_month),
            ET_hist_month = mean(ET_hist_month),
            SWE_hist_month = mean(SWE_hist_month))
etqswe_hist_month2 <- etqswe_hist_month %>% 
  group_by(watershed, month) %>% 
  summarize(Q_hist_month2 = mean(Q_hist_month),   # For plotting mean line
            ET_hist_month2 = mean(ET_hist_month),
            SWE_hist_month2 = mean(SWE_hist_month))

# Group historical by season so that they means can be joined to future scenarios
etqswe_hist_season <- etqswe_kings_d %>% 
  dplyr::filter(rcp=="hist") %>% 
  group_by(watershed, gcm, year, season) %>% 
  summarize(Q_hist_season = sum(Q),
            ET_hist_season = sum(ET),
            SWE_hist_season = max(SWE)) %>% 
  group_by(watershed, gcm, season) %>% 
  summarize(Q_hist_season = mean(Q_hist_season),
            ET_hist_season = mean(ET_hist_season),
            SWE_hist_season = mean(SWE_hist_season))
etqswe_hist_season2 <- etqswe_hist_season %>% 
  group_by(watershed, season) %>% 
  summarize(Q_hist_season2 = mean(Q_hist_season),   # For plotting mean line
            ET_hist_season2 = mean(ET_hist_season),
            SWE_hist_season2 = mean(SWE_hist_season))

# Group observed historical by year so that they means can be joined to future scenarios
etqswe_obs_year <- etqswe_kings_d %>% 
  dplyr::filter(rcp=="obs") %>% 
  group_by(watershed, year) %>% 
  summarize(Q_obs_annual = sum(Q),
            ET_obs_annual = sum(ET),
            SWE_obs_annual = max(SWE)) %>% 
  group_by(watershed) %>% 
  summarize(Q_obs_annual = mean(Q_obs_annual),
            ET_obs_annual = mean(ET_obs_annual),
            SWE_obs_annual = mean(SWE_obs_annual))

# Join historical scenarios to future projections and add future period groupings
etqswe_final <- etqswe_kings_d %>% 
  dplyr::filter(rcp %in% c("45","85")) %>% 
  left_join(etqswe_hist_year, by = c("watershed", "gcm")) %>% 
  left_join(etqswe_hist_month, by = c("watershed", "gcm", "month")) %>% 
  left_join(etqswe_hist_season, by = c("watershed", "gcm", "season")) %>% 
  left_join(etqswe_hist_year2, by = c("watershed")) %>% 
  left_join(etqswe_hist_month2, by = c("watershed", "month")) %>% 
  left_join(etqswe_hist_season2, by = c("watershed", "season")) %>% 
  dplyr::filter(year >= 2010) %>% 
  mutate(future_period = case_when(
    between(year,2010,2039) ~ "2010-39",
    between(year,2040,2069) ~ "2040-69",
    between(year,2070,2099) ~ "2070-99"
  ))

# ----

# Produce summarized annual data
etqswe_annual <- etqswe_final %>% 
  group_by(watershed, rcp, gcm, year, future_period) %>% 
  summarise(Q = sum(Q),
            ET = sum(ET),
            SWE = max(SWE),
            Q_hist_annual=mean(Q_hist_annual),
            ET_hist_annual=mean(ET_hist_annual),
            SWE_hist_annual=mean(SWE_hist_annual),
            Q_hist_annual2=mean(Q_hist_annual2),
            ET_hist_annual2=mean(ET_hist_annual2),
            SWE_hist_annual2=mean(SWE_hist_annual2)) %>% 
  mutate(Q_annual_diff = Q-Q_hist_annual,
         ET_annual_diff = ET-ET_hist_annual,
         SWE_annual_diff = SWE-SWE_hist_annual)
# Produce summarized annual data by future period
etqswe_annual_by_fp <- etqswe_annual %>% 
  group_by(watershed, rcp, gcm, future_period) %>% 
  summarise(Q = mean(Q), 
            ET = mean(ET),
            SWE = mean(SWE),
            Q_hist_annual=mean(Q_hist_annual),
            ET_hist_annual=mean(ET_hist_annual),
            SWE_hist_annual=mean(SWE_hist_annual),
            Q_hist_annual2=mean(Q_hist_annual2),
            ET_hist_annual2=mean(ET_hist_annual2),
            SWE_hist_annual2=mean(SWE_hist_annual2),
            Q_annual_diff=mean(Q_annual_diff),
            ET_annual_diff=mean(ET_annual_diff),
            SWE_annual_diff=mean(SWE_annual_diff))

# Produce summarized monthly data
etqswe_month <- etqswe_final %>% 
  group_by(watershed, rcp, gcm, year, month, future_period) %>% 
  summarise(Q = sum(Q),
            ET = sum(ET),
            SWE = max(SWE),
            Q_hist_month=mean(Q_hist_month),
            ET_hist_month=mean(ET_hist_month),
            SWE_hist_month=mean(SWE_hist_month),
            Q_hist_month2=mean(Q_hist_month2),
            ET_hist_month2=mean(ET_hist_month2),
            SWE_hist_month2=mean(SWE_hist_month2)) %>% 
  mutate(Q_month_diff = Q-Q_hist_month,
         ET_month_diff = ET-ET_hist_month,
         SWE_month_diff = SWE-SWE_hist_month)
# Produce summarized monthly data by future period
etqswe_month_by_fp <- etqswe_month %>% 
  group_by(watershed, rcp, gcm, month, future_period) %>% 
  summarise(Q = mean(Q), 
            ET = mean(ET),
            SWE = mean(SWE),
            Q_hist_month=mean(Q_hist_month),
            ET_hist_month=mean(ET_hist_month),
            SWE_hist_month=mean(SWE_hist_month),
            Q_hist_month2=mean(Q_hist_month2),
            ET_hist_month2=mean(ET_hist_month2),
            SWE_hist_month2=mean(SWE_hist_month2),
            Q_month_diff=mean(Q_month_diff),
            ET_month_diff=mean(ET_month_diff),
            SWE_month_diff=mean(SWE_month_diff))

# Produce summarized seasonal data
etqswe_season <- etqswe_final %>% 
  group_by(watershed, rcp, gcm, year, season, future_period) %>% 
  summarise(Q = sum(Q),
            ET = sum(ET),
            SWE = max(SWE),
            Q_hist_season=mean(Q_hist_season),
            ET_hist_season=mean(ET_hist_season),
            SWE_hist_season=mean(SWE_hist_season),
            Q_hist_season2=mean(Q_hist_season2),
            ET_hist_season2=mean(ET_hist_season2),
            SWE_hist_season2=mean(SWE_hist_season2)) %>% 
  mutate(Q_season_diff = Q-Q_hist_season,
         ET_season_diff = ET-ET_hist_season,
         SWE_season_diff = SWE-SWE_hist_season)
# Produce summarized seasonal data by future period
etqswe_season_by_fp <- etqswe_season %>% 
  group_by(watershed, rcp, gcm, season, future_period) %>% 
  summarise(Q = mean(Q), 
            ET = mean(ET),
            SWE = mean(SWE),
            Q_hist_season=mean(Q_hist_season),
            ET_hist_season=mean(ET_hist_season),
            SWE_hist_season=mean(SWE_hist_season),
            Q_hist_season2=mean(Q_hist_season2),
            ET_hist_season2=mean(ET_hist_season2),
            SWE_hist_season2=mean(SWE_hist_season2),
            Q_season_diff=mean(Q_season_diff),
            ET_season_diff=mean(ET_season_diff),
            SWE_season_diff=mean(SWE_season_diff))


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Box plot by time-periods (annual)

# Q
ggplot() + 
  geom_boxplot(data=etqswe_annual_by_fp, aes(future_period, Q, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Mean Annual Streamflow for Kings River", x = "Period", y = "Mean Annual Streamflow (mm)") +
  geom_hline(data=etqswe_annual_by_fp, aes(yintercept = Q_hist_annual2), color="gray20") +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  # Conversion to acre-feet from https://gist.github.com/ryanrbart/e4d274034d2c1737d73d. Kings watershed = 4001 km2
  scale_y_continuous(sec.axis = sec_axis(~.*4001*0.000000810713192, name = "Mean Annual Streamflow\n(millions of acre feet)")) +
  theme_bw()
ggsave("output/Q_box_annual.jpg", width = 5, height = 4)

# Changes in Q
ggplot() + 
  geom_boxplot(data=etqswe_annual_by_fp, aes(future_period, Q_annual_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Change in Mean Annual Streamflow for Kings River", x = "Period", y = "Change in Mean Annual Streamflow (mm)") +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  # Conversion to acre-feet from https://gist.github.com/ryanrbart/e4d274034d2c1737d73d. Kings watershed = 4001 km2
  scale_y_continuous(sec.axis = sec_axis(~.*4001*0.000000810713192, name = "Change in Mean Annual Streamflow\n(millions of acre feet)")) +
  theme_bw()
ggsave("output/Q_box_annual_delta.jpg", width = 4, height = 3)

# Baseline annual streamflow values
etqswe_annual_by_fp %>% 
  summarise(mean = mean(Q_hist_annual))


# ---------------------------------------------------------------------
# Box plot by time-periods (monthly)

month_id <- c(
  `1` = "January", `2` = "February", `3` = "March",`4` = "April",
  `5` = "May",`6` = "June",`7` = "July",`8` = "August",
  `9` = "September",`10` = "October",`11` = "November",`12` = "December"
)

# Q
ggplot() + 
  geom_boxplot(data=etqswe_month_by_fp, aes(future_period, Q, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Mean Monthly Streamflow for Kings River", x = "Period", y = "Mean Monthly Streamflow (mm)") +
  geom_hline(data=etqswe_month_by_fp, aes(yintercept = Q_hist_month2), color="gray20") +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  # Conversion to acre-feet from https://gist.github.com/ryanrbart/e4d274034d2c1737d73d. Kings watershed = 4001 km2
  scale_y_continuous(sec.axis = sec_axis(~.*4001*0.000000810713192, name = "Mean Monthly Streamflow (millions of acre feet)")) +
  facet_wrap(~month, labeller = as_labeller(month_id), nrow=4) +
  theme_bw(base_size = 13)
ggsave("output/Q_box_month.jpg", width = 8, height = 10)

# Changes in Q
ggplot() + 
  geom_boxplot(data=etqswe_month_by_fp, aes(future_period, Q_month_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Change in Mean Monthly Streamflow for Kings River", x = "Period", y = "Change in Mean Monthly Streamflow (mm)") +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  # Conversion to acre-feet from https://gist.github.com/ryanrbart/e4d274034d2c1737d73d. Kings watershed = 4001 km2
  scale_y_continuous(sec.axis = sec_axis(~.*4001*0.000000810713192, name = "Change in Mean Monthly Streamflow (millions of acre feet)")) +
  facet_wrap(~month, labeller = as_labeller(month_id)) +
  theme_bw()
ggsave("output/Q_box_month_delta.jpg", width = 8, height = 6)

# Baseline monthly streamflow values
etqswe_month_by_fp %>% 
  summarise(mean = mean(Q_hist_month))

# ---------------------------------------------------------------------
# Box plot by time-periods (seasonal)

season_id <- c(
  `1` = "Jan-Feb-Mar",
  `2` = "Apr-May-Jun",
  `3` = "Jul-Aug-Sep",
  `4` = "Oct-Nov-Dec"
)

# Q
ggplot() + 
  geom_boxplot(data=etqswe_season_by_fp, aes(future_period, Q, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Mean Seasonal Streamflow for Kings River", x = "Period", y = "Mean Seasonal Streamflow (mm)") +
  geom_hline(data=etqswe_season_by_fp, aes(yintercept = Q_hist_season2), color="gray20") +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  # Conversion to acre-feet from https://gist.github.com/ryanrbart/e4d274034d2c1737d73d. Kings watershed = 4001 km2
  scale_y_continuous(sec.axis = sec_axis(~.*4001*0.000000810713192, name = "Mean Seasonal Streamflow\n(millions of acre feet)")) +
  facet_wrap(~season, labeller = as_labeller(season_id)) +
  theme_bw()
ggsave("output/Q_box_season.jpg", width = 5, height = 4)

# Changes in Q
ggplot() + 
  geom_boxplot(data=etqswe_season_by_fp, aes(future_period, Q_season_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Change in Mean Seasonal Streamflow for Kings River", x = "Period", y = "Change in Mean Seasonal Streamflow (mm)") +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  # Conversion to acre-feet from https://gist.github.com/ryanrbart/e4d274034d2c1737d73d. Kings watershed = 4001 km2
  scale_y_continuous(sec.axis = sec_axis(~.*4001*0.000000810713192, name = "Change in Mean Seasonal Streamflow\n(millions of acre feet)")) +
  facet_wrap(~season, labeller = as_labeller(season_id)) +
  theme_bw()
ggsave("output/Q_box_season_delta.jpg", width = 5, height = 4)

# Baseline seasonal streamflow values
etqswe_season_by_fp %>% 
  summarise(mean = mean(Q_hist_season))





# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Box plot by time-periods (annual ET)

# ET
ggplot() + 
  geom_boxplot(data=etqswe_annual_by_fp, aes(future_period, ET, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Annual ET", x = "Period", y = "Annual ET (mm)") +
  geom_hline(data=etqswe_annual_by_fp, aes(yintercept = ET_hist_annual2), color="gray20") +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  theme_bw()
ggsave("output/ET_box_annual.jpg", width = 4, height = 3)

# Changes in ET
ggplot() + 
  geom_boxplot(data=etqswe_annual_by_fp, aes(future_period, ET_annual_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Change in Annual ET", x = "Period", y = "Change in Annual ET (mm)") +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  theme_bw()
ggsave("output/ET_box_annual_delta.jpg", width = 4, height = 3)

# Baseline annual ET values
etqswe_annual_by_fp %>% 
  summarise(mean = mean(ET_hist_annual))


# ---------------------------------------------------------------------
# Box plot by time-periods (monthly ET)

month_id <- c(
  `1` = "January", `2` = "February", `3` = "March",`4` = "April",
  `5` = "May",`6` = "June",`7` = "July",`8` = "August",
  `9` = "September",`10` = "October",`11` = "November",`12` = "December"
)

# ET
ggplot() + 
  geom_boxplot(data=etqswe_month_by_fp, aes(future_period, ET, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Mean Monthly Evapotranspiration (ET) in Kings Watershed", x = "Period", y = "Mean Monthly ET (mm)") +
  geom_hline(data=etqswe_month_by_fp, aes(yintercept = ET_hist_month2), color="gray20") +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  scale_y_continuous(sec.axis = sec_axis(~./25.4, name = "Mean Monthly ET (in)")) +
  facet_wrap(~month, labeller = as_labeller(month_id)) +
  theme_bw()
ggsave("output/ET_box_month.jpg", width = 8, height = 6)

# Changes in ET
ggplot() + 
  geom_boxplot(data=etqswe_month_by_fp, aes(future_period, ET_month_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Change in Mean Monthly ET", x = "Period", y = "Change in Mean Monthly ET (mm)") +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  scale_y_continuous(sec.axis = sec_axis(~./25.4, name = "Change in Mean Monthly ET (in)")) +
  facet_wrap(~month, labeller = as_labeller(month_id)) +
  theme_bw()
ggsave("output/ET_box_month_delta.jpg", width = 8, height = 6)

# Baseline monthly ET values
etqswe_month_by_fp %>% 
  summarise(mean = mean(ET_hist_month))

# ---------------------------------------------------------------------
# Box plot by time-periods (seasonal ET)

season_id <- c(
  `1` = "Jan-Feb-Mar",
  `2` = "Apr-May-Jun",
  `3` = "Jul-Aug-Sep",
  `4` = "Oct-Nov-Dec"
)

# ET
ggplot() + 
  geom_boxplot(data=etqswe_season_by_fp, aes(future_period, ET, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Mean Seasonal Evapotranspiration (ET) in Kings Watershed", x = "Period", y = "Mean Seasonal ET (mm)") +
  geom_hline(data=etqswe_season_by_fp, aes(yintercept = ET_hist_season2), color="gray20") +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  scale_y_continuous(limits = c(0,235),sec.axis = sec_axis(~./25.4, name = "Change in Mean Seasonal ET (in)")) +
  facet_wrap(~season, labeller = as_labeller(season_id)) +
  theme_bw()
ggsave("output/ET_box_season.jpg", width = 6.5, height = 5)

# Changes in ET
ggplot() + 
  geom_boxplot(data=etqswe_season_by_fp, aes(future_period, ET_season_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Change in Seasonal ET", x = "Period", y = "Change in Seasonal ET (mm)") +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_wrap(~season, labeller = as_labeller(season_id)) +
  theme_bw()
ggsave("output/ET_box_season_delta.jpg", width = 5, height = 4)

# Baseline seasonal ET values
etqswe_season_by_fp %>% 
  summarise(mean = mean(ET_hist_season))






# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Box plot by time-periods (annual peak SWE)

# SWE
ggplot() + 
  geom_boxplot(data=etqswe_annual_by_fp, aes(future_period, SWE, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Annual Peak Snow Water Equivalent (SWE)", x = "Period", y = "Annual Peak SWE (mm)") +
  geom_hline(data=etqswe_annual_by_fp, aes(yintercept = SWE_hist_annual2), color="gray20") +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  theme_bw()
ggsave("output/SWE_box_annual.jpg", width = 4, height = 3)

# Changes in SWE
ggplot() + 
  geom_boxplot(data=etqswe_annual_by_fp, aes(future_period, SWE_annual_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Change in Annual Peak Snow Water Equivalent (SWE)", x = "Period", y = "Change in Annual Peak SWE (mm)") +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  theme_bw()
ggsave("output/SWE_box_annual_delta.jpg", width = 4, height = 3)

# Baseline annual SWE values
etqswe_annual_by_fp %>% 
  summarise(mean = mean(SWE_hist_annual))


# ---------------------------------------------------------------------
# Box plot by time-periods (monthly SWE)

month_id <- c(
  `1` = "January", `2` = "February", `3` = "March",`4` = "April",
  `5` = "May",`6` = "June",`7` = "July",`8` = "August",
  `9` = "September",`10` = "October",`11` = "November",`12` = "December"
)

# SWE
ggplot() + 
  geom_boxplot(data=etqswe_month_by_fp, aes(future_period, SWE, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Monthly Peak Snow Water Equivalent (SWE) for Kings Watershed", x = "Period", y = "Mean Monthly Peak SWE (mm)") +
  geom_hline(data=etqswe_month_by_fp, aes(yintercept = SWE_hist_month2), color="gray20") +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  scale_y_continuous(sec.axis = sec_axis(~./25.4, name = "Mean Monthly Peak SWE (in)")) +
  facet_wrap(~month, labeller = as_labeller(month_id), nrow=4) +
  theme_bw(base_size = 13)
ggsave("output/SWE_box_month.jpg", width = 8.1, height = 10)

# Changes in SWE
ggplot() + 
  geom_boxplot(data=etqswe_month_by_fp, aes(future_period, SWE_month_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Change in Monthly Peak Snow Water Equivalent (SWE)", x = "Period", y = "Change in Mean Monthly Peak SWE (mm)") +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  scale_y_continuous(sec.axis = sec_axis(~.*25.4, name = "Change in Mean Monthly Peak SWE (in)")) +
  facet_wrap(~month, labeller = as_labeller(month_id)) +
  theme_bw()
ggsave("output/SWE_box_month_delta.jpg", width = 8, height = 6)

# Baseline monthly SWE values
etqswe_month_by_fp %>% 
  summarise(mean = mean(SWE_hist_month))

# ---------------------------------------------------------------------
# Box plot by time-periods (seasonal SWE)

season_id <- c(
  `1` = "Jan-Feb-Mar",
  `2` = "Apr-May-Jun",
  `3` = "Jul-Aug-Sep",
  `4` = "Oct-Nov-Dec"
)

# SWE
ggplot() + 
  geom_boxplot(data=etqswe_season_by_fp, aes(future_period, SWE, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Seasonal Peak Snow Water Equivalent (SWE)", x = "Period", y = "Seasonal Peak SWE (mm)") +
  geom_hline(data=etqswe_season_by_fp, aes(yintercept = SWE_hist_season2), color="gray20") +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_wrap(~season, labeller = as_labeller(season_id)) +
  theme_bw()
ggsave("output/SWE_box_season.jpg", width = 5, height = 4)

# Changes in SWE
ggplot() + 
  geom_boxplot(data=etqswe_season_by_fp, aes(future_period, SWE_season_diff, fill=rcp),
               color="black", outlier.shape = NA) +
  labs(title = "Projected Change in Seasonal Peak Snow Water Equivalent (SWE)", x = "Period", y = "Change in Seasonal Peak SWE (mm)") +
  geom_hline(yintercept = 0) +
  scale_fill_discrete(name="RCP", labels = c("4.5","8.5")) +
  facet_wrap(~season, labeller = as_labeller(season_id)) +
  theme_bw()
ggsave("output/SWE_box_season_delta.jpg", width = 5, height = 4)

# Baseline seasonal SWE values
etqswe_season_by_fp %>% 
  summarise(mean = mean(SWE_hist_season))




