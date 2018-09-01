# Mortality Analysis
# 

source("R/0_utilities.R")
#source("R/1.1_setup_proj_boundaries.R")
#source("R/1.2_setup_temp_precip.R")
#source("R/1.5_setup_prism.R")
#source("R/1.8_setup_mortality_young.R")

# ---------------------------------------------------------------------
# Code for generating PET via Hamon 1961 method

# Generate mean monthly daylight hours in day
daylight_hours = read_csv("data/cwd/daylight_hours_visalia.csv",
                          col_types=cols(day=col_skip()),
                          col_names = c("day","1","2","3","4","5","6",
                                        "7","8","9","10","11","12"))
daylight_hours <- daylight_hours %>% 
  summarize_all(., funs(mean(., na.rm = TRUE))) %>% 
  as_vector()/3600

# Function for Hamon PET method
pet_hamon <- function(temp, daylight_hours){
  # Hamon, W. R. Estimating potential evapotranspiration. J. Hydraul. Div. 87, 107–120 (1961)
  # Haith, Shoemaker GENERALIZED WATERSHED LOADING FUNCTIONS FOR STREAM FLOW NUTRIENTS
  # http://nest.su.se/mnode/Methods/penman.htm
  
  # e_sat - kPa
  e_sat <- 0.6108*exp((17.27*temp)/(237.3 + temp))
  
  # PET - mm/day
  PET = (2.1 * (daylight_hours)^2 * e_sat)/(temp + 273.2)
  return(PET)
}

# For converting Hamon output (mm/day) to mm/month
pet_hamon_month <- function(x, daylight_hours){
  pet_stack <- calc(x, fun = function(x){pet_hamon(x,daylight_hours)})
  days_per_month <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  pet_stack <- calc(pet_stack, fun = function(x){x*days_per_month})
  return(pet_stack)
}


# ---------------------------------------------------------------------
# Process mortality data

# Precip 2015
precip_2015 <- calc(prism_precip_stack, sum)
names(precip_2015) <- "precip_2015"

# PET 2015
pet_2015_stack <- pet_hamon_month(x=prism_temp_stack,daylight_hours=daylight_hours)
pet_2015 <- calc(pet_2015_stack, sum)
names(pet_2015) <- "pet_2015"

# PET 2085 (RCP4.5)
pet_2085_45_stack <- pet_hamon_month(x=(prism_temp_stack + 3.09), daylight_hours=daylight_hours) # 3.09 from mean annual projected temp increase for min and max
pet_2085_45 <- calc(pet_2085_45_stack, sum)
names(pet_2085_45) <- "pet_2085_45"

# PET 2085 (RCP8.5)
pet_2085_85_stack <- pet_hamon_month(x=(prism_temp_stack + 5.09), daylight_hours=daylight_hours) # 5.09 from mean annual projected temp increase for min and max
pet_2085_85 <- calc(pet_2085_85_stack, sum)
names(pet_2085_85) <- "pet_2085_85"

# CWD 2015
cwd_2015 <- precip_2015 - pet_2015
names(cwd_2015) <- "cwd_2015"

# CWD 2085 (RCP4.5)
# (Ideally we would use MACA projections, but they won't reflect severe drought conditions unless I search for driest years)
cwd_2085_45 <- precip_2015 - pet_2085_45
names(cwd_2085_45) <- "cwd_2085_45"

# CWD 2085 (RCP8.5)
cwd_2085_85 <- precip_2015 - pet_2085_85
names(cwd_2085_85) <- "cwd_2085_85"


happy_tib <- as_tibble(values(stack(y_15, cwd_2015, cwd_2085_45, cwd_2085_85)))
happy_tib <- bind_cols(happy_tib, tibble(num = seq_len(base::nrow(happy_tib))))   # Add unique value marker

happy_tib <- happy_tib %>%
  gather("future_period", "cwd", c(cwd_2015,cwd_2085_45, cwd_2085_85)) %>% 
  dplyr::filter(mort.tph>0)


# ---------------------------------------------------------------------
# Regression model

# Separate happy_tib by future period
happy_tib_baseline <- happy_tib %>% 
  dplyr::filter(future_period=="cwd_2015")
happy_tib_2085_45 <- happy_tib %>% 
  dplyr::filter(future_period=="cwd_2085_45")
happy_tib_2085_85 <- happy_tib %>% 
  dplyr::filter(future_period=="cwd_2085_85")

# ----
# Build different regression models

# Regr1 is used in appendix report
regr1 <- happy_tib %>% 
  dplyr::filter(future_period=="cwd_2015") %>% 
  lm((mort.tph)~ cwd+live.tph, data=.)
summary(regr1)

regr1_broom <- broom::tidy(regr1)
write_csv(regr1_broom, "output/regr1_model_values.csv")

# Regr2 is not used in appendix report
regr2 <- happy_tib %>% 
  dplyr::filter(future_period=="cwd_2015") %>% 
  lm((mort.tph)~cwd+live.bah, data=.)
summary(regr2)

# Make plot of regr1 (For appendix report methods)
ggplot(happy_tib) +
  geom_point(aes(x=live.tph, y=mort.tph, color = cwd)) +
  labs(title = "Forest Mortality (Wateryear 2015)",x = "Live Trees per Hectare", y = "Dead Trees per Hectare") +
  scale_color_continuous(low="red", high="blue", name="Water stress:\nP minus PET") +
  theme_bw(base_size =12) +
  NULL
#ggsave("output/mortality_2015.jpg", width = 6, height = 5)


# ----
# Predict the amount of mortality per ha for three scenarios
mort_tph_baseline <- mean(happy_tib$mort.tph)
mort_tph_2085_45 <- mean(predict(regr1, happy_tib_2085_45))
mort_tph_2085_85 <- mean(predict(regr1, happy_tib_2085_85))
mort_tph2_2085_45 <- mean(predict(regr2, happy_tib_2085_45))
mort_tph2_2085_85 <- mean(predict(regr2, happy_tib_2085_85))

mort_tph_2085_45/mort_tph_baseline
mort_tph_2085_85/mort_tph_baseline

mort_tph2_2085_45/mort_tph_baseline
mort_tph2_2085_85/mort_tph_baseline


# ----
# Compare observed data with modeled results

happy <- mutate(happy_tib_2085_45, mort_2085_45 = predict(regr1, happy_tib_2085_45))

happy <- full_join(
  dplyr::select(happy, num, mort_2085_45),
  mutate(happy_tib_2085_85, mort_2085_85 = predict(regr1, happy_tib_2085_85)),
  by = "num")

happy <- happy %>% 
  mutate(happy, change_45 = mort_2085_45-mort.tph) %>% 
  mutate(happy, change_85 = mort_2085_85-mort.tph)  

mort_change_tph_avg_45 <- mean(happy$change_45)
mort_change_tph_avg_85 <- mean(happy$change_85)

# Change in drought mortality (tph) divided by baseline drought mortality (tph)
# means <- tibble(change_45 = (mort_change_tph_avg_45+mort_tph_baseline)/mort_tph_baseline,
#                 change_85 = (mort_change_tph_avg_85+mort_tph_baseline)/mort_tph_baseline)
# means <- gather(means, change_45, change_85, key="period", value="mort_tph_change")

#Make plot of comparing hist and projected mortality (For appendix report)

happy_gather <- happy %>% 
  gather(change_45, change_85, key="period", value="mort_tph_change") %>% 
  dplyr::mutate(mort_tph_change_percent = mort_tph_change/mort_tph_baseline)

# Make plot 
ggplot(happy_gather,aes(x = period, y=mort_tph_change_percent)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Projected Change in Forest Mortality",x = "RCP scenario", y = "Percent Mortality Change (Trees per Hectare)") +
  theme_bw(base_size =12) +
  stat_summary(fun.y="mean", geom="point", size=2, color="blue") +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = quantile(happy_gather$mort_tph_change_percent, c(0.075, 0.98))) +
  scale_x_discrete(labels=c("4.5","8.5")) +
  NULL
#ggsave("output/mortality_projections.jpg", width = 5, height = 5)


plot(happy$mort.tph, happy$mort_2085_45)
plot(happy$mort.tph, happy$mort_2085_85)

# ----
# Hurdle model

library(pscl)




# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Older code


# ----

# This plot is similar to Young 2017.
happy_tib %>% 
  dplyr::filter(future_period=="cwd_2015") %>% 
  ggplot(data = .,aes(x=live.tph,y=mort.tph)) +
  geom_point(aes(color=cwd)) +
  scale_color_gradient(low="navajowhite1", high="black", name="Mortality\n(trees/ha)") +
  stat_smooth(method="lm") +
  xlim(0,1200) +
  #labs(title="Forest Mortality (Wateryear 2015)", x="Precipitation minus Potential ET (mm)",y=expression('Live Basal Area ('*m^2*"/ha)"), size=0.5) +
  theme_bw(base_size =12)
#ggsave("output/mortality_yes.jpg", width = 6, height = 5)




# ---------------------------------------------------------------------
# Figures

# Add a line that encloses the mortality region
#mort_line <- function(x)(x*.063+34)
mort_line <- function(x)(0.00004*((x+900)^2))

# This plot is similar to Young 2017 Fig 3.
happy_tib %>% 
  dplyr::filter(future_period=="cwd_2015") %>% 
  ggplot(data = .,aes(x=cwd,y=live.bah)) +
  geom_point(aes(color=mort.tph)) +
  scale_color_gradient(low="navajowhite1", high="black", name="Mortality\n(trees/ha)") +
  #stat_function(fun=mort_line) +
  labs(title="Forest Mortality (Wateryear 2015)", x="Precipitation minus Potential ET (mm)",y=expression('Live Basal Area ('*m^2*"/ha)"), size=0.5) +
  theme_bw(base_size =12)
ggsave("output/mortality_single.jpg", width = 6, height = 5)


ggplot(data = happy_tib,aes(x=cwd,y=live.bah)) +
  geom_point(color="gray") +
  scale_color_gradient(low="navajowhite1", high="black", name="Mortality\nTrees/Hect") +
  stat_function(fun=mort_line) +
  labs(title="Forest Mortality", x="Climatic Water Deficit",y="Live Basal Area per Hectare", size=0.5) +
  theme_bw(base_size =12) +
  facet_grid(.~future_period, labeller = as_labeller(c(
    `cwd_2015` = "Drought 2015",
    `cwd_2085_45` = "Drought 2085 (RCP4.5)",
    `cwd_2085_85` = "Drought 2085 (RCP8.5)")))
ggsave("output/mortality_multiple.jpg", width = 7, height = 5)


# ---------------------------------------------------------------------
# Rates of mortality above the line

happy_tib <- as_tibble(values(stack(y_15, cwd_2015, cwd_2085_45, cwd_2085_85)))

# Total number of pixels
mort_all <- happy_tib %>%
  dplyr::filter(mort.tph>=0, live.bah>=10)
nrow(mort_all)

# Total number of pixels with mort.tph > 10
mort_10 <- happy_tib %>%
  dplyr::filter(mort.tph>=5, live.bah>10)
nrow(mort_10)

# Historical scenario
# Pixels above mort line
a <- sum(mort_all$live.bah > mort_line(mort_all$cwd_2015), na.rm = TRUE)
b <- sum(mort_10$live.bah > mort_line(mort_10$cwd_2015), na.rm = TRUE)
mort_ratio <- b/a
mort_percent <- b/nrow(mort_all)
print(paste("Percent region affected:", round(mort_percent,3)))

# RCP4.5 Scenario
a <- sum(mort_all$live.bah > mort_line(mort_all$cwd_2085_45), na.rm = TRUE)
b <- sum(mort_10$live.bah > mort_line(mort_10$cwd_2085_45), na.rm = TRUE)
b_mod <- a*mort_ratio
mort_percent <- b_mod/nrow(mort_all)
print(paste("Percent region affected:", round(mort_percent,3)))

# RCP8.5 Scenario
a <- sum(mort_all$live.bah > mort_line(mort_all$cwd_2085_85), na.rm = TRUE)
b <- sum(mort_10$live.bah > mort_line(mort_10$cwd_2085_85), na.rm = TRUE)
b_mod <- a*mort_ratio
mort_percent <- b_mod/nrow(mort_all)
print(paste("Percent region affected:", round(mort_percent,3)))


.428/.324
.469/.324



# ---------------------------------------------------------------------
# Map of mortality

ggplot() +
  geom_sf(data = ss_mort_2015)


ggplot() +
  geom_sf(data = ss_mort_2016)

ggplot() +
  geom_sf(data = ss_mort_2017)




# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# USFS mortality data

head(ss_mort_2017)

unique(ss_mort_2017$RPT_YR)



# ---------------------------------------------------------------------
# Plotting

ggplot() +
  geom_sf(data=ss_border, fill=NA, col="black") +
  geom_sf(data=ss_mort_2017, fill="red", col="black")


ggplot() +
  geom_sf(data=mort_2016, fill="red", col="black") 





