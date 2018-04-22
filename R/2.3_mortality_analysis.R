# Mortality Analysis
# 

source("R/0_utilities.R")


# ---------------------------------------------------------------------
# Time-series (processing)



head(ss_mort_2017)

unique(ss_mort_2017$RPT_YR)



# ---------------------------------------------------------------------
# Plotting

ggplot() +
  geom_sf(data=ss_border, fill=NA, col="black") +
  geom_sf(data=ss_mort_2017, fill="red", col="black")


ggplot() +
  geom_sf(data=mort_2016, fill="red", col="black") 


