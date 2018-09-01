# Data import and processsing

# All data is reprojected (if necessary) to lat/long

source("R/0_utilities.R")
source("R/1.1_setup_proj_boundaries.R")


# ---------------------------------------------------------------------
# Import MACA (Temperature and Precipitation)

# Function that adjusts MACA x coordinate
chg_x_coord <- function(x){
  xmin(x) <- xmin(x)-360  # Change x coordinates
  xmax(x) <- xmax(x)-360  # Change x coordinates
  return(x)
}

# Function that adjusts MACA x coordinate and converts kelvin to celcius
chg_x_coord_temp_units <- function(x){
  x <- chg_x_coord(x)
  x <- x - 273.15   # K to C
  return(x)
}

# Import precip, tmax and tmin data

# CanESM2
p_hist_canesm2 <- brick("data/maca_precip_temp/agg_macav2metdata_pr_CanESM2_r1i1p1_historical_1950_2005_CONUS_monthly.nc")
p_45_canesm2 <- brick("data/maca_precip_temp/agg_macav2metdata_pr_CanESM2_r1i1p1_rcp45_2006_2099_CONUS_monthly.nc")
p_85_canesm2 <- brick("data/maca_precip_temp/agg_macav2metdata_pr_CanESM2_r1i1p1_rcp85_2006_2099_CONUS_monthly.nc")
tmax_hist_canesm2 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_CanESM2_r1i1p1_historical_1950_2005_CONUS_monthly.nc")
tmax_45_canesm2 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_CanESM2_r1i1p1_rcp45_2006_2099_CONUS_monthly.nc")
tmax_85_canesm2 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_CanESM2_r1i1p1_rcp85_2006_2099_CONUS_monthly.nc")
tmin_hist_canesm2 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_CanESM2_r1i1p1_historical_1950_2005_CONUS_monthly.nc")
tmin_45_canesm2 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_CanESM2_r1i1p1_rcp45_2006_2099_CONUS_monthly.nc")
tmin_85_canesm2 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_CanESM2_r1i1p1_rcp85_2006_2099_CONUS_monthly.nc")

# CCSM4
p_hist_ccsm4 <- brick("data/maca_precip_temp/agg_macav2metdata_pr_CCSM4_r6i1p1_historical_1950_2005_CONUS_monthly.nc")
p_45_ccsm4 <- brick("data/maca_precip_temp/agg_macav2metdata_pr_CCSM4_r6i1p1_rcp45_2006_2099_CONUS_monthly.nc")
p_85_ccsm4 <- brick("data/maca_precip_temp/agg_macav2metdata_pr_CCSM4_r6i1p1_rcp85_2006_2099_CONUS_monthly.nc")
tmax_hist_ccsm4 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_CCSM4_r6i1p1_historical_1950_2005_CONUS_monthly.nc")
tmax_45_ccsm4 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_CCSM4_r6i1p1_rcp45_2006_2099_CONUS_monthly.nc")
tmax_85_ccsm4 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_CCSM4_r6i1p1_rcp85_2006_2099_CONUS_monthly.nc")
tmin_hist_ccsm4 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_CCSM4_r6i1p1_historical_1950_2005_CONUS_monthly.nc")
tmin_45_ccsm4 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_CCSM4_r6i1p1_rcp45_2006_2099_CONUS_monthly.nc")
tmin_85_ccsm4 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_CCSM4_r6i1p1_rcp85_2006_2099_CONUS_monthly.nc")

# CNRM-CM5
p_hist_cnrm <- brick("data/maca_precip_temp/agg_macav2metdata_pr_CNRM-CM5_r1i1p1_historical_1950_2005_CONUS_monthly.nc")
p_45_cnrm <- brick("data/maca_precip_temp/agg_macav2metdata_pr_CNRM-CM5_r1i1p1_rcp45_2006_2099_CONUS_monthly.nc")
p_85_cnrm <- brick("data/maca_precip_temp/agg_macav2metdata_pr_CNRM-CM5_r1i1p1_rcp85_2006_2099_CONUS_monthly.nc")
tmax_hist_cnrm <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_CNRM-CM5_r1i1p1_historical_1950_2005_CONUS_monthly.nc")
tmax_45_cnrm <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_CNRM-CM5_r1i1p1_rcp45_2006_2099_CONUS_monthly.nc")
tmax_85_cnrm <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_CNRM-CM5_r1i1p1_rcp85_2006_2099_CONUS_monthly.nc")
tmin_hist_cnrm <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_CNRM-CM5_r1i1p1_historical_1950_2005_CONUS_monthly.nc")
tmin_45_cnrm <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_CNRM-CM5_r1i1p1_rcp45_2006_2099_CONUS_monthly.nc")
tmin_85_cnrm <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_CNRM-CM5_r1i1p1_rcp85_2006_2099_CONUS_monthly.nc")

# HadGEM-CC
p_hist_hadgemcc <- brick("data/maca_precip_temp/agg_macav2metdata_pr_HadGEM2-CC365_r1i1p1_historical_1950_2005_CONUS_monthly.nc")
p_45_hadgemcc <- brick("data/maca_precip_temp/agg_macav2metdata_pr_HadGEM2-CC365_r1i1p1_rcp45_2006_2099_CONUS_monthly.nc")
p_85_hadgemcc <- brick("data/maca_precip_temp/agg_macav2metdata_pr_HadGEM2-CC365_r1i1p1_rcp85_2006_2099_CONUS_monthly.nc")
tmax_hist_hadgemcc <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_HadGEM2-CC365_r1i1p1_historical_1950_2005_CONUS_monthly.nc")
tmax_45_hadgemcc <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_HadGEM2-CC365_r1i1p1_rcp45_2006_2099_CONUS_monthly.nc")
tmax_85_hadgemcc <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_HadGEM2-CC365_r1i1p1_rcp85_2006_2099_CONUS_monthly.nc")
tmin_hist_hadgemcc <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_HadGEM2-CC365_r1i1p1_historical_1950_2005_CONUS_monthly.nc")
tmin_45_hadgemcc <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_HadGEM2-CC365_r1i1p1_rcp45_2006_2099_CONUS_monthly.nc")
tmin_85_hadgemcc <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_HadGEM2-CC365_r1i1p1_rcp85_2006_2099_CONUS_monthly.nc")

# HadGEM-ES
p_hist_hadgemes <- brick("data/maca_precip_temp/agg_macav2metdata_pr_HadGEM2-ES365_r1i1p1_historical_1950_2005_CONUS_monthly.nc")
p_45_hadgemes <- brick("data/maca_precip_temp/agg_macav2metdata_pr_HadGEM2-ES365_r1i1p1_rcp45_2006_2099_CONUS_monthly.nc")
p_85_hadgemes <- brick("data/maca_precip_temp/agg_macav2metdata_pr_HadGEM2-ES365_r1i1p1_rcp85_2006_2099_CONUS_monthly.nc")
tmax_hist_hadgemes <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_HadGEM2-ES365_r1i1p1_historical_1950_2005_CONUS_monthly.nc")
tmax_45_hadgemes <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_HadGEM2-ES365_r1i1p1_rcp45_2006_2099_CONUS_monthly.nc")
tmax_85_hadgemes <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_HadGEM2-ES365_r1i1p1_rcp85_2006_2099_CONUS_monthly.nc")
tmin_hist_hadgemes <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_HadGEM2-ES365_r1i1p1_historical_1950_2005_CONUS_monthly.nc")
tmin_45_hadgemes <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_HadGEM2-ES365_r1i1p1_rcp45_2006_2099_CONUS_monthly.nc")
tmin_85_hadgemes <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_HadGEM2-ES365_r1i1p1_rcp85_2006_2099_CONUS_monthly.nc")

# MIROC5
p_hist_miroc5 <- brick("data/maca_precip_temp/agg_macav2metdata_pr_MIROC5_r1i1p1_historical_1950_2005_CONUS_monthly.nc")
p_45_miroc5 <- brick("data/maca_precip_temp/agg_macav2metdata_pr_MIROC5_r1i1p1_rcp45_2006_2099_CONUS_monthly.nc")
p_85_miroc5 <- brick("data/maca_precip_temp/agg_macav2metdata_pr_MIROC5_r1i1p1_rcp85_2006_2099_CONUS_monthly.nc")
tmax_hist_miroc5 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_MIROC5_r1i1p1_historical_1950_2005_CONUS_monthly.nc")
tmax_45_miroc5 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_MIROC5_r1i1p1_rcp45_2006_2099_CONUS_monthly.nc")
tmax_85_miroc5 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_MIROC5_r1i1p1_rcp85_2006_2099_CONUS_monthly.nc")
tmin_hist_miroc5 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_MIROC5_r1i1p1_historical_1950_2005_CONUS_monthly.nc")
tmin_45_miroc5 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_MIROC5_r1i1p1_rcp45_2006_2099_CONUS_monthly.nc")
tmin_85_miroc5 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_MIROC5_r1i1p1_rcp85_2006_2099_CONUS_monthly.nc")


# Temperature
temp_hist <- list(
  tmax_hist_canesm2=tmax_hist_canesm2,tmin_hist_canesm2=tmin_hist_canesm2,
  tmax_hist_ccsm4=tmax_hist_ccsm4,tmin_hist_ccsm4=tmin_hist_ccsm4,
  tmax_hist_cnrm=tmax_hist_cnrm,tmin_hist_cnrm=tmin_hist_cnrm,
  tmax_hist_hadgemcc=tmax_hist_hadgemcc,tmin_hist_hadgemcc=tmin_hist_hadgemcc,
  tmax_hist_hadgemes=tmax_hist_hadgemes,tmin_hist_hadgemes=tmin_hist_hadgemes,
  tmax_hist_miroc5=tmax_hist_miroc5,tmin_hist_miroc5=tmin_hist_miroc5
)

temp_proj <- list(
  tmax_45_canesm2=tmax_45_canesm2,tmax_85_canesm2=tmax_85_canesm2,
  tmin_45_canesm2=tmin_45_canesm2,tmin_85_canesm2=tmin_85_canesm2,
  tmax_45_ccsm4=tmax_45_ccsm4,tmax_85_ccsm4=tmax_85_ccsm4,
  tmin_45_ccsm4=tmin_45_ccsm4,tmin_85_ccsm4=tmin_85_ccsm4,
  tmax_45_cnrm=tmax_45_cnrm,tmax_85_cnrm=tmax_85_cnrm,
  tmin_45_cnrm=tmin_45_cnrm,tmin_85_cnrm=tmin_85_cnrm,
  tmax_45_hadgemcc=tmax_45_hadgemcc,tmax_85_hadgemcc=tmax_85_hadgemcc,
  tmin_45_hadgemcc=tmin_45_hadgemcc,tmin_85_hadgemcc=tmin_85_hadgemcc,
  tmax_45_hadgemes=tmax_45_hadgemes,tmax_85_hadgemes=tmax_85_hadgemes,
  tmin_45_hadgemes=tmin_45_hadgemes,tmin_85_hadgemes=tmin_85_hadgemes,
  tmax_45_miroc5=tmax_45_miroc5,tmax_85_miroc5=tmax_85_miroc5,
  tmin_45_miroc5=tmin_45_miroc5,tmin_85_miroc5=tmin_85_miroc5
)

# Precipitation
precip_hist <- list(
  p_hist_canesm2=p_hist_canesm2,p_hist_ccsm4=p_hist_ccsm4,
  p_hist_cnrm=p_hist_cnrm,p_hist_hadgemcc=p_hist_hadgemcc,
  p_hist_hadgemes=p_hist_hadgemes,p_hist_miroc5=p_hist_miroc5
)

precip_proj <- list(
  p_45_canesm2=p_45_canesm2,p_85_canesm2=p_85_canesm2,
  p_45_ccsm4=p_45_ccsm4,p_85_ccsm4=p_85_ccsm4,
  p_45_cnrm=p_45_cnrm,p_85_cnrm=p_85_cnrm,
  p_45_hadgemcc=p_45_hadgemcc,p_85_hadgemcc=p_85_hadgemcc,
  p_45_hadgemes=p_45_hadgemes,p_85_hadgemes=p_85_hadgemes,
  p_45_miroc5=p_45_miroc5,p_85_miroc5=p_85_miroc5
)

temp_hist <- map(temp_hist, chg_x_coord_temp_units)
temp_proj <- map(temp_proj, chg_x_coord_temp_units)
precip_hist <- map(precip_hist, chg_x_coord)
precip_proj <- map(precip_proj, chg_x_coord)


# ---------------------------------------------------------------------
# Export MACA (Temperature and Precipitation)

write_rds(temp_hist, "output/processed_data/temp_hist.rds")
write_rds(temp_proj, "output/processed_data/temp_proj.rds")
write_rds(precip_hist, "output/processed_data/precip_hist.rds")
write_rds(precip_proj, "output/processed_data/precip_proj.rds")



