# Data import and processsing

# All data is reprojected (if necessary) to lat/long

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Define projection

proj_longlat <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# ---------------------------------------------------------------------
# Import SSIRWMG Boundary

ss_border <- st_read(dsn = "data/ssirwmp_gis/", layer = "SSIRWM")
ss_border <- st_transform(ss_border, crs = proj_longlat)
ss_border_sp <- as(st_geometry(ss_border), "Spatial")  # Change from sf to sp object since raster::crop won't work with extent of sf object
e <- extent(ss_border_sp)
# Use ss_border for graphics, e for cropping/masks

# Generate kml
#kml(ss_border_sp, file.name = "data/kml/ssrwmg.kml")

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


# ------
# Null out raster areas outside of ss border
ss_border_rast <- rasterize(ss_border_sp, precip_hist[[1]])

# For loop instead of MAP cause I can't figure out how to rework embedded function to not contain <-
for (aa in seq_along(temp_hist)){
  temp_hist[[aa]][is.na(ss_border_rast) == TRUE] <- NA
}
for (aa in seq_along(temp_proj)){
  temp_proj[[aa]][is.na(ss_border_rast) == TRUE] <- NA
}
for (aa in seq_along(precip_hist)){
  precip_hist[[aa]][is.na(ss_border_rast) == TRUE] <- NA
}
for (aa in seq_along(precip_proj)){
  precip_proj[[aa]][is.na(ss_border_rast) == TRUE] <- NA
}

#values(temp_hist[[1]][[1]])
#values(temp_proj[[1]][[1]])
#values(precip_hist[[1]][[1]])
#values(precip_proj[[1]][[1]])


# ---------------------------------------------------------------------
# Import Forest Mortality Data

# Forest mortality geodatabase (https://www.fs.usda.gov/detail/r5/forest-grasslandhealth/?cid=fsbdev3_046696)

# Check layers
st_layers(dsn = "data/forest_service_mortality/ADS2016.gdb")
st_layers(dsn = "data/forest_service_mortality/ADS2017.gdb")

# Import data
mort_2016 <- st_read(dsn = "data/forest_service_mortality/ADS2016.gdb",
                     layer = "ADS16",
                     type = 6) # See sf vignette #3. Using default 0 produces geometry type: Geometry, which does not process well
mort_2017 <- st_read(dsn = "data/forest_service_mortality/ADS2017.gdb",
                     layer = "ADS17",
                     type = 6) # See sf vignette #3. Using default 0 produces geometry type: Geometry, which does not process well

# Reproject
mort_2016 <- st_transform(mort_2016, crs = proj_longlat)
mort_2017 <- st_transform(mort_2017, crs = proj_longlat)

# Subset by SS
ss_mort_2016 <- st_intersection(mort_2016, ss_border)
ss_mort_2017 <- st_intersection(mort_2017, ss_border)


# ---------------------------------------------------------------------
# Import Stream Temperature

# Import data
stream_lines <- st_read(dsn = "data/stream_temperature/", layer = "NorWeST_PredictedStreamTempLines_CentralCA")
stream_points <- st_read(dsn = "data/stream_temperature/", layer = "NorWeST_PredictedStreamTempPoints_CentralCA")

# Reproject
stream_lines <- st_transform(stream_lines, crs = proj_longlat)
stream_points <- st_transform(stream_points, crs = proj_longlat)

# Subset by SS
ss_stream_lines <- st_intersection(stream_lines, ss_border)
ss_stream_points <- st_intersection(stream_points, ss_border)

# Filter out non-valid data (Scenarios 37-41 have data when all others are -9999)
ss_stream_lines <- ss_stream_lines %>% 
  dplyr::filter(S1_93_11 != -9999.00)
ss_stream_points <- ss_stream_points %>% 
  dplyr::filter(S1_93_11 != -9999.00)


# ---------------------------------------------------------------------
# Import Snow - Klos
# https://zionklos.com/rain-snow_maps/

# List filenames
snow_files = list.files("data/snow_klos", full.names = T)

# Import data
snow <- stack(snow_files)
#snow <- stack("data/snow_klos/hist1.txt")

# Assign projection (NAD 83)
proj4string(snow) <- CRS("+init=epsg:4269") 

# Reproject
snow  <- projectRaster(snow, crs = proj_longlat) 

# Crop
snow <- crop(snow, e)

# Resample
snow <- resample(snow, tmax_45, method='ngb')

# Null out raster areas outside of ss border
snow[is.na(ss_border_rast) == TRUE] <- NA


# ---------------------------------------------------------------------
# Import DEM


happy <- raster("data/dem/dem90_hf/dem90_hf/dem90_hf/w001001.adf")

# https://gis.stackexchange.com/questions/147797/unable-to-read-raster-into-r-tiffreaddirectoryfailed-to-read-directory-at-off



