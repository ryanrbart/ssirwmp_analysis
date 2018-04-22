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

# Function that adjusts MACA x coordinate and converts kelvin to celcius
chg_x_coord <- function(x){
  xmin(x) <- xmin(x)-360  # Change x coordinates
  xmax(x) <- xmax(x)-360  # Change x coordinates
  return(x)
}

chg_x_coord_temp_units <- function(x){
  x <- chg_x_coord(x)
  x <- x - 273.15   # K to C
  return(x)
}

# Import precip, tmax and tmin data
p_hist <- brick("data/maca_precip_temp/agg_macav2metdata_pr_CCSM4_r6i1p1_historical_1950_2005_CONUS_monthly.nc")
p_hist <- chg_x_coord(p_hist)

p_45 <- brick("data/maca_precip_temp/agg_macav2metdata_pr_CCSM4_r6i1p1_rcp45_2006_2099_CONUS_monthly.nc")
p_45 <- chg_x_coord(p_45)

p_85 <- brick("data/maca_precip_temp/agg_macav2metdata_pr_CCSM4_r6i1p1_rcp85_2006_2099_CONUS_monthly.nc")
p_85 <- chg_x_coord(p_85)

tmax_hist <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_CCSM4_r6i1p1_historical_1950_2005_CONUS_monthly.nc")
tmax_hist <- chg_x_coord_temp_units(tmax_hist)

tmax_45 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_CCSM4_r6i1p1_rcp45_2006_2099_CONUS_monthly.nc")
tmax_45 <- chg_x_coord_temp_units(tmax_45)

tmax_85 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmax_CCSM4_r6i1p1_rcp85_2006_2099_CONUS_monthly.nc")
tmax_85 <- chg_x_coord_temp_units(tmax_85)

tmin_hist <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_CCSM4_r6i1p1_historical_1950_2005_CONUS_monthly.nc")
tmin_hist <- chg_x_coord_temp_units(tmin_hist)

tmin_45 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_CCSM4_r6i1p1_rcp45_2006_2099_CONUS_monthly.nc")
tmin_45 <- chg_x_coord_temp_units(tmin_45)

tmin_85 <- brick("data/maca_precip_temp/agg_macav2metdata_tasmin_CCSM4_r6i1p1_rcp85_2006_2099_CONUS_monthly.nc")
tmin_85 <- chg_x_coord_temp_units(tmin_85)

# ------
# Null out raster areas outside of ss border

ss_border_rast <- rasterize(ss_border_sp, p_hist[[1]])

p_hist[is.na(ss_border_rast) == TRUE] <- NA
p_45[is.na(ss_border_rast) == TRUE] <- NA
p_85[is.na(ss_border_rast) == TRUE] <- NA
tmax_hist[is.na(ss_border_rast) == TRUE] <- NA
tmax_45[is.na(ss_border_rast) == TRUE] <- NA
tmax_85[is.na(ss_border_rast) == TRUE] <- NA
tmin_hist[is.na(ss_border_rast) == TRUE] <- NA
tmin_45[is.na(ss_border_rast) == TRUE] <- NA
tmin_85[is.na(ss_border_rast) == TRUE] <- NA


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
# Import 









