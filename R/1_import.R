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
# Import temperature

tmax_files <- list.files(path = 'data/temperature_california_annual_4.5', pattern='tasmax_', full.names = T)
tmax_1 <- raster::raster(tmax_files[1])
ss_tmax_1 <- crop(tmax_1, e, snap="out")
tmax_stack = raster::stack(tmax_files)
ss_tmax_stack <- crop(tmax_stack, e, snap="out")
#ss_tmax_brick <- brick(ss_tmax_stack)

# ---------------------------------------------------------------------
# Import precipitation

precip_files <- list.files(path = 'data/precip_california_annual_4.5', pattern='pr_year_', full.names = T)
precip_1 <- raster::raster(precip_files[1])
ss_precip_1 <- crop(precip_1, e, snap="out")
precip_stack = raster::stack(precip_files)
ss_precip_stack <- crop(precip_stack, e, snap="out")

# ---------------------------------------------------------------------
# Import MACA

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
# Import ????



