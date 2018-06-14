# Data import and processsing

# All data is reprojected (if necessary) to lat/long

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Define projection

proj_longlat <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj_prism <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

# ---------------------------------------------------------------------
# Import SSIRWMG Boundary

ss_border <- st_read(dsn = "data/ssirwmp_gis/", layer = "SSIRWM")
ss_border <- st_transform(ss_border, crs = proj_longlat)
ss_border_sp <- as(st_geometry(ss_border), "Spatial")  # Change from sf to sp object since raster::crop won't work with extent of sf object
e <- extent(ss_border_sp)
# Use ss_border for graphics, e for cropping/masks

# Generate kml
#kml(ss_border_sp, file.name = "data/kml/ssrwmg.kml")

# Nearby cities
# Read in site data
cities <- read.table("data/ssirwmp_gis/ssirwmp_cities.txt", sep = ",", header = TRUE)

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

# Temperature and precipitation data for maps
temp_hist_map <- temp_hist
temp_proj_map <- temp_proj
precip_hist_map <- precip_hist
precip_proj_map <- precip_proj

# ------
# Temperature and precipitation data for analysis
# Null out raster areas outside of ss border
ss_border_rast <- rasterize(ss_border_sp, precip_hist[[1]])

# Using a for loop instead of purrr::MAP cause I can't figure out how to rework embedded function to not contain <-
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
# Import Young mortality data

albers.proj <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# Function to convert data frame to spatial data
filter_rasterize=function(data, year, layer){
  data <- dplyr::filter(data, year==year)
  coordinates(data) = ~alb.x + alb.y
  gridded(data) = T
  r <- raster(data,layer=layer)
  return(r)
}

# Import data
young <- read_csv("data/forest_service_mortality/young2017_data/Young_et_al_Data.csv")

y_15_live.bah <- filter_rasterize(data=young, year=2015, layer="live.bah")
y_15_live.tph <- filter_rasterize(data=young, year=2015, layer="live.tph")
y_15_mort.bin <- filter_rasterize(data=young, year=2015, layer="mort.bin")
y_15_mort.tph <- filter_rasterize(data=young, year=2015, layer="mort.tph")
y_15_Defnorm <- filter_rasterize(data=young, year=2015, layer="Defnorm")
y_15_Def0 <- filter_rasterize(data=young, year=2015, layer="Def0")
y_15_Defz0 <- filter_rasterize(data=young, year=2015, layer="Defz0")
y_15_Defquant <- filter_rasterize(data=young, year=2015, layer="Defquant")

y_15 <- stack(y_15_live.bah,y_15_live.tph,y_15_mort.bin,y_15_mort.tph,
              y_15_Defnorm,y_15_Def0,y_15_Defz0,y_15_Defquant)

# Assign projection (Albers (meter units))
proj4string(y_15) <- CRS("+init=epsg:3310") 

# Reproject, resample and set new extent
y_15 <- spatial_sync_raster(y_15,temp_hist[[1]],method="bilinear")

#plot(y_15[[1]])

# ---------------------------------------------------------------------
# Import PRISM Data

# PRISM monthly precipitation - WY2015

precip_file_stack <- list.files(path="data/cwd/PRISM_ppt_stable_4kmM3_2015_all_asc/", pattern = "PRISM_ppt_stable_4kmM3_201", full.names = TRUE)
prism_precip_stack = stack(precip_file_stack)
# Assign prism projection
proj4string(prism_precip_stack) <- proj_prism
# Reproject, resample and set new extent
prism_precip_stack <- spatial_sync_raster(prism_precip_stack,temp_hist[[1]],method="bilinear")

# ----

# PRISM monthly mean temperature - WY2015

temp_file_stack <- list.files(path="data/cwd/PRISM_tmean_stable_4kmM2_2015_all_asc/", pattern = "PRISM_tmean_stable_4kmM2_201", full.names = TRUE)
prism_temp_stack = stack(temp_file_stack)
# Assign prism projection
proj4string(prism_temp_stack) <- proj_prism
# Reproject, resample and set new extent
prism_temp_stack <- spatial_sync_raster(prism_temp_stack,temp_hist[[1]],method="bilinear")

# Generate PET from temperature

# Generate mean monthly daylight hours in day
daylight_hours = read_csv("data/cwd/daylight_hours_visalia.csv",
                          col_types=cols(day=col_skip()),
                          col_names = c("day","1","2","3","4","5","6",
                                        "7","8","9","10","11","12"))
daylight_hours <- daylight_hours %>% 
  summarize_all(., funs(mean(., na.rm = TRUE))) %>% 
  as_vector()/3600

# Function for Harom PET method
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
days_per_month <- c(31,28,31,30,31,30,31,31,30,31,30,31)

pet_stack <- calc(prism_temp_stack, fun = function(x){pet_hamon(x,daylight_hours)})
pet_stack <- calc(pet_stack, fun = function(x){x*days_per_month})
# Sum months to annual total
pet_2015 <- calc(pet_stack, sum)

# ---------------------------------------------------------------------
# Import Stream Temperature

# Import data
stream_lines <- st_read(dsn = "data/stream_temperature/", layer = "NorWeST_PredictedStreamTempLines_CentralCA")

# Reproject
stream_lines <- st_transform(stream_lines, crs = proj_longlat)

# Subset by SS
#ss_stream_lines <- st_intersection(stream_lines, ss_border) # Just SS region
ss_stream_lines <- st_crop(stream_lines, st_bbox(st_buffer(ss_border,0.05))) # Full SS box extent

# Filter out non-valid data (Scenarios 37-41 have data when all others are -9999)
ss_stream_lines <- ss_stream_lines %>% 
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
snow <- resample(snow, temp_hist[[1]], method='ngb')

# Null out raster areas outside of ss border
snow[is.na(ss_border_rast) == TRUE] <- NA



# ---------------------------------------------------------------------
# Import wildfire data
# Source: http://cal-adapt.org/data/wildfire/
# Using the annual averages. The number datasets show individual modeled fires.

# Select files
fire_files <- list.files("data/fire", pattern = "*.nc", full.names = TRUE)
fire_files_names <- list.files("data/fire", pattern = "*.nc", full.names = FALSE)

# Create fire raster
fire <- map(fire_files, function(x) stack(x))
names(fire) <- fire_files_names

# Reproject, resample and set new extent
fire_stack <- map(fire, function(x) spatial_sync_raster(x,temp_hist[[1]],method="bilinear"))

# Rename layers of raster stack to year
newname <- paste("X",seq(1954,2100), sep="")
map(fire_stack, ~setNames(.x, newname))

# Create variable with full extent to use with fire maps
fire_stack_map <- fire_stack

# Using a for loop instead of purrr::MAP cause I can't figure out how to rework embedded function to not contain <-
for (aa in seq_along(fire_stack)){
  fire_stack[[aa]][is.na(ss_border_rast) == TRUE] <- NA
}


# ---------------------------------------------------------------------
# Import VIC data

# Import basin-averaged daily VIC data
etqswe_kings_canesm2_hist <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CanESM2_Historical_Q_ET_SWE_1950-2005.csv")
etqswe_kings_canesm2_45 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CanESM2_RCP45_Q_ET_SWE_2006-2099.csv")
etqswe_kings_canesm2_85 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CanESM2_RCP85_Q_ET_SWE_2006-2099.csv")

etqswe_kings_ccsm4_hist <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CCSM4_Historical_Q_ET_SWE_1950-2005.csv")
etqswe_kings_ccsm4_45 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CCSM4_RCP45_Q_ET_SWE_2006-2099.csv")
etqswe_kings_ccsm4_85 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CCSM4_RCP85_Q_ET_SWE_2006-2099.csv")

etqswe_kings_cnrm_hist <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CNRM-CM5_Historical_Q_ET_SWE_1950-2005.csv")
etqswe_kings_cnrm_45 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CNRM-CM5_RCP45_Q_ET_SWE_2006-2099.csv")
etqswe_kings_cnrm_85 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CNRM-CM5_RCP85_Q_ET_SWE_2006-2099.csv")

etqswe_kings_hadgemcc_hist <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_HadGEM2-CC365_Historical_Q_ET_SWE_1950-2005.csv")
etqswe_kings_hadgemcc_45 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_HadGEM2-CC365_RCP45_Q_ET_SWE_2006-2099.csv")
etqswe_kings_hadgemcc_85 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_HadGEM2-CC365_RCP85_Q_ET_SWE_2006-2099.csv")

etqswe_kings_hadgemec_hist <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_HadGEM2-ES365_Historical_Q_ET_SWE_1950-2005.csv")
etqswe_kings_hadgemec_45 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_HadGEM2-ES365_RCP45_Q_ET_SWE_2006-2099.csv")
etqswe_kings_hadgemec_85 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_HadGEM2-ES365_RCP85_Q_ET_SWE_2006-2099.csv")

etqswe_kings_miroc5_hist <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_MIROC5_Historical_Q_ET_SWE_1950-2005.csv")
etqswe_kings_miroc5_45 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_MIROC5_RCP45_Q_ET_SWE_2006-2099.csv")
etqswe_kings_miroc5_85 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_MIROC5_RCP85_Q_ET_SWE_2006-2099.csv")

# Observed values
etqswe_kings_obs <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_Obs_Q_ET_SWE_1950-2011.csv")

# Consolidate basin-averaged daily data to lists
etqswe_kings <- list(
  kings_canesm2_hist=etqswe_kings_canesm2_hist,
  kings_ccsm4_hist=etqswe_kings_ccsm4_hist,
  kings_cnrm_hist=etqswe_kings_cnrm_hist,
  kings_hadgemcc_hist=etqswe_kings_hadgemcc_hist,
  kings_hadgemec_hist=etqswe_kings_hadgemec_hist,
  kings_miroc5_hist=etqswe_kings_miroc5_hist,
  kings_canesm2_45=etqswe_kings_canesm2_45,kings_canesm2_85=etqswe_kings_canesm2_85,
  kings_ccsm4_45=etqswe_kings_ccsm4_45,kings_ccsm4_85=etqswe_kings_ccsm4_85,
  kings_cnrm_45=etqswe_kings_cnrm_45,kings_cnrm_85=etqswe_kings_cnrm_85,
  kings_hadgemcc_45=etqswe_kings_hadgemcc_45,kings_hadgemcc_85=etqswe_kings_hadgemcc_85,
  kings_hadgemec_45=etqswe_kings_hadgemec_45,kings_hadgemec_85=etqswe_kings_hadgemec_85,
  kings_miroc5_45=etqswe_kings_miroc5_45,kings_miroc5_85=etqswe_kings_miroc5_85,
  kings_magicalgcm_obs=etqswe_kings_obs
)


# ----
# Sierra (aka WY average ET and Peak SWE - distributed)
wy_et_canesm2_hist <- readr::read_csv("../../VIC_Modeling_SS/Sierra/CanESM2_Historical_1951_2005_WY_ET.csv")



# ---------------------------------------------------------------------
# Import DEM


# happy <- raster("data/dem/dem90_hf/dem90_hf/dem90_hf/w001001.adf")

# https://gis.stackexchange.com/questions/147797/unable-to-read-raster-into-r-tiffreaddirectoryfailed-to-read-directory-at-off



