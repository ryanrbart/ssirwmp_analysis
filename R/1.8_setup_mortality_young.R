# Data import and processsing

# All data is reprojected (if necessary) to lat/long

source("R/0_utilities.R")
source("R/1.1_setup_proj_boundaries.R")
source("R/1.2_setup_temp_precip.R")
source("R/1.5_setup_prism.R")

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

