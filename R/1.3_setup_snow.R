# Data import and processsing

# All data is reprojected (if necessary) to lat/long

source("R/0_utilities.R")
source("R/1.1_setup_proj_boundaries.R")


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
snow <- crop(snow, (e+0.2))

# Resample
snow <- resample(snow, temp_hist[[1]], method='ngb')

snow_map <- snow

# Null out raster areas outside of ss border
snow[is.na(ss_border_rast) == TRUE] <- NA

