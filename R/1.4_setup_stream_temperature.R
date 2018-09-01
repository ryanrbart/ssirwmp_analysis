# Data import and processsing

# All data is reprojected (if necessary) to lat/long

source("R/0_utilities.R")
source("R/1.1_setup_proj_boundaries.R")


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

