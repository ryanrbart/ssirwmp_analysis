# Data import and processsing

# All data is reprojected (if necessary) to lat/long

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Define projections

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
# Import Kings Boundary

kings_border <- st_read(dsn = "data/kings_gis/", layer = "Kings")
kings_border <- st_transform(kings_border, crs = proj_longlat)
kings_border_sp <- as(st_geometry(kings_border), "Spatial")  # Change from sf to sp object since raster::crop won't work with extent of sf object
kings_e <- extent(kings_border_sp)
# Use kings_border for graphics, kings_e for cropping/masks



