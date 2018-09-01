# Data import and processsing

# All data is reprojected (if necessary) to lat/long

source("R/0_utilities.R")
source("R/1.1_setup_proj_boundaries.R")


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


