# Data import and processsing

# All data is reprojected (if necessary) to lat/long

source("R/0_utilities.R")
source("R/1.1_setup_proj_boundaries.R")
source("R/1.2_setup_temp_precip.R")

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

