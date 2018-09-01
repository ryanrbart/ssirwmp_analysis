# Utilities for Kings River downscale analysis
# Includes files/directories and functions


# ---------------------------------------------------------------------
# Libraries

library(tidyverse)
library(broom)
library(lubridate)
library(beepr)
library(devtools)
library(raster)
library(sf)
library(sp)
library(rgdal)
library(fasterize)
library(rasterVis)
library(plotKML)
library(spatial.tools)

# Young2017 recommended packages
library(ncdf4)
library(gdalUtils)
library(plyr)
library(reshape2)
library(lme4)
library(rgeos)
library(data.table)


# ---------------------------------------------------------------------
# Files and Directories


# ---------------------------------------------------------------------
# Functions


# Utility functions (from Young2017)
fn.bin <- function(x){ifelse(x >0,1,0)} # is a value over 0 or not?



