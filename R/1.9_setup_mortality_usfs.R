# Import and rasterize mortality data
# Code adapted from Young et al. "Long-term climate and competition explain forest mortality patterns under extreme drought"
# Dryad Digital Repository: http://dx.doi.org/10.5061/dryad.7vt36

# All data is reprojected (if necessary) to lat/long

source("R/0_utilities.R")
source("R/1.1_setup_proj_boundaries.R")


# ---------------------------------------------------------------------
# Grid-level variables (not dependent on year)

# Create master rasters (the grid to hold all variables)
#ss_border_mort <- gBuffer(ss_border_sp,width=0.1) # buffer by 0.1 degrees
ss_border_mort <- ss_border_sp

# Define resolutions
res.fine <- 0.000416658 # spatial resolution for rasterization of flight and mortality data
res.coarse <- 0.0416658 # resolution for aggregation of mortality data and extraction of climate data for statistical analysis
agg.factor <- round(res.coarse/res.fine) # aggregation factor for conversion from fine to coarse

## Create raster templates
# Raster master.fine is for initial rasterization of flight and mortality data (in meters)
master.fine <- raster(ss_border_mort,res=res.fine) # define extent and coord system based on calif shapefile (should be in Albers--meters)
# Raster master.coarse is for aggregation of mortality data to a coarse scale as well as for extarction of climate and other environmental data. This is the scale for statistical analysis
master.coarse <- aggregate(master.fine,agg.factor,fun=mode) # coarse grid that aligns exactly with fine grid


## Extract the parameters needed to re-create the same grids from scratch
master.fine.res <- res(master.fine)
master.fine.extent <- extent(master.fine)[c(1,3,2,4)]

master.coarse.res <- res(master.coarse)
master.coarse.extent <- extent(master.coarse)[c(1,3,2,4)]

albers.proj <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


# ### Create coarse grid that represents the proportion of each coarse grid cell that is land
# # Open waterbody layer
# waterbody <- readOGR("waterbody","wtrbdyp010") # Polygon shapefile layer of major waterbodies. We used: https://catalog.data.gov/dataset/usgs-small-scale-dataset-1-1000000-scale-waterbodies-and-wetlands-of-the-united-states-201403-
# waterbody$water <- 1 # Everywhere that has a polygon is water. Coded as 1.
# waterbody <- spTransform(waterbody,projection(master.fine)) # Project to the same projection as the fine-resolution raster template
# writeOGR(waterbody, getwd(),"waterbody_test",driver="ESRI Shapefile",overwrite=TRUE) # Write to shapefile for rasterization
# 
# # Rasterize it at fine resolution
# water.raster <- gdal_rasterize("waterbody_test.shp","waterbodyraster_test.tif",
#                                a="water", tr=master.fine.res, te=master.fine.extent,
#                                l="waterbody_test",a_nodata=NA,verbose=TRUE,output_Raster=TRUE)
# 
# #Aggregate water raster to the scale for statistical analysis, so that the values in coarse cells reflect the proportion of the coarse cell that is water
# waterraster.coarse <- aggregate(water.raster,agg.factor,fun=sum,na.rm=TRUE)
# propwater <- waterraster.coarse/(agg.factor^2) # calculate proportion as number of fine cells that were water divided by total number of cells
# propland <- 1-propwater # calculate the proportion that is land
# propland[is.na(propland)] <- 1 # if the value was NA, that means there was no water there, so it was completely land
# propland[propland < .5] <- NA # if it's more than half water, set it to NA so that we do not use those cells as data points.


# ------------------






# fl.2015 <- st_read(dsn = "data/forest_service_mortality/ADS2015.gdb",
#                      layer = "FL15",
#                      type = 0)

mort.2015 <- st_read(dsn = "data/forest_service_mortality/ADS2015.gdb",
                     layer = "ADS15",
                     type = 0)
mort.2016 <- st_read(dsn = "data/forest_service_mortality/ADS2016.gdb",
                     layer = "ADS16",
                     type = 6)

sp.2015 <- unique(c(mort.2015$HOST1,mort.2015$HOST2,mort.2015$HOST3))
sp.2016 <- unique(c(mort.2016$HOST1,mort.2016$HOST2,mort.2016$HOST3))


focal.sp.full <- union(sp.2015,sp.2016)
focal.sp.full.all <- focal.sp.full[focal.sp.full > 0]
species.set <- list(focal.sp.full.all)


### Rasterize mortality data for each year
year.set <- c(15:16)
for(i in 1:length(year.set)){
  
  #define year of interest; and aerial survey file for that year
  year <- year.set[i]
  mort.file <- paste("ADS",year,sep="")
  flight.file <- paste("FL",year,sep="")
  
  ## Open corresponding shapefiles
  # Survey shapefiles should be in ESRI shapefile format, in the folder "Survey shapefiles" which is a subfolder of the working directory, with naming "ADS_2009.shp" etc.
  # There should also be flown area shapefiles (same format) in the same subfolder, one file for each year, with naming "Flown_area_2009.shp" etc.
  # Annual survey spatial data accessible here: http://www.fs.usda.gov/detail/r5/forest-grasslandhealth/?cid=fsbdev3_046696 (needs to be converted from geodatabases into shapefiles before opening in this script)
  flight.polygon <- st_read(dsn = "data/forest_service_mortality/ADS2015.gdb",
                     layer = flight.file,
                     type = 0)
  mort.polygon <- st_read(dsn = "data/forest_service_mortality/ADS2015.gdb",
                       layer = mort.file,
                       type = 0)

  flight.polygon <- as(flight.polygon, 'Spatial')
  mort.polygon <- as(mort.polygon, 'Spatial')
  
  # Reproject to Albers (units in meters and compatible with master rasters)
  mort.polygon <- spTransform(mort.polygon,albers.proj)
  flight.polygon <- spTransform(flight.polygon,albers.proj)
  
  ## Make sure at least one of the damage types was mortality (because defoliation is an option too), and remove those that aren't
  mortality <- (mort.polygon$DMG_TYPE1 == 2) | (mort.polygon$DMG_TYPE2 == 2) | (mort.polygon$DMG_TYPE3 == 2)
  mort.polygon <- mort.polygon[mortality,]
  
  ## Remove the one outlier (the only point in all years that is >1200 TPA). TPA is trees per acre, a field reported in the aerial surveys
  mort.polygon <- mort.polygon[mort.polygon$TPA1 < 1200,]
  
  
  #Add column to flight shapefile that indicates that it was flown (every polygon will have the value 1 for this attribute; useful for rasterization later)
  flight.polygon$FLOWN1 <- 1
  
  ## Eliminate polygons where mortality was attributed to non drought-related agents/causes
  fire <- 30000:30005
  animals <- 41000:42900
  physical <- c(50004:50006,50011:50020)
  human <- c(70000:70011,70013:71001)
  non.drought <- c(fire,animals,physical,human)
  mort.polygon <- mort.polygon[!((mort.polygon$DCA1 %in% non.drought) | (mort.polygon$DCA2 %in% non.drought) | (mort.polygon$DCA3 %in% non.drought)),]
  
  # Sum the three TPA columns whever they are positive for total dead trees per acre
  tpa.cols <- cbind(mort.polygon$TPA1,mort.polygon$TPA2,mort.polygon$TPA3)
  tpa.cols[tpa.cols <= 0] <- NA # if any values are 0 or less, set to NA so they are disregarded
  mort.polygon$TPA.tot <- rowSums(tpa.cols,na.rm=TRUE)
  
  ## Eliminate "background mortality" using filters based on personal communication with Zachary Heath and Jeffrey Moore  
  # Eliminate polygons where total TPA == 1 (when TPA is less than 1 it is usually from large polygons with a small fixed number of trees, which are likely not "background" mortality)
  mort.polygon <- mort.polygon[mort.polygon$TPA.tot != 1,]
  # Eliminate polygons where the number of trees is 3 or less
  mort.polygon <- mort.polygon[mort.polygon$NO_TREES1 > 3 , ]
  
  ## When multiple host species present, divide TPA by the number of hosts
  # Tally number of hosts listed
  host.cols <- cbind((mort.polygon$HOST1), (mort.polygon$HOST2), (mort.polygon$HOST3))
  host.cols[host.cols < 1] <- NA
  n.host <- rowSums(host.cols > 0,na.rm=TRUE)
  
  # Divide TPA by number of hosts
  mort.polygon$TPA.split <- mort.polygon$TPA.tot/n.host
  mort.polygon$TPA.split[mort.polygon$TPA.split == Inf] <- 0 # because sometimes there are zero hosts with a host id > 0
  # Now TPA.split holds the average number of dead trees PER HOST
  
  
  
  #### Write flight path polygon to shapefile for rasterization
  # Write it
  writeOGR(flight.polygon, getwd(),"flightpolygon_test",driver="ESRI Shapefile",overwrite=TRUE)
  
  # Rasterize it to the master.fine grid
  flightraster <- gdal_rasterize("flightpolygon_test.shp","flightrasterfocal_test.tif",
                                 a="FLOWN1", tr=master.fine.res, te=master.fine.extent,
                                 l="flightpolygon_test",a_nodata=NA,verbose=TRUE,output_Raster=TRUE)
  
  # Aggregate flight raster to the scale for statistical analysis
  # Note that this operation sets all coarse cells that are partially outside the flight path to NA
  flightraster.coarse <- aggregate(flightraster,agg.factor,fun=mean,na.rm=FALSE)
  
  
  
  # For each set of species (we only have one set, which includes all species, but this coding would allow later extension to work with multiple species sets or individual species)
  for(j in 1:length(species.set)){
    
    # Define tree species set 
    focal.sp <- species.set[[j]] # focal.sp now holds a vector of species IDs
    
    ## For each mortality polygon, convert polygon TPA to TPA of focal species only (i.e., set TPA 0 if polygon does not contain any of focal species)
    # See if focal species is present
    n.host.match.focal <- (mort.polygon$HOST1 %in% focal.sp) + (mort.polygon$HOST2 %in% focal.sp) + (mort.polygon$HOST3 %in% focal.sp)
    
    # Multiply the average per-host TPA by the number of hosts that are focal species
    mort.polygon$tpa.focal <- mort.polygon$TPA.split * n.host.match.focal
    mort.polygon$tpa.focal <- ifelse(is.na(mort.polygon$tpa.focal),0,mort.polygon$tpa.focal) # if number of hosts was set to NA (meaning no matching hosts), set it to 0 so it can be added
    
    # Eliminate polygons with none of the current focal species
    if(sum(mort.polygon$tpa.focal) > 0) {
      mort.polygon.focal <- mort.polygon[mort.polygon$tpa.focal > 0,]
    } else {
      mort.polygon.focal <- mort.polygon[0,]
    }
    
    # Sort the polygons so the polygons with larger mortality density come later
    mort.polygon.focal <- mort.polygon.focal[order(mort.polygon.focal$TPA1),]
    
    ## Rasterize mortality polygon; define unobserved cells, i.e. not in flight path, as NA
    # Write mortality polygon of focal species to shapefile for next step (rasterization)
    writeOGR(mort.polygon.focal, getwd(),"mortpolygonfocal_test", driver="ESRI Shapefile",overwrite=TRUE)
    
    mort.raster <- readOGR("mortpolygonfocal_test.shp")
    
    # Rasterize
    mortraster <- gdal_rasterize("mortpolygonfocal_test.shp","mortrasterfocal_test.tif",
                                 a="tpa_fcl",tr=master.fine.res, te=master.fine.extent,   # tpa_fcl name seems to be generated automatically from tpa.focal
                                 l="mortpolygonfocal_test",verbose=TRUE,output_Raster=TRUE)
    mortraster[is.na(mortraster)] <- 0 # for raster cells that did not have any overlapping polygons, set mortality value to 0
    
    # Aggregate fine-scale raster to the scale for statistical analysis
    mortraster.coarse <- aggregate(mortraster,agg.factor,fun=mean,na.rm=FALSE)
    
    # Set aggregated mortality cells that are at all outside flight path to NA (because cells in flightraster.coarse that are outside the flight path are NA)
    mort.flight <- mortraster.coarse * flightraster.coarse
    
    # Divide mortality density by proportion of cell that is land to obtain the on-land mortality density
#    mort.flight <- mort.flight / propland
    
    
    #### Prepare for statistical analysis
    
    # Stack rasters, name layers, and write to external file
    raster_stack <- stack(mort.flight)
    spgrp.text <- sprintf("%02d",j) # add leading zero
    
    layer.names <- paste("Y",year,".spgrp",spgrp.text,".",c("mort.tpa"),sep="")
    names(raster_stack) <- layer.names

#   writeRaster(raster_stack, file=paste0("RasterOutput/Y",year,"_","spgrp",j,".grd",sep=""),overwrite=T)
    writeRaster(raster_stack, file="happy.grd",overwrite=T)
    cat("\rFinished Year",year,", species group",j)
  }
}




# ---------------------------------------------------------------------
# Older code - When I was initially trying to import USFS data
# Import Forest Mortality Data

# Forest mortality geodatabase (https://www.fs.usda.gov/detail/r5/forest-grasslandhealth/?cid=fsbdev3_046696)

# Check layers
st_layers(dsn = "data/forest_service_mortality/ADS2015.gdb")
st_layers(dsn = "data/forest_service_mortality/ADS2016.gdb")
st_layers(dsn = "data/forest_service_mortality/ADS2017.gdb")

# Import data
mort_2015 <- st_read(dsn = "data/forest_service_mortality/ADS2015.gdb",
                     layer = "ADS15",
                     type = 0) # See sf vignette #3. Using default 0 produces geometry type: Geometry, which does not process well
mort_2016 <- st_read(dsn = "data/forest_service_mortality/ADS2016.gdb",
                     layer = "ADS16",
                     type = 6) # See sf vignette #3. Using default 0 produces geometry type: Geometry, which does not process well
mort_2017 <- st_read(dsn = "data/forest_service_mortality/ADS2017.gdb",
                     layer = "ADS17",
                     type = 6) # See sf vignette #3. Using default 0 produces geometry type: Geometry, which does not process well

# Reproject
mort_2015 <- st_transform(mort_2015, crs = proj_longlat)
mort_2016 <- st_transform(mort_2016, crs = proj_longlat)
mort_2017 <- st_transform(mort_2017, crs = proj_longlat)

# Subset by SS
ss_mort_2015 <- st_crop(mort_2015, ss_border)
ss_mort_2016 <- st_crop(mort_2016, ss_border)
ss_mort_2017 <- st_crop(mort_2017, ss_border)


# Rasterize data

# Single layer
#happy <- rasterize(ss_mort_2015, ss_border_rast, field = "SEVERITY1")
# Multiple layer (this doesn't work)
#happy <- rasterize(ss_mort_2015, ss_border_rast, field = c("SEVERITY1","SEVERITY2"))



# Import cumulative data (note: these are old products, 2010 and prior)
st_layers(dsn = "data/forest_service_mortality/cumulative_by_tree_per_acre")
mort_cum_tpa <- st_read(dsn = "data/forest_service_mortality/cumulative_by_tree_per_acre",
                        layer = "final04_14tpa",
                        type = 0)
mort_cum_tpa <- st_transform(mort_cum_tpa, crs = proj_longlat)
ss_mort_cum_tpa <- st_crop(mort_cum_tpa, ss_border)

# Import data
st_layers(dsn = "data/forest_service_mortality/cumulative_by_year")
mort_cum_year <- st_read(dsn = "data/forest_service_mortality/cumulative_by_year",
                         layer = "04_14yr",
                         type = 0)
mort_cum_year <- st_transform(mort_cum_year, crs = proj_longlat)
ss_mort_cum_year <- st_crop(mort_cum_year, ss_border)

# ---------------------------------------------------------------------






