# Data import and processsing


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data


# Vector
# Source - http://hydra.ucdavis.edu/resources/gis
KAW_watershed_kml <- "http://hydra.ucdavis.edu/files/hydra/KAW_watersheds.kml"
download.file(KAW_watershed_kml, "data/watershed_kml/KAW_watershed.kml")
KNG_watershed_kml <- "http://hydra.ucdavis.edu/files/hydra/KNG_watersheds.kml"
download.file(KNG_watershed_kml, "data/watershed_kml/KNG_watershed.kml")
KRN_watershed_kml <- "http://hydra.ucdavis.edu/files/hydra/KRN_watersheds.kml"
download.file(KRN_watershed_kml, "data/watershed_kml/KRN_watershed.kml")
SJN_watershed_kml <- "http://hydra.ucdavis.edu/files/hydra/SJN_watersheds.kml"
download.file(SJN_watershed_kml, "data/watershed_kml/SJN_watershed.kml")
TUL_watershed_kml <- "http://hydra.ucdavis.edu/files/hydra/TUL_watersheds.kml"
download.file(TUL_watershed_kml, "data/watershed_kml/TUL_watershed.kml")

KAW_watershed <- st_read(dsn = "data/watershed_kml/KAW_watershed.kml")
KNG_watershed <- st_read(dsn = "data/watershed_kml/KNG_watershed.kml")
KRN_watershed <- st_read(dsn = "data/watershed_kml/KRN_watershed.kml")
SJN_watershed <- st_read(dsn = "data/watershed_kml/SJN_watershed.kml")
TUL_watershed <- st_read(dsn = "data/watershed_kml/TUL_watershed.kml")


# Raster
precip_files <- list.files(path = 'data/precip_california_annual_4.5', pattern='pr_year_', full.names = T)
p_1 <- raster::raster(precip_files[1])

precipStack = raster::stack(precip_files)
#plot(precipStack)

tmax_files <- list.files(path = 'data/temperature_california_annual_4.5', pattern='tasmax_', full.names = T)
tmax_1 <- raster::raster(tmax_files[1])

tmaxStack = raster::stack(tmax_files)
#plot(precipStack)


ssirwmp_border <- st_read(dsn = "data/ssirwmp_gis/", layer = "SSIRWM")
ssirwmp_border <- st_transform(ssirwmp_border, crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
plot(ssirwmp_border, add=TRUE)


# Forest mortality geodatabase (https://www.fs.usda.gov/detail/r5/forest-grasslandhealth/?cid=fsbdev3_046696)

# Check layers
ogrListLayers(dsn = "data/forest_service_mortality_2017/ADS2017.gdb")
mort_2017 <- readOGR(dsn = "data/forest_service_mortality_2017/ADS2017.gdb")
summary(mort_2017)

#plot(mort_2017)


# ---------------------------------------------------------------------
# Plot data

plot(p_1)
#hist(p_1)

plot(KAW_watershed$geometry, add=TRUE)
plot(KNG_watershed$geometry, add=TRUE)
plot(KRN_watershed$geometry, add=TRUE)
plot(SJN_watershed$geometry, add=TRUE)
plot(TUL_watershed$geometry, add=TRUE)


KNG_raster <- fasterize(KNG_watershed, p_1)

precip_KNG <- crop(p_1, KNG_raster, snap="out")
#precip_KNG <- crop(precipStack, extent(KNG_watershed), snap="out")
#plot(precip_KNG)
#plot(KNG_watershed, add=TRUE)

# Dummy raster for mask
dummy <- setValues(precip_KNG, NA)

# Putting NA values in all the raster cells outside the shapefile boundaries 
precip_KNG.masked <- mask(x=precip_KNG, mask=KNG_raster) 

plot(precip_KNG.masked)
plot(KNG_watershed, add=TRUE)


 
# ---------------------------------------------------------------------
# Alternative approach for cropping and masking watershed

# This import function readOGR has to be used for importing vector, not st_read
KNG_watershed <- readOGR(dsn = "data/watershed_kml/KAW_watershed.kml")

# Crop

precip_KNG <- crop(p_1, extent(KNG_watershed), snap="out")
#precip_KNG <- crop(precipStack, extent(KNG_watershed), snap="out")
#plot(precip_KNG)
#plot(KNG_watershed, add=TRUE)

# Dummy raster for mask
dummy <- setValues(precip_KNG, NA)
# Rasterize the catchment boundaries, with NA outside the catchment boundaries 
precip_KNG.r <- rasterize(KNG_watershed, dummy) 

# Putting NA values in all the raster cells outside the shapefile boundaries 
precip_KNG.masked <- mask(x=precip_KNG, mask=precip_KNG.r) 

plot(precip_KNG.masked)
plot(KNG_watershed, add=TRUE)

# Summarize across a watershed

zonal(precip_KNG.masked, precip_KNG.masked, fun='count') 
zonal(precip_KNG.masked, precip_KNG.masked, fun='mean') 
zonal(precip_KNG.masked, precip_KNG.masked, fun='') 

# ---------------------------------------------------------------------
# Alternative approach for cropping and masking watershed - Temp single layer

# This import function readOGR has to be used for importing vector, not st_read
KNG_watershed <- readOGR(dsn = "data/watershed_kml/KAW_watershed.kml")

# Crop

tmax_KNG <- crop(tmax_1, extent(KNG_watershed), snap="out")
#tmax_KNG <- crop(precipStack, extent(KNG_watershed), snap="out")
#plot(tmax_KNG)
#plot(KNG_watershed, add=TRUE)

# Dummy raster for mask
dummy <- setValues(tmax_KNG, NA)
# Rasterize the catchment boundaries, with NA outside the catchment boundaries 
tmax_KNG.r <- rasterize(KNG_watershed, dummy) 

# Putting NA values in all the raster cells outside the shapefile boundaries 
tmax_KNG.masked <- mask(x=tmax_KNG, mask=tmax_KNG.r) 

plot(tmax_KNG.masked)
plot(KNG_watershed, add=TRUE)

# Summarize across a watershed

zonal(tmax_KNG.masked, setValues(tmax_KNG, 1), fun='count')    # Zones are rounded to nearest integer
zonal(tmax_KNG.masked, setValues(tmax_KNG, 1), fun='mean') 


# ---------------------------------------------------------------------
# Alternative approach for cropping and masking watershed - Temp multiple layer

# This import function readOGR has to be used for importing vector, not st_read
KNG_watershed <- readOGR(dsn = "data/watershed_kml/KAW_watershed.kml")

# Crop

tmax_KNG <- crop(tmaxStack, extent(KNG_watershed), snap="out")
#tmax_KNG <- crop(precipStack, extent(KNG_watershed), snap="out")
#plot(tmax_KNG)
#plot(KNG_watershed, add=TRUE)

# Dummy raster for mask
dummy <- setValues(tmax_KNG, NA)
# Rasterize the catchment boundaries, with NA outside the catchment boundaries 
tmax_KNG.r <- rasterize(KNG_watershed, dummy) 

# Putting NA values in all the raster cells outside the shapefile boundaries 
tmax_KNG.masked <- mask(x=tmax_KNG, mask=tmax_KNG.r) 

plot(tmax_KNG.masked)
plot(KNG_watershed, add=TRUE)

# Summarize across a watershed

zonal(tmax_KNG.masked, setValues(tmax_KNG, 1), fun='count')    # Zones are rounded to nearest integer
zonal(tmax_KNG.masked[[1]], setValues(tmax_KNG, 1)[[1]], fun='mean') 

tmp <- setValues(tmax_KNG, 1)[[1]]
calc(tmax_KNG.masked, fun = function(x){ zonal(x, tmp, fun='mean')})


# ----------------
# Examining SSIRWMP datasets

happy <- readOGR(dsn = "data/SSIRWM_gis/", layer = "ADS_Tulare_Kern")
plot(happy)

happy <- readOGR(dsn = "data/SSIRWM_gis/", layer = "Kawea")
plot(happy, add=TRUE, col="gray")

#happy <- readOGR(dsn = "data/SSIRWM_gis/", layer = "Prop1_Funding_Area")
#plot(happy)


happy <- readOGR(dsn = "data/SSIRWM_gis/", layer = "SSIRWM")
plot(happy)

happy <- readOGR(dsn = "data/SSIRWM_gis/", layer = "Streamgage")
plot(happy, add=TRUE, col="blue")

happy <- readOGR(dsn = "data/SSIRWM_gis/", layer = "Tulare_Kern")
plot(happy, add=TRUE, col="red")

happy <- readOGR(dsn = "data/SSIRWM_gis/", layer = "Tule")
plot(happy, add=TRUE, col="blue")

happy <- readOGR(dsn = "data/SSIRWM_gis/", layer = "watersheds")
plot(happy)





# ------------------
# ------------------
# ------------------
# Importing netcdf with ncdf4
# Source: http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html

library(ncdf4)

nc_path <- "data/maca/"
#nc_name <- "macav2livneh_tasmax_CCSM4_r6i1p1_rcp45_2086_2099_CONUS_monthly"
nc_name <- "macav2metdata_tasmax_CCSM4_r6i1p1_rcp45_2096_2099_CONUS_monthly"
ncfname <- paste(nc_path, nc_name, ".nc", sep="")
dname <- "air_temperature"

ncin <- nc_open(ncfname)
print(ncin)

lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt

# get temperature
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

library(chron)
library(lattice)
library(RColorBrewer)

tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))

plot(tmp_array[,,1])

m <- 1
tmp_slice <- tmp_array[,,m]

image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-5,-2.5,0,2.5,5,7.5,10,12.5,15,17.5,20)
levelplot((tmp_slice-273.15) ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))

# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)

# vector of `tmp` values
tmp_vec <- as.vector(tmp_slice)
length(tmp_vec)

# create dataframe and add names
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
head(na.omit(tmp_df01), 10)


# ------------------
# ------------------
# Importing netcdf with raster

#happy <- stack("data/maca/macav2livneh_tasmax_CCSM4_r6i1p1_rcp45_2086_2099_CONUS_monthly.nc")
happy <- stack("data/maca/macav2metdata_tasmax_CCSM4_r6i1p1_rcp45_2096_2099_CONUS_monthly.nc")



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Common raster functions
# From raster package vignette

names(ss_tmax_stack)
crs(ss_tmax_stack)
extent(ss_tmax_stack)
xres(ss_tmax_stack)
yres(ss_tmax_stack)

hasValues(ss_tmax_stack)
values(ss_tmax_stack)
values(ss_tmax_stack[[1]])
getValues(ss_tmax_stack[[1]])

inMemory(ss_tmax_stack) 

nrow(ss_tmax_stack)
ncol(ss_tmax_stack)
ncell(ss_tmax_stack)
rowFromCell(ss_tmax_stack, 1)
colFromCell(ss_tmax_stack, 1)
cellFromRowCol(ss_tmax_stack,2,1)
xyFromCell(ss_tmax_stack, 99)
cellFromXY(ss_tmax_stack, c(-118.9, 37.5))
colFromX(ss_tmax_stack, -118.85)
rowFromY(ss_tmax_stack, 37)

getValues(ss_tmax_stack, 5)
cellFromRowCol(ss_tmax_stack, 2, 5:9) 
extract(ss_tmax_stack, c(2)) 
ss_tmax_stack[2]

hist(ss_tmax_stack,maxpixels=ncell(ss_tmax_stack))
plotRGB(ss_tmax_stack, r=2, g=2, b=2, stretch="lin")

levelplot(ss_tmax_stack)


# -------------
# Example of buffer

# Test of buffer
a <- st_buffer(ss_border, dist = .2)
ggplot() +
  geom_sf(data=a, fill=NA, col="black") +
  geom_sf(data=ss_border, fill=NA, col="black")


# -------------






