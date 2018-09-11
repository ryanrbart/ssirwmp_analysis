# Data import and processsing

# All data is reprojected (if necessary) to lat/long


source("R/1.1_setup_proj_boundaries.R")
source("R/1.2_setup_temp_precip.R")

# ---------------------------------------------------------------------
# Import Envision Spatial Data


idu_kings <- st_read(dsn = "data/EnvisionKingsIDU_Streams/", layer = "IDU_3")
streams_kings <- st_read(dsn = "data/EnvisionKingsIDU_Streams/", layer = "Streams")

idu_kings <- st_transform(idu_kings, crs = proj_longlat)
streams_kings <- st_transform(streams_kings, crs = proj_longlat)

ls(idu_kings)
ls(streams_kings)

# -----
# Stream Network Figure
x <- ggplot() +
  geom_sf(data=streams_kings, aes(size=PlusFlo_12), color="blue") +
  geom_sf(data=kings_border, fill=NA, col="black") +
  scale_size_continuous(labels = function(x)round(x/35.3),
                        name = expression("Mean Annual\nStreamflow ("*m^3*"/s)"),
                        # breaks = c(.3,2),
                        range = c(0.2,2.5)) +
  labs(title="Kings River Network", x="Longitude",y="Latitude", size=0.5) +
  #theme_classic(base_size =12) +
  theme(legend.position="none") +
  NULL
ggsave("output/map_envision_stream_network.jpg",plot = x, width = 4.5, height = 3.5)


# -----
# Distributed Precipitation 

# Rasterize
r <- raster(ncol=75, nrow=43)
extent(r) <- extent(kings_e)
r <- rasterize(idu_kings, r, 'PRECIP_YR')
r_tib_p <- r %>% 
  rasterToPoints() %>% 
  as_tibble()

x <- ggplot() +
  geom_raster(data=r_tib_p, aes(x=x,y=y,fill=layer)) +
  #geom_sf(data=kings_border, fill=NA, col="black") +
  scale_fill_continuous(low="red", high="blue", name="Precipitation\n(mm)") +
  #scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  #scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  labs(title="Mean Annual Precipitation", x="Longitude",y="Latitude", size=0.5) +
  theme_classic(base_size =12) +
  NULL
ggsave("output/map_envision_precipitation.jpg",plot = x, width = 6, height = 3.5)


# -----
# Distributed ET 

# Rasterize
r <- raster(ncol=75, nrow=43)
extent(r) <- extent(kings_e)
r <- rasterize(idu_kings, r, 'ET_yr')
r_tib_et <- r %>% 
  rasterToPoints() %>% 
  as_tibble()

x <- ggplot() +
  geom_raster(data=r_tib_et, aes(x=x,y=y,fill=layer)) +
  #geom_sf(data=kings_border, fill=NA, col="black") +
  scale_fill_continuous(low="red", high="blue", name="ET (mm)") +
  #scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  #scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  labs(title="Mean Annual Evapotranspiration", x="Longitude",y="Latitude", size=0.5) +
  theme_classic(base_size =12) +
  NULL
ggsave("output/map_envision_et.jpg",plot = x, width = 6, height = 3.5)


# -----
# Distributed Runoff

# Rasterize
r <- raster(ncol=75, nrow=43)
extent(r) <- extent(kings_e)
r <- rasterize(idu_kings, r, 'Runoff_yr')
r_tib_r <- r %>% 
  rasterToPoints() %>% 
  as_tibble()

x <- ggplot() +
  geom_raster(data=r_tib_r, aes(x=x,y=y,fill=layer)) +
  #geom_sf(data=kings_border, fill=NA, col="black") +
  scale_fill_continuous(low="red", high="blue", name="Runoff (mm)") +
  #scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  #scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  labs(title="Mean Annual Runoff", x="Longitude",y="Latitude", size=0.5) +
  theme_classic(base_size =12) +
  NULL
ggsave("output/map_envision_runoff.jpg",plot = x, width = 6, height = 3.5)


# -----
# Distributed Snowpack

# Rasterize
r <- raster(ncol=75, nrow=43)
extent(r) <- extent(kings_e)
r <- rasterize(idu_kings, r, 'SNOW_APR10')
r_tib_s <- r %>% 
  rasterToPoints() %>% 
  as_tibble()

x <- ggplot() +
  geom_raster(data=r_tib_s, aes(x=x,y=y,fill=layer)) +
  #geom_sf(data=kings_border, fill=NA, col="black") +
  scale_fill_continuous(low="red", high="blue", name="Snowpack\n(mm)") +
  #scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  #scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  labs(title="Mean April 1 Snowpack", x="Longitude",y="Latitude", size=0.5) +
  theme_classic(base_size =12) +
  NULL
ggsave("output/map_envision_snow.jpg",plot = x, width = 6, height = 3.5)


# -----
# Distributed Misc

# Rasterize
r <- raster(ncol=75, nrow=43)
extent(r) <- extent(kings_e)
r <- rasterize(idu_kings, r, 'Storage_yr')
r_tib <- r %>% 
  rasterToPoints() %>% 
  as_tibble()

x <- ggplot() +
  geom_raster(data=r_tib, aes(x=x,y=y,fill=layer)) +
  #geom_sf(data=kings_border, fill=NA, col="black") +
  scale_fill_continuous(low="red", high="blue", name="?? (mm)") +
  #scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  #scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  labs(title="Mean Annual ??", x="Longitude",y="Latitude", size=0.5) +
  theme_classic(base_size =12) +
  NULL
ggsave("output/map_envision_misc.jpg",plot = x, width = 6, height = 3.5)





# ---------------------------------------------------------------------
# Import Envision Time-Series Data

e_date_seq <- seq(ymd("1997-1-1"), ymd("2000-12-31"), by='days')
e_date_seq <- e_date_seq[e_date_seq != ymd("2000-2-29")]

# ----------
# Streamflow
pine_flat_inflow <- readr::read_csv("data/envision_timeseries/Pine_Flat_Inflow_HBV_Calibration_Run0.csv")
pine_flat_inflow2 <- pine_flat_inflow %>% 
  bind_cols(date = e_date_seq,.) %>% 
  dplyr::rename(mod = `Pine Flat Inflow`, obs = `Obs:Pine Flat Inflow`) %>% 
  tidyr::gather(run, flow, mod, obs) %>% 
  dplyr::select(-Time)


x <- ggplot(pine_flat_inflow2) +
  geom_line(aes(x=date, y=flow, color=run), size=.5) +
  labs(title = "Hydrograph for Kings River", x = "Date", y = expression("Streamflow ("*m^3*"s)")) +
  scale_color_manual(values = c("red", "blue"),
                        name="Streamflow", 
                        labels = c("Modeled","Observed")) +
  theme_bw(base_size =15) +
  NULL
ggsave("output/ts_streamflow.jpg",plot = x, width = 6, height = 3.5)


# ----------
# Snowpack
bcb <- readr::read_csv("data/envision_timeseries/BCB_(mm)_HBV_Calibration_Run0.csv")
bim <- readr::read_csv("data/envision_timeseries/BIM_(mm)_HBV_Calibration_Run0.csv")
bsh <- readr::read_csv("data/envision_timeseries/BSH_(mm)_HBV_Calibration_Run0.csv")
crl <- readr::read_csv("data/envision_timeseries/CRL_(mm)_HBV_Calibration_Run0.csv")
mtm <- readr::read_csv("data/envision_timeseries/MTM_(mm)_HBV_Calibration_Run0.csv")
stl <- readr::read_csv("data/envision_timeseries/STL_(mm)_HBV_Calibration_Run0.csv")
ubc <- readr::read_csv("data/envision_timeseries/UBC_(mm)_HBV_Calibration_Run0.csv")
wwc <- readr::read_csv("data/envision_timeseries/WWC_(mm)_HBV_Calibration_Run0.csv")


# BSH - Manually patch gaps in observed data
#View(bsh)
bsh$`Obs:BSH (mm)`[60] = 890
bsh$`Obs:BSH (mm)`[75] <- 930
bsh$`Obs:BSH (mm)`[78] <- 906
bsh$`Obs:BSH (mm)`[79] <- 906
bsh$`Obs:BSH (mm)`[80] <- 906
bsh$`Obs:BSH (mm)`[81] <- 906
bsh$`Obs:BSH (mm)`[110:114] <- 1200
bsh$`Obs:BSH (mm)`[115:120] <- 1300
bsh$`Obs:BSH (mm)`[122:125] <- 1250
bsh$`Obs:BSH (mm)`[136:139] <- 1180
bsh$`Obs:BSH (mm)`[175] <- 850
bsh$`Obs:BSH (mm)`[425] <- 393
bsh$`Obs:BSH (mm)`[486] <- 568
bsh$`Obs:BSH (mm)`[501] <- 470
bsh$`Obs:BSH (mm)`[503] <- 440
bsh$`Obs:BSH (mm)`[505] <- 410
bsh$`Obs:BSH (mm)`[507:508] <- 370
bsh$`Obs:BSH (mm)`[519] <- 160
bsh$`Obs:BSH (mm)`[748] <- 100
bsh$`Obs:BSH (mm)`[851] <- 740
bsh$`Obs:BSH (mm)`[878:879] <- 300
bsh$`Obs:BSH (mm)`[1155] <- 410
bsh$`Obs:BSH (mm)`[1173] <- 522
bsh$`Obs:BSH (mm)`[1195] <- 580
bsh$`Obs:BSH (mm)`[1212:1214] <- 600
bsh$`Obs:BSH (mm)`[1216] <- 550


happy <- bsh %>% 
  bind_cols(date = e_date_seq, .) %>% 
  dplyr::rename(mod = `BSH (mm)`, obs = `Obs:BSH (mm)`) %>% 
  tidyr::gather(run, depth, mod, obs) %>% 
  dplyr::select(-Time)


x <- ggplot(happy) +
  geom_line(aes(x=date, y=depth, color=run), size=.8) +
  labs(title = "Snowpack - Bishop Pass", x = "Date", y = "Snowpack (mm)") +
  scale_color_manual(values = c("red", "blue"),
                     name="Snowpack", 
                     labels = c("Modeled","Observed")) +
  theme_bw(base_size =15) +
  NULL
ggsave("output/ts_bsh.jpg",plot = x, width = 6, height = 3.5)



# ----
# BIM - Manually patch gaps in observed data
#View(dplyr::select(bim,-Time))
bim$`Obs:BIM (mm)`[15] = 672
bim$`Obs:BIM (mm)`[77:80] = 1850
bim$`Obs:BIM (mm)`[419] = 607
bim$`Obs:BIM (mm)`[444] = 633
bim$`Obs:BIM (mm)`[452] = 711
bim$`Obs:BIM (mm)`[488:489] = 280
bim$`Obs:BIM (mm)`[490] = 240
bim$`Obs:BIM (mm)`[709] = 180
bim$`Obs:BIM (mm)`[711:715] = 235
bim$`Obs:BIM (mm)`[748:749] = 443
bim$`Obs:BIM (mm)`[754] = 550
bim$`Obs:BIM (mm)`[825:831] = 1250
bim$`Obs:BIM (mm)`[1065:1070] = 300
bim$`Obs:BIM (mm)`[1083:1086] = 320
bim$`Obs:BIM (mm)`[1087:1102] = 400
bim$`Obs:BIM (mm)`[1105] = 470


happy <- bim %>% 
  bind_cols(date = e_date_seq, .) %>% 
  dplyr::rename(mod = `BIM (mm)`, obs = `Obs:BIM (mm)`) %>% 
  tidyr::gather(run, depth, mod, obs) %>% 
  dplyr::select(-Time)


x <- ggplot(happy) +
  geom_line(aes(x=date, y=depth, color=run), size=.8) +
  labs(title = "Snowpack - Big Meadow", x = "Date", y = "Snowpack (mm)") +
  scale_color_manual(values = c("red", "blue"),
                     name="Snowpack", 
                     labels = c("Modeled","Observed")) +
  theme_bw(base_size =15) +
  NULL
ggsave("output/ts_bim.jpg",plot = x, width = 6, height = 3.5)


