# Mortality Analysis
# 

source("R/0_utilities.R")


# ---------------------------------------------------------------------
# Code for generating PET via Hamon 1961 method

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
pet_hamon_month <- function(x, daylight_hours){
  pet_stack <- calc(x, fun = function(x){pet_hamon(x,daylight_hours)})
  days_per_month <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  pet_stack <- calc(pet_stack, fun = function(x){x*days_per_month})
  return(pet_stack)
}


# ---------------------------------------------------------------------
# 

# Precip 2015
precip_2015 <- calc(prism_precip_stack, sum)
names(precip_2015) <- "precip_2015"

# PET 2015
pet_2015_stack <- pet_hamon_month(x=prism_temp_stack,daylight_hours=daylight_hours)
pet_2015 <- calc(pet_2015_stack, sum)
names(pet_2015) <- "pet_2015"

# PET 2085 (RCP4.5)
pet_2085_stack <- pet_hamon_month(x=(prism_temp_stack + 3.25), daylight_hours=daylight_hours)
pet_2085 <- calc(pet_2085_stack, sum)
names(pet_2085) <- "pet_2085"

# CWD 2015
cwd_2015 <- precip_2015 - pet_2015
names(cwd_2015) <- "cwd_2015"

# CWD 2085
cwd_2085 <- precip_2015 - pet_2085
names(cwd_2085) <- "cwd_2085"


#plot(cwd)
#plot(y_15[[4]])

happy_tib <- as_tibble(values(stack(y_15, precip_2015, pet_2015, pet_2085, cwd_2015, cwd_2085)))

happy_tib <- happy_tib %>% 
  dplyr::filter(mort.tph<5)

# -----
# Young 'equivalent' plot

#test <- function(x)(x*.063+34)
test <- function(x)(0.00004*((x+900)^2))

ggplot(data = happy_tib,aes(x=cwd_2015,y=live.bah)) +
  geom_point(aes(color=mort.tph)) +
  scale_color_gradient(low="yellow", high="black") +
  xlim(-850,250) +
  #ylim(0,60) +
  stat_function(fun=test)

ggplot(data = happy_tib,aes(x=cwd_2085,y=live.bah)) +
  geom_point(aes(color=mort.tph)) +
  scale_color_gradient(low="yellow", high="black") +
  xlim(-850,250) +
  ylim(0,60) +
  stat_function(fun=test)



sum(happy_tib$live.bah > test(happy_tib$cwd_2015), na.rm = TRUE)
sum(happy_tib$live.bah > test(happy_tib$cwd_2085), na.rm = TRUE)








# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Time-series (processing)

head(ss_mort_2017)

unique(ss_mort_2017$RPT_YR)



# ---------------------------------------------------------------------
# Plotting

ggplot() +
  geom_sf(data=ss_border, fill=NA, col="black") +
  geom_sf(data=ss_mort_2017, fill="red", col="black")


ggplot() +
  geom_sf(data=mort_2016, fill="red", col="black") 





