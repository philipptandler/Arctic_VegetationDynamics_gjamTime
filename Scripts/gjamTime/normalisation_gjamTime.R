
#' This script writes the parameters used to normalize the geospatial rasterdate in gjamtime. 
#' For each varible in masterlist in setup_gjamTime.R

# load dataframes and write mean and sd

#set up environment:
library(here)
setwd(here::here())
source("Scripts/gjamTime/setup_gjamTime.R")

ref_period <- c("2015-2020")

varlist <- list(
  topography = c("elev", "slope", "aspect", "tpi"),
  x = TRUE,
  y = TRUE,
  climate = c("tas", "tasw", "tass", "pr", "prw", "prs"),
  wildfire = c(),
  soil = c("wvol", "wvol05", "wvol15", "wvol30", "wvol60"),
  periods = ref_period,
  version = c("full")
)

alldata <- get_geodata(varlist, dropgroup = TRUE, dropperiod = TRUE)

ref_list <- list()

# for all columns
# add ro ref list ref_list$var$mean and ref_list$var$sd
# save list as rdata

# some change
