# This Scirpt prepares the ydata and xdata and fits the gjamTime function

#set up environment:
library(here)
setwd(here::here())
source("Scripts/gjamTime/setup_gjamTime.R")

## set parameters instructions ####

## define different sets of parameters to fit ####

# periods and version must match between xvars_list and yvars_list !

xvars_all <- list(
  topography = c("elev", "slope", "aspect", "tpi"),
  y = FALSE, # to get latitude
  climate = c("tass", "tasw", "pr"),
  soil = c("wvol")
)
yvars_all <- list(
  vegetation = c("sh", "cf", "hb", "lc")
)
try_big <- list(
  name = "trybig",
  version = "full",
  periods = c("1984-1990",
              "1991-1996",
              "1997-2002",
              "2003-2008",
              "2009-2014",
              "2015-2020"),
  xvars = xvars_all,
  yvars = yvars_all
)


# makes sure to have a valid input, initializes and prints call
try_big <- assert_gjamCall(try_big)


# set model specifications:
termB <- FALSE    # include immigration/emigration term XB
termR <- TRUE     # include DI population growth term VL
termA <- TRUE    # include DD spp interaction term UA

## get xdata ####
cat("loading xdata: \n")
try_big$xdata <- get_geodata(try_big$xvars, dropgroup = FALSE, dropperiod = FALSE)


## get ydata ####
cat("loading ydata: \n")
try_big$ydata <- get_geodata(try_big$yvars, dropgroup = TRUE, dropperiod = TRUE)


## fit gjamTime ####
cat("fitting data in gjam: \n")
output_try_big <- fit_gjamTime(setup = try_big,
                            termB = termB,
                            termR = termR,
                            termA = termA,
                            saveOutput = TRUE,
                            showPlot = TRUE)

