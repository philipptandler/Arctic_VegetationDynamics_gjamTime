# This Scirpt prepares the ydata and xdata and fits the gjamTime function

#set up environment:
library(here)
setwd(here::here())
source("Scripts/gjamTime/setup_gjamTime.R")

## set parameters instructions ####

## define different sets of parameters to fit ####

# periods and version must match between xvars_list and yvars_list !

xvars_short <- list(
  topography = c("tpi", "elev"),
  y = FALSE, # to get latitude
  climate = c("tass", "pr")
)
yvars_short <- list(
  vegetation = c("sh", "cf")
)
test1 <- list(
  name = "test1",
  version = "crop",
  periods = c("1984-1990","1991-1996"),
  xvars = xvars_short,
  yvars = yvars_short
)
# to track failures of .gjam
tracking <- TRUE

# set model specifications:
termB <- FALSE    # include immigration/emigration term XB
termR <- TRUE     # include DI population growth term VL
termA <- TRUE    # include DD spp interaction term UA

# makes sure to have a valid input, initializes and prints call
test1 <- assert_gjamCall(test1)

## get xdata ####
cat("loading xdata: \n")
test1$xdata <- get_geodata(test1$xvars, dropgroup = FALSE, dropperiod = FALSE)


## get ydata ####
cat("loading ydata: \n")
test1$ydata <- get_geodata(test1$yvars, dropgroup = TRUE, dropperiod = TRUE)

## loading Testdata ####
load("testdata.Rdata")

## fit gjamTime ####
cat("fitting data in gjam: \n")
if(tracking){start_track_gjam()}
output_test1 <- fit_gjamTime(setup = test1,
                            termB = termB,
                            termR = termR,
                            termA = termA,
                            saveOutput = TRUE,
                            showPlot = TRUE)
if(tracking){stop_track_gjam()}

cat("fitting completed. \n")

