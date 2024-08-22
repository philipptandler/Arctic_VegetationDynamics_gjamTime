#' This Scirpt prepares the ydata and xdata and fits the gjamTime function
#' I run the 'crop' version with subset = FALSE on my laptop (32 GB RAM) without
#' and problems. 
#' The 'full' version I run on the computing cluster Euler at ETH Zürich with
#' 128 GB memory.Beware, the 'full' version with subset = FASLE overflows the
#' memory even on the comuptin cluster 


#set up environment:
library(here)
setwd(here::here())
source("Scripts/gjamTime/setup_gjamTime.R")


## general setup ####

## define version

vlist <- list(
  vers = "crop", # "full" or "crop"
  subset = TRUE, #recommended TRUE: FALSE might crash due to memory overflow
  subFact = 4, # for subs
  subSeed = 9696 # for subs
)

# validates input and reads system variables if called from console
vlist <- updateArgs(vlist, sysArgs)

## define the variables here
callName <- "test_someVars"

xvars <- list(
  topography = c("elev", "slope", "cosasp", "tpi"),
  y = FALSE, # to get latitude
  x = FALSE, # to get longitude
  climate = c(),
  soil = c()
)
yvars <- list(
  vegetation = c("sh", "cf", "hb", "lc")
)

periods <- c("1984-1990",
             "1991-1996")

# makes sure to have a valid input, initializes and prints call
call <- assert_gjamCall(vlist, xvars, yvars, periods, callName)

## prepare the geodata to load  ####
# if call$subset: writes subset of data/gjamTime_data to data/gjamTime_tmpSubset
cat("prepare geodata:\n")
prepare_geodata(call)


## get xdata ####
cat("loading xdata: \n")
call$xdata <- get_geodata(call, which = "xdata",
                          dropgroup = FALSE, dropperiod = FALSE)


## get ydata ####
cat("loading ydata: \n")
call$ydata <- get_geodata(call, which = "ydata",
                          dropgroup = TRUE, dropperiod = TRUE)


## fit gjamTime ####
cat("fitting data in gjam: \n")
output_call <- fit_gjamTime(setup = call,
                            termB = termB,
                            termR = termR,
                            termA = termA,
                            saveOutput = TRUE,
                            showPlot = TRUE)
cat("fitting completed. \n")

