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
  vers = "full", # "full" or "crop"
  subset = TRUE, #recommended TRUE: FALSE might crash due to memory overflow
  subFact = 100, # for subs
  subSeed = 9696 # for subs
)

# validates input and reads system variables if called from console
vlist <- updateArgs(vlist, sysArgs)

## define the variables here
callName <- "gjam_official"

xvars <- list(
  topography = c("elev", "slope", "cosasp", "tpi"),
  y = FALSE, # to get latitude
  x = FALSE, # to get longitude
  climate = c("tasw", "tass","prw", "prs"),
  soil = c("scwd"),
  interaction = c("elev:slope", "elev:cosasp", "elev:tpi", "slope:tpi", 
                  "elev:tass", "tass:prs", "tass:prw",
                  "scwd:elev", "scwd:prs", "scwd:prw") 
)
# must match variables above, no lat/lon, no quadratic samevar:samevar
yvars <- list(
  vegetation = c("sh", "cf", "hb", "lc")
)

periods <- c("1984-1990",
             "1991-1996",
             "1997-2002",
             "2003-2008",
             "2009-2014",
             "2015-2020")

# makes sure to have a valid input, initializes and prints call
call <- assert_gjamCall(vlist, xvars, yvars, periods, callName)

## prepare the geodata to load  ####
# if call$subset: writes subset of data/gjamTime_data to data/gjamTime_tmpSubset
cat("prepare geodata:\n")
time1 <- start_time()
prepare_geodata(call)
cat("done. (", end_time(time1), ")\n")

## get xdata ####
cat("loading xdata: \n")
time2 <- start_time()
call$xdata <- get_geodata(call, which = "xdata",
                          dropgroup = FALSE, dropperiod = FALSE)
cat("done. (", end_time(time2), ")\n")

## get ydata ####
cat("loading ydata: \n")
time3 <- start_time()
call$ydata <- get_geodata(call, which = "ydata",
                          dropgroup = TRUE, dropperiod = TRUE)
cat("done. (", end_time(time3), ")\n")


## fit gjamTime ####
cat("fitting data in gjam: \n")
time4 <- start_time()
output_call <- fit_gjamTime(setup = call,
                            termB = termB,
                            termR = termR,
                            termA = termA,
                            normalize = "ref",
                            saveOutput = TRUE)
cat("done. (", end_time(time4), ")\n")
cat("fitting completed. Total:", end_time(time1), "\n\n\n\n\n")
