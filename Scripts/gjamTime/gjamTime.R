# This Scirpt prepares the ydata and xdata and fits the gjamTime function

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
  subSeed = 0 # for subs
)

# validates input and reads system variables if called from console
vlist <- updateArgs(vlist, sysArgs)

## define the variables here

callName <- "fit_gjam"

xvars <- list(
  topography = c("elev", "slope", "aspect", "tpi"),
  y = FALSE, # to get latitude
  x = FALSE, # to get longitude
  climate = c("tass", "tasw", "pr"),
  soil = c("wvol")
)
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

## get xdata ####
cat("loading xdata: \n")
call$xdata <- get_geodata(call$xvars, vlist,
                          dropgroup = FALSE, dropperiod = FALSE)


## get ydata ####
cat("loading ydata: \n")
call$ydata <- get_geodata(call$yvars, vlist,
                          dropgroup = TRUE, dropperiod = TRUE)

## loading Testdata ####
# save(call, file ="testdata_r100_all.Rdata")
# load("testdata_r100_all.Rdata")
# call$name = "test_x(all)_v(all)_p(all)_r100_1"

## fit gjamTime ####
## set model specifications:

cat("fitting data in gjam: \n")
output_call <- fit_gjamTime(setup = call,
                            termB = termB,
                            termR = termR,
                            termA = termA,
                            saveOutput = TRUE,
                            showPlot = TRUE)
cat("fitting completed. \n")

