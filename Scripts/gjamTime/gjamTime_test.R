# This Scirpt prepares the ydata and xdata and fits the gjamTime function

#set up environment:
library(here)
setwd(here::here())
source("Scripts/gjamTime/setup_gjamTime.R")

## define different sets of parameters to fit ####

vlist <- list(
  vers = "full", # "full" or "crop"
  subset = TRUE, #recommended, FALSE might crash due to memory overflow
  subFact = 100, # for subs
  subSeed = 0 # for subs
)

vlist <- updateArgs(vlist, sysArgs)

## define the variables here

xvars <- list(
  topography = c("elev", "slope", "aspect", "tpi"),
  y = FALSE, # to get latitude
  x = FALSE,
  climate = c("tass", "tasw", "pr"),
  soil = c("wvol")
)
yvars <- list(
  vegetation = c("sh", "cf", "hb", "lc")
)
call <- list(
  name = paste("test_allvars",
               vlist$name,
               sprintf(paste0("%04d"), vlist$subSeed),
               sep = "_"),
  version = vlist$vers,
  periods = c("1984-1990",
              "1991-1996",
              "1997-2002",
              "2003-2008",
              "2009-2014",
              "2015-2020"),
  xvars = xvars,
  yvars = yvars
)


# makes sure to have a valid input, initializes and prints call
call <- assert_gjamCall(call, vlist)

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

