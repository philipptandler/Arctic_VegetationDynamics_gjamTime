# This Scirpt prepares the ydata and xdata and fits the gjamTime function

#set up environment:
library(here)
setwd(here::here())
source("Scripts/gjamTime/setup_gjamTime.R")

## define different sets of parameters to fit ####
vers <- "r100" #version
seed <- 0

## input parameters  #### IGNORE IN RSTUDIO
args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0) {
  vers <- args[1]
  if(vers != "full" &&
     vers != "r100" &&
     vers != "crop"){stop("Invalid version")}
  if (vers == "r100" && length(args) > 0){
    seed <- as.integer(args[2])
    seed <- seed%%10000
  } else {
    seed <- 0
  }
}

## defined variables

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
  name = paste("call_x()_y()_time()",
               vers, sprintf("%04d", seed), sep = "_"),
  version = vers,
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
call <- assert_gjamCall(call)

## get xdata ####
cat("loading xdata: \n")
call$xdata <- get_geodata(call$xvars, seed = seed,
                          dropgroup = FALSE, dropperiod = FALSE)


## get ydata ####
cat("loading ydata: \n")
call$ydata <- get_geodata(call$yvars, seed = seed,
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

