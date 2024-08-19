# This Scirpt prepares the ydata and xdata and fits the gjamTime function

#set up environment:
library(here)
setwd(here::here())
source("Scripts/gjamTime/setup_gjamTime.R")
# test_lib_path <- "C:/Users/phili/GitHub/testLib"
# detach("package:gjam", unload=TRUE)
# # install.packages("C:/Users/phili/GitHub/gjam_2.6.2.tar.gz", repos = NULL, type = "source", lib = test_lib_path)
# library(gjam, lib.loc = test_lib_path)

# to track failures of .gjam
fixWarning <- TRUE
tracking <- FALSE

## set parameters instructions ####

## define different sets of parameters to fit ####

# periods and version must match between xvars_list and yvars_list !

xvars_short <- list(
  topography = c("elev", "slope", "aspect", "tpi"),
  y = FALSE, # to get latitude
  climate = c("tass", "tasw", "pr"),
  soil = c("wvol")
)
yvars_short <- list(
  vegetation = c("sh", "cf", "hb", "lc")
)
test1 <- list(
  name = "t_local_x(all)_v(all)_p(all)_r100_1",
  version = "crop",
  periods = c("1984-1990",
              "1991-1996",
              "1997-2002",
              "2003-2008",
              "2009-2014",
              "2015-2020"),
  xvars = xvars_short,
  yvars = yvars_short
)


# makes sure to have a valid input, initializes and prints call
test1 <- assert_gjamCall(test1)

## get xdata ####
cat("loading xdata: \n")
test1$xdata <- get_geodata(test1$xvars, dropgroup = FALSE, dropperiod = FALSE)


## get ydata ####
# cat("loading ydata: \n")
# test1$ydata <- get_geodata(test1$yvars, dropgroup = TRUE, dropperiod = TRUE)

## loading Testdata ####
# save(test1, file ="testdata_r100_all.Rdata")
# cat("done.\n")
load("testdata.Rdata")
test1$name = "test_x(all)_v(all)_p(all)_r100_1"

## fit gjamTime ####
## set model specifications:
termB <- FALSE    # include immigration/emigration term XB
termR <- TRUE     # include DI population growth term VL
termA <- TRUE    # include DD spp interaction term UA


cat("fitting data in gjam: \n")
if(fixWarning){redirect_gjam()}
if(tracking){start_track_gjam()}
output_test1 <- fit_gjamTime(setup = test1,
                            termB = termB,
                            termR = termR,
                            termA = termA,
                            saveOutput = TRUE,
                            showPlot = TRUE)
if(tracking || fixWarning){stop_redirect_gjam()}

cat("fitting completed. \n")

