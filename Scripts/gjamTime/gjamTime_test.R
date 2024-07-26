# This Scirpt prepares the ydata and xdata and fits the gjamTime function

#set up environment:
library(here)
setwd(here::here())
source("Scripts/gjamTime/setup_gjamTime.R")

## set parameters instructions ####

## define different sets of parameters to fit ####

# periods and version must match between xvars_list and yvars_list !

xvars_short <- list(
  topography = c("elev", "tpi", "slope", "aspect"),
  y = FALSE, # to get latitude
  climate = c("tass", "pr")
)
yvars_short <- list(
  vegetation = c("sh", "cf", "hb", "lc")
)
test1 <- list(
  name = "test1",
  version = "crop",
  periods = c("1984-1990","1991-1996"),
  xvars = xvars_short,
  yvars = yvars_short
)


test1 <- assert_gjamCall(test1)


# set model specifications:
termB <- FALSE    # include immigration/emigration term XB
termR <- TRUE     # include DI population growth term VL
termA <- TRUE    # include DD spp interaction term UA

## get xdata ####

# choose variable set
test1$xdata <- get_geodata(test1$xvars, dropgroup = FALSE, dropperiod = FALSE)


## get ydata ####

# choose variable set
test1$ydata <- get_geodata(test1$yvars, dropgroup = TRUE, dropperiod = TRUE)


## fit gjamTime ####

output_test1 <- fit_gjamTime(setup = test1,
                            termB = termB,
                            termR = termR,
                            termA = termA,
                            saveOutput = TRUE,
                            showPlot = TRUE)

