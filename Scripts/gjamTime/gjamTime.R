# This Scirpt prepares the ydata and xdata and fits the gjamTime function

#set up environment:
setwd("C:/Users/phili/Documents/Dateien_Philipp/EnvironmentalSciencesMSc/MasterThesis/")
source("Scripts/gjamTime/setup_gjamTime.R")

## set parameters instructions ####

## define different sets of parameters to fit ####

# periods and version must match between xvars_list and yvars_list !

xvars_short <- list(
  topography = c(),
  y = FALSE, # to get latitude
  climate = c("tass", "prs")
)

yvars_short <- list(
  vegetation = c("cf", "sh")
)

try1 <- list(
  name = "try1",
  version = "crop",
  periods = c("1984-1990","1991-1996"),
  xvars = xvars_short,
  yvars = yvars_short
)

try1 <- assert_gjamCall(try1)


# set model specifications:
termB <- FALSE    # include immigration/emigration term XB
termR <- TRUE     # include DI population growth term VL
termA <- TRUE    # include DD spp interaction term UA

## get xdata ####

# choose variable set
try1$xdata <- get_geodata(try1$xvars, dropgroup = FALSE, dropperiod = FALSE)


## get ydata ####

# choose variable set
try1$ydata <- get_geodata(try1$yvars, dropgroup = TRUE, dropperiod = TRUE)


## fit gjamTime ####

output_try1 <- fit_gjamTime(setup = try1,
                            termB = termB,
                            termR = termR,
                            termA = termA,
                            saveOutput = TRUE,
                            showPlot = TRUE)

