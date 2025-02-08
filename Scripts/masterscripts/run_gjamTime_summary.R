library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/1_gjamTime/gjamTime_summary.R")

## read system Arguments and set call
sysArgs <- commandArgs(trailingOnly = TRUE)

if(length(sysArgs) > 0){
  arg <- sysArgs[1]
}
if(!exists("arg")){
  stop("Missing Argument: calling script")
}

## call gjamTime_summary
gjamTime_summary(arg)