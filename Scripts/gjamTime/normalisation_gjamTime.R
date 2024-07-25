
#' This script writes the parameters used to normalize the geospatial rasterdate in gjamtime. 
#' For each varible in masterlist in setup_gjamTime.R

# load dataframes and write mean and sd

#set up environment:
library(here)
setwd(here::here())
source("Scripts/gjamTime/setup_gjamTime.R")

ref_period <- c("2015-2020")

varlist <- list(
  # topography = c("elev", "slope", "aspect", "tpi"),
  # x = TRUE,
  # y = TRUE,
  # climate = c("tas", "tasw", "tass", "pr", "prw", "prs"),
  wildfire = c(),
  # soil = c("wvol", "wvol05", "wvol15", "wvol30", "wvol60"),
  periods = ref_period,
  version = c("r100")
)
varlist <- assert_geodata(varlist)

alldata <- get_geodata(varlist, dropgroup = TRUE, dropperiod = TRUE)

ref_list <- list()
ref_list_name <- "normalisation.rds"
path_ref_list <- "Scripts/gjamTime/"
path_ref_list_fullname <- paste0(path_ref_list, ref_list_name)
if(file.exists(path_ref_list_fullname)){ref_list <- readRDS(path_ref_list_fullname)}

# for all columns
for (col in colnames(alldata)){
  colvalues <- alldata[[col]]
  ref_list[[col]]$mean <- mean(colvalues)
  ref_list[[col]]$sd <- sd(colvalues)
}

# save list as rdata
saveRDS(ref_list, path_ref_list_fullname)
