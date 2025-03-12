
#' This script writes the parameters used to normalize the geospatial rasterdate in gjamtime. 
#' For each varible in masterlist in setup_gjamTime.R

# load dataframes and write mean and sd

#set up environment:
library(here)
setwd(here::here())
source("Scripts/gjamTime/setup_gjamTime.R")

refPeriods <- c("1984-1990",
                "1991-1996",
                "1997-2002",
                "2003-2008",
                "2009-2014",
                "2015-2020",
                "const")

varlist <- list(
  topography = c("elev", "slope", "cosasp", "tpi"),
  x = TRUE,
  y = TRUE,
  climate = c("tas", "tasw", "tass", "pr", "prw", "prs"),
  wildfire = c(),
  soil = c("scwd"),
  periods = refPeriods,
  version = c("full")
)


varlist <- assert_geodata(varlist)
varvec <- getvars(varlist)

ref_list <- list()
ref_list_name <- "normalization.rds"
path_ref_list <- "Scripts/gjamTime/"
path_ref_list_fullname <- paste0(path_ref_list, ref_list_name)
if(file.exists(path_ref_list_fullname)){ref_list <- readRDS(path_ref_list_fullname)}
refPeriodPattern <- paste(refPeriods, collapse = "|")

# for all columns
for (var in varvec){
  if(var != "lat" && var != "lon"){
    pattern <- paste0(".*_(", refPeriodPattern, ")_", var, "_full\\.tif$")
    files <- list.files(path = path_vars, pattern = pattern, full.names = TRUE)
    raster <- rast(files)
    ref_list[[var]]$mean <- mean(values(raster), na.rm = TRUE)
    ref_list[[var]]$sd <- sd(values(raster), na.rm = TRUE)
  }
  if(var == "lat"){
    extent <- ext(rast(file.path(path_masks, "study_region_mask.tif")))
    ref_list[[var]]$mean <- mean(c(extent$ymin, extent$ymax))
    ref_list[[var]]$sd <- sd(c(extent$ymin, extent$ymax))
  }
  if(var == "lon"){
    extent <- ext(rast(file.path(path_masks, "study_region_mask.tif")))
    ref_list[[var]]$mean <- mean(c(extent$xmin, extent$xmax))
    ref_list[[var]]$sd <- sd(c(extent$xmin, extent$xmax))
  }
}

# for all interactions
for (var1 in varvec){
  for (var2 in varvec){
    raster1 <- NULL
    raster2 <- NULL
    if(var1 != "lat" && var1 != "lon" && var2 != "lat" && var2 != "lon"){
      pattern <- paste0(".*_(", refPeriodPattern, ")_", var1, "_full\\.tif$")
      files <- list.files(path = path_vars, pattern = pattern, full.names = TRUE)
      raster1 <- rast(files)
      pattern <- paste0(".*_(", refPeriodPattern, ")_", var2, "_full\\.tif$")
      files <- list.files(path = path_vars, pattern = pattern, full.names = TRUE)
      raster2 <- rast(files)
      
      combined <- c(raster1, raster2)
      combined_df <- spatSample(combined, 10000, method="random")
      combined_df <- na.omit(combined_df)
      prod_vec <- combined_df[,1]*combined_df[,2]
      
      ref_list[[paste0(var1, ":", var2)]]$mean <- mean(prod_vec)
      ref_list[[paste0(var1, ":", var2)]]$sd <- sd(prod_vec)
    }
  }
}

# save list as rdata
saveRDS(ref_list, path_ref_list_fullname)
