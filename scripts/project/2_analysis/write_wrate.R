library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/wrate_geospatial.R")

folder <- "test_higherorder1_alphaNeg1"
type = "w_obs" # match file name

# assert correct order
files <- list.files(file.path(path_analysis, folder), pattern = type,
                    full.names = TRUE)

## call fixpt_geospatial()
wobs <- wrate_geospatial(files, rate = TRUE, linear_model = TRUE, datatype = "INT2S")
