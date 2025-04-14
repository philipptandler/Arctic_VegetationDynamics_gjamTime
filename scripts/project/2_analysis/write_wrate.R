library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/wrate_geospatial.R")

folder <- "probe1_base"
type = "w_star" # match file name

# assert correct order
files <- list.files(file.path(path_analysis, folder), pattern = type,
                    full.names = TRUE)

## call fixpt_geospatial()
wobs <- wrate_geospatial(files, rate = FALSE, linear_model = TRUE, datatype = "INT2S")
