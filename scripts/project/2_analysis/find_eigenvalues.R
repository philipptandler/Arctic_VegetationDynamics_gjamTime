library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/eigenvalues_geospatial.R")

# arg <- "probe1_base"
arg <- "test_higherorder1_alphaNeg1"

## call fixpt_geospatial()
lamda <- eigenvalues_geospatial(argument = arg, n_chunks = 5)
