library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/eigenvalues_geospatial.R")

# folder <- "probe1_base"
folder <- "test_higherorder1_alphaNeg1"

type = "jacobian_" # match file name !jacobian_... and jacobinaInv_ may exist

# assert correct order
files <- list.files(file.path(path_analysis, folder), pattern = type,
                    full.names = TRUE)

## call eigenvalues_geospatial()
lamda <- eigenvalues_geospatial(jacobian_list = files, n_chunks = 5)
