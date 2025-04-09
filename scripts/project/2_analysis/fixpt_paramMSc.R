library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/fixpt.R")

output <- .get_argument("gjam_interaction", "output.rdata")

alpha <- output$alphaMu
rho <- t(output$rhoMu)
x <- rast(file.path(path_analysis, "gjam_interaction", "x_1990.tif"))

## call fixpt_geospatial()
wstar <- fixpt(rho, alpha, x, chunk_process = TRUE, n_chunks = 100)

writeRaster(wstar, file.path(path_analysis, "gjam_interaction", "w_star_1900.tif"),
            datatype = "INT2S")