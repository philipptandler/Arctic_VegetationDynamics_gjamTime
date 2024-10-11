#set up environment:
library(here)
setwd(here::here())
useScratch <- TRUE
source("Scripts/Analysis/analysisHfunctions.R")

## set paths and names

name_ndvi_trend <- "ndvi_trend"
name_ndvi_sig <- "ndvi_sig"
name_ndvi_abs_trend <- "ndvi_abs_trend"
name_ndvi_abs_sig <- "ndvi_abs_sig"

## load response
ndvi_abs_trend <- rast(file.path(path_ndvi, paste0(name_ndvi_abs_trend, ".tif")))
ndvi_abs_sig <- rast(file.path(path_ndvi, paste0(name_ndvi_abs_sig, ".tif")))
ndvi_trend <- rast(file.path(path_ndvi, paste0(name_ndvi_trend, ".tif")))
ndvi_sig <- rast(file.path(path_ndvi, paste0(name_ndvi_sig, ".tif")))

ndvi_abs_trend_nf <- rast(file.path(path_ndvi, paste0(name_ndvi_abs_trend, "_nofire.tif")))
ndvi_abs_sig_nf <- rast(file.path(path_ndvi, paste0(name_ndvi_abs_sig, "_nofire.tif")))
ndvi_trend_nf <- rast(file.path(path_ndvi, paste0(name_ndvi_trend, "_nofire.tif")))
ndvi_sig_nf <- rast(file.path(path_ndvi, paste0(name_ndvi_sig, "_nofire.tif")))


response_list <- list(
  "ndvi_abs_trend"=ndvi_abs_trend,
  "ndvi_trend"=ndvi_trend,
  "ndvi_abs_trend_nf"=ndvi_abs_trend_nf,
  "ndvi_trend_nf"=ndvi_trend_nf,
  "ndvi_abs_sig"=ndvi_abs_sig,
  "ndvi_sig"=ndvi_sig,
  "ndvi_abs_sig_nf"=ndvi_abs_sig_nf,
  "ndvi_sig_nf"=ndvi_trend_nf
)

## load predictors
#lamda shurb
lamda_sh <- rast(file.path(path_analysis_data_rast,"lamda_sh_mean_1990-2020.tif"))
#lamda conifer
lamda_cf <- rast(file.path(path_analysis_data_rast,"lamda_cf_mean_1990-2020.tif"))
#lamda_shurb_conifer
lamda_shcf_harm <- rast(file.path(path_analysis_data_rast,
                                  "lamda_shcf_harmonic_mean_1990-2020.tif"))
lamda_shcf_dom <- rast(file.path(path_analysis_data_rast,
                                 "lamda_shcf_dominant_mean_1990-2020.tif"))


predictor_list <- list(
  "lamda_sh"=lamda_sh,
  "lamda_cf"=lamda_cf,
  "lamda_shcf_harmonic"=lamda_shcf_harm,
  "lamda_shcf_dominant"=lamda_shcf_dom
)


## run Linear Models for all combinations ####
sink(file.path(path_analysis_data_rast,"outputLM_coefList.txt"))
subsample <- FALSE
plot <- FALSE
subSize <- 100000
lm_list <- list()
for(response in names(response_list)){
  lm_list_response <- list()
  for(predictor in names(predictor_list)){
    r <- c(response_list[[response]],predictor_list[[predictor]])
    cat("=====================================================================\n")
    if(subsample){r <- spatSample(r, subSize, as.raster=TRUE)}
    y <- values(r[[1]])
    x <- values(r[[2]])
    lm_this <- lm(y~x)
    cat("Linear Model:", response, "~", predictor,":\n")
    print(summary(lm_this))
    if(subsample & plot){
      plot(y~x, cex = 0.05, pch = 16, ylab=response, xlab=predictor)
      abline(h=0)
      abline(a = lm_this$coefficients[1], b = lm_this$coefficients[2])
    }
    lm_list_response[[predictor]] <- lm_this
    cat("\n\n\n")
  }
  lm_list[[response]] <- lm_list_response
  cat("\n\n\n\n")
}

lm_matrix <- lm_matrix_summary(lm_list, coef="slope")
print(lm_matrix[,1:4])
print(lm_matrix[,5:8])
sink()
## for Rstudio, comment out
# # r[[1]] is treated as y, r[[2]] is treated as x
# printlm <- function(r, sample=TRUE, size = 1, n_samples=1){
#   if(TRUE){
#     for(i in 1:n_samples){
#       r_subs <- spatSample(r, size)
#       y <- r_subs[,1]
#       x <- r_subs[,2]
#       lm_this <- lm(y~x)
#       cat("Linear Model",i,":", names(r_subs[1]), "~", names(r_subs[2]),":\n")
#       print(summary(lm_this))
#       plot(y~x, cex = 0.05, pch = 16)
#       abline(h=0)
#       abline(a = lm_this$coefficients[1], b = lm_this$coefficients[2])
#       cat("\n\n\n")
#     }
#   }
# }
# 
# printlm(c(ndvi_abs_trend_nf, lamda_sh), size = 1000, n_samples = 1)


