#set up environment:
library(here)
setwd(here::here())
useScratch <- TRUE # change to TRUE for run on cluster
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
  "ndvi_trend"=ndvi_trend,
  "ndvi_trend_nf"=ndvi_trend_nf,
  "ndvi_sig"=ndvi_sig,
  "ndvi_sig_nf"=ndvi_sig_nf
)

## load predictors
lamda_sh <- rast(file.path(path_analysis_data_rast,"lamda_sh_mean_1990-2020.tif"))
lamda_cf <- rast(file.path(path_analysis_data_rast,"lamda_cf_mean_1990-2020.tif"))
lamda_shcf_harm <- rast(file.path(path_analysis_data_rast,
                                  "lamda_shcf_harmonic_mean_1990-2020.tif"))

wrate_sh <- rast(file.path(path_analysis_data_rast,"wrate_sh.tif"))
wrate_cf <- rast(file.path(path_analysis_data_rast,"wrate_cf.tif"))
wrate_shcf <- rast(file.path(path_analysis_data_rast,"wrate_shcf.tif"))


predictor_list <- list(
  "lamda_sh"=lamda_sh,
  "lamda_cf"=lamda_cf,
  "lamda_shcf_harmonic"=lamda_shcf_harm,
  "wrate_sh" = wrate_sh,
  "wrate_cf" = wrate_cf,
  "wrate_shcf" = wrate_shcf
)


## run Linear Models for all combinations ####
sink(file.path(path_analysis_data_rast,"LM_overview.txt"))
subsample <- FALSE
plot <- FALSE
subSize <- 1000
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
    lm_summary <- summary(lm_this)
    print(lm_summary)
    if(subsample & plot){
      plot(y~x, cex = 0.05, pch = 16, ylab=response, xlab=predictor)
      abline(h=0)
      abline(a = lm_this$coefficients[1], b = lm_this$coefficients[2])
    }
    lm_list_response[[predictor]] <- lm_summary$coefficients
  }
  lm_list[[response]] <- lm_list_response
  cat("\n\n\n\n")
}

saveRDS(lm_list, file = file.path(path_analysis_data_rast, "lm_sum.rds"))

cat("=====================================================================\n")
lm_matrix <- lm_matrix_summary(lm_list, coef="slope", measure = "estimate")
print(lm_matrix)
cat("\n\n")
lm_matrix <- lm_matrix_summary(lm_list, coef="slope", measure = "p-value")
print(lm_matrix)
sink()


# for Rstudio, comment out
# r[[1]] is treated as y, r[[2]] is treated as x
# printPlotlm <- function(r, sample=TRUE, size = 1, n_samples=1){
#   if(TRUE){
#     for(i in 1:n_samples){
#       r_subs <- spatSample(r, size)
#       y <- r_subs[,1]
#       x <- r_subs[,2]
#       lm_this <- lm(y~x)
#       cat("Linear Model",i,":", names(r_subs[1]), "~", names(r_subs[2]),":\n")
#       print(summary(lm_this))
#       plot(y~x, cex = 0.05, pch = 16, ylab=names(r)[1], xlab=names(r)[2])
#       abline(h=0)
#       abline(a = lm_this$coefficients[1], b = lm_this$coefficients[2])
#       cat("\n\n\n")
#     }
#   }
# }
# 
# printPlotlm(c(ndvi_sig,predictor_list[["wrate_sh"]]), size = 200000, n_samples = 1)
