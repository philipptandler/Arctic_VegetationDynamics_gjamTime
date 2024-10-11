#set up environment:
library(here)
setwd(here::here())
useScratchifTerminal <- TRUE
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
  "ndvi_abs_sig"=ndvi_abs_sig,
  "ndvi_trend"=ndvi_trend,
  "ndvi_sig"=ndvi_sig,
  "ndvi_abs_trend_nf"=ndvi_abs_trend_nf,
  "ndvi_abs_sig_nf"=ndvi_abs_sig_nf,
  "ndvi_trend_nf"=ndvi_trend_nf,
  "ndvi_sig_nf"=ndvi_trend_nf
)

## load predictors
lamda_shcf_1990 <- rast(file.path(path_analysis_data_rast,"lamda_shcf_harmonic_1990.tif"))
lamda_shcf_2020 <- rast(file.path(path_analysis_data_rast,"lamda_shcf_harmonic_2020.tif"))
lamda_shcf <- mean()
lamda_sh <- rast(file.path(path_analysis_data_rast,"lamda_sh_mean_1990-2020.tif"))

predictor_list <- list(
  "lamda_sh"=lamda_sh,
  "tau_sh"=tau_sh,
  "lamda_shcf"=
)


sink("output.txt")
## linear model
for(response in names(response_list)){
  for(predictor in names(predictor_list)){
    r <- c(response_list[[response]],predictor_list[[predictor]])
    cat("=====================================================================\n")
    if(sample){
      for(i in 1:n_samples){
        r_subs <- spatSample(r, 10000)
        cat("Linear Model",i,":", response, "~", predictor,":\n")
        y <- values(r_subs[[1]])
        x <- values(r_subs[[2]])
        this_lm <- lm(y ~ x)
        print(summary(this_lm))
      }
    }else{
      cat("Linear Model",i,":", response, "~", predictor,":\n")
      y <- values(r[[1]])
      x <- values(r[[2]])
      this_lm <- lm(y ~ x)
      print(summary(this_lm))
      cat("\n\n\n")
    }
    cat("\n\n\n\n\n")
  }
}
sink()

## for Rstudio
# r[[1]] is treated as y, r[[2]] is treated as x
printlm <- function(r, sample=TRUE, size = 1, n_samples=1){
  if(sample){
    for(i in 1:n_samples){
      r_subs <- spatSample(r, size)
      y <- r_subs[,1]
      x <- r_subs[,2]
      lm_this <- lm(y~x)
      cat("Linear Model",i,":", names(r_subs[1]), "~", names(r_subs[2]),":\n")
      cat("\ny:", names(r_subs)[1], "\n")
      print(summary(y))
      cat("\nx:", names(r_subs)[2], "\n")
      print(summary(x))
      print(summary(lm_this))
      plot(y~x, cex = 0.1, pch = 16)
      abline(h=0)
      abline(a = lm_this$coefficients[1], b = lm_this$coefficients[2])
      cat("\n\n\n")
    }
  }
}

printlm(c(ndvi_abs_trend_nf, lamda_sh), size = 10000, n_samples = 1)


