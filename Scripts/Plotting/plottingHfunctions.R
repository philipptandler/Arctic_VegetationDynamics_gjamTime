# Load libraries
library(terra)
library(ggplot2)
library(paletteer)
library(scales)
library(grid)

## making hexplot
make_hexplot <- function(df,
                         n_bins = 80,
                         abline_a = 0,
                         abline_b = -1,
                         abline_h_width = 0.3,
                         abline_lm_width = 0.3,
                         xname,
                         yname,
                         xlim = c(NA,NA)){
  xmin <- xlim[1]
  if(is.na(xmin)){xmin <- range(df$x)[1]}
  xmax <- xlim[2]
  if(is.na(xmax)){xmax <- range(df$x)[2]}
  colpalette <- rev(paletteer_c("viridis::magma", 30)[1:29])
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_hex(bins = n_bins) +
    xlim(xmin,xmax) +
    # Add a horizontal abline at y = 0
    geom_hline(yintercept = 0, linewidth = abline_h_width) +
    # Add an abline with intercept and slope from the linear model
    geom_abline(intercept = abline_a, slope = abline_b, linewidth = abline_lm_width) +
    # coord_fixed(ratio = 1) +
    # coord_equal() +
    scale_fill_gradientn(colors = colpalette, name = "Density") +
    # scale_fill_gradientn(colors = my_palette, name = "Density",
    #                      breaks = seq(0, 30, by = 5),  # Specify breaks
    #                      labels = as.character(seq(0, 30, by = 5))) +
    guides(fill = guide_colourbar(barwidth = 0.7,
                                  barheight = 12))+
    labs(x = xname, y = yname) + 
    theme(
      aspect.ratio=1,
      panel.background = element_blank(),    # Remove plot background
      panel.grid = element_blank(),          # Remove grid lines
      # axis.line = element_line(color = "black"),  # Keep axis lines
      axis.ticks = element_line(color = "black"), # Keep axis ticks
      axis.text = element_text(color = "black"),  # Keep axis labels
      legend.position = "right",             # Adjust legend position
      panel.border = element_rect(colour = "black", fill=NA)
    )
  return(p)
}

## testing plot
make_testplot <- function(){
  set.seed(1)
  df <- data.frame(x = rnorm(20000), y = rnorm(20000))
  p <- make_hexplot(df, xname = "TESTPLOT", yname = "TESTPLOT")
  print(p)
}
make_testplot()


## plotting ####
hexagon_plot <- function(raster, lnames = c("y", "x"), lm_sum, sample = TRUE, samplesize = 1e4,
                         nsamples = 1, setseed = 1234, save=FALSE,showplot=TRUE, filename=NULL,
                         path = path_analysis_plots){
  for(i in 1:nsamples){
    
    # linear model
    cat("Linear Model:", names(raster)[1], "~", names(raster)[2],":\n")
    print(lm_sum)
    
    # prepare plot
    set.seed(setseed+(i-1))
    r_subs <- spatSample(raster, samplesize)
    y <- r_subs[,1]
    x <- r_subs[,2]
    df <- data.frame(x = x, y = y)
    df <- df[is.finite(df$x) & is.finite(df$y), ]
    ## make hexagon density plot
    p <- make_hexplot(df,
                      abline_a = lm_sum[1], abline_b = lm_sum[2],
                      # xlim = c(-0.45,0),
                      xname = lnames[2], yname = lnames[1])
    if(save){
      ggsave(file.path(path_analysis_plots, filename), 
             plot = p, width=12, height=9, units='cm')  # Save the plot as file
    }
    if(showplot){print(p)}
  }
}


## names and pahts ####
# set this! select gjam model fit
# gjamModel <- "gjam_singleVars"
gjamModel <- "gjam_interaction"

# paths
path_gjamTime_outputs <- "data/gjamTime_outputs"
path_norm_list <- "Scripts/gjamTime/"
path_ndvi <- "data/NDVI_trends_Ju"


if(gjamModel == "gjam_singleVars"){
  gjam_out_pattern <- "gjam_official_full_subs100_[0-9]{4}"
  path_analysis_saveParameters <- "Scripts/Analysis/.parameters_singleVars"
  path_analysis_data_rast <- "data/analysis_singleVars/rasters"
  path_analysis_plots <- "data/analysis_singleVars/plots"
} else if(gjamModel == "gjam_interaction"){
  gjam_out_pattern <- "gjam_interaction_full_subs100_[0-9]{4}"
  path_analysis_saveParameters <- "Scripts/Analysis/.parameters_interaction"
  path_analysis_data_rast <- "data/analysis_interaction/rasters"
  path_analysis_plots <- "data/analysis_interaction/plots"
} else {stop("please specify gjamModel, i.e. wihch gjam fitted gjam model to use")}


## names
name_norm_list <- "normalization.rds"

## load rasters
# load response
ndvi_trend <- rast(file.path(path_ndvi, "ndvi_trend.tif"))
ndvi_sig <- rast(file.path(path_ndvi, "ndvi_sig.tif"))
ndvi_trend_nf <- rast(file.path(path_ndvi, "ndvi_trend_nofire.tif"))
ndvi_sig_nf <- rast(file.path(path_ndvi, "ndvi_sig_nofire.tif"))
# load predictors
lamda_sh <- rast(file.path(path_analysis_data_rast,"lamda_sh_mean_1990-2020.tif"))
lamda_cf <- rast(file.path(path_analysis_data_rast,"lamda_cf_mean_1990-2020.tif"))
lamda_shcf_harm <- rast(file.path(path_analysis_data_rast,
                                  "lamda_shcf_harmonic_mean_1990-2020.tif"))

wrate_sh <- rast(file.path(path_analysis_data_rast,"wrate_sh.tif"))
wrate_cf <- rast(file.path(path_analysis_data_rast,"wrate_cf.tif"))
wrate_shcf <- rast(file.path(path_analysis_data_rast,"wrate_shcf.tif"))


