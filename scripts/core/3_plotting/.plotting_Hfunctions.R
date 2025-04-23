################################################################################
## libraries ####
################################################################################

library(terra)
library(ggplot2)
library(paletteer)
library(scales)
library(grid)

################################################################################
## hexagon plot ####
################################################################################

.make_hexplot <- function(df,
                          n_bins = 80,
                          abline_a = 0,
                          abline_b = -1,
                          abline_h_width = 0.3,
                          abline_lm_width = 0.3,
                          xname,
                          yname,
                          xlim = c(NA,NA),
                          ylim = c(NA,NA)){
  xmin <- xlim[1]
  if(is.na(xmin)){xmin <- range(df$x)[1]}
  xmax <- xlim[2]
  if(is.na(xmax)){xmax <- range(df$x)[2]}
  ymin <- ylim[1]
  if(is.na(ymin)){ymin <- range(df$y)[1]}
  ymax <- ylim[2]
  if(is.na(ymax)){ymax <- range(df$y)[2]}
  colpalette <- rev(paletteer_c("viridis::magma", 30)[1:29])
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_hex(bins = n_bins) +
    xlim(xmin,xmax) +
    ylim(ymin,ymax) +
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


.hex_plot_geospatial <- function(x, y, xlab = NULL, ylab = NULL, subsample = TRUE, 
                                 size = 1e4, n_samples = 1, seed = 1234, abline = TRUE,
                                 abline_intercept = NULL, abline_slope = NULL,
                                 xlim = NULL, ylim = NULL, showplot = TRUE,
                                 save = FALSE, file = NULL){
  
  if(ext(x) != ext(y)) stop("Extents of x and y do not match\n")
  r <- c(x, y)
  for(i in 1:n_samples){
    set.seed(seed+(i-1))
    if(sample) rs <- spatSample(r, min(ncell(r), size))
    else rs <- spatSample(r, ncell(r))
    y <- rs[,1]
    x <- rs[,2]
    df <- data.frame(x = x, y = y)
    df <- df[is.finite(df$x) & is.finite(df$y), ]

    p <- .make_hexplot(df,)
    if(save){
      if(!file.exists(dirname(file))){
        file <- file.path(getwd(), "Rplot.png")
        i <- 1
        while(file.exists(file)){
          file <- file.path(getwd(), paste0("Rplot", i,"png"))
          i <- i+1
        }
        
      }
      ggsave(file.path(path_analysis_plots, filename), 
             plot = p, width=12, height=9, units='cm')  # Save the plot as file
    }
    if(showplot){print(p)}
    
    
  }
}

