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
                          abline = TRUE,
                          abline_horizontal = TRUE,
                          abline_intercept = NULL,
                          abline_slope = NULL,
                          abline_h_width = 0.3,
                          abline_lm_width = 0.3,
                          ylab = colnames(df)[1],
                          xlab = colnames(df)[2],
                          ylim = NULL,
                          xlim = NULL,
                          col_palette = NULL,
                          col_palette_select = NULL){
  
  # check limits
  if(is.null(ylim)){
    ymin <- range(df[[1]])[1]
    ymax <- range(df[[1]])[2]
  } else {
    ymin <- ylim[1]
    ymax <- ylim[2]
  }
  if(is.null(xlim)){
    xmin <- range(df[[2]])[1]
    xmax <- range(df[[2]])[2]
  } else {
    xmin <- xlim[1]
    xmax <- xlim[2]
  }
  df <- df[df$x >= xmin & df$x <= xmax & df$y >= ymin & df$y <= ymax, ]
  df <- na.omit(df)

  # make colpalette
  if(is.null(col_palette)) col_palette <- "viridis::magma"
  if(is.null(col_palette_select)) col_palette_select <- c(1:29)
  colpalette <- rev(paletteer_c(col_palette, 30)[col_palette_select])
  
  # set colnames
  colnames(df) <- c("y", "x")
  
  # plot main
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_hex(bins = n_bins) +
    xlim(xmin,xmax) +
    ylim(ymin,ymax) +
    scale_fill_gradientn(colors = colpalette, name = "Density") +
    guides(fill = guide_colourbar(barwidth = 0.7,
                                  barheight = 12)) +
    labs(x = xlab, y = ylab) + 
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
  
  # optional
  if(abline_horizontal){
    p <- p + geom_hline(yintercept = 0, linewidth = abline_h_width)
  }
  if(abline){
    p <- p + geom_abline(intercept = abline_intercept, slope = abline_slope, 
                         linewidth = abline_lm_width)
  }
  return(p)
}

.hexplot_geospatial <- function(y, x,
                                n_bins = 80,
                                abline = TRUE,
                                abline_horizontal = TRUE,
                                abline_intercept = NULL,
                                abline_slope = NULL,
                                abline_h_width = 0.3,
                                abline_lm_width = 0.3,
                                ylab = NULL, xlab = NULL, 
                                ylim = NULL, xlim = NULL,
                                col_palette = NULL,
                                col_palette_select = NULL,
                                subsample = TRUE, 
                                size = 1e5, n_samples = 1, seed = 1234, 
                                showplot = TRUE, save = FALSE, file = NULL,
                                width=12, height=9, units='cm'){
  
  if(ext(x) != ext(y)) stop("Extents of x and y do not match.\n")
  if(nlyr(y) != 1){
    warning( "More than one layer in y, only first layer considered.\n")
    y <- y[[1]]
  }
  if(nlyr(x) != 1){
    warning( "More than one layer in x, only first layer considered.\n")
    x <- x[[1]]
  }
  # concatenate raster
  r <- c(y, x)
  # for many samples
  plotlist <- list()
  for(samp in 1:n_samples){
    cat("Drawing hexplot...")
    set.seed(seed+(samp-1))
    if(subsample) rs <- spatSample(r, min(ncell(r), size))
    else rs <- spatSample(r, ncell(r)) #TODO check
    y <- rs[,1]
    x <- rs[,2]
    df <- data.frame(y = y, x = x)
    df <- df[is.finite(df$x) & is.finite(df$y), ]
    # check linear model
    if(abline && (is.null(abline_intercept) || is.null(abline_slope))){
      lm <- lm(y ~ x, data = df)
      if(is.null(abline_intercept)) abline_intercept <- lm$coefficients[1] 
      if(is.null(abline_slope)) abline_slope <- lm$coefficients[2]
    }
    #check lab
    if(is.null(ylab)) ylab <- names(r)[1]
    if(is.null(xlab)) xlab <- names(r)[2]
    # make plot
    p <- .make_hexplot(df,
                       n_bins = n_bins,
                       abline = abline,
                       abline_horizontal= abline_horizontal,
                       abline_intercept = abline_intercept,
                       abline_slope = abline_slope,
                       abline_h_width = abline_h_width,
                       abline_lm_width = abline_lm_width,
                       ylab = ylab,
                       xlab = xlab,
                       ylim = ylim,
                       xlim = xlim,
                       col_palette = col_palette,
                       col_palette_select = col_palette_select)
    #save
    if(save){
      if(!file.exists(dirname(file))){
        dir.create(dirname(file))
      }
      if(file.exists(file)){
        dir <- dirname(file)
        basename <- tools::file_path_sans_ext(basename(file))
        ext <- tools::file_ext(file)
        i <- 1
        while(file.exists(file)){
          file <- file.path(dir, paste0(basename, "-", i, ".", ext))
          i = i+1
        }
      }
      ggsave(filename = file, plot = p, width=width, height=height, units=units)  # Save the plot as file
    }
    if(showplot){print(p)}
    plotlist[[samp]] <- p
    cat("done.\n")
  }
  plotlist
}

