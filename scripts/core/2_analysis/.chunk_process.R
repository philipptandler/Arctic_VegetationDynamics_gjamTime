# this function processes files in chunks
# n_chunks
# chunk_size in cells
.chunk_process <- function(rasters = list(), FUN, n_chunks=NULL, chunk_size=NULL, extra_args = list()){
  cat("chunk processing\n")
  
  horizontal <- TRUE
  x_dim <- dim(rasters[[1]])[2]
  y_dim <- dim(rasters[[1]])[1]
  
  if(x_dim < y_dim){
    horizontal = FALSE
    x_dim <- dim(rasters[[1]])[1]
    y_dim <- dim(rasters[[1]])[2]
  }
  
  # ensure at least one chunking parameter is provided
  if(is.null(n_chunks) && is.null(chunk_size)){
    stop("error in .chunk_process: either n_chunks or chunk_size must be specified.")
  }
  
  # compute chunking parameters
  if(is.null(n_chunks)){
    chunk_size <- min(ncell(rasters[[1]]), chunk_size)
    chunk_size <- max(y_dim, chunk_size)
    chunk_size_h <- ceiling(chunk_size/y_dim)
    n_chunks <- ceiling(x_dim/chunk_size_h)
  }
  if(is.null(chunk_size)){
    n_chunks <- min(x_dim, n_chunks)
    n_chunks <- max(1, n_chunks)
    chunk_size_h <- ceiling(x_dim/n_chunks)
    n_chunks <- ceiling(x_dim/chunk_size_h)
  }
  
  # loop over all chunks
  for(pos in 1:n_chunks){
    cat(pos, "out of", n_chunks, "\n")
    # define chunk bounds
    xmin <- (pos-1)*chunk_size_h+1
    xmax <- min(pos*chunk_size_h, x_dim)
    ymin <- 1
    ymax <- y_dim
    subset <- c(xmin, xmax, ymin, ymax)
    if(!horizontal) subset <- c(ymin, ymax, xmin, xmax)
    
    # subset rasters
    chunk_rasters <- lapply(rasters, function(r){
      r[subset[3]:subset[4], subset[1]:subset[2], drop = FALSE]
    })
    names(chunk_rasters) <- names(rasters)
    
    # do call
    processed_chunk <- do.call(FUN, c(chunk_rasters, extra_args))
    # save
    processed_chunk_name <- paste0("chunk_process_id-", pos, ".tif")
    writeRaster(processed_chunk, file.path(path_analysis_tmp, processed_chunk_name))
  }
  cat("done.\n returning processed raster.\n")
  # merge and return
  processed_chunk_list <- list.files(path = path_analysis_tmp,
                                     pattern = "^chunk_process_id-*",
                                     full.names = TRUE)
  processed_rasters <- lapply(processed_chunk_list, rast)
  
  merged_raster <- do.call(mosaic, processed_rasters)
  return(merged_raster)
}