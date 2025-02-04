## get path for saving folder in increments if path exists
.get_outfolder <- function(path, name, full = TRUE){
  dir_path <- file.path(path, name)
  
  # Check if the directory already exists
  if (dir.exists(dir_path)) {
    # If it exists, find a new directory name
    i <- 1
    while (dir.exists(dir_path)) {
      dir_path <- file.path(path, paste0(name, "-", i))
      i <- i + 1
    }
  }
  # return full path or foldername
  if(full){return(dir_path)}
  else{return(basename(dir_path))}
}
