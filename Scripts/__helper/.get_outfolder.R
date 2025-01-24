## get path for saving folder in increments if path exists
.get_outfolder <- function(path, name, full = TRUE){
  new_name <- name
  dir_path <- paste0(path, name)
  
  # If directory exists, find a new directory name
  i <- 0
  while (dir.exists(dir_path)) {
    i <- i + 1
    new_name <- paste0(name, "-", i)
    dir_path <- file.path(path, new_name)
  }
  
  # return full path or foldername
  if(full){return(dir_path)}
  else{return(new_name)}
}