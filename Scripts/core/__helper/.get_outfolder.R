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


## looks for outfolder for a given script
.find_callscrpt_folder <- function(path, name, hash_id, create = FALSE){
  
}

## returns dir
.get_gjamTime_outfolder <- function(path, invar){
  if(dir.exists(invar)){
    return(invar)
  } 
  if(dir.exists(file.path(path, invar))){
    return(file.path(path, invar))
  } else {
    # if invar is call_script
    if(!file.exists(invar)){stop("Invalid Argument .get_outfolder()")}
    call <- .initialize_and_validate_call(call_scrpt)
    hash_id <-.get_hash_id(call_script)
    gjamTime_outfolder <- .find_gjamTime_outfolder(path_gjamTime_out,
                                                     call$name,
                                                     hash_id)
    return(gjamTime_outfolder)
  }
}
