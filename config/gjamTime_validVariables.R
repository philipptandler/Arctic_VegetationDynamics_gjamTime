#' this script is for setting the potential variables that are used. gjamTime 
#' will look in path_gjam_in for those variables

# periods captures the names of the observed periods in order
valid_times <- c("1984-1990",
                 "1991-1996",
                 "1997-2002",
                 "2003-2008",
                 "2009-2014",
                 "2015-2020")

# name of period for variables that are constant over all periods
valid_time_const <- "const"

# var_list captures all variables
# groupname is used to access this variable group in path_gjam_in
# groupname makes the files more organized in path_gjam_in
# isDynamic tells if variable changes over time (TRUE) or is constant (FALSE)
# safe the files as groupname_time_variable_version.tif. E.g.:
# veg_1984-1990_sh_full.tif, or topo_const_elev_full.tif

valid_variables_list <- list(
  vegetation = list(
    groupname = "veg",
    isDynamic = TRUE,
    varnames = c("sh", "cf", "hb", "lc", "wt", "br")
  ),
  climate = list(
    groupname = "clim",
    isDynamic = TRUE,
    varnames = c("tas", "tasw", "tass", "pr", "prw", "prs")
  ),
  topography = list(
    groupname = "topo",
    isDynamic = FALSE,
    varnames = c("elev", "slope", "aspect", "cosasp", "tpi")
  ),
  soil = list(
    groupname = "soil",
    isDynamic = FALSE,
    varnames = c("wvol", "wvol05", "wvol15", "wvol30", "wvol60", "scwd")
  )
)

# allow for different versions here
valid_versions <- c("full")
