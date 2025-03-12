#set working directory
library(here)
setwd(here::here())

# install the terra package
# install.packages('terra')

# load the package to get access to the package's routines/functions in your environment
library(terra)

# create a new string variable containing the path to your image file
path_VCFsh <- "Nill_ArcticVCF/1984-1990_krr-avg_int16-10e3_class-1-sh.tif"
# read the file into raster object
img_VCFsh <- rast(path_VCFsh)
# print metadata to console
print(img_VCFsh)
plot(img_VCFsh)

# create a new string variable containing the path to your DEM file
path_DEM <- "ArcticDEM/output_hh.tif"
# read the file into raster object
img_DEM <- rast(path_DEM)
# print metadata to console
print(img_DEM)
plot(img_DEM)

# Stacking a raster
# Load all 6 VCF files:
setwd("C:/Users/phili/Documents/Dateien_Philipp/EnvironmentalSciencesMSc/MasterThesis/data/Nill_ArcticVCF")


# Create a vector using c() containing the filepaths;
# note that we explicitly consider the order, i.e. we start with blue and
# end with shortwave-infrared 2 (index = shorter to longer wavelengths)
VCF_bands <-  list.files(pattern = "1984-1990*") # list.files(path = ".", pattern = NULL
# provide vector of filenames to rast() function; check "?rast" for details
img_VCF <- rast(VCF_bands) #same! spatial resolution
# print result to console
print(img_VCF)


# Access elements
img_VCF[[6]][7173, 9358] # row, column. e.g. i,j. Topleft corner = c(1,1) 
img_DEM[[1]][9358, 7173]
# further operations
dim(img_VCF)
# names(img_VCF) <- c("","", ...)

# safe as dataframe
VCF_bands_df <- as.data.frame(img_VCF[[1]], xy = TRUE)

# safe all raster layeers in a dataframe
VCF_bands__allVeg <- as.data.frame(img_VCF, cells = TRUE)
print(head(VCF_bands__allVeg))

# create a dataframe out of topodata(_r100)
img_VCF <- mask(img_VCF, mastermask_r100, maskvalues=0, updatevalue=NA)
topodata_masked_df <- as.data.frame(img_VCF, xy = TRUE,
                                    cells = TRUE, na.rm = NA)


# project raster
img_DEMrep<- project(img_DEM, img_VCFsh, method = "bilinear")
writeRaster(x = img_DEMrep, filename = "../test_NA_in_reproj_raster2.tif",
            overwrite = TRUE)

# writing a raster
writeRaster(x = img_VCF, filename = "../test_save_raster.tif",
            datatype = "INT2U",
            overwrite = TRUE)
# create 3D array
a <- array(data=c(1:12), dim=c(4,3,2))
# access:
a[3,2,1]
a[,,2]
# create matrix (2D)
m <- matrix(data=c(1:10), nrow=5, ncol=2, byrow = T)



# Create data.frame with four columns of different data types
df <- data.frame(sensor = c('Landsat-5 TM', 'Landsat-7 ETM+', 'Landsat-8 OLI-TIRS'), 
                 n_bands = c(7, 8, 11), 
                 active = c(F, T, T), 
                 launched = c(1984, 1999, 2013))
print(df)


water_data <- rast("gjamTime_data/veg_2015-2020_wt_full.tif")
sh_data <- rast("gjamTime_data/veg_2015-2020_sh_full.tif")
cf_data <- rast("gjamTime_data/veg_2015-2020_cf_full.tif")
hb_data <- rast("gjamTime_data/veg_2015-2020_hb_full.tif")
lc_data <- rast("gjamTime_data/veg_2015-2020_lc_full.tif")
br_data <- rast("gjamTime_data/veg_2015-2020_br_full.tif")

