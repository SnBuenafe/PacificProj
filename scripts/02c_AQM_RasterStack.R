# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This creates a RasterStack of all the bycatch species.
# Inputs include the following:
# 1. path: where the .rds files of the bycatch are found
# 2. bycatch: turtle, mammal, bird
# 3. olayer: surface
# 4. outdir: directory where RasterStack will be saved

# The following must also be defined prior to running the function.
# rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

bycatch_rasterstack <- function(path, bycatch, olayer, outdir, ...) {

  ####################################################################################
  ####### Defining the main packages (trying to auto this)
  ####################################################################################
  # List of pacakges that we will use
  list.of.packages <- c("raster", "fasterize", "sf", "sp", "readxl", "tidyverse")
  # If is not installed, install the pacakge
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # Load packages
  lapply(list.of.packages, require, character.only = TRUE)
  
  ####################################################################################
  ####### Calling files
  ####################################################################################
  
  #defining the bycatch species
  #make sure that all excel files are closed when running this
  dir <- path
  
  if(bycatch == "turtle") {
    bycatch_data <- list.files(path = dir, pattern = "*bycatch.*.xlsx$", full.names = TRUE) %>% 
      read_excel() %>% 
      dplyr::filter(group == "turtle")
  #  print(bycatch_data)
  } else if(bycatch == "mammal") {
    bycatch_data <- list.files(path = dir, pattern = "*bycatch.*.xlsx$", full.names = TRUE) %>% 
      read_excel() %>% 
      dplyr::filter(group == "mammal")
    #  print(bycatch_data)
  } else if(bycatch == "bird") {
    bycatch_data <- list.files(path = dir, pattern = "*bycatch.*.xlsx$", full.names = TRUE) %>% 
      read_excel() %>% 
      dplyr::filter(group == "bird")
    #  print(bycatch_data)
  } else {
    print("fail")
  }
  
  ####################################################################################
  ####### Creating RasterStack for the bycatch 
  ####################################################################################
  
  #creating an empty stack for stacking rasters
  stack <- stack()
  
  #creating RasterStack
  for (i in 1:nrow(bycatch_data)) {
    
    pattern <- paste0(paste0(bycatch_data$aqm_spcode[i], "_", olayer, ".rds"))
    aqm_spcode <- list.files(path = dir, pattern = pattern, full.names = TRUE)
    rds_file <- readRDS(aqm_spcode) %>% 
      dplyr::mutate(new_prob = Probability/Probability)
    
    common_name <- bycatch_data$common[i]
  
    # Creating a empty raster with the same resolution every time
    rs <- raster(ncol = 360*2, nrow = 180*2) 
    rs_rob <- projectRaster(rs, crs = crs(rob_pacific), method = "ngb", res = 20000)
    rs_rob[] <- 1:ncell(rs_rob)
    
    # Creating raster file of bycatch distribution
    temp <- fasterize(rds_file, rs_rob)
    names(temp) <- common_name
    stack <- stack(stack, temp)
    
  }
  
  #writing stack
  writeRaster(stack, filename = file.path(outdir, bycatch), format = "raster", overwrite = TRUE)
  
}
####################################################################################
####### Running the bycatch_distribution function
####################################################################################

# Run the function
bycatch_rasterstack(path = "outputs/AQM_wflow/02b_bycatch",
                     bycatch = "turtle",
                     olayer = "surface",
                     outdir = "outputs/AQM_wflow/02c_distribution")
