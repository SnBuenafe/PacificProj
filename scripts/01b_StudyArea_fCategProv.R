# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This categorizes planning units according to their Longhurst Provinces.
# creates a .csv file, .rds file and .shp files of the PUs.
# Inputs include the following:
# 1. pu_file: .shp or .rds file of the planning units
# 2. province_file: .shp file of the Longhurst Provinces
# 5. outdir: path of the output

# Function is ran at 01c.

fCategProv <- function(pu_file, province_file, outdir, ...) {
  
  ##########################################
  ####### Defining the main packages #######
  ##########################################
  # List of pacakges that we will use
  list.of.packages <- c("raster", "tidyverse", "data.table", "rgeos", "rgdal", "sf", "foreach", "doParallel", "magrittr")
  # If is not installed, install the pacakge
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # Load packages
  lapply(list.of.packages, require, character.only = TRUE)
  
  ##########################################
  ####### Calling relevant functions #######
  ##########################################
  source("scripts/study_area/fConvert2PacificRobinson.R")
  
  #####################################
  ####### Defining generalities #######
  #####################################
  rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
  longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  #############################
  ####### Reading files #######
  #############################
  # Planning unit files
  if(stringr::str_detect(string = pu_file, pattern = ".rds") == TRUE) {
    pu_file <- pu_file
    pu_region <- readRDS(pu_file)
  } else if (stringr::str_detect(string = pu_file, pattern = ".shp") == TRUE) {
    pu_file <- pu_file
    pu_region <- st_read(pu_file) %>% 
      st_transform(crs = rob_pacific)
  }
  
  # Reading Marine Province Shapefile (Longhurst)
  province <- province_file
  bioprovince <- st_read(province) %>% 
    st_transform(crs = longlat)
  bioprovince_sp <- as(bioprovince, "Spatial")
  
  bioprovince1 <- rgeos::gBuffer(bioprovince_sp, byid=TRUE, width=0)
  bioprovince2 <- as(bioprovince1, "sf")
  
  ######################################################################
  ####### Creating Longhurst Province Pacific-Centered Shapefile #######
  ######################################################################
  bioprovince_rob <- bioprovince2 %>% 
    st_make_valid() %>% 
    fConvert2PacificRobinson(buff = 0.01)
  # check plot again
  #ggplot() +
  #  geom_sf(data = bioprovince_rob, aes(fill = ProvCode)) 
  
  #writing Pacific-Centered shapefile
  saveRDS(bioprovince_rob, "outputs/01_StudyArea/01b_Longhurst/PacificCenterLonghurst.rds")
  #st_write(bioprovince_rob, dsn = "inputs/shapefiles/PacificCenterLonghurst", driver = "ESRI Shapefile", append = FALSE)
  
  ##########################################
  ####### Matching PUs with Province #######
  ##########################################
  PUs <- read_rds(pu_file)
  
  nr <- st_nearest_feature(PUs, bioprovince_rob)
  
  LPs <- PUs %>% 
    mutate(province = bioprovince_rob$ProvCode[nr],
           prov_descr = bioprovince_rob$ProvDescr[nr])
  
  ############################
  ####### Saving files #######
  ############################

  #st_write(pu_region, dsn = "inputs/shapefiles/PacificABNJGrid_05deg_Longhurst", driver = "ESRI Shapefile", append = FALSE)
  #st_write(pu_region, dsn = "outputs/01_StudyArea/01b_Longhurst/PacificABNJGrid_05deg_Longhurst", driver = "ESRI Shapefile", append = FALSE)
  
  #saveRDS(pu_region, file = "inputs/rdsfiles/PacificABNJGrid_05deg_Longhurst.rds")
  saveRDS(LPs, file = "outputs/01_StudyArea/01b_Longhurst/PacificABNJGrid_05deg_Longhurst.rds")
  
  return(LPs)
}
