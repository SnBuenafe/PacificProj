# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# This categorizes planning units according to their Longhurst Provinces.
# creates a .csv file, .rds file and .shp files of the PUs.
# Inputs include the following:
# 1. pu_file: .shp or .rds file of the planning units
# 2. province_file: .shp file of the Longhurst Provinces
# 3. prov_name: Longhurst
# 4. olayer: surface
# 5. outdir: path of the output


pu_by_provinces <- function(pu_file, province_file, prov_name, olayer, outdir) {
  
  library(raster)
  library(dplyr)
  library(sf)
  library(rgeos)
  library(rgdal)
  library(data.table)
  library(foreach)
  library(doParallel)
  library(ggplot2)
  library(magrittr)
  
  # Defining crs
  rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
  longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  # Reading Planning Unit Region Shapefile
  
  if(stringr::str_detect(string = pu_file, pattern = ".rds") == TRUE) {
    pu_file <- pu_file
    pu_region <- readRDS(pu_file)
  } else if (stringr::str_detect(string = pu_file, pattern = ".shp") == TRUE) {
    pu_file <- pu_file
    pu_region <- st_read(pu_file) %>% 
      st_transform(crs = rob_pacific)
  }
  
  # Reading Marine Province Shapefile
  province <- province_file
  bioprovince <- st_read(province) %>% 
    st_transform(crs = longlat)
  bioprovince_sp <- as(bioprovince, "Spatial")
  
  bioprovince1 <- rgeos::gBuffer(bioprovince_sp, byid=TRUE, width=0)
  bioprovince2 <- as(bioprovince1, "sf")
  
  # Creating Longhurst Province Pacific-Centered Shapefile
  prov_name = "Longhurst"
  
  # Define a long & slim polygon that overlaps the meridian line & set its CRS to match # that of world
  polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                       c(0, 90),
                                       c(0, -78.50016),
                                       c(-0.0001, -78.50016),
                                       c(-0.0001, 90)))) %>%
    st_sfc() %>%
    st_set_crs(4326)
  
  # Modify world dataset to remove overlapping portions with world's polygons
  bioprovince_rob <- bioprovince2 %>% 
    st_difference(polygon) %>% 
    st_transform(crs = rob_pacific)
  
  ggplot() +
    geom_sf(data = bioprovince_rob, aes(fill = ProvCode)) 
  
  # Fix those extra boundaries
  bbox <-  st_bbox(bioprovince_rob)
  bbox[c(1,3)]  <-  c(-1e-5,1e-5)
  polygon2 <- st_as_sfc(bbox)
  crosses <- bioprovince_rob %>%
    st_intersects(polygon2) %>%
    sapply(length) %>%
    as.logical %>%
    which
  # Adding buffer 0
  bioprovince_rob[crosses,] %<>%
    st_buffer(0) 
  # check plot again
  ggplot() +
    geom_sf(data = bioprovince_rob, aes(fill = ProvCode)) 
  
  #writing Pacific-Centered shapefile
  st_write(bioprovince_rob, dsn = "inputs/shapefiles/PacificCenterLonghurst", driver = "ESRI Shapefile", append = FALSE)
  
  # Match PUs with Provinces
  ncores <- 24
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  # Get the indicator for the provinces
  prov_code <- as.character(bioprovince_rob$ProvCode)
  prov_list <- list() # to allocate results
  prov_par <- foreach(i = 1:length(prov_code), .packages = c("raster", "sf", "data.table", "dplyr")) %dopar% {
    single <- bioprovince_rob %>% filter(ProvCode == prov_code[i])
    dt1 <- st_intersection(pu_region, single) %>% 
      filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
    if(nrow(dt1) > 0) { 
      prov_list[[i]] <- dt1 %>% mutate(province = prov_code[i]) # save the output    
    }
  }
  stopCluster(cl)
  
  # Merge all the output
  pus_prov <- do.call(rbind, prov_par) %>% group_by(province)
  # Match and establish categories
  pu_region$province <- pus_prov$province[match(pu_region$geometry, pus_prov$geometry)]
  pu_region$prov_descr <- pus_prov$ProvDescr[match(pu_region$geometry, pus_prov$geometry)]
  pu_region$province <- ifelse(is.na(pu_region$province), 
                               paste("non-categ", prov_name, sep = "_"), 
                               paste(pu_region$province, prov_name, sep = "_"))
  pu_region$prov_descr <- ifelse(is.na(pu_region$prov_descr), 
                               paste("non-categ", prov_name, sep = "_"), 
                               paste(pu_region$province, prov_name, sep = "_"))
  
  pu_region <- as.data.frame(pu_region)
  pu_csv <- paste(paste("pus", olayer, sep = "-"), prov_name, ".csv", sep = "_")
  fwrite(dplyr::select(pu_region, -geometry), paste(outdir, pu_csv, sep = ""))
  
  pu_region <- pu_region %>% 
    st_as_sf()

  st_write(pu_region, dsn = "inputs/shapefiles/PacificABNJGrid_05deg_Longhurst", driver = "ESRI Shapefile", append = FALSE)
  st_write(pu_region, dsn = "outputs/Provinces/PacificABNJGrid_05deg_Longhurst", driver = "ESRI Shapefile", append = FALSE)
  
  saveRDS(pu_region, file = "inputs/rdsfiles/PacificABNJGrid_05deg_Longhurst.rds")
  saveRDS(pu_region, file = "outputs/Provinces/PacificABNJGrid_05deg_Longhurst.rds")
  print("success!")
  
  return(pu_region)
}

#############################   
# RUNNING THE FUNCTION #
#############################

run01 <- pu_by_provinces(pu_file = "inputs/rdsfiles/PacificABNJGrid_05deg.rds",
                province_file = "inputs/shapefiles/Longhurst/Longhurst_world_v4_2010.shp", 
                prov_name = "Longhurst",
                olayer = "surface",
                outdir = "outputs/Provinces/")
run01

library(RColorBrewer)

pal_prov <- rev(brewer.pal(9, "Set3"))

# the following must be defined:
# 1. Bndry
# 2. world_sf

ggplot()+
  geom_sf(data = run01, aes(color = province)) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Planning Units divided according to Longhurst Provinces") +
  theme_bw()
  
