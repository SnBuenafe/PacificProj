# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This creates a Distribution Plots of all the bycatch species.
# Inputs include the following:
# 1. path: where the .rds files of the bycatch are found
# 2. bycatch: turtle, mammal, bird
# 3. land_file: Robinson projected land masses
# 4. eez_file: Robinson projected EEZs
# 5. north: north boundary in degrees
# 6. south: south boundary in degrees
# 7. west: west boundary in degrees
# 8. east: east boundary in degrees

# Function is ran in 02d.

fBycatchDistMap <- function(path, bycatch, land_file, eez_file, north, south, west, east, ...) {

  ####################################################################################
  ####### Defining packages needed
  ####################################################################################
    # List of pacakges that we will use
    list.of.packages <- c("RColorBrewer", "tidyverse", "readxl", "patchwork", "sf", "proj4")
    # If is not installed, install the pacakge
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    # Load packages
    lapply(list.of.packages, require, character.only = TRUE)
    
  ####################################################################################
  ####### Defining generalities
  ####################################################################################
  rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  world_eez <- readRDS(eez_file)
  world_sf <- readRDS(land_file)
  
  source("scripts/study_area/fCreateRobinsonBoundary.R")
  Bndry <- fCreateRobinsonBoundary(west, east, north, south)
  
  ####################################################################################
  ####### Creating bycatch data to be plotted
  ####################################################################################
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
  ####### Creating distribution plots for the bycatch
  ####################################################################################
    plots <- list()
    #creating objects for ggplot
    for (i in 1:nrow(bycatch_data)) {
      
      pattern <- paste0(paste0(bycatch_data$aqm_spcode[i], "_surface", ".rds"))
      aqm_spcode <- list.files(path = dir, pattern = pattern, full.names = TRUE)
      rds_file <- readRDS(aqm_spcode) %>% 
        dplyr::mutate(new_prob = Probability/Probability)
      
      common_name <- bycatch_data$common[i]
      pal_dist <- rev(brewer.pal(9, "Set3"))
      
      if(bycatch == "turtle") {
        x <- i
      } else if (bycatch == "mammal") {
        if(i <= 9) {
          x <- i
        } else if(i <= 18) {
          x <- i-9
        } else {
          x <- i-18
        }
      } else {
        print("fail")
      }
      
      p <- ggplot() +
        geom_sf(data = rds_file, fill = pal_dist[x], color = pal_dist[x], size = 0.1) +
        geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
        geom_sf(data = world_eez, size = 1, fill = NA, show.legend = FALSE) +
        scale_color_manual(values = "grey30") +
        coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                 ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                 expand = TRUE) +
        labs(title = (paste0(common_name," ","distribution"))) +
        theme_bw()
      
      plots[[i]] <- p
    } 
  return(plots)
}