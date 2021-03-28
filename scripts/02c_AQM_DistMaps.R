# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.


bycatch_distribution <- function(path, bycatch, olayer, outdir, ...) {

  ####################################################################################
  ####### Defining packages to be used
  ####################################################################################
  
  library(raster)
  library(fasterize)
  library(sf)
  library(sp)
  library(readxl)
  library(stringr)
  
  ####################################################################################
  ####### Calling files
  ####################################################################################
  
  #defining the bycatch species
  #make sure that all excel files are closed when running this
  dir <- path
  
  if(bycatch == "turtle") {
    bycatch_data <- list.files(path = dir, pattern = "*bycatch.*.xlsx$", full.names = TRUE) %>% 
      read_excel() %>% 
      filter(group == "turtle")
  #  print(bycatch_data)
  } else if(bycatch == "mammal") {
    bycatch_data <- list.files(path = dir, pattern = "*bycatch.*.xlsx$", full.names = TRUE) %>% 
      read_excel() %>% 
      filter(group == "mammal")
    #  print(bycatch_data)
  } else if(bycatch == "bird") {
    bycatch_data <- list.files(path = dir, pattern = "*bycatch.*.xlsx$", full.names = TRUE) %>% 
      read_excel() %>% 
      filter(group == "bird")
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
  for (i in 1:length(bycatch_data)) {
    
    pattern <- paste0(paste0(bycatch_data$aqm_spcode[i], "_", olayer, ".rds"))
    aqm_spcode <- list.files(path = dir, pattern = pattern, full.names = TRUE)
    rds_file <- readRDS(aqm_spcode) %>% 
      mutate(new_prob = Probability/Probability)
    
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
  
  ####################################################################################
  ####### Creating distribution plots for the bycatch
  ####################################################################################
  
  pal_dist <- rev(brewer.pal(9, "BuPu"))
  
  #creating objects for ggplot
  for (i in 1:length(bycatch_data)) {
    
    pattern <- paste0(paste0(bycatch_data$aqm_spcode[i], "_", olayer, ".rds"))
    aqm_spcode <- list.files(path = dir, pattern = pattern, full.names = TRUE)
    rds_file <- readRDS(aqm_spcode) %>% 
      mutate(new_prob = Probability/Probability)
    
    common_name <- bycatch_data$common[i]
    
    p <- ggplot() +
      geom_sf(data = rds_file, color = pal_dist[i]) +
      geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
      geom_sf(data = world_eez, size = 1, aes(color = Boundary), fill = NA, show.legend = FALSE) +
      scale_color_manual(values = "grey30") +
      coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
               ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
               expand = TRUE) +
      labs(title = paste0(common_name," ","distribution")) +
      theme_bw()
    
    assign(paste0("p",i), p)
      
  }

}

####################################################################################
####### Running the bycatch_distribution function
####################################################################################

# Run the function
bycatch_distribution(path = "outputs/AQM_wflow/02b_bycatch",
                     bycatch = "turtle",
                     olayer = "surface",
                     outdir = "outputs/AQM_wflow/02c_distribution")

#arranging the plots using patchwork
plots <- (p1 | p2 | p3) / (p4 | p5 | p6) + 
          plot_annotation(tag_levels = "i",
                          tag_suffix = ".",
                          title = "Distribution of Marine Turtles in the Pacific Ocean",
                          caption = "Data from AquaMaps (2019)")

plots + 
  ggsave("pdfs/PacificTurtles.pdf", width = 20, height = 10, dpi = 300) +
  ggsave("pdfs/PacificTurtles.jpg", width = 20, height = 10, dpi = 300)
