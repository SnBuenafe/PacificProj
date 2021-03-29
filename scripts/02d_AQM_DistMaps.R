# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This creates a Distribution Plots of all the bycatch species.
# Inputs include the following:
# 1. path: where the .rds files of the bycatch are found
# 2. bycatch: turtle, mammal, bird

# The following must also be defined prior to running the code.
# 1. Bndry: boundaries of study area;
# 2. world_eez: eez in robinson's projection;
# 3. world_sf: landmasses in robinson's projection;
# 4. rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

bycatch_distmap <- function(path, bycatch, ...) {

####################################################################################
####### Defining packages needed
####################################################################################

library(RColorBrewer)
library(ggplot2)
library(readxl)
library(stringr)
library(patchwork)
library(ggfittext)
library(sf)

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
    
    pattern <- paste0(paste0(bycatch_data$aqm_spcode[i], "_", olayer, ".rds"))
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
      geom_sf(data = rds_file, color = pal_dist[x]) +
      geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
      geom_sf(data = world_eez, size = 1, aes(color = Boundary), fill = NA, show.legend = FALSE) +
      scale_color_manual(values = "grey30") +
      coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
               ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
               expand = TRUE) +
      labs(title = (paste0(common_name," ","distribution"))) +
      theme_bw()
    
    plots[[i]] <- p
  } 

  if (bycatch == "turtle") {
  turt_plot <- (plots[[1]]) + (plots[[2]]) + (plots[[3]]) + (plots[[4]]) + (plots[[5]]) + (plots[[6]]) +
    plot_layout(ncol = 3, nrow = 2)
  
  turt_plot + plot_annotation(tag_levels = "i",
                              tag_suffix = ".",
                              title = "Distribution of Marine Turtles in the Pacific Ocean ",
                              caption = "Data from AquaMaps (2019)") +
    ggsave("pdfs/PacificTurtles.pdf", width = 20, height = 10, dpi = 300)
  
  } else if(bycatch == "mammal") {
    mam_plot1 <- (plots[[1]]) + (plots[[2]]) + (plots[[3]]) + (plots[[4]]) + (plots[[5]]) + (plots[[6]]) + (plots[[7]]) + (plots[[8]]) + (plots[[9]]) +
      plot_layout(ncol = 3, nrow = 3)
    
    mam_plot1 + plot_annotation(tag_levels = "i",
                                tag_suffix = ".",
                                title = "Distribution of Marine Mammals in the Pacific Ocean ",
                                subtitle = "1 of 3",
                                caption = "Data from AquaMaps (2019)") +
      ggsave("pdfs/PacificMammals1.pdf", width = 20, height = 10, dpi = 300)
    
    mam_plot2 <- (plots[[10]]) + (plots[[11]]) + (plots[[12]]) + (plots[[13]]) + (plots[[14]]) + (plots[[15]]) + (plots[[16]]) + (plots[[17]]) + (plots[[18]]) +
      plot_layout(ncol = 3, nrow = 3)
    
    mam_plot2 + plot_annotation(tag_levels = "i",
                                tag_suffix = ".",
                                title = "Distribution of Marine Mammals in the Pacific Ocean ",
                                subtitle = "2 of 3",
                                caption = "Data from AquaMaps (2019)") +
      ggsave("pdfs/PacificMammals2.pdf", width = 20, height = 10, dpi = 300)
    
    mam_plot3 <- (plots[[19]]) + (plots[[20]]) + (plots[[21]]) + (plots[[22]]) + (plots[[23]]) + (plots[[24]]) + (plots[[25]]) + (plots[[26]]) + (plots[[27]])
      plot_layout(ncol = 3, nrow = 3)
    
    mam_plot3 + plot_annotation(tag_levels = "i",
                                tag_suffix = ".",
                                title = "Distribution of Marine Mammals in the Pacific Ocean ",
                                subtitle = "3 of 3",
                                caption = "Data from AquaMaps (2019)") +
      ggsave("pdfs/PacificMammals3.pdf", width = 20, height = 10, dpi = 300)
  } else {print("fail")}
}


####################################################################################
####### Running function
####################################################################################

bycatch_distmap(path = "outputs/AQM_wflow/02b_bycatch",
     bycatch = "mammal")
