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

# The following must also be defined prior to running the code.
# 1. Bndry: boundaries of study area;
# 2. world_eez: eez in robinson's projection;
# 3. world_sf: landmasses in robinson's projection;
# 4. rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# Function is found in 02d.

source("scripts/02d_AQM_DistMaps.R")

####################################################################################
####### Running function
####################################################################################

turtle_plots <- bycatch_distmap(path = "outputs/AQM_wflow/02b_bycatch", 
                                bycatch = "turtle",
                                land_file = "inputs/shapefiles/PacificCenterLand/PacificCenterLand.shp",
                                eez_file = "inputs/shapefiles/PacificCenterEEZ/PacificCenterEEZ.shp",
                                north = 51,
                                south = 60,
                                west = 78,
                                east = 140)

#plots for turtles
turt_plot <- (turtle_plots[[1]]) + (turtle_plots[[2]]) + (turtle_plots[[3]]) + (turtle_plots[[4]]) + (turtle_plots[[5]]) + 
  (turtle_plots[[6]]) +
  plot_layout(ncol = 3, nrow = 2)

turt_plot + plot_annotation(tag_levels = "i",
                            tag_suffix = ".",
                            title = "Distribution of Marine Turtles in the Pacific Ocean ",
                            caption = "Data from AquaMaps (2019)") #+
#  ggsave("pdfs/PacificTurtles.pdf", width = 20, height = 10, dpi = 300)

#plots for mammals
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

