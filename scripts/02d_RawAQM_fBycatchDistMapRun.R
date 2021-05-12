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

# Function is found in 02c.

source("scripts/02c_RawAQM_fBycatchDistMap.R")

######################################
#### Running function for turtles ####
######################################

turtle_plots <- fBycatchDistMap(path = "outputs/02_RawAQM/02b_fAquaStart_filtered", 
                                bycatch = "turtle",
                                land_file = "outputs/01_StudyArea/01a_StudyArea/PacificCenterLand.rds",
                                eez_file = "outputs/01_StudyArea/01a_StudyArea/PacificCenterEEZ.rds",
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
#  ggsave("pdfs/02_RawAQM/02d_fBycatchDistMapPacificTurtles.pdf", width = 20, height = 10, dpi = 300)

######################################
#### Running function for mammals ####
######################################

mammal_plots <- fBycatchDistMap(path = "outputs/02_RawAQM/02b_fAquaStart_filtered", 
                                bycatch = "mammal",
                                land_file = "outputs/01_StudyArea/01a_StudyArea/PacificCenterLand.rds",
                                eez_file = "outputs/01_StudyArea/01a_StudyArea/PacificCenterEEZ.rds",
                                north = 51,
                                south = 60,
                                west = 78,
                                east = 140)

# plot for mammals
mam_plot1 <- (mammal_plots[[1]]) + (mammal_plots[[2]]) + (mammal_plots[[3]]) + (mammal_plots[[4]]) + (mammal_plots[[5]]) + 
  (mammal_plots[[6]]) + (mammal_plots[[7]]) + (mammal_plots[[8]]) + (mammal_plots[[9]]) +
  plot_layout(ncol = 3, nrow = 3)

mam_plot1 + plot_annotation(tag_levels = "i",
                            tag_suffix = ".",
                            title = "Distribution of Marine Mammals in the Pacific Ocean ",
                            subtitle = "1 of 3",
                            caption = "Data from AquaMaps (2019)") #+
#  ggsave("pdfs/02_RawAQM/02d_fBycatchDistMapPacificMammals1.pdf", width = 20, height = 10, dpi = 300)

mam_plot2 <- (mammal_plots[[10]]) + (mammal_plots[[11]]) + (mammal_plots[[12]]) + (mammal_plots[[13]]) + (mammal_plots[[14]]) + (mammal_plots[[15]]) + 
  (mammal_plots[[16]]) + (mammal_plots[[17]]) + (mammal_plots[[18]]) +
  plot_layout(ncol = 3, nrow = 3)

mam_plot2 + plot_annotation(tag_levels = "i",
                            tag_suffix = ".",
                            title = "Distribution of Marine Mammals in the Pacific Ocean ",
                            subtitle = "2 of 3",
                            caption = "Data from AquaMaps (2019)") #+
#  ggsave("pdfs/02_RawAQM/02d_fBycatchDistMapPacificMammals2.pdf", width = 20, height = 10, dpi = 300)

mam_plot3 <- (mammal_plots[[19]]) + (mammal_plots[[20]]) + (mammal_plots[[21]]) + (mammal_plots[[22]]) + (mammal_plots[[23]]) + 
  (mammal_plots[[24]]) + (mammal_plots[[25]]) + (mammal_plots[[26]]) + (mammal_plots[[27]]) +
  plot_layout(ncol = 3, nrow = 3)

mam_plot3 + plot_annotation(tag_levels = "i",
                            tag_suffix = ".",
                            title = "Distribution of Marine Mammals in the Pacific Ocean ",
                            subtitle = "3 of 3",
                            caption = "Data from AquaMaps (2019)") #+
#  ggsave("pdfs/02_RawAQM/02d_fBycatchDistMapPacificMammals3.pdf", width = 20, height = 10, dpi = 300)

