# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code creates a function that intersects the IUCN sea turtle data (feature) with planning units (PUs).
# creates a .rds file PUs x features layer.
# Function is run at the bottom of the file.

# Inputs include the following:
# 1. input = IUCN .shp file;
# 2. pu_file = PUs .rds file;
# 3. outdir = path of the outputs of the function.

# Function is in 04a.
source("scripts/04a_IUCN_fIUCNIntersect.R")

############################
# RUNNING THE CODE
############################

IUCN_run01 <- fIUCNIntersect(input = "inputs/rasterfiles/IUCN/data_0.shp",
                  pu_file = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
                  outdir = "outputs/04_IUCN/04b_fIUCNIntersect/"
)

#################################
# PLOTTING THE DISTRIBUTIONS
#################################

# Defining generalities and objects for plotting
library(RColorBrewer)
library(patchwork)
pal_dist <- rev(brewer.pal(6, "Set3"))
world_sf <- readRDS("outputs/01_StudyArea/01a_StudyArea/PacificCenterLand.rds")

source("scripts/study_area/fCreateRobinsonBoundary.R")
Bndry <- fCreateRobinsonBoundary(west = 78, east = 140, north = 51, south = 60)

loggerhead <- readRDS("outputs/04_IUCN/04b_fIUCNIntersect/Caretta_caretta_IUCN.rds")
green <- readRDS("outputs/04_IUCN/04b_fIUCNIntersect/Chelonia_mydas_IUCN.rds")
leatherback <- readRDS("outputs/04_IUCN/04b_fIUCNIntersect/Dermochelys_coriacea_IUCN.rds")
hawksbill <- readRDS("outputs/04_IUCN/04b_fIUCNIntersect/Eretmochelys_imbricata_IUCN.rds")
kemp <- readRDS("outputs/04_IUCN/04b_fIUCNIntersect/Lepidochelys_kempii_IUCN.rds")
olive <- readRDS("outputs/04_IUCN/04b_fIUCNIntersect/Lepidochelys_olivacea_IUCN.rds")

loggerhead_plot <- ggplot() + 
                      geom_sf(data = loggerhead, colour = pal_dist[1]) + 
                      geom_sf(data = world_sf, fill = "grey20", colour = NA) +
                      geom_sf(data = world_eez, size = 1, aes(color = Boundary), fill = NA, show.legend = FALSE) +
                      scale_color_manual(values = "grey30") +
                      coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                               ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                               expand = TRUE) +
                      labs(title = "Loggerhead sea turtles") +
                      theme_bw()

green_plot <- ggplot() + 
                  geom_sf(data = green, colour = pal_dist[2]) + 
                  geom_sf(data = world_sf, fill = "grey20", colour = NA) +
                  geom_sf(data = world_eez, size = 1, aes(color = Boundary), fill = NA, show.legend = FALSE) +
                  scale_color_manual(values = "grey30") +
                  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                           expand = TRUE) +
                  labs(title = "Green sea turtles") +
                  theme_bw()

leatherback_plot <- ggplot() + 
                        geom_sf(data = leatherback, colour = pal_dist[3]) + 
                        geom_sf(data = world_sf, fill = "grey20", colour = NA) +
                        geom_sf(data = world_eez, size = 1, aes(color = Boundary), fill = NA, show.legend = FALSE) +
                        scale_color_manual(values = "grey30") +
                    coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                             ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                             expand = TRUE) +
                    labs(title = "Leatherback sea turtles") +
                    theme_bw()

hawksbill_plot <- ggplot() + 
                      geom_sf(data = hawksbill, colour = pal_dist[4]) + 
                      geom_sf(data = world_sf, fill = "grey20", colour = NA) +
                      geom_sf(data = world_eez, size = 1, aes(color = Boundary), fill = NA, show.legend = FALSE) +
                      scale_color_manual(values = "grey30") +
                      coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                               ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                               expand = TRUE) +
                      labs(title = "Hawksbill sea turtles") +
                      theme_bw()

kemp_plot <- ggplot() + 
                geom_sf(data = kemp, colour = pal_dist[5]) + 
                geom_sf(data = world_sf, fill = "grey20", colour = NA) +
                geom_sf(data = world_eez, size = 1, aes(color = Boundary), fill = NA, show.legend = FALSE) +
                scale_color_manual(values = "grey30") +
                coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                         ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                         expand = TRUE) +
                labs(title = "Kemp's ridley sea turtles") +
                theme_bw()

olive_plot <- ggplot() + 
                geom_sf(data = olive, colour = pal_dist[6]) + 
                geom_sf(data = world_sf, fill = "grey20", colour = NA) +
                geom_sf(data = world_eez, size = 1, aes(color = Boundary), fill = NA, show.legend = FALSE) +
                scale_color_manual(values = "grey30") +
                coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                         ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                         expand = TRUE) +
                labs(title = "Olive ridley sea turtles") +
                theme_bw()

# removing kemp_plot because there are no planning units
iucn_turtle_plots <- (loggerhead_plot | green_plot | leatherback_plot) / (hawksbill_plot | olive_plot) +
  plot_annotation(tag_levels = "i",
                  tag_suffix = ".",
                  title = "Distribution of Sea Turtles in the Pacific Ocean ",
                  caption = "Data from IUCN")
iucn_turtle_plots
ggsave("pdfs/04_IUCN/PacificTurtles_IUCN.pdf", width = 29.7, height = 21, dpi = 300)
