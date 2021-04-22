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

# Function is detailed in 10a_IUCNdata.R

source("scripts/09a_IUCNdata.R")

############################
# RUNNING THE CODE
############################

run29 <- iucn_int(input = "inputs/rasterfiles/IUCN/data_0.shp",
                  pu_file = "inputs/rdsfiles/PacificABNJGrid_05deg.rds",
                  outdir = "outputs/IUCN_wflow/09b_IUCNxPU/"
)

#################################
# PLOTTING THE DISTRIBUTIONS
#################################

# Defining generalities and objects for plotting
library(RColorBrewer)
library(patchwork)
pal_dist <- rev(brewer.pal(6, "Set3"))
world_sf <- st_read("inputs/shapefiles/PacificCenterLand/PacificCenterLand.shp") %>% 
  st_transform(crs = rob_pacific)

loggerhead <- readRDS("outputs/IUCN_wflow/09b_IUCNxPU/Caretta_caretta_IUCN.rds")
green <- readRDS("outputs/IUCN_wflow/09b_IUCNxPU/Chelonia_mydas_IUCN.rds")
leatherback <- readRDS("outputs/IUCN_wflow/09b_IUCNxPU/Dermochelys_coriacea_IUCN.rds")
hawksbill <- readRDS("outputs/IUCN_wflow/09b_IUCNxPU/Eretmochelys_imbricata_IUCN.rds")
kemp <- readRDS("outputs/IUCN_wflow/09b_IUCNxPU/Lepidochelys_kempii_IUCN.rds")
olive <- readRDS("outputs/IUCN_wflow/09b_IUCNxPU/Lepidochelys_olivacea_IUCN.rds")

loggerhead_plot <- ggplot() + 
                      geom_sf(data = loggerhead, colour = pal_dist[1]) + 
                      geom_sf(data = world_sf, fill = "grey20", colour = NA) +
                      geom_sf(data = world_eez, size = 1, aes(color = Boundary), fill = NA, show.legend = FALSE) +
                      scale_color_manual(values = "grey30") +
                      coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                               ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                               expand = TRUE) +
                      labs(title = "Loggerhead sea turtles' distribution") +
                      theme_bw()
loggerhead_plot

green_plot <- ggplot() + 
                  geom_sf(data = green, colour = pal_dist[2]) + 
                  geom_sf(data = world_sf, fill = "grey20", colour = NA) +
                  geom_sf(data = world_eez, size = 1, aes(color = Boundary), fill = NA, show.legend = FALSE) +
                  scale_color_manual(values = "grey30") +
                  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                           expand = TRUE) +
                  labs(title = "Green sea turtles' distribution") +
                  theme_bw()
green_plot

leatherback_plot <- ggplot() + 
                        geom_sf(data = leatherback, colour = pal_dist[3]) + 
                        geom_sf(data = world_sf, fill = "grey20", colour = NA) +
                        geom_sf(data = world_eez, size = 1, aes(color = Boundary), fill = NA, show.legend = FALSE) +
                        scale_color_manual(values = "grey30") +
                    coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                             ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                             expand = TRUE) +
                    labs(title = "Leatherback sea turtles' distribution") +
                    theme_bw()
leatherback_plot

hawksbill_plot <- ggplot() + 
                      geom_sf(data = hawksbill, colour = pal_dist[4]) + 
                      geom_sf(data = world_sf, fill = "grey20", colour = NA) +
                      geom_sf(data = world_eez, size = 1, aes(color = Boundary), fill = NA, show.legend = FALSE) +
                      scale_color_manual(values = "grey30") +
                      coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                               ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                               expand = TRUE) +
                      labs(title = "Hawksbill sea turtles' distribution") +
                      theme_bw()
hawksbill_plot

kemp_plot <- ggplot() + 
                geom_sf(data = kemp, colour = pal_dist[5]) + 
                geom_sf(data = world_sf, fill = "grey20", colour = NA) +
                geom_sf(data = world_eez, size = 1, aes(color = Boundary), fill = NA, show.legend = FALSE) +
                scale_color_manual(values = "grey30") +
                coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                         ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                         expand = TRUE) +
                labs(title = "Kemp's ridley sea turtles' distribution") +
                theme_bw()
kemp_plot

olive_plot <- ggplot() + 
                geom_sf(data = olive, colour = pal_dist[6]) + 
                geom_sf(data = world_sf, fill = "grey20", colour = NA) +
                geom_sf(data = world_eez, size = 1, aes(color = Boundary), fill = NA, show.legend = FALSE) +
                scale_color_manual(values = "grey30") +
                coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                         ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                         expand = TRUE) +
                labs(title = "Olive ridley sea turtles' distribution") +
                theme_bw()

# removing kemp_plot because there are no planning units
iucn_turtle_plots <- (loggerhead_plot | green_plot | leatherback_plot) / (hawksbill_plot | olive_plot) +
  plot_annotation(tag_levels = "i",
                  tag_suffix = ".",
                  title = "Distribution of Sea Turtles in the Pacific Ocean ",
                  caption = "Data from IUCN")
iucn_turtle_plots
ggsave("pdfs/PacificTurtles_IUCN.pdf", width = 20, height = 10, dpi = 300)
