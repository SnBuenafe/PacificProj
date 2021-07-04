# script by Tin Buenafe, 2021 (tinbuenafe@gmail.com)

# This code intersects the IUCN features with the planning units.

# The function fFeaturesInt() requires the following inputs:
# 1. path: folder's name where species conservation feature files are located
# 2. outdir: where to put the final sf-.rds object
# 3. pu_shp: .shp or .rds of the PUs
# 4. data: "global", "pacific", "AQM", "IUCN"

source("scripts/02_Features_fFeaturesInt.R")

#######################################
#### Running fFeaturesInt function ####
#######################################
#running with .rds
IUCN_FeatInt_run01 <- fFeaturesInt(path = "outputs/03_IUCN/03b_fIUCNIntersect/",
                                  outdir = "outputs/03_IUCN/03d_fFeaturesInt/",
                                  pu_shp = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
                                  data = "IUCN")
IUCN_FeatInt_run01

#################################
# PLOTTING THE DISTRIBUTIONS
#################################

# Defining generalities and objects for plotting
library(patchwork)
world_sf <- readRDS("outputs/01_StudyArea/01a_StudyArea/PacificCenterLand.rds")

source("scripts/study_area/fCreateRobinsonBoundary.R")
Bndry <- fCreateRobinsonBoundary(west = 78, east = 140, north = 51, south = 60)

loggerhead <- filter(IUCN_FeatInt_run01, feature_names == 'Caretta_caretta_IUCN')
green <- filter(IUCN_FeatInt_run01, feature_names == 'Chelonia_mydas_IUCN')
leatherback <- filter(IUCN_FeatInt_run01, feature_names == 'Dermochelys_coriacea_IUCN')
hawksbill <- filter(IUCN_FeatInt_run01, feature_names == 'Eretmochelys_imbricata_IUCN')
olive <- filter(IUCN_FeatInt_run01, feature_names == 'Lepidochelys_olivacea_IUCN')

loggerhead_plot <- ggplot() + 
  geom_sf(data = loggerhead, fill = 'limegreen', size = 0.08, color = 'grey20') + 
  geom_sf(data = world_sf, fill = "grey20", colour = NA) +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Loggerhead sea turtles") +
  theme_bw()

green_plot <- ggplot() + 
  geom_sf(data = green, fill = 'olivedrab', size = 0.08, color = 'grey20') + 
  geom_sf(data = world_sf, fill = "grey20", colour = NA) +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Green sea turtles") +
  theme_bw()

leatherback_plot <- ggplot() + 
  geom_sf(data = leatherback, fill = 'darkseagreen', size = 0.08, color = 'grey20') + 
  geom_sf(data = world_sf, fill = "grey20", colour = NA) +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Leatherback sea turtles") +
  theme_bw()

hawksbill_plot <- ggplot() + 
  geom_sf(data = hawksbill, fill = 'darkolivegreen3', size = 0.08, color = 'grey20') + 
  geom_sf(data = world_sf, fill = "grey20", colour = NA) +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Hawksbill sea turtles") +
  theme_bw()

olive_plot <- ggplot() + 
  geom_sf(data = olive, fill = 'springgreen1', size = 0.08, color = 'grey20') + 
  geom_sf(data = world_sf, fill = "grey20", colour = NA) +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  labs(title = "Olive ridley sea turtles") +
  theme_bw()

# plotting all the plots using patchwork
iucn_turtle_plots <- (loggerhead_plot | green_plot | leatherback_plot) / (hawksbill_plot | olive_plot) +
  plot_annotation(tag_levels = "A",
                  tag_suffix = ")",
                  caption = 'shapefiles from IUCN Red List (2021)')
iucn_turtle_plots
ggsave("pdfs/03_IUCN/PacificTurtles_IUCN.pdf", width = 29.7, height = 21, dpi = 300)

## plotting richness
IUCN_FeatInt_run01 <- readRDS("outputs/03_IUCN/03d_fFeaturesInt/bycatch_features.rds")
temp_bycatch <- IUCN_FeatInt_run01 %>% 
  group_by(cellsID) %>% 
  dplyr::summarise(freq = length(unique(feature_names)))
temp_bycatch

bycatch_richness <- ggplot() +
  geom_sf(data = world_robinson, color = "grey20", fill="grey20", size = 0.1, show.legend = FALSE) +
  geom_sf(data = temp_bycatch, aes(fill = as.factor(freq)), colour = "grey64", size = 0.1) + 
  scale_fill_brewer(name = 'Bycatch richness',
                    type = 'seq',
                    palette = 'BuGn',
                    aesthetics = 'fill') +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), # Set limits based on Bndry bbox
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25),
        legend.key.width = unit(1,"cm"))
bycatch_richness
ggsave("pdfs/03_IUCN/PacificTurtles_IUCN_richness.png", width = 20, height = 10, dpi = 600)



