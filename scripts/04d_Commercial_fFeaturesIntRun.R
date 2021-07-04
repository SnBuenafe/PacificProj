# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This uses the same function as the function that intersects AquaMaps distribution maps with the PUs.
# See 03_Features_fFeaturesInt.R for full function,
# What this code does is it intersects the commercial features with the PUs
# This makes distribution maps.

source("scripts/02_Features_fFeaturesInt.R")

global_features <- fFeaturesInt(path = "outputs/04_Commercial/04b_fCommercialFeat/",
                                outdir = "outputs/04_Commercial/04d_fFeaturesInt/Global/",
                                pu_shp = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_04deg.rds",
                                data = "global")

# plotting richness
global_features <- readRDS("outputs/04_Commercial/04d_fFeaturesInt/Global/commercial_features.rds")
temp_commercial <- global_features %>% 
  group_by(cellsID) %>% 
  dplyr::summarise(freq = length(unique(feature_names)))
temp_commercial

commercial_richness <- ggplot() +
  geom_sf(data = world_robinson, color = "grey20", fill="grey20", size = 0.1, show.legend = FALSE) +
  geom_sf(data = temp_commercial, aes(fill = as.factor(freq)), colour = "grey64", size = 0.1) + 
  scale_fill_brewer(name = "Commercial species' spawning ground richness",
                    type = 'seq',
                    palette = 'PuBu',
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
commercial_richness
ggsave("pdfs/04_Commercial/PacificCommercial_Mercer_richness.png", width = 20, height = 10, dpi = 600)
