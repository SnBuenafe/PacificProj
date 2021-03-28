#code modified by Tin Buenafe, 2021 (tinbuenafe@gmail.com)

# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

####################################################################################
####### Running the aqua_start function
####################################################################################
# Use the source argument to call the function into the R environment
source("scripts/02a_AQM_CSVfxn.R") 
# Run the function
test01 <- aqua_start(path = "inputs/AQM",
                     outdir = "outputs/AQM_wflow/02a_aqua_start/",
                     olayer = "surface",
                     prob_threshold = 0.5,
                     sp_env = 1,
                     type = "Pacific",
                     region = "inputs/rasterfiles/PacificCentred_05deg/PacificCentred_05deg.tif")


####################################################################################
####### Plotting species richness
####################################################################################

library(RColorBrewer)
library(ggplot2)
library(sf)
library(patchwork)
library(tidyverse)

rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


dt <- readRDS("outputs/AQM_wflow/02a_aqua_start/01_spp-richness_surface.rds")
final <- dt %>%
  dplyr::mutate(richness_log = log10(richness)) %>% 
  dplyr::mutate(rich_categ = ifelse(richness_log == 0, 1,
                                    ifelse(richness_log > 0 & richness_log <= 1, 2,
                                           ifelse(richness_log > 1 & richness_log <= 1.69897, 3,
                                                  ifelse(richness_log > 1.69897 & richness_log <= 2, 4, 
                                                         ifelse(richness_log > 2 & richness_log <= 2.69897, 5, 
                                                                ifelse(richness_log > 2.69897 & richness_log <= 3, 6, 7)))))))
st_crs(final) <- rob_pacific # just in case...

# Defining generalities
pal_rich <- rev(brewer.pal(7, "RdYlBu"))
cv_rich <- c("1", "1 - 10", "10 - 50", "50 - 100", "100 - 500", "500 - 1000", "> 1000")

world_sf <- readRDS("inputs/rdsfiles/WorldPacificCentred/WorldPacificCentred.rds")
st_crs(world_sf) <- rob_pacific

world_abnj <- st_read("inputs/shapefiles/PacificCenterABNJ/PacificCenterABNJ.shp")
st_crs(world_abnj) <- rob_pacific

world_eez <- st_read("inputs/shapefiles/PacificCenterEEZ/PacificCenterEEZ.shp")
st_crs(world_eez) <- rob_pacific
Boundary = "EEZ"
world_eez <- cbind(world_eez,Boundary)

# Plotting the figures
# Bndry parameter from 01_StudyArea_CreatingPUs.R
p <- ggplot() +
  geom_sf(data = final, aes(fill = rich_categ), color = NA) +
  scale_fill_gradientn(name = "Richness",
                       colours = pal_rich,
                       limits = c(1, 7),
                       breaks = seq(1, 7, 1),
                       labels = cv_rich) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  geom_sf(data = world_eez, size = 1, aes(color = Boundary), fill = NA, show.legend = FALSE) +
  scale_color_manual(values = "grey30") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  theme_bw()

p1 <- ggplot() +
  geom_sf(data = final, aes(fill = rich_categ), color = NA) +
  scale_fill_gradientn(name = "Richness",
                       colours = pal_rich,
                       limits = c(1, 7),
                       breaks = seq(1, 7, 1),
                       labels = cv_rich) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  geom_sf(data = world_eez, size = 1, aes(color = Boundary), fill = "grey69") +
  scale_color_manual(values = "grey30") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  theme_bw()

#arranging plots, adding labels
plots <- p + p1 + 
  plot_layout(guides = 'collect') +
  plot_annotation(
    title = "Richness of species present within ABNJ of Pacific Ocean",
    subtitle = "ABNJ: areas outside the boundaries of EEZ",
    caption = "Data from AquaMaps (2019)"
  )
plots

#saving plot
plots +
  ggsave("pdfs/PacificRichness.pdf", width = 20, height = 10, dpi = 300) +
  ggsave("pdfs/PacificRichness.jpg", width = 20, height = 10, dpi = 300)

