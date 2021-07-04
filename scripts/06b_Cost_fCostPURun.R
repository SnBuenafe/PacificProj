# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This code intersects the cost layer and the study area (in PUs).
# creates a .rds file and .shp files of the PUs x cost layer.
# Inputs include the following:
# 1. input: raster file of the cost layer
# 2. pu_shp: .shp or .rds file of the PUs
# 3. outdir: path of the output
# 4. layer: "all" or "pelagics"
# 5. stack_num: if layer == "all": NA, if layer == "pelagics", input the stack number (if multiple use c())

# Function is found in 06a

source("scripts/06a_Cost_fCostPU.R")

########################################
# RUNNING THE FUNCTION #
########################################
# running using large + medium pelagics (large = 14, medium = 19)
COST_largexmedium <- fCostPU(input = "inputs/rasterfiles/CostLayer/Cost_RasterStack_byFunctionalGroup.grd",
                      pu_shp = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
                      outdir = "outputs/06_Cost/Large_Medium/",
                      layer = "pelagics",
                      stack_num = c(14,19),
                      window_size = 5
)

###################################################
# Defining generalities for plotting #
###################################################
library(RColorBrewer)
library(patchwork)
# Defining palette
myPalette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))
sc <- scale_colour_gradientn(name = "log10(cost)", colours = myPalette(100), limits=c(0, 4), aesthetics = c("color","fill"))

rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
world_sf <- st_read("inputs/shapefiles/PacificCenterLand/PacificCenterLand.shp") %>% 
  st_transform(crs = rob_pacific)
source("scripts/study_area/fCreateRobinsonBoundary.R")
Bndry <- fCreateRobinsonBoundary(west = 78, east = 140, north = 51, south = 60)

###################################################
# PLOTTING THE INTERSECTION OF PUs AND COST LAYER #
###################################################
#############################################
#### Cost Layer: Large + Medium Pelagics ####
#############################################
costplot_largexmedium <- ggplot()+
                            geom_sf(data = COST_largexmedium, aes(color = cost_log, fill = cost_log)) +
                            sc +
                            geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
                            coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
                                     ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
                                     expand = TRUE) +
                            theme_bw() +
                            theme(axis.text.x = element_text(size = 25),
                                  axis.text.y = element_text(size = 25),
                                  legend.title = element_text(size = 25),
                                  legend.text = element_text(size = 25),
                                  legend.key.width = unit(1,"cm"))
  
costplot_largexmedium +
  labs(caption = "2006 - 2015 catch data from Watson (2017) and ex-vessel prices from Tai et al. (2017)")
ggsave("pdfs/06_Cost/CostLayer_LargexMedium.pdf", width = 20, height = 10, dpi = 300)
ggsave("pdfs/06_Cost/CostLayer_LargexMedium.png", width = 20, height = 10, dpi = 600)
