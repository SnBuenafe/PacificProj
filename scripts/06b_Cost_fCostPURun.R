# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

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
#COST_all <-  fCostPU(input = "inputs/rasterfiles/Costlayer/02-epipelagic_Cost_Raster_Sum.tif",
#                  pu_shp = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
#                  outdir = "outputs/06_Cost/All/",
#                  layer = "all",
#                  stack_num = NA
#)

# running using just large pelagics
#COST_large <- fCostPU(input = "inputs/rasterfiles/CostLayer/Cost_RasterStack_byFunctionalGroup.grd",
#                  pu_shp = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
#                  outdir = "outputs/06_Cost/Large/",
#                  layer = "pelagics",
#                  stack_num = 19
#)

# running using just medium pelagics
#COST_medium <- fCostPU(input = "inputs/rasterfiles/CostLayer/Cost_RasterStack_byFunctionalGroup.grd",
#                      pu_shp = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
#                      outdir = "outputs/06_Cost/Medium/",
#                      layer = "pelagics",
#                      stack_num = 14
#)

# running using large + medium pelagics
COST_largexmedium <- fCostPU(input = "inputs/rasterfiles/CostLayer/Cost_RasterStack_byFunctionalGroup.grd",
                      pu_shp = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
                      outdir = "outputs/06_Cost/Large_Medium/",
                      layer = "pelagics",
                      stack_num = c(14,19)
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
#########################
#### Cost Layer: All ####
#########################
# costplot_all <- ggplot()+
#                  geom_sf(data = COST_all, aes(color = cost_log, fill = cost_log)) +
#                  sc +
#                  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
#                  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
#                           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
#                           expand = TRUE) +
#                  theme_bw()
#costplot_all +
#  labs(title = "Cost Layer", subtitle = "Sum of costs of all species", 
#       caption = "Used 2006 - 2015 catch data from Watson (2017) and ex-vessel prices from Tai et al. (2017)")

# ggsave("pdfs/06_Cost/CostLayer_All.pdf", width = 20, height = 10, dpi = 300)

####################################
#### Cost Layer: Large Pelagics ####
####################################
#costplot_large <- ggplot()+
#                    geom_sf(data = COST_large, aes(color = cost_log, fill = cost_log)) +
#                    sc +
#                    geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
#                    coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
#                             ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
#                             expand = TRUE) +
#                    theme_bw()
#costplot_large +
#  labs(title = "Cost Layer", subtitle = "Sum of costs of all large pelagics",
#       caption = "Used 2006 - 2015 catch data from Watson (2017) and ex-vessel prices from Tai et al. (2017)")

#ggsave("pdfs/06_Cost/CostLayer_Large.pdf", width = 20, height = 10, dpi = 300)

#####################################
#### Cost Layer: Medium Pelagics ####
#####################################
#costplot_medium <- ggplot()+
#                      geom_sf(data = COST_medium, aes(color = cost_log, fill = cost_log)) +
#                      sc +
#                      geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
#                      coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
#                               ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
#                               expand = TRUE) +
#                      theme_bw()
#costplot_medium +
#  labs(title = "Cost Layer", subtitle = "Sum of costs of all medium pelagics",
#       caption = "Used 2006 - 2015 catch data from Watson (2017) and ex-vessel prices from Tai et al. (2017)")

#ggsave("pdfs/06_Cost/CostLayer_Medium.pdf", width = 20, height = 10, dpi = 300)

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
                            theme_bw()
costplot_largexmedium +
  labs(title = "Cost Layer", subtitle = "Sum of costs of all medium and large pelagics",
       caption = "Used 2006 - 2015 catch data from Watson (2017) and ex-vessel prices from Tai et al. (2017)")

#ggsave("pdfs/06_Cost/CostLayer_LargexMedium.pdf", width = 20, height = 10, dpi = 300)

##################################
#### Plotting all cost layers ####
##################################
#p1 <- costplot_all + labs(title = "All")
#p2 <- costplot_large + labs(title = "Large Pelagics")
#p3 <- costplot_medium + labs(title = "Medium Pelagics")
#p4 <- costplot_largexmedium + labs(title = "Large + Medium Pelagics")

#(p1 | p2) / (p3 | p4) +
#  plot_layout(guides = "collect") +
#  plot_annotation(tag_levels = 'i', title = "Cost Layers", caption = "Used 2006 - 2015 catch data from Watson (2017) and ex-vessel prices from Tai et al. (2017)")

#ggsave("pdfs/06_Cost/CostLayers_Compiled.pdf", width = 20, height = 10, dpi = 300)

###################################################
# CHECKING NEW COST LAYER #
###################################################

#r <- stack("inputs/rasterfiles/CostLayer/Cost_RasterStack_byFunctionalGroup.grd")
#names(r)

#r_unstack <- unstack(r)
#large_pelagics <- r_unstack[[19]]