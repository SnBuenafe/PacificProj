# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

####################################################################################
####### Running features_pus function
####################################################################################
# Use the source argument to call the function into the R environment
source("scripts/03a_AQM_Featuresfxn.R") 
#running with .rds
run_a <- features_pus(path = "outputs/AQM_wflow/02b_bycatch",
                      outdir = "outputs/AQM_wflow/03b_features/",
                      pu_shp = "inputs/rdsfiles/PacificABNJGrid_05deg.rds",
                      olayer = "surface")
run_a

#running with .shp
run_b <- features_pus(path = "outputs/AQM_wflow/02b_bycatch",
                      outdir = "outputs/AQM_wflow/03b_features/",
                      pu_shp = "inputs/shapefiles/PacificABNJGrid_05deg/PacificABNJGrid_05deg.shp",
                      olayer = "surface")

#creating data matrix for the total areas of the features (and their percentages)
#total number of features: 31917 features
#area per cell: 2667.6 km^2
total_PU_area <- 2667.6 * 31917
temp <- as.data.frame(run_a) %>% 
  rename(features = feature_names) %>% 
  group_by(features) %>% 
  summarize(total_area = sum(area_km2), percentage = (sum(area_km2)/total_PU_area)*100) 
print(temp, n = 1000)

# just to test it out
ggplot()+
  geom_sf(data = run_a, aes(color = feature_names)) +
  theme_bw()# +
#  ggsave("pdfs/features.pdf", width = 20, height = 10, dpi = 300)

