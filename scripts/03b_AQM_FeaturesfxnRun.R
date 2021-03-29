# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

####################################################################################
####### Running features_pus function
####################################################################################
# Use the source argument to call the function into the R environment
source("scripts/03a_AQM_Featuresfxn.R") 
run01 <- features_pus(path = "outputs/AQM_wflow/02b_bycatch",
                      outdir = "outputs/AQM_wflow/03b_features/",
                      pu_shp = "inputs/rdsfiles/PacificABNJGrid_05deg.rds",
                      olayer = "surface")
run01

ggplot()+
  geom_sf(data = run01, aes(color = as.factor(feature_names))) +
  theme_bw() +
  ggsave("pdfs/features.pdf", width = 20, height = 10, dpi = 300)

