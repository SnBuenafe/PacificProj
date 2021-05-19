# with some modifications by Tin Buenafe, 2021 (tinbuenafe@gmail.com)

# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: Create a general dataframe that would be the core for generate input file for prioritizr analyses (conventional MARXAN)
# path: folder's name where species conservation feature files are located
# outdir: where to put the final sf-.rds object
# pu_shp: .shp or .rds of the PUs
# data: "global", "pacific", "AQM"

source("scripts/03_Features_fFeaturesInt.R") 

#######################################
#### Running fFeaturesInt function ####
#######################################
#running with .rds
AQM_FeatInt_run01 <- fFeaturesInt(path = "outputs/02_RawAQM/02b_fAquaStart_filtered/",
                                  outdir = "outputs/03_AQM/03a_fFeaturesInt/",
                                  pu_shp = "outputs/01_StudyArea/01a_StudyArea/PacificABNJGrid_05deg.rds",
                                  data = "AQM")
AQM_FeatInt_run01

########################
##### TRY PLOTTING #####
########################
#creating data matrix for the total areas of the features (and their percentages)
#total number of features: 31917 features
#area per cell: 2667.6 km^2
#total_PU_area <- 2667.6 * 31917
#temp <- as.data.frame(run_a) %>% 
#  rename(features = feature_names) %>% 
#  group_by(features) %>% 
#  summarize(total_area = sum(area_km2), percentage = (sum(area_km2)/total_PU_area)*100) 
#print(temp, n = 1000)

# just to test it out
#ggplot()+
#  geom_sf(data = run_a, aes(color = feature_names)) +
#  theme_bw()# +
#  ggsave("pdfs/features.pdf", width = 20, height = 10, dpi = 300)