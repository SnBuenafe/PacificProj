# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code intersects the features with the Longhurst provinces.
# Creates new unique "features" with these intersects.
# Saves this new sf object as an .rds

longhurst <- readRDS("outputs/Provinces/PacificABNJGrid_05deg_Longhurst.rds")
features <- readRDS("outputs/AQM_wflow/03b_features/bycatch_features_surface.rds")

temp <- st_intersection(longhurst, features) %>% 
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) 

temp1 <- temp %>% 
  dplyr::mutate(feature = paste0(province,"_",feature_names)) 

temp2 <- temp1 %>% 
  dplyr::group_by(province) %>% 
  dplyr::select(-cellsID, -area_km2)

# save as an RDS
saveRDS(temp2, "outputs/AQM_wflow/03c_featprov/bycatch_provinces.rds")

# to check if it works
ggplot()+
  geom_sf(data = temp1, aes(color = feature))

# to calculate for the total area per feature (per province)
total_PU_area <- 2667.6 * 31917
temp3 <- temp1 %>% 
  dplyr::group_by(feature) %>% 
  dplyr::summarise(total_area = sum(area_km2), percent_area = (sum(area_km2)/total_PU_area)*100)
