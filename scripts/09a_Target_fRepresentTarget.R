# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# most are modified from IBM's code.

# This code assigns the representation targets for each feature depending on the IUCN category of the species.
# If the species belongs in a threatened category (i.e. "EX","EW","CR","EN","VU"), the target is 100%.
# If the species does not belong in a threatened category, the target is computed as follows:
# target_max * (PUs / PUtotal) * (target_max - target_min)
# Note that the areas of the features here are those belonging to the lower quartile of the RCE | velocity 
# It saves the summarized output as an .rds file 

# The function represent_target() requires the following inputs:
# 1. number_PU = total number of PUs in study area.
# 2. target_max = in proportion e.g. 1
# 3. target_min = in proportion e.g. 0
# 4. file_spec_info = .xlsx file of the species information (with the codes and the scientific names)
# 5. inpdir = directory where the .rds files of the conservation features for each scenario are
# 6. scenario = climate scenario (e.g. SSP126)
# 7. outdir = directory where the .rds files will be saved.
# 8. prov = TRUE/FALSE (including provinces or not)

# The function is run in 09b and 09d for different scenarios (for climate-smart) and 09c and 09e for climate-uninformed.

fRepresentTarget <- function(number_PU, target_max, target_min, file_spec_info, inpdir, scenario, outdir, prov, ...) {

  ##################################
  ### Defining the main packages ###
  ##################################
  list.of.packages <- c("rredlist", "tidyverse", "readxl", "sf", "magrittr")
  # If is not installed, install the pacakge
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # Load packages
  lapply(list.of.packages, require, character.only = TRUE)
  
  ############################
  ### Loading generalities ###
  ############################
  rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  ####################
  ### Calling data ###
  ####################
  # Calling .xlsx file of species information
  spec_info <- read_xlsx(file_spec_info, na = "NA")
      
  pattern_list <- c(paste0("\\Q",scenario,"\\E"))
  call_temp <- list.files(path = inpdir, pattern = pattern_list)
      
  temp_categ <- list()
      
  #########################
  ### Assigning targets ###
  #########################
  for(j in 1:length(call_temp)) {
      for(i in 1:nrow(spec_info)) {
        temp_categ[i] <- rl_search(paste(spec_info$genus[i], spec_info$species[i], sep = " "))$result[13]
      }
      categories <- do.call(rbind, temp_categ)
      spec_info1 <- spec_info %>% mutate(category = categories[,1])
          
      # Calling feature x 25 perc file
      feature <- readRDS(paste0(inpdir,call_temp[j]))
      if(prov == FALSE){
        feature %<>% as_tibble() %>% 
          rename(species = new_features) %>% 
          st_as_sf(crs = rob_pacific)
      }else{ }
          
      # Match the categories from the data from IUCN to the shapefiles with all the new features (prov x feature; filtered < 25 percentile of climate features)
      spec_info2 <- spec_info1 %>% 
        dplyr::select(code, category) %>% 
        rename(species = code)
      feature1 <- left_join(feature, spec_info2, by = "species")
      if(prov == FALSE){
        feature1 %<>% rename(new_features = species)
      }else{ }
          
      # Setting targets for each conservation feature
      feature2 <- feature1 %>% 
        mutate(area_km2 = as.numeric(st_area(feature1)/1e+06)) %>% 
        group_by(new_features) %>% 
        summarize(total_area = unique(total_area), category = first(category))
          
      total_PU_area = number_PU*2667.6
      target_max = target_max
      target_min = target_min
          
      trans_target <- list()
          
      for(x in 1:nrow(feature2)) {
        if(feature2$category[x] %in% c("EX","EW","CR","EN","VU")) {
          trans_target[[x]] <- target_max*100
        }else {
          trans_target[[x]] <- target_max*(feature2$total_area[x]/total_PU_area)*(target_max-target_min)*100
        }
      }
          
      target_list <- do.call(rbind, trans_target)
          
      feat_targ <- feature2 %>% 
        mutate(target = target_list[,1])
          
      saveRDS(feat_targ, paste0(outdir,scenario,"/","target_",unlist(strsplit(basename(call_temp[j]),"_"))[1],".rds"))
  }
}
