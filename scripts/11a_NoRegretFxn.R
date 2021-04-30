# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This function creates no-regret closures, requiring the ff. inputs:
# 1. inpdir: where the prioritizr_run results are found.
# 2. outdir: where to save the results of this function.
# 3. target: e.g. Target100

# Runs are found in 11b_NoRegretRunAQM, and 11c_NoRegretRunIUCN.

create_noregret <- function(inpdir, outdir, target, ...) {
  
  ##################################
  ### Defining the main packages ###
  ##################################
  
  # List of packages that we will use
  list.of.packages <- c("tidyverse", "sf")
  # If is not installed, install the pacakge
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # Load packages
  lapply(list.of.packages, require, character.only = TRUE)
  
  #######################
  #### Calling Files ####
  #######################
  files <- list.files(path = inpdir, pattern = "*.rds")
  # Calling the Planning Unit files.
  temp_pu <- readRDS("inputs/rdsfiles/PacificABNJGrid_05deg.rds")
  planning_units <- temp_pu %>%
    dplyr::mutate(cellsID = 1:nrow(temp_pu)) %>% 
    as_tibble()
  
  ###########################
  #### Modifying tibbles ####
  ###########################
  for(i in 1:length(files)){
    x <- unlist(strsplit(unlist(strsplit(basename(files[i]),"_"))[2],"[.]"))[1]
    x_file <- list.files(path = inpdir, pattern = x)
    temp_file <- readRDS(paste0(inpdir, x_file))
    
    temp_plan <- temp_file %>% 
      select(-cellsID.2.x, -province.y, -prov_descr.y, -area_km2.y, -species.y, -total_area.y, -cellsID.2.y, -velocity.y, -velo_tvalue.y,
             -RCE.y, -RCE_tvalue.y) %>% 
      rename(province = province.x, prov_descr = prov_descr.x, area_km2 = area_km2.x, species = species.x, total_area = total_area.x,
             velocity = velocity.x, velo_tvalue = velo_tvalue.x, RCE = RCE.x, RCE_tvalue = RCE_tvalue.x, solution = solution_1)
    
    assign(x = paste0("plan_",x), value = temp_plan)
  }
  
  temp <- st_intersection(plan_SSP126, plan_SSP245) %>% 
    dplyr::filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
  
  temp_final <- st_intersection(temp, plan_SSP585) %>% 
    dplyr::filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))

  final_list <- temp_final %>% 
    dplyr::filter(solution == TRUE & solution.1 == TRUE & solution.2 == TRUE) %>% 
    select(cellsID, area_km2, cost, solution, geometry) %>% 
    as_tibble()

  #######################
  #### Join with PUs ####
  #######################
  no_regret <- left_join(planning_units, final_list, by = "cellsID") %>% 
    dplyr::mutate(solution = replace_na(solution, "FALSE")) %>% 
    select(-geometry.y) %>% 
    rename(geometry = geometry.x) %>% 
    st_as_sf(sf_column_name = "geometry")
  
  no_regret <- no_regret %>% 
    mutate(area_km2 = as.numeric(st_area(no_regret)/1e+06)) # NA costs are not part of the plans

  saveRDS(no_regret, paste0(outdir,"noregretclosures_",target,".rds"))
  return(no_regret)
}
