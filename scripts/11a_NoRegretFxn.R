# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This function creates no-regret closures, requiring the ff. inputs:
# 1. inpdir: where the prioritizr_run results are found.
# 2. outdir: where to save the results of this function.
# 3. target_name: e.g. Target100
# 4. pu_file: 
# 5. climate_scenario: noregret

# Runs are found in 11b_NoRegretRunAQM, and 11c_NoRegretRunIUCN.

create_noregret <- function(inpdir, outdir, target_name, pu_file, climate_scenario, ...) {
  
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
  temp_pu <- readRDS(pu_file)
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
      select(cellsID, solution_1, geometry)
    
    assign(x = paste0("plan_",x), value = temp_plan)
  }
  
  temp <- st_intersection(plan_SSP126, plan_SSP245) %>% 
    filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
  
  temp_final <- st_intersection(temp, plan_SSP585) %>% 
    filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON"))
  
  final_list <- temp_final %>% 
    dplyr::filter(solution_1 == TRUE & solution_1.1 == TRUE & solution_1.2 == TRUE) %>% 
    select(solution_1, cellsID, geometry) %>% 
    rename(solution = solution_1) %>% 
    as_tibble()

  #######################
  #### Join with PUs ####
  #######################
  no_regret <- left_join(planning_units, final_list, by = "cellsID") %>% 
    dplyr::mutate(solution = replace_na(solution, "FALSE"),
                  area_km2 = as.numeric(st_area(shp_PU_sf)/1e+06)) %>% 
    select(-geometry.y) %>% 
    rename(geometry = geometry.x) %>% 
    st_as_sf(sf_column_name = "geometry")
  
  ##########################
  ## Saving files as .csv ##
  ##########################
  total = 31917*2667.6
  
  summary <- no_regret %>% 
    dplyr::filter(solution == "TRUE")
  
  summary1 <- summary %>% 
    summarize(sum_area = sum(area_km2),percent_area = (sum(area_km2)/total)*100, 
              num_pu = length(st_geometry(summary)),
              total_cost = sum(cost)) %>% 
    as_tibble() %>% 
    select(-geometry)
  
  write_csv(summary1, paste0(outexcel,target_name,"_summary",climate_scenario,".csv"))

  saveRDS(no_regret, paste0(outdir,"noregretclosures_",target_name,".rds"))
  return(no_regret)
}

