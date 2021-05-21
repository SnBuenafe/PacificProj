# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.
# Modified from Jase's code.

# This function runs prioritizr and solves the spatial plan problem using the
# Minimum set objective function.
# It requires the following inputs:
# 1. cost_file: PUs with the cellsID and the costs per PU (.rds)
# 2. commercial_targetfile: lists the targets per commercial feature x province (.rds)
# 3. bycatch_targetfile: lists the targets per bycatch feature x province (.rds)
# 4. commercial_file: file containing the commercial features (.rds)
# 5. bycatch_file: file containing the bycatch features (.rds)
# 6. climate_scenario: climate scenario (e.g. SSP126)
# 7. outdir: path where the solution will be saved.
# 8. outexcel: where to save the .csv files of the summaries 
# 9. target_name: e.g. Target100
# 10. prov: TRUE/FALSE (including provinces or not)

# Runs are found in 10b and 10c

fPrioritizrRun <- function(cost_file, commercial_targetfile, bycatch_targetfile, 
                           commercial_file, bycatch_file, climate_scenario, outdir, outexcel, target_name, prov, ...) {
  
  #######################################
  ####### Loading packages needed #######
  #######################################
  #install.packages("pacman", "BiocManager")
  #pacman::p_load(BiocManager)
  #devtools::install_github("prioritizr/prioritizr")
  #BiocManager::install("lpsymphony")
  #install.packages("/Library/gurobi911/mac64/R/gurobi_9.1-1_R_4.0.2.tgz", repos = NULL)
  #install.packages('slam')
  
  pacman::p_load(sf, dplyr, sp, raster, prioritizr, prioritizrdata,
                 BiocManager, tidyverse, fasterize, doParallel)
  pacman::p_load(lpsymphony, prioritizr, gurobi, slam)
  
  ############################
  ####### Calling data #######
  ############################
  # Load PUs with the cost (don't really need PUs as cost is complete)
  cost <- readRDS(cost_file) %>% 
    as_tibble()
  
  # Calling targets
  commercial_target <- readRDS(commercial_targetfile) %>% 
    as_tibble() %>% 
    dplyr::mutate(target_prop = target/100) %>% # converting targets in percentages into proportions
    dplyr::select(-geometry, -total_area)
  bycatch_target <- readRDS(bycatch_targetfile) %>% 
    as_tibble() %>% 
    dplyr::mutate(target_prop = target/100) %>% # converting targets in percentages into proportions
    dplyr::select(-geometry, -total_area)
  
  # Loading features (commercial & bycatch)
  commercial_features <- readRDS(commercial_file) %>%  # Load commercial features
    as_tibble() %>% 
    dplyr::select(-geometry)
  bycatch_features <- readRDS(bycatch_file) %>% # Load bycatch features
    as_tibble() %>% 
    dplyr::select(-geometry)
  
  #########################################
  ####### Manipulating data formats #######
  #########################################
  
  ###############
  ### TARGETS ###
  ###############
  # joining all targets
  targets <- full_join(commercial_target, bycatch_target)
  
  # creating a list of relative targets
  target_list <- targets %>% 
    dplyr::select(-category, -target) %>% 
    dplyr::arrange(new_features) # arranging rows alphabetically according to the new_features
  
  target_listf <- target_list$target_prop # creating a list of the targets arranged according to new_features
  
  ################
  ### FEATURES ###
  ################
  # joining all the features
  if(prov == TRUE) {
    if(climate_scenario == "uninformed"){
      features <- full_join(commercial_features, bycatch_features, by = c("cellsID", "new_features", "province",
                                                                          "prov_descr","area_km2"))
    }else{
    features <- full_join(commercial_features, bycatch_features, by = c("cellsID", "new_features", "province",
                                                                        "prov_descr", "area_km2", "velocity",
                                                                        "velo_tvalue", "RCE", "RCE_tvalue", "cellsID.2"))
    }
  }else if(prov == FALSE){
    if(climate_scenario == "uninformed"){
      features <- full_join(commercial_features, bycatch_features, by = c("cellsID", "new_features","area_km2"))
    }else{
      features <- full_join(commercial_features, bycatch_features, by = c("cellsID", "new_features","area_km2", "velocity",
                                                                          "velo_tvalue", "RCE", "RCE_tvalue", "cellsID.2"))
    }
  }
  
  # A character list of features to analyse
  features_list = unique(features$new_features) %>% 
    str_sort() # make sure that they are in alphabetical order (to make it consistent)
  
  # Just get species data out in wide format
  # pivot the table in a wide format (columns would be the new_features and the values are the presence/absence)
  species <- features %>% 
    dplyr::select(c(new_features, cellsID)) %>% 
    mutate(Presence = 1) %>% # if the feature has the particular cellsID, it is present
    pivot_wider(names_from = new_features, values_from = Presence) %>% 
    replace(is.na(.), 0)
  
  ########################
  ### FINAL OBJECT (x) ###
  ########################
  # x has the cost, the features, ...
  x <- features %>%
    dplyr::select(-new_features) %>% 
    distinct(cellsID, .keep_all = TRUE) %>% 
    left_join(species, by = "cellsID") %>% 
    right_join(cost, by = "cellsID") %>% 
    st_as_sf(sf_column_name = "geometry")
  
  #####################################################
  ####### Solving the problem using prioritizr! #######
  #####################################################
  p1 <- prioritizr::problem(x, features_list, "cost") %>% 
    add_min_set_objective() %>%
    add_relative_targets(target_listf) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 0.1, verbose = FALSE) # using Gurobi Solver
  
  s1 <- solve(p1) %>% 
    st_as_sf(sf_column_name = "geometry") # Output changes from sf to df so we change it back
  solution <- s1 %>% 
    mutate(solution_1 = as.logical(solution_1)) # Change solution to logical for plotting
  
  ####################################
  ###### Summaries of solutions ######
  ####################################
  # Printing important figures.
  total = 31917*2667.6
  rep <- eval_feature_representation_summary(p1, s1[, "solution_1"])
  rep
  write_csv(rep, paste0(outexcel,target_name,"_features_rep_",climate_scenario,".csv"))
  
  summary <- s1 %>% 
    filter(solution_1 == 1)
  
  if(climate_scenario == "uninformed"){
    summary1 <- summary %>% 
      summarize(sum_area = sum(area_km2), percent_area = (sum(area_km2)/total)*100, num_pu = length(st_geometry(summary)),
                total_cost = sum(cost)) %>% 
      as_tibble() %>% 
      dplyr::select(-geometry)
  }
  else{
  summary1 <- summary %>% 
    summarize(sum_area = sum(area_km2), percent_area = (sum(area_km2)/total)*100, num_pu = length(st_geometry(summary)),
              total_cost = sum(cost), median_velocity = median(velocity), median_tvelocity = median(velo_tvalue),
              median_RCE = median(RCE), median_RCEtvalue = median(RCE_tvalue)) %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  }
  summary1
  write_csv(summary1, paste0(outexcel,target_name,"_summary",climate_scenario,".csv"))
  
  saveRDS(solution, paste0(outdir,"solution_",climate_scenario,".rds"))
  return(solution)
}
