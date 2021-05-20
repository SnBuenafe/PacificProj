# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This function creates .csv summary of the features selected in no-regret plans, requiring the ff. inputs:
# NOTE: Make sure files correspond with value of "prov" (TRUE/FALSE)
# 1. commercial_file: commercial file (global/pacific)
# 2. bycatch_file: bycatch file (AQM/IUCN)
# 3. prov: TRUE/FALSE (including provinces or not)
# 4. noregret_file: no-regret .rds file for each target
# 5. outexcel: where to save .csv output
# 6. target_name: e.g. Target100

# Function is ran in 11g

fCreateNRSummary <- function(commercial_file, bycatch_file, prov, noregret_file, outexcel, target_name, ...){

    ########################################
    ####### Defining packages needed #######
    ########################################
    # List of pacakges that we will use
    list.of.packages <- c("sf", "tidyverse", "doParallel")
    # If is not installed, install the pacakge
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    # Load packages
    lapply(list.of.packages, require, character.only = TRUE)

    ########################################
    ####### Calling files #######
    ########################################
    # Loading features (commercial & bycatch)
    commercial_features <- readRDS(commercial_file) %>%  # Load commercial features
      as_tibble() %>% 
      dplyr::select(-geometry)
    bycatch_features <- readRDS(bycatch_file) %>% # Load bycatch features
      as_tibble() %>% 
      dplyr::select(-geometry)
    
    # Load No-Regret file
    noregret <- readRDS(noregret_file) %>% 
      dplyr::select(-cost_categ, -cost_log, -area_km2) %>% 
      rename(solution = solution_1)
    
    ########################################
    ####### Manipulating data #######
    ########################################
    
    # joining all the features
    if(prov == TRUE) {
        features <- full_join(commercial_features, bycatch_features, by = c("cellsID", "feature_names", "province",
                                                                            "prov_descr","area_km2", "feature")) %>% 
          dplyr::select(-province, -prov_descr, -feature_names) %>% 
          rename(new_features = feature)
      } else if(prov == FALSE){
        features <- full_join(commercial_features, bycatch_features, by = c("cellsID", "feature_names","area_km2")) %>% 
          rename(new_features = feature_names)
    }
    
    # Just get species data out in wide format
    # pivot the table in a wide format (columns would be the new_features and the values are the presence/absence)
    species <- features %>% 
      dplyr::select(c(new_features, cellsID)) %>% 
      mutate(Presence = 1) %>% # if the feature has the particular cellsID, it is present
      pivot_wider(names_from = new_features, values_from = Presence) %>% 
      replace(is.na(.), 0)
    
    x <- features %>%
      dplyr::select(-new_features) %>% 
      distinct(cellsID, .keep_all = TRUE) %>% 
      left_join(species, by = "cellsID") %>% 
      right_join(noregret, by = "cellsID") %>% 
      st_as_sf(sf_column_name = "geometry")
    
    ######################################################
    ####### Creating summaries for no-regret plans #######
    ######################################################
    # total amount of PUs per feature, total PUs retained (filter solution = TRUE), and relative
    n <- ncol(species)
    num_feat <- colnames(species[,2:n]) # -1 to remove column of "cellsID"
    total_list <- list()
    mask <- x %>% 
      as_tibble()
    
    for(i in 1:length(num_feat)){
      sum <- sum(mask[,i+2] == 1, na.rm = TRUE)
      mask1 <- mask %>% 
        filter(solution == TRUE)
      retained <- sum(mask1[,i+2] == 1, na.rm = TRUE)
      total_list[[i]] <- c(sum,retained)
    }
    temp_df <- do.call(rbind, total_list)
    temp_df1 <- cbind(temp_df,num_feat)
    colnames(temp_df1) <- c("sum","retained","features")
    temp_df2 <- temp_df1 %>% 
      transform(sum = as.numeric(sum), retained = as.numeric(retained))
    final_df <- temp_df2 %>% 
      as_tibble() %>% 
      dplyr::mutate(relative = retained/sum)
    
    write_csv(final_df, paste0(outexcel,target_name,"_features_rep_noregret",".csv"))
}