# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This code saves .csv files of a summary of the metrics for each representation target.
# 1. for the variables of the solution itself, and 
# 2. for the relative representation of all of the features

# This function, solution_summary() requres the following inputs:
# 1. inpdir = where the .csv files of each solution are found.
# 2. outdir = where to save the final .csv files
# 3. plans = only if noregret

# Function is run at 12b.

fSolutionSummary <- function(inpdir, outdir, plans, ...){
  ##################################
  ### Defining the main packages ###
  ##################################
  
  # List of packages that we will use
  list.of.packages <- c("tidyverse", "doParallel", "foreach")
  # If is not installed, install the pacakge
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # Load packages
  lapply(list.of.packages, require, character.only = TRUE)
  
  ################################################################################
  ## Creating one .csv file for the summaries for each target and each scenario ##
  ################################################################################
  pat <- paste0("+summary")
  temp_list <- list.files(path = inpdir, pattern = pat)
  
  # Set up parallel structure
  ncores <- detectCores()
  cl <- makeCluster(ncores -1)
  registerDoParallel(cl)
  
  # create an empty list
  table <- list()
  
  table <- foreach(i = 1:length(temp_list), .packages = c("tidyverse")) %dopar% {
      temp_summary <- read_csv(paste0(inpdir,temp_list[i]))
      x <- unlist(strsplit(temp_list[i],"[.]"))[1]
      temp_summary1 <- temp_summary %>% 
        pivot_longer(everything(),
                     names_to = "variables", values_to = x)
      
      if(i > 1){temp_summary1 %>% 
          select(-variables)}else{temp_summary1}
  }
  stopCluster(cl)
  
  table_final <- do.call(cbind, table)
  table_final
  
  write_csv(table_final, paste0(outdir,"AQM_solution_summary.csv"))
  
  ################################################################################################
  ## Creating one .csv file for the summaries of each feature for each target and each scenario ##
  ################################################################################################
  if(plans == "climate-smart") {
  pat1 <- paste0("+features_rep")
  temp_list1 <- list.files(path = inpdir, pattern = pat1)
  
  # Set up parallel structure
  ncores <- detectCores()
  cl <- makeCluster(ncores -1)
  registerDoParallel(cl)
  
  # create an empty list
  table1 <- list()
  
  table1 <- foreach(i = 1:length(temp_list), .packages = c("tidyverse")) %dopar% {
    temp_summary <- read_csv(paste0(inpdir,temp_list1[i]))
    x <- unlist(strsplit(temp_list1[i],"[.]"))[1]
    y <- unlist(strsplit(x,"_"))[1:4]
    z <- str_c(y, collapse = "_")
    
    temp_summary1 <- temp_summary %>% 
      select(feature, relative_held)
    
    colnames(temp_summary1)[2] <- z
    
    if(i > 1){temp_summary1 %>% 
        select(-feature)}else{temp_summary1}
  }
  stopCluster(cl)
  
  table_final1 <- do.call(cbind, table1)
  table_final1
  
  write_csv(table_final1, paste0(outdir,"AQM_features_rep_summary.csv"))
  }else{}
}
