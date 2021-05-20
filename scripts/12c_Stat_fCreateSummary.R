# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This function further summarizes the statistics generated from the prioritizr() runs
# It requires the following inputs:
# 1. inpdir: where the .csv files of the summaries are
# 2. outdir: where the .csv files of the summaries should be saved (separate for w/ Provinces and w/o Provinces)
# 3. name: e.g. "AQM", "IUCN", "AQM_noregret", "IUCN_noregret"

# Function is ran at 12d.

fCreateSummary <- function(inpdir, outdir, name, ...) {
  
  ############################
  ### Defining libraries #####
  ############################
  library(tidyverse)
  
  ############################
  ### Manipulating data #####
  ############################
  FeatRep_pattern <- paste0("{1}",name, "_features_rep")
  FeatRepFile <- list.files(inpdir, pattern = FeatRep_pattern)
  
  Solution_pattern <- paste0("{1}",name, "_solution")
  SolutionFile <- list.files(inpdir, pattern = Solution_pattern)
  
  featrep_file <- read_csv(paste0(inpdir,FeatRepFile[1])) %>% 
    as_tibble
  solution_file <- read_csv(paste0(inpdir,SolutionFile[1])) %>% 
    as_tibble()
  
  masked_featrep_file <- featrep_file %>% 
    dplyr::select(-feature)
  
  sumif <- function(x) {
    sum(x >= 0.3)
  }
  
  new_row <- lapply(masked_featrep_file, sumif) %>% 
    as_tibble()
  new_row$feature <- c('represented_features')
  new_row %<>% relocate(feature)
  colnames(new_row) <- colnames(solution_file)

  solution_df <- rbind(solution_file, new_row) %>% 
    pivot_longer(cols = starts_with("Target"), names_to = 'target_scenarios') %>% 
    pivot_wider(names_from = variables, values_from = value)
  solution_df1 <- solution_df %>% 
    mutate(target = case_when(str_detect(solution_df$target_scenarios, pattern = "Target10_") ~ "10",
                              str_detect(solution_df$target_scenarios, pattern = "Target20_") ~ "20",
                              str_detect(solution_df$target_scenarios, pattern = "Target30_") ~ "30",
                              str_detect(solution_df$target_scenarios, pattern = "Target40_") ~ "40",
                              str_detect(solution_df$target_scenarios, pattern = "Target50_") ~ "50",
                              str_detect(solution_df$target_scenarios, pattern = "Target60_") ~ "60",
                              str_detect(solution_df$target_scenarios, pattern = "Target70_") ~ "70",
                              str_detect(solution_df$target_scenarios, pattern = "Target80_") ~ "80",
                              str_detect(solution_df$target_scenarios, pattern = "Target90_") ~ "90",
                              str_detect(solution_df$target_scenarios, pattern = "Target100_") ~ "100"),
           scenario = case_when(str_detect(solution_df$target_scenarios, pattern = "SSP126") ~ "SSP126",
                                str_detect(solution_df$target_scenarios, pattern = "SSP245") ~ "SSP245",
                                str_detect(solution_df$target_scenarios, pattern = "SSP585") ~ "SSP585",
                                str_detect(solution_df$target_scenarios, pattern = "uninformed") ~ "uninformed",
                                str_detect(solution_df$target_scenarios, pattern = "noregret") ~ "noregret"))
  
  ##########################
  ##### Writing File #######
  ##########################
  write_csv(solution_df1, paste0(outdir,name,"_stat_summary.csv"))
  
  return(solution_df1)
}
