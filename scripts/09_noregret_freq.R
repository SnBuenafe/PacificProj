library(tidyverse)
library(sf)

# for 25percentile
s1 <- readRDS('outputs/08_Prioritizr/08a_25perc/spatial_plans/solve_SSP126_target90.rds') %>% 
  as_tibble() %>% dplyr::select(-area_km2, -velo_tvalue, -RCE_tvalue)
colnames(s1)[2:15] <- paste0(colnames(s1)[2:15], '_1')

s2 <- readRDS('outputs/08_Prioritizr/08a_25perc/spatial_plans/solve_SSP245_target90.rds') %>% 
  as_tibble() %>% dplyr::select(-area_km2, -velo_tvalue, -RCE_tvalue)
colnames(s2)[2:15] <- paste0(colnames(s2)[2:15], '_2')
s2

s3 <- readRDS('outputs/08_Prioritizr/08a_25perc/spatial_plans/solve_SSP585_target90.rds') %>% 
  as_tibble() %>% dplyr::select(-area_km2, -velo_tvalue, -RCE_tvalue)
colnames(s3)[2:15] <- paste0(colnames(s3)[2:15], '_3')

s4 <- readRDS('outputs/08_Prioritizr/08a_25perc/spatial_plans/solve_uninformed_target90.rds') %>% 
  as_tibble() %>% 
  dplyr::select(-area_km2)
colnames(s4)[2:13] <- paste0(colnames(s4)[2:13], '_4')

joined_solutions <- full_join(s1, s2, by = c('cellsID', 'geometry')) %>% 
  full_join(s3, by = c('cellsID', 'geometry')) %>% 
  full_join(s4, by = c('cellsID', 'geometry')) %>% 
  st_as_sf(sf_column_name = 'geometry') %>% 
  dplyr::mutate(frequency_selection = solution_1_1 + solution_1_2 + solution_1_3 + solution_1_4)
joined_solutions_25perc <- joined_solutions
rm(s1, s2, s3, s4, joined_solutions)

# for as features
s1 <- readRDS('outputs/08_Prioritizr/08b_asfeat/spatial_plans/solve_SSP126_target90.rds') %>% 
  as_tibble() %>% dplyr::select(-area_km2)
colnames(s1)[2:14] <- paste0(colnames(s1)[2:14], '_1')
s1

s2 <- readRDS('outputs/08_Prioritizr/08b_asfeat/spatial_plans/solve_SSP245_target90.rds') %>% 
  as_tibble() %>% dplyr::select(-area_km2)
colnames(s2)[2:14] <- paste0(colnames(s2)[2:14], '_2')

s3 <- readRDS('outputs/08_Prioritizr/08b_asfeat/spatial_plans/solve_SSP585_target90.rds') %>% 
  as_tibble() %>% dplyr::select(-area_km2)
colnames(s3)[2:14] <- paste0(colnames(s3)[2:14], '_3')

s4 <- readRDS('outputs/08_Prioritizr/08b_asfeat/spatial_plans/solve_uninformed_target90.rds') %>% 
  as_tibble() %>% 
  dplyr::select(-area_km2)
colnames(s4)[2:12] <- paste0(colnames(s4)[2:12], '_4')

joined_solutions <- full_join(s1, s2, by = c('cellsID', 'geometry')) %>% 
  full_join(s3, by = c('cellsID', 'geometry')) %>% 
  full_join(s4, by = c('cellsID', 'geometry')) %>% 
  st_as_sf(sf_column_name = 'geometry') %>% 
  dplyr::mutate(frequency_selection = solution_1_1 + solution_1_2 + solution_1_3 + solution_1_4)

joined_solutions_asfeat <- joined_solutions
rm(s1, s2, s3, s4, joined_solutions)

# for penalty
s1 <- readRDS('outputs/08_Prioritizr/08c_penalty/spatial_plans/solve_SSP126_target90.rds') %>% 
  as_tibble() %>% dplyr::select(-area_km2)
colnames(s1)[2:15] <- paste0(colnames(s1)[2:15], '_1')

s2 <- readRDS('outputs/08_Prioritizr/08c_penalty/spatial_plans/solve_SSP245_target90.rds') %>% 
  as_tibble() %>% dplyr::select(-area_km2)
colnames(s2)[2:15] <- paste0(colnames(s2)[2:15], '_2')
s2

s3 <- readRDS('outputs/08_Prioritizr/08c_penalty/spatial_plans/solve_SSP585_target90.rds') %>% 
  as_tibble() %>% dplyr::select(-area_km2)
colnames(s3)[2:15] <- paste0(colnames(s3)[2:15], '_3')

s4 <- readRDS('outputs/08_Prioritizr/08c_penalty/spatial_plans/solve_uninformed_target90.rds') %>% 
  as_tibble() %>% 
  dplyr::select(-area_km2)
colnames(s4)[2:13] <- paste0(colnames(s4)[2:13], '_4')

joined_solutions <- full_join(s1, s2, by = c('cellsID', 'geometry')) %>% 
  full_join(s3, by = c('cellsID', 'geometry')) %>% 
  full_join(s4, by = c('cellsID', 'geometry')) %>% 
  st_as_sf(sf_column_name = 'geometry') %>% 
  dplyr::mutate(frequency_selection = solution_1_1 + solution_1_2 + solution_1_3 + solution_1_4)
joined_solutions_penalty <- joined_solutions
rm(s1, s2, s3, s4, joined_solutions)

hist(joined_solutions_25perc$frequency_selection)
summary_25perc <- joined_solutions_25perc %>% 
                    group_by(frequency_selection) %>% 
                    summarise(sum_freq = length(frequency_selection)) %>% 
                    as_tibble() %>% 
                    dplyr::select(-geometry)
summary_25perc %>% 
  dplyr::filter(frequency_selection > 1) %>% 
  colSums()
hist(joined_solutions_asfeat$frequency_selection)
summary_asfeat <- joined_solutions_asfeat %>% 
                    group_by(frequency_selection) %>% 
                    summarise(sum_freq = length(frequency_selection)) %>% 
                    as_tibble() %>% 
                    dplyr::select(-geometry)
summary_asfeat %>% 
  dplyr::filter(frequency_selection > 1) %>% 
  colSums()
hist(joined_solutions_penalty$frequency_selection)
summary_penalty <- joined_solutions_penalty %>% 
                    group_by(frequency_selection) %>% 
                    summarise(sum_freq = length(frequency_selection)) %>% 
                    as_tibble() %>%
                    dplyr::select(-geometry)
summary_penalty %>% 
  dplyr::filter(frequency_selection > 1) %>% 
  colSums()
