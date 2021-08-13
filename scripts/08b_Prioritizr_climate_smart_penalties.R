# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This code employs the method of creating climate-smart plans by using the climate metrics as a linear 
# penalty adding this to the problem() of prioritizr using the function add_linear_penalties()

# loading packages
#install.packages("pacman", "BiocManager", 'slam)
#pacman::p_load(BiocManager)
#devtools::install_github("prioritizr/prioritizr")
#BiocManager::install("lpsymphony")
#install.packages("/Library/gurobi911/mac64/R/gurobi_9.1-1_R_4.0.2.tgz", repos = NULL)

pacman::p_load(sf, dplyr, sp, raster, prioritizr, prioritizrdata, BiocManager, tidyverse, fasterize, 
               doParallel, patchwork, magrittr, irr, corrplot, patchwork)
pacman::p_load(lpsymphony, prioritizr, gurobi, slam)

#######################################################
## Calling recurring directories/functions/libraries ##
#######################################################
# climate metrics
velocity_directory <- 'outputs/05_Climate/Velocity/'
RCE_directory <- 'outputs/05_Climate/RCE/'

# bycatch file
bycatch_directory <- 'outputs/03_IUCN/03d_fFeaturesInt/'

# commercial file
commercial_directory <- 'outputs/04_Commercial/04d_fFeaturesInt/Global/'

# cost file with PU
cost <- readRDS('outputs/06_Cost/Large_Medium/costlayer.rds') %>% 
  dplyr::select(-x, -y, -cost_log, -cost_categ)

# create target list
target <- seq(20, 100, by = 10) %>% 
  as_tibble() %>% 
  dplyr::rename(target = value) %>% 
  dplyr::mutate(eff_target = target*0.25)

# source functions
source("scripts/07_Filter_fFilterQuartile.R")
source('scripts/07_Prioritizr_create_object.R')
source('scripts/07_Summary_create_circularbplot.R')
source('scripts/07_Summary_create_kappacorrplot.R')

# defining plot generalities
library(RColorBrewer)
library(patchwork)
pal_rich <- c("FALSE" = "lightsteelblue2", "TRUE" = "steelblue4")
solution <- c("Not selected PUs", "Selected PUs")
world_sf <- readRDS("outputs/01_StudyArea/01a_StudyArea/PacificCenterLand.rds")

source("scripts/study_area/fCreateRobinsonBoundary.R")
Bndry <- fCreateRobinsonBoundary(west = 78, east = 140, north = 51, south = 60)

##################################
### Creating plans ###
##################################
# - no filtering when creating uninformed plans
# - using RCE and velocity as penalties instead! (climate-smart)
scenario_list <- c('SSP126', 'SSP245', 'SSP585', 'uninformed')
feature_list <- c('bycatch','commercial')

outdir = "outputs/07_Filter/07b_Filter100/" # output directory
data = 'uninformed' # climate-uninformed
prov = FALSE
quantile_value = NA #2 for 25th percentile, 3 for 50th percentile
velocity = NA
RCE = NA
scenario = 'uninformed'

for(j in 1:length(feature_list)) {
  if(j == 1){
    directory <- bycatch_directory
  }else{
    directory <- commercial_directory
  }
  
  conservation <- paste0(directory, feature_list[j], '_features.rds')
  a <- fFilterQuartile(velocity, RCE, conservation, outdir, scenario, feature_list[j], data, prov, quantile_value)
  assign(paste0('filter_',feature_list[j],'_uninformed'), a)
  rm(a)
}

# join bycatch and commercial features for each climate scenario (as i), and creating a feature list for each
commercial <- filter_commercial_uninformed %>% 
  as_tibble()
bycatch <-  filter_bycatch_uninformed %>% 
  as_tibble()
conservation_features <- full_join(commercial, bycatch)
assign(paste0('joined_conservationfeat_uninformed'), conservation_features)
features_list <- unique(conservation_features$new_features) %>% 
  str_sort() # to make sure they are sorted alphabetically.
assign(paste0('features_list_uninformed'), features_list)
rm(conservation_features, features_list)

## creating objects, RCE and velocity penalties for each scenario (as i)
x_uninformed <- create_object(joined_conservationfeat_uninformed, cost)

for(i in 1:3){ #only until 3 to exclude 'uninformed' scenario
  RCE <- readRDS(paste0(RCE_directory,'RCE',scenario_list[i],'.rds')) %>% 
    dplyr::select(cellsID, geometry, value) %>% 
    dplyr::rename(RCE = value) %>% 
    as_tibble()
  velocity <- readRDS(paste0(velocity_directory,'velocity',scenario_list[i],'.rds')) %>% 
    dplyr::select(cellsID, geometry, value) %>% 
    dplyr::rename(velocity = value) %>% 
    as_tibble()
  x <- x_uninformed %>% 
    left_join(RCE, by = c('cellsID','geometry')) %>% 
    left_join(velocity, by = c('cellsID', 'geometry'))
  assign(paste0('x_',scenario_list[i]),x)
  rm(RCE, velocity,x)
}

# SSP126 max's for: RCE = 115.6165 (x2), velocity = 50.06828 (x4), cost = 813.37098
# SSP245 max's for: RCE = 211.538 (x1), velocity = 184.4885 (x1.5)
# SSP585 max's for: RCE = 1006.79 (x0.2), velocity = 731.6799 (x0.3)

# creating problems and solutions across climate-scenarios (as i) and targets (as k)
for(k in 1:nrow(target)){
  for(i in 1:length(scenario_list)){
  x <- get(paste0('x_',scenario_list[i]))
  p <- problem(x, features_list_uninformed, 'cost') %>% 
    add_min_set_objective() %>% 
    add_binary_decisions() %>% 
    add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
    add_relative_targets(as.numeric(target[k,2]/100))
  if(i == 1){
    p %<>% add_linear_penalties(2, 'RCE') %>% 
      add_linear_penalties(4, 'velocity')
  }else if(i == 2){
    p %<>% add_linear_penalties(1, 'RCE') %>% 
      add_linear_penalties(1.5, 'velocity')
  }else if(i == 3){
    p %<>% add_linear_penalties(0.2, 'RCE') %>% 
      add_linear_penalties(0.3, 'velocity')
  }else{}
  s <- solve(p) %>% 
    st_as_sf(sf_column_name = 'geometry')
  assign(paste0('prob_', scenario_list[i], '_target',target[k,1]),p)
  assign(paste0('solve_', scenario_list[i], '_target',target[k,1]),s)
  saveRDS(s, paste0('outputs/08_Prioritizr/08c_penalty/spatial_plans/', 'solve_', scenario_list[i], '_target', target[k,1], '.rds'))
  rm(x,p,s)
  }
}

##################################################
## Creating and saving summaries and statistics ##
##################################################
# representation features!
feat <- list()
feat.nested <- list()
for(k in 1:nrow(target)) {
  for(i in 1:length(scenario_list)) {
    p <- get(paste0('prob_', scenario_list[i], "_target", target[k,1]))
    s <- get(paste0('solve_', scenario_list[i], "_target", target[k,1]))
    
    feat[[i]] <- eval_feature_representation_summary(p, s[, 'solution_1']) %>% 
      dplyr::select(relative_held) %>% 
      mutate(relative_held = relative_held*100)
    colnames(feat[[i]]) <- paste0('target_', target[k,1], "_", scenario_list[i])
    
    rm(p,s)
  }
  feat.nested[[k]] <- feat
}
feat.nested #[[k]][[i]], with k for each target and i for each scenario
represent_feat_b <- bind_cols(features_list_uninformed, feat.nested) #just using features_list_SSP126, since it's the same for every climate scenario anynway.
colnames(represent_feat_b)[1] <- 'features'
write_csv(represent_feat_b, 'outputs/08_Prioritizr/08c_penalty/summary_represent_feat.csv')

# summary stats!
total_area = 31917*2667.6
summary <- list()
summary.nested <- list()
for(k in 1:nrow(target)) {
  for(i in 1:length(scenario_list)) {
    s <- get(paste0('solve_', scenario_list[i], "_target", target[k,1]))
    temp_sol <- s %>% filter(solution_1 == 1)
    x <- temp_sol %>% 
      as_tibble() %>% 
      dplyr::select(-geometry)
    if(i < 4) {
      x %<>% dplyr::summarize(sum_area = sum(area_km2, na.rm = TRUE), total_cost = sum(cost), median_velocity = median(velocity), median_RCE = median(RCE))
    }else{
      x %<>% dplyr::summarize(sum_area = sum(area_km2, na.rm = TRUE), total_cost = sum(cost), median_velocity = NA, median_RCE = NA)
    }
      summary[[i]] <- x %>% 
        dplyr::mutate(percent_area = sum_area*100/total_area, num_pu = length(st_geometry(temp_sol))) %>% 
        dplyr::mutate(target = as.numeric(target[k,1]))
    summary[[i]]$scenario <- scenario_list[i]
    
    rm(temp_sol,s, x)
  }
  summary.nested[[k]] <- summary
}
summary.nested #[[k]][[i]], with k for each target and i for each scenario
summary_stat_b <- bind_rows(summary.nested)
write_csv(summary_stat_b, 'outputs/08_Prioritizr/08c_penalty/summary_stat.csv')

##################################
## Plotting all solutions ##
##################################
# creating gg objects per target (k) and per scenario (i) and saving them as .pdfs
for(k in 1:nrow(target)) {
  for(i in 1:length(scenario_list)) {
    s <- get(paste0('solve_', scenario_list[i], "_target", target[k,1])) %>% 
      mutate(solution_1 = as.logical(solution_1)) # Change solution to logical for plotting
    gg <- ggplot() + 
      geom_sf(data = s, aes(fill = solution_1), color = "grey64", size = 0.02) +
      scale_fill_manual(name = "Solution",
                        values = pal_rich,
                        labels = solution) +
      geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
      coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
               ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
               expand = TRUE) +
      ggtitle(scenario_list[i]) +
      theme_bw()
    assign(paste0('gg_b_',scenario_list[i],'_',target[k,1]), gg)
    
    rm(s, gg)
  }
  gg1 <- get(paste0('gg_b_', scenario_list[1], '_', target[k,1]))
  gg2 <- get(paste0('gg_b_', scenario_list[2], '_', target[k,1]))
  gg3 <- get(paste0('gg_b_', scenario_list[3], '_', target[k,1]))
  gg4 <- get(paste0('gg_b_', scenario_list[4], '_', target[k,1]))
  
  gg_targ <- gg1 + gg2 + gg3 + gg4 +
    plot_layout(ncol = 2, nrow = 2, guides = "collect") +
    plot_annotation(tag_levels = 'A',
                    tag_suffix = ')')
  ggsave(filename = paste0('plan_target', target[k,1], '.pdf'),
         plot = gg_targ, width = 21, height = 29.7, dpi = 300,
         path = 'pdfs/08_Prioritizr/08c_penalty/')
  assign(paste0('gg_full_b_', target[k,1]), gg_targ)
  rm(gg1, gg2, gg3, gg4)
}

# create plots subsetting 90, 60, 30; columns will be the scenarios and rows will be the targets
gg_subset <- gg_b_SSP126_90 + gg_b_SSP245_90 + gg_b_SSP585_90 + gg_b_uninformed_90 +
  gg_b_SSP126_60 + gg_b_SSP245_60 + gg_b_SSP585_60 + gg_b_uninformed_60 +
  gg_b_SSP126_30 + gg_b_SSP245_30 + gg_b_SSP585_30 + gg_b_uninformed_30 +
  plot_layout(ncol = 4, nrow = 3, guides = 'collect') & theme(title = element_blank())
ggsave('outputs/08_Prioritizr/08c_penalty/subset_plans.png', gg_subset, width = 29.7, height = 21, dpi = 300)

###################################
###### Creating stat figures ######
###################################
# representation of features (not much differences between the % protection per feature)
represent_feat.summary_b <- represent_feat_b %>% 
  pivot_longer(cols = starts_with('target_'), names_to = 'plan', values_to = 'representation')

# subset according to target and loop through it
for(k in 1:nrow(target)) {
  subsetted <- subset(represent_feat.summary_b, str_detect(plan, pattern = paste0('_', target[k,1], '_')))
  circbplot <- create_circularbplot(subsetted, target[k,2])
  assign(paste0('circbplot_b_target_', target[k,1]), circbplot)
  rm(subsetted, circbplot)
}

# saving circular barplots
plot1 <- (circbplot_b_target_100 + circbplot_b_target_90 + circbplot_b_target_80) +
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
ggsave('pdfs/08_Prioritizr/08c_penalty/circbplot_page1.pdf', plot1, width = 29.7, height = 21)

plot2 <- (circbplot_b_target_70 + circbplot_b_target_60 + circbplot_b_target_50) +
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
ggsave('pdfs/08_Prioritizr/08c_penalty/circbplot_page2.pdf', plot2, width = 29.7, height = 21)

plot3 <- (circbplot_b_target_40 + circbplot_b_target_30 + circbplot_b_target_20) +
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
ggsave('pdfs/08_Prioritizr/08c_penalty/circbplot_page3.pdf', plot3, width = 29.7, height = 21)

plot_subset <- (circbplot_b_target_90 + circbplot_b_target_60 + circbplot_b_target_30) +
  plot_layout(guides = 'collect', nrow = 1) & theme(legend.position = 'bottom')
ggsave('pdfs/08_Prioritizr/08c_penalty/circbplot_subset.pdf', plot_subset, width = 29.7, height = 10)
ggsave('outputs/08_Prioritizr/08c_penalty/circbplot_subset.png', plot_subset, width = 29.7, height = 10)

# cost
# plot total cost for all plans across climate scenarios
scenario.legend_color <- c('SSP126' = 'yellow3', 'SSP245' = 'orange2', 
                           'SSP585' = 'salmon4', 'uninformed' = 'lightslategray', 'NA' = NA)
cost_all_b <- ggplot(data = summary_stat_b, aes(x = target*0.25, group = scenario)) +
  geom_line(aes(color = scenario, y = log10(total_cost)), size = 0.8) +
  geom_point(aes(color = scenario, y = log10(total_cost))) +
  scale_color_manual(name = 'Climate scenario',
                     values = scenario.legend_color) +
  xlab(" ") + 
  ylab("log10(cost)") +
  theme(legend.position = "bottom") +
  theme_classic()
protected.area_all_b <- ggplot(data = summary_stat_b, aes(x = target*0.25, group = scenario)) +
  geom_line(aes(color = scenario, y = percent_area), size = 0.8) +
  geom_point(aes(color = scenario, y = percent_area)) +
  scale_color_manual(name = 'Climate scenario',
                     values = scenario.legend_color) +
  xlab(" ") + 
  ylab("Protected area (%)") +
  theme(legend.position = "bottom") +
  theme_classic()
med.RCE_all_b <- ggplot(data = summary_stat_b %>% filter(scenario != 'uninformed'), aes(x = target*0.25, group = scenario)) +
  geom_line(aes(color = scenario, y = median_RCE), size = 0.8, show.legend = FALSE) +
  geom_point(aes(color = scenario, y = median_RCE), show.legend = FALSE) +
  scale_color_manual(name = 'Climate scenario',
                     values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("Median RCE") +
  theme(legend.position = "bottom") +
  theme_classic()
med.velocity_all_b <- ggplot(data = summary_stat_b %>% filter(scenario != 'uninformed'), aes(x = target*0.25, group = scenario)) +
  geom_line(aes(color = scenario, y = median_velocity), size = 0.8, show.legend = FALSE) +
  geom_point(aes(color = scenario, y = median_velocity), show.legend = FALSE) +
  scale_color_manual(name = 'Climate scenario',
                     values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("Median Climate Velocity") +
  theme(legend.position = "bottom") +
  theme_classic()

summary.all <- cost_all_b + protected.area_all_b + med.RCE_all_b + med.velocity_all_b +
  plot_layout(guides = 'collect', width = 15, height = 10, ncol = 2, nrow = 2) +
  plot_annotation(tag_level = 'A', tag_suffix = ')')
ggsave('pdfs/08_Prioritizr/08c_penalty/summary_all.pdf', summary.all, width = 8, height = 5, dpi = 300)

# plotting cost, protected area, median RCE and median velocity for runs that will be reported in the main body
subset_b <- summary_stat_b %>% 
  filter(target %in% c('30','60','90'))

# intersecting RCE and velocity with uninformed plans across all climate scenarios
stack <- list()
target_stack <- list()
for(k in 1:nrow(target)) {
  for(i in 1:length(scenario_list)) { # only until 3rd
    num = as.numeric(target[k,1])
    sumstat <- summary_stat_b %>% 
      dplyr::filter(target == num)
    if(i < 4){
      RCE_temp <- readRDS(paste0(RCE_directory, 'RCE', scenario_list[i], '.rds')) %>% 
        as_tibble() %>% 
        dplyr::select(-area_km2, -trans_value, -rce_categ) %>% 
        rename(RCE = value)
      velocity_temp <- readRDS(paste0(velocity_directory, 'velocity', scenario_list[i], '.rds')) %>% 
        as_tibble() %>% 
        dplyr::select(-area_km2, -trans_value, -velo_categ) %>% 
        rename(velocity = value)
      xx <- get(paste0('solve_uninformed_target',target[k,1])) %>% 
        as_tibble() %>% 
        dplyr::select(cellsID, geometry, solution_1)
      yy <- full_join(xx, RCE_temp, by = c('cellsID','geometry')) %>% 
        full_join(velocity_temp, by = c('cellsID','geometry')) %>% 
        dplyr::filter(solution_1 == 1) %>% 
        dplyr::summarise(median_RCE = median(RCE), median_velocity = median(velocity)) %>% 
        dplyr::mutate(scenario = 'uninformed', uninformed = scenario_list[i])
      yy$target <- num
      zz <- sumstat %>% 
        filter(scenario == scenario_list[i]) %>% 
        dplyr::select(median_velocity, median_RCE, target, scenario) %>% 
        dplyr::mutate(uninformed = scenario_list[i])
      stack[[i]] <- bind_rows(yy, zz)
      rm(RCE_temp, velocity_temp, xx, yy, zz, sumstat, num)
    }else{}
  }
  target_stack[[k]] <- stack
}
stack_df <- bind_rows(target_stack)
write_csv(stack_df, 'outputs/08_Prioritizr/08c_penalty/summary_climate_stat.csv')

stack_df.summary_b <- stack_df %>% 
  filter(target %in% c('30','60','90')) %>% 
  dplyr::mutate(target = ifelse(target == 30, 7.5,
                                ifelse(target == 60, 15,
                                       ifelse(target == 90, 22.5, NA))))

med.RCE_subset_b <- ggplot(data = stack_df.summary_b, aes(x = as.factor(uninformed), group = scenario)) +
  geom_bar(aes(y = median_RCE, fill = scenario), stat = 'identity', position = 'stack') +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Climate Scenario") + 
  ylab("Median RCE") +
  facet_grid(.~target) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

med.velocity_subset_b <- ggplot(data = stack_df.summary_b, aes(x = as.factor(uninformed), group = scenario)) +
  geom_bar(aes(y = median_velocity, fill = scenario), stat = 'identity', position = 'stack') +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Climate Scenario") + 
  ylab("Median Climate Velocity") +
  facet_grid(.~target) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

cost_subset_b <- ggplot(data = subset_b, aes(x = as.factor(target*0.25), group = scenario)) +
  geom_bar(aes(y = log10(total_cost), fill = scenario), stat = 'identity', position = position_dodge()) +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("log10(cost") +
  theme(legend.position = "bottom") +
  theme_classic()

protected.area_subset_b <- ggplot(data = subset_b, aes(x = as.factor(target*0.25), group = scenario)) +
  geom_bar(aes(y = percent_area, fill = scenario), stat = 'identity', position = position_dodge()) +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("Protected Area (%)") +
  theme(legend.position = "bottom") +
  theme_classic()

summary.subset <- cost_subset_b + protected.area_subset_b + med.RCE_subset_b + med.velocity_subset_b +
  plot_layout(guides = 'collect', width = 15, height = 10, ncol = 2, nrow = 2) +
  plot_annotation(tag_level = 'A', tag_suffix = ')')
ggsave('pdfs/08_Prioritizr/08c_penalty/summary_subset.pdf', summary.subset, width = 8, height = 5, dpi = 300)
ggsave('outputs/08_Prioritizr/08c_penalty/summary_subset.png', summary.subset, width = 8, height = 5, dpi = 300)


##############################
## Creating no-regret plans ##
##############################
# - no-regret plans are those PUs that are selected in all climate scenarios, even the climate-uninformed plans

# creating no-regret plans across all targets (as k) by intersecting all the data across all scenarios (as i)
for(k in 1:nrow(target)){
  for(i in 1:length(scenario_list)){
    s <- get(paste0('solve_',scenario_list[i],'_target',target[k,1]))
    if(i < 4){s %<>% dplyr::select(-area_km2)
      colnames(s)[2:15] <- paste0(colnames(s)[2:15], '_', i)}else{s %<>% dplyr::select(-area_km2)
        colnames(s)[2:13] <- paste0(colnames(s)[2:13], '_', i)}
    s %<>% as_tibble()
    assign(paste0('s',i), s)
    rm(s)
  }
  joined_solutions <- full_join(s1, s2, by = c('cellsID', 'geometry')) %>% 
    full_join(s3, by = c('cellsID', 'geometry')) %>% 
    full_join(s4, by = c('cellsID', 'geometry')) %>% 
    st_as_sf(sf_column_name = 'geometry') %>% 
    dplyr::mutate(frequency_selection = solution_1_1 + solution_1_2 + solution_1_3 + solution_1_4)
  
  # s_no.regret shows the full data of the no-regret
  s_no.regret <- joined_solutions %>% 
    dplyr::mutate(final_solution = case_when(solution_1_1 == TRUE & solution_1_2 == TRUE & solution_1_3 == TRUE ~ TRUE, 
                                             TRUE ~ FALSE))
  assign(paste0('s_no.regret_target_', target[k,1]), s_no.regret)
  
  sfreq_no.regret <- joined_solutions %>% 
    dplyr::select(cellsID, frequency_selection, geometry)
  assign(paste0('sfreq_no.regret_target_', target[k,1]), sfreq_no.regret)
  
  rm(s1, s2, s3, s4, joined_solutions, s_no.regret, sfreq_no.regret)
}

#############################################
#### Plotting and saving no-regret plans ####
#############################################
# frequency of selection
for(k in 1:nrow(target)) {
  s <- get(paste0('sfreq_no.regret_target_',target[k,1]))
  g <- ggplot() +
    geom_sf(data = s, aes(fill = as.factor(frequency_selection)), color = 'grey20', size = 0.01) +
    scale_fill_brewer(name = 'Frequency of selection',
                      type = 'seq',
                      palette = 'OrRd',
                      aesthetics = 'fill') +
    geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
    coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
             ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
             expand = TRUE) +
    labs(title = paste0('Target: ', target[k,2], '%')) + 
    theme_bw()
  
  assign(paste0('ggfreq_b_no.regret_target_',target[k,1]), g)
  rm(s, g)
}
# all
gg_freq_no.regret_all <- ggfreq_b_no.regret_target_100 + ggfreq_b_no.regret_target_90 + ggfreq_b_no.regret_target_80 +
  ggfreq_b_no.regret_target_70 + ggfreq_b_no.regret_target_60 + ggfreq_b_no.regret_target_50 + ggfreq_b_no.regret_target_40 +
  ggfreq_b_no.regret_target_30 + ggfreq_b_no.regret_target_20 + 
  plot_layout(ncol = 3, nrow = 3, guides = 'collect') +
  plot_annotation(tag_levels = 'A', tag_suffix = ')')
ggsave('pdfs/08_Prioritizr/08c_penalty/freq_no.regret.plan_all.pdf', gg_freq_no.regret_all, width = 21, height = 29.7, dpi = 300)
# subset
gg_freq_no.regret_subset <- ggfreq_b_no.regret_target_90 + ggfreq_b_no.regret_target_60 + ggfreq_b_no.regret_target_30 +
  plot_layout(nrow = 1, guides = 'collect') +
  plot_annotation(tag_levels = 'A', tag_suffix = ')')
ggsave('pdfs/08_Prioritizr/08c_penalty/freq_no.regret.plan_subset.pdf', gg_freq_no.regret_subset, width = 15, height = 5, dpi = 300)
ggsave('outputs/08_Prioritizr/08c_penalty/freq_no.regret.plan_subset.png', gg_freq_no.regret_subset, width = 15, height = 5, dpi = 300)

# no-regret plans
for(k in 1:nrow(target)){
  s <- get(paste0('s_no.regret_target_', target[k,1]))
  g <- ggplot() +
    geom_sf(data = s, aes(fill = as.logical(final_solution)), color = 'grey64', size = 0.02) +
    scale_fill_manual(name = "Solution",
                      values = pal_rich,
                      labels = solution) +
    geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
    coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
             ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
             expand = TRUE) +
    labs(title = paste0('Target: ', target[k,2], '%')) + 
    theme_bw()
  assign(paste0('gg_b_no.regret_target_',target[k,1]), g)
  rm(s, g)
}
# all
gg_no.regret_all <- gg_b_no.regret_target_100 + gg_b_no.regret_target_90 + gg_b_no.regret_target_80 + gg_b_no.regret_target_70 +
  gg_b_no.regret_target_60 + gg_b_no.regret_target_50 + gg_b_no.regret_target_40 + gg_b_no.regret_target_30 + gg_b_no.regret_target_20 +
  plot_layout(ncol = 3, nrow = 3, guides = 'collect') +
  plot_annotation(tag_levels = 'A', tag_suffix = ')')
ggsave('pdfs/08_Prioritizr/08c_penalty/no.regret.plan_all.pdf', gg_no.regret_all, width = 21, height = 29.7, dpi = 300)
# subset
gg_no.regret_subset <- gg_b_no.regret_target_90 + gg_b_no.regret_target_60 + gg_b_no.regret_target_30 + cost_noregret_b +
  protected.area_noregret_b +
  plot_layout(nrow = 2, ncol = 3, guides = 'collect') +
  plot_annotation(tag_levels = 'A', tag_suffix = ')')
ggsave('pdfs/08_Prioritizr/08c_penalty/no.regret.plan_subset.pdf', gg_no.regret_subset, width = 10, height = 5, dpi = 300)
ggsave('outputs/08_Prioritizr/08c_penalty/no.regret.plan_subset.png', gg_no.regret_subset, width = 10, height = 5, dpi = 300)

###################################
###### Creating stat figures ######
###################################
# creating feature summaries
no.regret_list <- list()
for(k in 1:nrow(target)) {
  ss <- get(paste0('s_no.regret_target_', target[k,1])) %>% 
    filter(final_solution > 0) %>% 
    st_as_sf(column = 'geometry')
  no.regret_list[[k]] <- ss %>% 
    mutate(area_km2 = as.numeric(st_area(ss)/1e+06)) %>% 
    dplyr::summarize(sum_area = sum(area_km2), total_cost = sum(cost_1)) %>% 
    mutate(percent_area = sum_area*100/total_area, num_pu = length(st_geometry(ss))) %>% 
    mutate(target = as.numeric(target[k,1])) %>% 
    as_tibble()
}
no.regret_summary_b <- bind_rows(no.regret_list) %>% 
  as_tibble() %>% 
  dplyr::select(-geometry)
write_csv(no.regret_summary_b, 'outputs/08_Prioritizr/08c_penalty/summary_noregret.csv')

cost_noregret_b <- ggplot(data = no.regret_summary_b %>% filter(target %in% c('30','60','90')), aes(x = as.factor(target*0.25))) +
  geom_bar(aes(y = log10(total_cost)), stat = 'identity', position = position_dodge(), fill = 'thistle3') +
  xlab("Target (%)") + 
  ylab("log10(cost)") +
  theme(legend.position = "bottom") +
  theme_classic()

protected.area_noregret_b <- ggplot(data = no.regret_summary_b %>% filter(target %in% c('30','60','90')), aes(x = as.factor(target*0.25))) +
  geom_bar(aes(y = percent_area), stat = 'identity', position = position_dodge(), fill = 'indianred2') +
  xlab("Target (%)") + 
  ylab("Protected Area (%)") +
  theme(legend.position = "bottom") +
  theme_classic()
# add these two objects to summaries of noregret subsets above!

##########################################################################
###### Creating Kappa Corrplots for selected spatial plan solutions ######
##########################################################################
s126 <- solve_SSP126_target90
s245 <- solve_SSP245_target90
s585 <- solve_SSP585_target90
suninformed <- solve_uninformed_target90
dir <- 'outputs/08_Prioritizr/08c_penalty/'

pdf('pdfs/08_Prioritizr/08c_penalty/kappa_target90.pdf', width = 8, height = 8)
kappa_90 <- create_kappacorrplot(s126, s245, s585, suninformed, dir)
dev.off()

s126 <- solve_SSP126_target60
s245 <- solve_SSP245_target60
s585 <- solve_SSP585_target60
suninformed <- solve_uninformed_target60
dir <- 'outputs/08_Prioritizr/08c_penalty/'

pdf('pdfs/08_Prioritizr/08c_penalty/kappa_target60.pdf', width = 8, height = 8)
kappa_60 <- create_kappacorrplot(s126, s245, s585, suninformed, dir)
dev.off()

s126 <- solve_SSP126_target30
s245 <- solve_SSP245_target30
s585 <- solve_SSP585_target30
suninformed <- solve_uninformed_target30
dir <- 'outputs/08_Prioritizr/08c_penalty/'

pdf('pdfs/08_Prioritizr/08c_penalty/kappa_target30.pdf', width = 8, height = 8)
kappa_30 <- create_kappacorrplot(s126, s245, s585, suninformed, dir)
dev.off()
