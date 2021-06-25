# make sure objects from 08b have been created before running these
# just doing it for one target: 25%

# SSP126 max's for: RCE = 115.6165 (x2), velocity = 50.06828 (x4), cost = 813.37098
# SSP245 max's for: RCE = 211.538 (x1), velocity = 184.4885 (x1.5)
# SSP585 max's for: RCE = 1006.79 (x0.2), velocity = 731.6799 (x0.3)

##################################################
## Run for 50% weight for both RCE and velocity ##
##################################################
# creating problems and solutions across climate-scenarios (as i)
scenario_list <- c('SSP126', 'SSP245', 'SSP585', 'uninformed')
for(i in 1:length(scenario_list)){
    x <- get(paste0('x_',scenario_list[i]))
    p <- problem(x, features_list_uninformed, 'cost') %>% 
      add_min_set_objective() %>% 
      add_binary_decisions() %>% 
      add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
      add_relative_targets(as.numeric(target[k,2]/100))
    if(i == 1){
      p %<>% add_linear_penalties(3.518, 'RCE') %>% 
        add_linear_penalties(8.123, 'velocity')
    }else if(i == 2){
      p %<>% add_linear_penalties(0.017, 'RCE') %>% 
        add_linear_penalties(2.204, 'velocity')
    }else if(i == 3){
      p %<>% add_linear_penalties(1.65E-05, 'RCE') %>% 
        add_linear_penalties(5.56E-01, 'velocity')
    }else{}
    s <- solve(p) %>% 
      st_as_sf(sf_column_name = 'geometry')
    assign(paste0('prob_sens_', scenario_list[i], '_50%'),p)
    assign(paste0('solve_sens_', scenario_list[i], '_50%'),s)
    rm(x,p,s)
}

## plotting solutions
for(i in 1:length(scenario_list)) {
  s <- get(paste0('solve_sens_', scenario_list[i], "_50%")) %>% 
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
  assign(paste0('gg_sens',scenario_list[i],'_50%'), gg)
  
  rm(s, gg)
}
gg1 <- get(paste0('gg_sens', scenario_list[1], '_50%'))
gg2 <- get(paste0('gg_sens', scenario_list[2], '_50%'))
gg3 <- get(paste0('gg_sens', scenario_list[3], '_50%'))
gg4 <- get(paste0('gg_sens', scenario_list[4], '_50%'))

gg_targ <- gg1 + gg2 + gg3 + gg4 +
  plot_layout(nrow = 1, guides = "collect") +
  plot_annotation(tag_levels = 'A',
                  tag_suffix = ')',
                  title = '50% weight')
assign(paste0('gg_full_sens_50'), gg_targ)
rm(gg1, gg2, gg3, gg4, gg_targ)
ggsave('outputs/09_Tests/spatial_plans_sens_50.png', gg_full_sens_50, width = 15, height = 5, dpi = 300)


# summary stats!
total_area = 31917*2667.6
summary <- list()
  for(i in 1:length(scenario_list)) {
    s <- get(paste0('solve_sens_', scenario_list[i], "_50%"))
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
summary #[[i]], with i for each scenario
summary_stat <- bind_rows(summary)

scenario.legend_color <- c('SSP126' = 'yellow3', 'SSP245' = 'orange2', 
                           'SSP585' = 'salmon4', 'uninformed' = 'lightslategray', 'NA' = NA)

# intersecting RCE and velocity with uninformed plans across all climate scenarios
stack <- list()
for(i in 1:length(scenario_list)) { # only until 3rd
  if(i < 4){
    num = 100
    RCE_temp <- readRDS(paste0(RCE_directory, 'RCE', scenario_list[i], '.rds')) %>% 
        as_tibble() %>% 
        dplyr::select(-area_km2, -trans_value, -rce_categ) %>% 
        rename(RCE = value)
      velocity_temp <- readRDS(paste0(velocity_directory, 'velocity', scenario_list[i], '.rds')) %>% 
        as_tibble() %>% 
        dplyr::select(-area_km2, -trans_value, -velo_categ) %>% 
        rename(velocity = value)
      xx <- get(paste0('solve_sens_uninformed','_50%')) %>% 
        as_tibble() %>% 
        dplyr::select(cellsID, geometry, solution_1)
      yy <- full_join(xx, RCE_temp, by = c('cellsID','geometry')) %>% 
        full_join(velocity_temp, by = c('cellsID','geometry')) %>% 
        dplyr::filter(solution_1 == 1) %>% 
        dplyr::summarise(median_RCE = median(RCE), median_velocity = median(velocity)) %>% 
        dplyr::mutate(scenario = 'uninformed', uninformed = scenario_list[i])
      yy$target <- num
      zz <- summary_stat %>% 
        filter(scenario == scenario_list[i]) %>% 
        dplyr::select(median_velocity, median_RCE, target, scenario) %>% 
        dplyr::mutate(uninformed = scenario_list[i])
      stack[[i]] <- bind_rows(yy, zz)
      rm(RCE_temp, velocity_temp, xx, yy, zz, num)
    }else{}
  }
stack_df <- bind_rows(stack)

stack_df <- stack_df %>% 
  dplyr::mutate(target = ifelse(target == 100, 25, NA))

med.RCE_subset_50 <- ggplot(data = stack_df, aes(x = as.factor(uninformed), group = scenario)) +
  geom_bar(aes(y = median_RCE, fill = scenario), stat = 'identity', position = 'stack') +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Climate Scenario") + 
  ylab("Median RCE") +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

med.velocity_subset_50 <- ggplot(data = stack_df, aes(x = as.factor(uninformed), group = scenario)) +
  geom_bar(aes(y = median_velocity, fill = scenario), stat = 'identity', position = 'stack') +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Climate Scenario") + 
  ylab("Median Climate Velocity") +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

cost_subset_50 <- ggplot(data = summary_stat, aes(x = as.factor(target*0.25), group = scenario)) +
  geom_bar(aes(y = log10(total_cost), fill = scenario), stat = 'identity', position = position_dodge()) +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("log10(cost") +
  theme(legend.position = "bottom") +
  theme_classic()

protected.area_subset_50 <- ggplot(data = summary_stat, aes(x = as.factor(target*0.25), group = scenario)) +
  geom_bar(aes(y = percent_area, fill = scenario), stat = 'identity', position = position_dodge()) +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("Protected Area (%)") +
  theme(legend.position = "bottom") +
  theme_classic()

summary.subset_50 <- cost_subset_50 + protected.area_subset_50 + med.RCE_subset_50 + med.velocity_subset_50 +
  plot_layout(guides = 'collect', width = 15, height = 10, nrow = 1) +
  plot_annotation(tag_level = 'A', tag_suffix = ')', title = '50% weight')
ggsave('outputs/09_Tests/summary_subset_50.png', summary.subset_50, width = 15, height = 5, dpi = 300)

rm(cost_subset, protected.area_subset, med.RCE_subset, med.velocity_subset, summary_stat, stack_df)

##################################################
## Run for 40% weight for both RCE and velocity ##
##################################################
# creating problems and solutions across climate-scenarios (as i)
scenario_list <- c('SSP126', 'SSP245', 'SSP585', 'uninformed')
for(i in 1:length(scenario_list)){
  x <- get(paste0('x_',scenario_list[i]))
  p <- problem(x, features_list_uninformed, 'cost') %>% 
    add_min_set_objective() %>% 
    add_binary_decisions() %>% 
    add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
    add_relative_targets(as.numeric(target[k,2]/100))
  if(i == 1){
    p %<>% add_linear_penalties(2.814, 'RCE') %>% 
      add_linear_penalties(6.498, 'velocity')
  }else if(i == 2){
    p %<>% add_linear_penalties(0.013, 'RCE') %>% 
      add_linear_penalties(1.764, 'velocity')
  }else if(i == 3){
    p %<>% add_linear_penalties(1.32E-05, 'RCE') %>% 
      add_linear_penalties(4.45E-01, 'velocity')
  }else{}
  s <- solve(p) %>% 
    st_as_sf(sf_column_name = 'geometry')
  assign(paste0('prob_sens_', scenario_list[i], '_40%'),p)
  assign(paste0('solve_sens_', scenario_list[i], '_40%'),s)
  rm(x,p,s)
}

## plotting solutions
for(i in 1:length(scenario_list)) {
  s <- get(paste0('solve_sens_', scenario_list[i], "_40%")) %>% 
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
  assign(paste0('gg_sens',scenario_list[i],'_40%'), gg)
  
  rm(s, gg)
}
gg1 <- get(paste0('gg_sens', scenario_list[1], '_40%'))
gg2 <- get(paste0('gg_sens', scenario_list[2], '_40%'))
gg3 <- get(paste0('gg_sens', scenario_list[3], '_40%'))
gg4 <- get(paste0('gg_sens', scenario_list[4], '_40%'))

gg_targ <- gg1 + gg2 + gg3 + gg4 +
  plot_layout(nrow = 1, guides = "collect") +
  plot_annotation(tag_levels = 'A',
                  tag_suffix = ')',
                  title = '40% weight')
assign(paste0('gg_full_sens_40'), gg_targ)
rm(gg1, gg2, gg3, gg4, gg_targ)
ggsave('outputs/09_Tests/spatial_plans_sens_40.png', gg_full_sens_40, width = 15, height = 5, dpi = 300)


# summary stats!
total_area = 31917*2667.6
summary <- list()
for(i in 1:length(scenario_list)) {
  s <- get(paste0('solve_sens_', scenario_list[i], "_40%"))
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
summary #[[i]], with i for each scenario
summary_stat <- bind_rows(summary)

scenario.legend_color <- c('SSP126' = 'yellow3', 'SSP245' = 'orange2', 
                           'SSP585' = 'salmon4', 'uninformed' = 'lightslategray', 'NA' = NA)

# intersecting RCE and velocity with uninformed plans across all climate scenarios
stack <- list()
for(i in 1:length(scenario_list)) { # only until 3rd
  if(i < 4){
    num = 100
    RCE_temp <- readRDS(paste0(RCE_directory, 'RCE', scenario_list[i], '.rds')) %>% 
      as_tibble() %>% 
      dplyr::select(-area_km2, -trans_value, -rce_categ) %>% 
      rename(RCE = value)
    velocity_temp <- readRDS(paste0(velocity_directory, 'velocity', scenario_list[i], '.rds')) %>% 
      as_tibble() %>% 
      dplyr::select(-area_km2, -trans_value, -velo_categ) %>% 
      rename(velocity = value)
    xx <- get(paste0('solve_sens_uninformed','_40%')) %>% 
      as_tibble() %>% 
      dplyr::select(cellsID, geometry, solution_1)
    yy <- full_join(xx, RCE_temp, by = c('cellsID','geometry')) %>% 
      full_join(velocity_temp, by = c('cellsID','geometry')) %>% 
      dplyr::filter(solution_1 == 1) %>% 
      dplyr::summarise(median_RCE = median(RCE), median_velocity = median(velocity)) %>% 
      dplyr::mutate(scenario = 'uninformed', uninformed = scenario_list[i])
    yy$target <- num
    zz <- summary_stat %>% 
      filter(scenario == scenario_list[i]) %>% 
      dplyr::select(median_velocity, median_RCE, target, scenario) %>% 
      dplyr::mutate(uninformed = scenario_list[i])
    stack[[i]] <- bind_rows(yy, zz)
    rm(RCE_temp, velocity_temp, xx, yy, zz, num)
  }else{}
}
stack_df <- bind_rows(stack)

stack_df <- stack_df %>% 
  dplyr::mutate(target = ifelse(target == 100, 25, NA))

med.RCE_subset_40 <- ggplot(data = stack_df, aes(x = as.factor(uninformed), group = scenario)) +
  geom_bar(aes(y = median_RCE, fill = scenario), stat = 'identity', position = 'stack') +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Climate Scenario") + 
  ylab("Median RCE") +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

med.velocity_subset_40 <- ggplot(data = stack_df, aes(x = as.factor(uninformed), group = scenario)) +
  geom_bar(aes(y = median_velocity, fill = scenario), stat = 'identity', position = 'stack') +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Climate Scenario") + 
  ylab("Median Climate Velocity") +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

cost_subset_40 <- ggplot(data = summary_stat, aes(x = as.factor(target*0.25), group = scenario)) +
  geom_bar(aes(y = log10(total_cost), fill = scenario), stat = 'identity', position = position_dodge()) +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("log10(cost") +
  theme(legend.position = "bottom") +
  theme_classic()

protected.area_subset_40 <- ggplot(data = summary_stat, aes(x = as.factor(target*0.25), group = scenario)) +
  geom_bar(aes(y = percent_area, fill = scenario), stat = 'identity', position = position_dodge()) +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("Protected Area (%)") +
  theme(legend.position = "bottom") +
  theme_classic()

summary.subset_40 <- cost_subset_40 + protected.area_subset_40 + med.RCE_subset_40 + med.velocity_subset_40 +
  plot_layout(guides = 'collect', width = 15, height = 10, nrow = 1) +
  plot_annotation(tag_level = 'A', tag_suffix = ')', title = '40% weight')
ggsave('outputs/09_Tests/summary_subset_40.png', summary.subset_40, width = 15, height = 5, dpi = 300)

rm(cost_subset, protected.area_subset, med.RCE_subset, med.velocity_subset, summary_stat, stack_df)

##################################################
## Run for 30% weight for both RCE and velocity ##
##################################################
# creating problems and solutions across climate-scenarios (as i)
scenario_list <- c('SSP126', 'SSP245', 'SSP585', 'uninformed')
for(i in 1:length(scenario_list)){
  x <- get(paste0('x_',scenario_list[i]))
  p <- problem(x, features_list_uninformed, 'cost') %>% 
    add_min_set_objective() %>% 
    add_binary_decisions() %>% 
    add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
    add_relative_targets(as.numeric(target[k,2]/100))
  if(i == 1){
    p %<>% add_linear_penalties(2.111, 'RCE') %>% 
      add_linear_penalties(4.874, 'velocity')
  }else if(i == 2){
    p %<>% add_linear_penalties(0.010, 'RCE') %>% 
      add_linear_penalties(1.323, 'velocity')
  }else if(i == 3){
    p %<>% add_linear_penalties(9.91E-06, 'RCE') %>% 
      add_linear_penalties(3.33E-01, 'velocity')
  }else{}
  s <- solve(p) %>% 
    st_as_sf(sf_column_name = 'geometry')
  assign(paste0('prob_sens_', scenario_list[i], '_30%'),p)
  assign(paste0('solve_sens_', scenario_list[i], '_30%'),s)
  rm(x,p,s)
}

## plotting solutions
for(i in 1:length(scenario_list)) {
  s <- get(paste0('solve_sens_', scenario_list[i], "_30%")) %>% 
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
  assign(paste0('gg_sens',scenario_list[i],'_30%'), gg)
  
  rm(s, gg)
}
gg1 <- get(paste0('gg_sens', scenario_list[1], '_30%'))
gg2 <- get(paste0('gg_sens', scenario_list[2], '_30%'))
gg3 <- get(paste0('gg_sens', scenario_list[3], '_30%'))
gg4 <- get(paste0('gg_sens', scenario_list[4], '_30%'))

gg_targ <- gg1 + gg2 + gg3 + gg4 +
  plot_layout(nrow = 1, guides = "collect") +
  plot_annotation(tag_levels = 'A',
                  tag_suffix = ')',
                  title = '30% weight')
assign(paste0('gg_full_sens_30'), gg_targ)
rm(gg1, gg2, gg3, gg4, gg_targ)
ggsave('outputs/09_Tests/spatial_plans_sens_30.png', gg_full_sens_30, width = 15, height = 5, dpi = 300)


# summary stats!
total_area = 31917*2667.6
summary <- list()
for(i in 1:length(scenario_list)) {
  s <- get(paste0('solve_sens_', scenario_list[i], "_30%"))
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
summary #[[i]], with i for each scenario
summary_stat <- bind_rows(summary)

scenario.legend_color <- c('SSP126' = 'yellow3', 'SSP245' = 'orange2', 
                           'SSP585' = 'salmon4', 'uninformed' = 'lightslategray', 'NA' = NA)

# intersecting RCE and velocity with uninformed plans across all climate scenarios
stack <- list()
for(i in 1:length(scenario_list)) { # only until 3rd
  if(i < 4){
    num = 100
    RCE_temp <- readRDS(paste0(RCE_directory, 'RCE', scenario_list[i], '.rds')) %>% 
      as_tibble() %>% 
      dplyr::select(-area_km2, -trans_value, -rce_categ) %>% 
      rename(RCE = value)
    velocity_temp <- readRDS(paste0(velocity_directory, 'velocity', scenario_list[i], '.rds')) %>% 
      as_tibble() %>% 
      dplyr::select(-area_km2, -trans_value, -velo_categ) %>% 
      rename(velocity = value)
    xx <- get(paste0('solve_sens_uninformed','_30%')) %>% 
      as_tibble() %>% 
      dplyr::select(cellsID, geometry, solution_1)
    yy <- full_join(xx, RCE_temp, by = c('cellsID','geometry')) %>% 
      full_join(velocity_temp, by = c('cellsID','geometry')) %>% 
      dplyr::filter(solution_1 == 1) %>% 
      dplyr::summarise(median_RCE = median(RCE), median_velocity = median(velocity)) %>% 
      dplyr::mutate(scenario = 'uninformed', uninformed = scenario_list[i])
    yy$target <- num
    zz <- summary_stat %>% 
      filter(scenario == scenario_list[i]) %>% 
      dplyr::select(median_velocity, median_RCE, target, scenario) %>% 
      dplyr::mutate(uninformed = scenario_list[i])
    stack[[i]] <- bind_rows(yy, zz)
    rm(RCE_temp, velocity_temp, xx, yy, zz, num)
  }else{}
}
stack_df <- bind_rows(stack)

stack_df <- stack_df %>% 
  dplyr::mutate(target = ifelse(target == 100, 25, NA))

med.RCE_subset_30 <- ggplot(data = stack_df, aes(x = as.factor(uninformed), group = scenario)) +
  geom_bar(aes(y = median_RCE, fill = scenario), stat = 'identity', position = 'stack') +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Climate Scenario") + 
  ylab("Median RCE") +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

med.velocity_subset_30 <- ggplot(data = stack_df, aes(x = as.factor(uninformed), group = scenario)) +
  geom_bar(aes(y = median_velocity, fill = scenario), stat = 'identity', position = 'stack') +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Climate Scenario") + 
  ylab("Median Climate Velocity") +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

cost_subset_30 <- ggplot(data = summary_stat, aes(x = as.factor(target*0.25), group = scenario)) +
  geom_bar(aes(y = log10(total_cost), fill = scenario), stat = 'identity', position = position_dodge()) +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("log10(cost") +
  theme(legend.position = "bottom") +
  theme_classic()

protected.area_subset_30 <- ggplot(data = summary_stat, aes(x = as.factor(target*0.25), group = scenario)) +
  geom_bar(aes(y = percent_area, fill = scenario), stat = 'identity', position = position_dodge()) +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("Protected Area (%)") +
  theme(legend.position = "bottom") +
  theme_classic()

summary.subset_30 <- cost_subset_30 + protected.area_subset_30 + med.RCE_subset_30 + med.velocity_subset_30 +
  plot_layout(guides = 'collect', width = 15, height = 10, nrow = 1) +
  plot_annotation(tag_level = 'A', tag_suffix = ')', title = '30% weight')
ggsave('outputs/09_Tests/summary_subset_30.png', summary.subset_30, width = 15, height = 5, dpi = 300)

rm(cost_subset, protected.area_subset, med.RCE_subset, med.velocity_subset, summary_stat, stack_df)

##################################################
## Run for 20% weight for both RCE and velocity ##
##################################################
# creating problems and solutions across climate-scenarios (as i)
scenario_list <- c('SSP126', 'SSP245', 'SSP585', 'uninformed')
for(i in 1:length(scenario_list)){
  x <- get(paste0('x_',scenario_list[i]))
  p <- problem(x, features_list_uninformed, 'cost') %>% 
    add_min_set_objective() %>% 
    add_binary_decisions() %>% 
    add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
    add_relative_targets(as.numeric(target[k,2]/100))
  if(i == 1){
    p %<>% add_linear_penalties(1.407, 'RCE') %>% 
      add_linear_penalties(3.249, 'velocity')
  }else if(i == 2){
    p %<>% add_linear_penalties(0.007, 'RCE') %>% 
      add_linear_penalties(0.882, 'velocity')
  }else if(i == 3){
    p %<>% add_linear_penalties(6.61E-06, 'RCE') %>% 
      add_linear_penalties(2.22E-01, 'velocity')
  }else{}
  s <- solve(p) %>% 
    st_as_sf(sf_column_name = 'geometry')
  assign(paste0('prob_sens_', scenario_list[i], '_20%'),p)
  assign(paste0('solve_sens_', scenario_list[i], '_20%'),s)
  rm(x,p,s)
}

## plotting solutions
for(i in 1:length(scenario_list)) {
  s <- get(paste0('solve_sens_', scenario_list[i], "_20%")) %>% 
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
  assign(paste0('gg_sens',scenario_list[i],'_20%'), gg)
  
  rm(s, gg)
}
gg1 <- get(paste0('gg_sens', scenario_list[1], '_20%'))
gg2 <- get(paste0('gg_sens', scenario_list[2], '_20%'))
gg3 <- get(paste0('gg_sens', scenario_list[3], '_20%'))
gg4 <- get(paste0('gg_sens', scenario_list[4], '_20%'))

gg_targ <- gg1 + gg2 + gg3 + gg4 +
  plot_layout(nrow = 1, guides = "collect") +
  plot_annotation(tag_levels = 'A',
                  tag_suffix = ')',
                  title = '20% weight')
assign(paste0('gg_full_sens_20'), gg_targ)
rm(gg1, gg2, gg3, gg4, gg_targ)
ggsave('outputs/09_Tests/spatial_plans_sens_20.png', gg_full_sens_20, width = 15, height = 5, dpi = 300)


# summary stats!
total_area = 31917*2667.6
summary <- list()
for(i in 1:length(scenario_list)) {
  s <- get(paste0('solve_sens_', scenario_list[i], "_20%"))
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
summary #[[i]], with i for each scenario
summary_stat <- bind_rows(summary)

scenario.legend_color <- c('SSP126' = 'yellow3', 'SSP245' = 'orange2', 
                           'SSP585' = 'salmon4', 'uninformed' = 'lightslategray', 'NA' = NA)

# intersecting RCE and velocity with uninformed plans across all climate scenarios
stack <- list()
for(i in 1:length(scenario_list)) { # only until 3rd
  if(i < 4){
    num = 100
    RCE_temp <- readRDS(paste0(RCE_directory, 'RCE', scenario_list[i], '.rds')) %>% 
      as_tibble() %>% 
      dplyr::select(-area_km2, -trans_value, -rce_categ) %>% 
      rename(RCE = value)
    velocity_temp <- readRDS(paste0(velocity_directory, 'velocity', scenario_list[i], '.rds')) %>% 
      as_tibble() %>% 
      dplyr::select(-area_km2, -trans_value, -velo_categ) %>% 
      rename(velocity = value)
    xx <- get(paste0('solve_sens_uninformed','_20%')) %>% 
      as_tibble() %>% 
      dplyr::select(cellsID, geometry, solution_1)
    yy <- full_join(xx, RCE_temp, by = c('cellsID','geometry')) %>% 
      full_join(velocity_temp, by = c('cellsID','geometry')) %>% 
      dplyr::filter(solution_1 == 1) %>% 
      dplyr::summarise(median_RCE = median(RCE), median_velocity = median(velocity)) %>% 
      dplyr::mutate(scenario = 'uninformed', uninformed = scenario_list[i])
    yy$target <- num
    zz <- summary_stat %>% 
      filter(scenario == scenario_list[i]) %>% 
      dplyr::select(median_velocity, median_RCE, target, scenario) %>% 
      dplyr::mutate(uninformed = scenario_list[i])
    stack[[i]] <- bind_rows(yy, zz)
    rm(RCE_temp, velocity_temp, xx, yy, zz, num)
  }else{}
}
stack_df <- bind_rows(stack)

stack_df <- stack_df %>% 
  dplyr::mutate(target = ifelse(target == 100, 25, NA))

med.RCE_subset_20 <- ggplot(data = stack_df, aes(x = as.factor(uninformed), group = scenario)) +
  geom_bar(aes(y = median_RCE, fill = scenario), stat = 'identity', position = 'stack') +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Climate Scenario") + 
  ylab("Median RCE") +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

med.velocity_subset_20 <- ggplot(data = stack_df, aes(x = as.factor(uninformed), group = scenario)) +
  geom_bar(aes(y = median_velocity, fill = scenario), stat = 'identity', position = 'stack') +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Climate Scenario") + 
  ylab("Median Climate Velocity") +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

cost_subset_20 <- ggplot(data = summary_stat, aes(x = as.factor(target*0.25), group = scenario)) +
  geom_bar(aes(y = log10(total_cost), fill = scenario), stat = 'identity', position = position_dodge()) +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("log10(cost") +
  theme(legend.position = "bottom") +
  theme_classic()

protected.area_subset_20 <- ggplot(data = summary_stat, aes(x = as.factor(target*0.25), group = scenario)) +
  geom_bar(aes(y = percent_area, fill = scenario), stat = 'identity', position = position_dodge()) +
  scale_fill_manual(name = 'Climate scenario',
                    values = scenario.legend_color) +
  xlab("Target (%)") + 
  ylab("Protected Area (%)") +
  theme(legend.position = "bottom") +
  theme_classic()

summary.subset_20 <- cost_subset_20 + protected.area_subset_20 + med.RCE_subset_20 + med.velocity_subset_20 +
  plot_layout(guides = 'collect', width = 15, height = 10, nrow = 1) +
  plot_annotation(tag_level = 'A', tag_suffix = ')', title = '20% weight')
ggsave('outputs/09_Tests/summary_subset_20.png', summary.subset_20, width = 15, height = 5, dpi = 300)

rm(cost_subset, protected.area_subset, med.RCE_subset, med.velocity_subset, summary_stat, stack_df)

### 
# plotting everything all together
##

summary <- cost_subset_50 + protected.area_subset_50 + med.RCE_subset_50 + med.velocity_subset_50 +
  cost_subset_40 + protected.area_subset_40 + med.RCE_subset_40 + med.velocity_subset_40 +
  cost_subset_30 + protected.area_subset_30 + med.RCE_subset_30 + med.velocity_subset_30 +
  cost_subset_20 + protected.area_subset_20 + med.RCE_subset_20 + med.velocity_subset_20 +
  plot_layout(guides = 'collect', nrow = 4, ncol = 4)
ggsave('outputs/09_Tests/summary_subset_all.png', summary, width = 20, height = 20, dpi = 300)

gg_all <- `gg_sensSSP126_50%`+`gg_sensSSP245_50%`+`gg_sensSSP585_50%`+`gg_sensuninformed_50%` +
  `gg_sensSSP126_40%`+`gg_sensSSP245_40%`+`gg_sensSSP585_40%`+`gg_sensuninformed_40%` +
  `gg_sensSSP126_30%`+`gg_sensSSP245_30%`+`gg_sensSSP585_30%`+`gg_sensuninformed_30%` +
  `gg_sensSSP126_20%`+`gg_sensSSP245_20%`+`gg_sensSSP585_20%`+`gg_sensuninformed_20%` +
  plot_layout(guides = 'collect', nrow = 4, ncol = 4)
ggsave('outputs/09_Tests/plans_all.png', gg_all, width = 29.7, height = 21, dpi = 300)
