# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This function creates circular bar plots to study the representation of each features with respect to their targets
# To run, it requires the following inputs:
# 1. bycatch = 'bycatch' or 'bycatchIUCN' for AQM and IUCN datasets, respectively
# 2. commercial = 'commercial' or 'commercialpac' for global and pacific datasets, respectively
# 3. target_inpdir = directory of the targets for each max representation target
# 4. features.summary_csv = .csv file of the summary of the solutions of prioritizr (feat representation)
# 5. target = target name (must be consistent)

# Function is run in 12h.

fCreateCircBarPlot <- function(bycatch, commercial, target_inpdir, features.summary_csv, target, ...) {
  
  ########################
  ## Defining libraries ##
  ########################
  library(tidyverse)
  library(magrittr)
  library(sf)
  
  ########################################
  ## Calling and manipulating .csv file ##
  ########################################
  feat <- read_csv(features.summary_csv)
  
  if(bycatch == 'bycatch') {
    feat_modified <- feat %>% 
      mutate(feat_type = case_when(str_detect(feat$feature, pattern = "Rep-") ~ "bycatch",
                                   str_detect(feat$feature, pattern = "ALB") ~ "commercial",
                                   str_detect(feat$feature, pattern = "YFT") ~ "commercial",
                                   str_detect(feat$feature, pattern = "SKP") ~ "commercial",
                                   str_detect(feat$feature, pattern = "SWO") ~ "commercial"))
  }else if(bycatch == 'bycatchIUCN'){
    feat_modified <- feat %>% 
      mutate(feat_type = case_when(str_detect(feat$feature, pattern = "_") ~ "bycatch",
                                   str_detect(feat$feature, pattern = "ALB") ~ "commercial",
                                   str_detect(feat$feature, pattern = "YFT") ~ "commercial",
                                   str_detect(feat$feature, pattern = "SKP") ~ "commercial",
                                   str_detect(feat$feature, pattern = "SWO") ~ "commercial"))
  }
  
  
  # summary for all scenarios with the specified target
  feat_represented <- feat_modified %>% 
    as_tibble() %>% 
    dplyr::select(feature, contains(paste0(target,'_')), feat_type)
  
  # creating generalities for loop below
  scenario_list <- c('SSP126', 'SSP245', 'SSP585', 'uninformed')
  data_final <- list()
  
  #####################################################
  ## Creating 1 df with all scenarios for the target ##
  #####################################################
  for(j in 1:length(scenario_list)) {
      
      # Bycatch
      bycatch.target_pattern <- paste0(bycatch, scenario_list[j])
      bycatch.target_file <- list.files(paste0(target_inpdir, scenario_list[j], '/'), pattern = bycatch.target_pattern)
      
      bycatch_file <- readRDS(paste0(target_inpdir, scenario_list[j], '/', bycatch.target_file)) %>% 
        as_tibble() %>% 
        dplyr::select(new_features, target) 
      if(str_detect(scenario_list[j], pattern = 'SSP') == TRUE){
        bycatch_file <- bycatch_file %>% 
        mutate(target = target*0.25)
      } else { }
      
      # Commercial
      commercial.target_pattern <- paste0(commercial, scenario_list[j])
      commercial.target_file <- list.files(paste0(target_inpdir, scenario_list[j], '/'), pattern = commercial.target_pattern)
    
      commercial_file <- readRDS(paste0(target_inpdir, scenario_list[j], '/',commercial.target_file)) %>% 
        as_tibble() %>% 
        dplyr::select(new_features, target)
      if(str_detect(scenario_list[j], pattern = 'SSP') == TRUE){
        commercial_file <- commercial_file %>% 
          mutate(target = target*0.25)
      } else { }
        
      # Joining targets for Bycatch and Commercial
      targets <- full_join(bycatch_file, commercial_file) %>% 
        rename(individual = new_features)
        
      data_temp <- feat_represented %>% 
        dplyr::select(feature, feat_type, contains(scenario_list[j])) %>% 
        rename(individual = feature,
               group = feat_type,
               value = contains(scenario_list[j])) %>% 
        mutate(value = value*100)
      
      if(bycatch == 'bycatch') {
        data_final[[j]] <- data_temp %>% 
          full_join(targets, by = 'individual') %>% 
          mutate(individual = case_when(str_detect(data_temp$individual, pattern = "Rep-1347") ~ "Loggerhead turtle",
                                        str_detect(data_temp$individual, pattern = "Rep-2226") ~ "Green turtle",
                                        str_detect(data_temp$individual, pattern = "Rep-3437") ~ "Leatherback turtle",
                                        str_detect(data_temp$individual, pattern = "Rep-4127") ~ "Hawksbill turtle",
                                        str_detect(data_temp$individual, pattern = "Rep-5414") ~ "Olive ridley turtle",
                                        str_detect(data_temp$individual, pattern = "ALB") ~ "Albacore tuna",
                                        str_detect(data_temp$individual, pattern = "SKP") ~ "Skipjack tuna",
                                        str_detect(data_temp$individual, pattern = "SWO") ~ "Swordfish",
                                        str_detect(data_temp$individual, pattern = "YFT") ~ "Yellowfin tuna")) %>% 
          mutate(scenario = scenario_list[j])
      }else if(bycatch == 'bycatchIUCN'){
        data_final[[j]] <- data_temp %>% 
          full_join(targets, by = 'individual') %>% 
          mutate(individual = case_when(str_detect(data_temp$individual, pattern = "Caretta_caretta_IUCN") ~ "Loggerhead turtle",
                                        str_detect(data_temp$individual, pattern = "Chelonia_mydas_IUCN") ~ "Green turtle",
                                        str_detect(data_temp$individual, pattern = "Dermochelys_coriacea_IUCN") ~ "Leatherback turtle",
                                        str_detect(data_temp$individual, pattern = "Eretmochelys_imbricata_IUCN") ~ "Hawksbill turtle",
                                        str_detect(data_temp$individual, pattern = "Lepidochelys_olivacea_IUCN") ~ "Olive ridley turtle",
                                        str_detect(data_temp$individual, pattern = "ALB") ~ "Albacore tuna",
                                        str_detect(data_temp$individual, pattern = "SKP") ~ "Skipjack tuna",
                                        str_detect(data_temp$individual, pattern = "SWO") ~ "Swordfish",
                                        str_detect(data_temp$individual, pattern = "YFT") ~ "Yellowfin tuna")) %>% 
          mutate(scenario = scenario_list[j])
      }
  }
  
  final_df <- do.call(rbind, data_final) %>% 
    arrange(by_group = individual)
  
  #############################################
  ### Manipulating data for bar plot ###
  ############################################# 
  
  # Mask
  data <- final_df
  
  ##################
  ## Empty Bars ##
  ##################
  empty_bar <- 1
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(as.factor(data$group)), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$group <- rep(levels(as.factor(data$group)), each=empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(group)
  data$id <- seq(1, nrow(data))
  
  ##########################
  ## Data/lines of groups ##
  ##########################
  base_data <- data %>% 
    group_by(group) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  ##########################
  ## Data/lines of species ##
  ##########################
  species_data <- data %>% 
    group_by(individual) %>% 
    summarize(start = min(id), end = max(id)) %>% 
    mutate(title = mean(c(start, end))) %>% 
    filter(!is.na(individual))
  # Get the name and the y position of each label for the species
  label_sp <- data %>% 
    group_by(individual) %>% 
    summarize(individual = unique(individual),
              id = mean(id)) %>% 
    filter(!is.na(individual))
  number_of_bar <- nrow(label_sp)
  angle <- 360 - 90 * (label_sp$id - 2.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_sp$hjust <- ifelse( angle < -90, 1, 0)
  label_sp$angle <- ifelse(angle < -90, angle+180, angle)
  
  ############################################
  ## Data/lines of scales (100, 75, 50, 25) ##
  ############################################
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1.5
  grid_data$start <- grid_data$end - 1
  grid_data <- grid_data[-1,]
  
  #############################################
  ### Defining colors for circular bar plot ###
  ############################################# 
  scenario.legend_color <- c('SSP126' = 'yellow3', 'SSP245' = 'orange2', 
                             'SSP585' = 'salmon4', 'uninformed' = 'lightslategray', 'NA' = NA)
  scenario.legend_list <- c('SSP126', 'SSP245', 'SSP585', 'uninformed', ' ')
  
  species.legend_color <- c('Albacore tuna' = 'cornflowerblue', 'Skipjack tuna' = 'cadetblue4', 
                            'Swordfish' = 'skyblue', 'Yellowfin tuna' = 'paleturquoise3',
                            'Green turtle' = 'olivedrab', 'Hawksbill turtle' = 'darkolivegreen3', 
                            'Leatherback turtle' = 'darkseagreen', 'Loggerhead turtle' = 'limegreen', 
                            'Olive ridley turtle' = 'springgreen1')
  
  group.legend_color <- c('bycatch' = 'chartreuse4', 'commercial' = 'royalblue')
  
  #########################################
  ### Creating actual circular bar plot ###
  #########################################
  p <- ggplot(data, aes(x = as.factor(id), y = value, fill = scenario)) + 
    
    # plotting the bars
    geom_bar(aes(x = as.factor(id), y = value, fill = scenario), 
             stat = "identity", 
             position = 'dodge', 
             alpha = 0.5) +
  
    # defining colors of the bars
    scale_fill_manual(name = "Solution",
                      values = scenario.legend_color,
                      labels = scenario.legend_list) +
    
    # Add text showing the value of each 100/75/50/25 lines
    geom_segment(data = grid_data, 
                 aes(x = end, y = 5, xend = start, yend = 5), 
                 colour = "grey", 
                 alpha = 1, 
                 size = 0.5 , 
                 inherit.aes = FALSE ) +
    geom_segment(data = grid_data, 
                 aes(x = end, y = 10, xend = start, yend = 10), 
                 colour = "grey", 
                 alpha = 1, 
                 size = 0.5 , 
                 inherit.aes = FALSE ) +
    geom_segment(data = grid_data, 
                 aes(x = end, y = 15, xend = start, yend = 15), 
                 colour = "grey", 
                 alpha = 1, 
                 size = 0.5, 
                 inherit.aes = FALSE ) +
    geom_segment(data = grid_data, 
                 aes(x = end, y = 20, xend = start, yend = 20), 
                 colour = "grey", 
                 alpha = 1,
                 size = 0.5,
                 inherit.aes = FALSE ) +
    geom_segment(data = grid_data, 
                 aes(x = end, y = 25, xend = start, yend = 25), 
                 colour = "grey", 
                 alpha = 1,
                 size = 0.5,
                 inherit.aes = FALSE ) +
    annotate("text", x = rep(max(data$id),5), 
             y = c(5, 10, 15, 20, 25), 
             label = c('5','10','15','20','25'), 
             color = "grey", 
             size=4, 
             angle = -5, 
             fontface = "bold", 
             hjust=0.5) +
    
    # setting limitations of actual plot
    ylim(-50,30) +
    theme_minimal() +
    coord_polar() + 
  
    # Add base line information (commercial + bycatch labels)
    geom_segment(data = base_data, 
                 aes(x = start, y = -5, xend = end, yend = -5), 
                 colour = group.legend_color,
                 alpha = 0.8, 
                 size = 0.6, 
                 inherit.aes = FALSE, 
                 show.legend = FALSE)  +
    geom_text(data = base_data, 
              aes(x = title, y = -18, label = group), 
              color = group.legend_color,
              hjust = c(1,0), 
              alpha = 0.8, 
              size = 4, 
              fontface = "bold", 
              inherit.aes = FALSE, 
              show.legend = FALSE) +
  
    # Adding the lines for the species
    geom_segment(data = species_data, 
                 aes(x = start, y = 30, xend = end, yend = 30, color = individual), 
                 alpha = 1, 
                 size = 5, 
                 inherit.aes = FALSE)  +
  
    # Defining colors of these lines
    scale_color_manual(name = "Species",
                      values = species.legend_color) +
    
    # Adding the target % for each species
    geom_errorbar(aes(y = target, ymax = target, ymin = target), 
                  color = 'red', 
                  linetype = 'dashed', 
                  size = 1) +
  
    theme(
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(0.5,4), "cm") 
    ) +
  
    ggtitle(paste0("Maximum Representation: ",target))
  
  return(p)
}
