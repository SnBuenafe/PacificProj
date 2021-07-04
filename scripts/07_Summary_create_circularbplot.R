create_circularbplot <- function(df, target) {
  # colors
  scenario.legend_color <- c('SSP126' = 'darkseagreen', 'SSP245' = 'tan3', 
                             'SSP585' = 'salmon4', 'uninformed' = 'grey30', 'NA' = NA)
  scenario.legend_list <- c('SSP126', 'SSP245', 'SSP585', 'uninformed', ' ')
  species.legend_color <- c('Albacore tuna' = 'cornflowerblue', 'Skipjack tuna' = 'cadetblue4', 
                            'Swordfish' = 'skyblue', 'Yellowfin tuna' = 'paleturquoise3',
                            'Green turtle' = 'olivedrab', 'Hawksbill turtle' = 'darkolivegreen3', 
                            'Leatherback turtle' = 'darkseagreen', 'Loggerhead turtle' = 'limegreen', 
                            'Olive ridley turtle' = 'springgreen1')
  group.legend_color <- c('bycatch' = 'chartreuse4', 'commercial' = 'royalblue')
  
  
  # manipulating df
  target <- as.numeric(target)
  df.manip <- df %>% 
    dplyr::mutate(scenario = case_when(str_detect(plan, pattern = 'SSP126') ~ 'SSP126',
                                str_detect(plan, pattern = 'SSP245') ~ 'SSP245',
                                str_detect(plan, pattern = 'SSP585') ~ 'SSP585',
                                str_detect(plan, pattern = 'uninformed') ~ 'uninformed'),
           group = case_when(str_detect(features, pattern = "_") ~ "bycatch",
                                str_detect(features, pattern = "ALB") ~ "commercial",
                                str_detect(features, pattern = "YFT") ~ "commercial",
                                str_detect(features, pattern = "SKP") ~ "commercial",
                                str_detect(features, pattern = "SWO") ~ "commercial")) %>% 
    dplyr::mutate(individual = case_when(str_detect(features, pattern = "Caretta_caretta_IUCN") ~ "Loggerhead turtle",
                                str_detect(features, pattern = "Chelonia_mydas_IUCN") ~ "Green turtle",
                                str_detect(features, pattern = "Dermochelys_coriacea_IUCN") ~ "Leatherback turtle",
                                str_detect(features, pattern = "Eretmochelys_imbricata_IUCN") ~ "Hawksbill turtle",
                                str_detect(features, pattern = "Lepidochelys_olivacea_IUCN") ~ "Olive ridley turtle",
                                str_detect(features, pattern = "ALB") ~ "Albacore tuna",
                                str_detect(features, pattern = "SKP") ~ "Skipjack tuna",
                                str_detect(features, pattern = "SWO") ~ "Swordfish",
                                str_detect(features, pattern = "YFT") ~ "Yellowfin tuna")) %>% 
    dplyr::rename(value = representation)
  
  # adding NA rows for each feature
  ALB <- data.frame(features = 'ALB', plan = NA, value = 0, scenario = NA, group = 'commercial', individual = 'Albacore tuna')
  Caretta <- data.frame(features = 'Caretta_caretta_IUCN', plan = NA, value = 0, scenario = NA, group = 'bycatch')
  Chelonia <- data.frame(features = 'Chelonia_mydas_IUCN', plan = NA, value = 0, scenario = NA, group = 'bycatch')
  Dermochelys <- data.frame(features  = 'Dermochelys_coriacea_IUCN', plan = NA, value = 0, scenario = NA, group = 'bycatch')
  Eretmochelys <- data.frame(features  = 'Eretmochelys_imbricata_IUCN', plan = NA, value = 0, scenario = NA, group = 'bycatch')
  Lepidochelys <- data.frame(features  = 'Lepidochelys_olivacea_IUCN', plan = NA, value = 0, scenario = NA, group = 'bycatch')
  SKP <- data.frame(features  = 'SKP', plan = NA, value = 0, scenario = NA, group = 'commercial')
  SWO <- data.frame(features  = 'SWO', plan = NA, value = 0, scenario = NA, group = 'commercial')
  YFT <- data.frame(features  = 'YFT', plan = NA, value = 0, scenario = NA, group = 'commercial')
  

  data <- df.manip %>% 
    bind_rows(ALB) %>% 
    bind_rows(Caretta) %>% 
    bind_rows(Chelonia) %>% 
    bind_rows(Dermochelys) %>% 
    bind_rows(Eretmochelys) %>% 
    bind_rows(Lepidochelys) %>% 
    bind_rows(SKP) %>% 
    bind_rows(SWO) %>% 
    bind_rows(YFT) %>% 
    group_by(group) %>% 
    arrange(features)
  
  # creating plot
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 1
  to_add <- data.frame(matrix(NA, empty_bar*length(unique(data$group)), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$group <- rep(levels(as.factor(data$group)), each=empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(group)
  data$id <- seq(1, nrow(data)) 
  
  # prepare a data frame for base lines
  base_data <- data %>% 
    group_by(group) %>% 
    dplyr::summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    dplyr::mutate(title = mean(c(start, end)))
  
  ##########################
  ## Data/lines of species ##
  ##########################
  species_data <- data %>% 
    group_by(individual) %>% 
    dplyr::summarize(start = min(id), end = max(id)) %>% 
    dplyr::mutate(title = mean(c(start, end)))
  
  species_data[1,3] <- 30

  # Get the name and the y position of each label for the species
  label_sp <- data %>% 
    group_by(individual) %>% 
    dplyr::summarize(individual = unique(individual, na.rm = TRUE),
              id = mean(id, na.rm = TRUE))
  number_of_bar <- nrow(label_sp)
  angle <- 360 - 90 * (label_sp$id - 2.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_sp$hjust <- ifelse( angle < -90, 1, 0)
  label_sp$angle <- ifelse(angle < -90, angle+180, angle)
  
  # for the percentage lines
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1.5
  grid_data$start <- grid_data$end - 1
  grid_data <- grid_data[-1,]
  
  p <- ggplot(data, aes(x = as.factor(id), y = value, fill = scenario)) + 
    
    # plotting the bars
    geom_bar(aes(x = as.factor(id), y = value, fill = scenario), 
             stat = "identity", 
             position = 'dodge') +
    
    # defining colors of the bars
    scale_fill_manual(name = "Solution",
                      values = scenario.legend_color,
                      labels = scenario.legend_list) +
    
    # Add text showing the value of each 100/75/50/25 lines
    geom_segment(data = grid_data, 
                 aes(x = end, y = 10, xend = start, yend = 10), 
                 colour = "grey50", 
                 alpha = 1, 
                 size = 0.5 , 
                 inherit.aes = FALSE ) +
    geom_segment(data = grid_data, 
                 aes(x = end, y = 20, xend = start, yend = 20), 
                 colour = "grey50", 
                 alpha = 1,
                 size = 0.5,
                 inherit.aes = FALSE ) +
    geom_segment(data = grid_data, 
                 aes(x = end, y = 30, xend = start, yend = 30), 
                 colour = "grey50", 
                 alpha = 1,
                 size = 0.5,
                 inherit.aes = FALSE ) +
    geom_segment(data = grid_data, 
                 aes(x = end, y = 40, xend = start, yend = 40), 
                 colour = "grey50", 
                 alpha = 1,
                 size = 0.5,
                 inherit.aes = FALSE ) +
    geom_segment(data = grid_data, 
                 aes(x = end, y = 50, xend = start, yend = 50), 
                 colour = "grey50", 
                 alpha = 1,
                 size = 0.5,
                 inherit.aes = FALSE ) +
    annotate("text", x = rep(max(data$id),5), 
             y = c(10, 20, 30, 40, 50), 
             label = c('10','20','30','40','50'), 
             color = "grey50", 
             size=4, 
             angle = -5, 
             fontface = "bold", 
             hjust=0.5) +
    
    # setting limitations of actual plot
    ylim(-50,55) +
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
              aes(x = title, y = -8, label = group), 
              color = group.legend_color,
              hjust = c(1,0), 
              alpha = 0.8, 
              size = 4, 
              fontface = "bold", 
              inherit.aes = FALSE, 
              show.legend = FALSE) +
    
    # Adding the lines for the species
    geom_segment(data = species_data, 
                 aes(x = start, y = 55, xend = end, yend = 55, color = individual), 
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
                  size = 0.8) +
    
    theme(
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(0.5,4), "cm") 
    )
 
  return(p)
}  