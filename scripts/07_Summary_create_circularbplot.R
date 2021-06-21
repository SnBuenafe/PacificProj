create_circularbplot <- function(df, target) {
  # colors
  scenario.legend_color <- c('SSP126' = 'yellow3', 'SSP245' = 'orange2', 
                             'SSP585' = 'salmon4', 'uninformed' = 'lightslategray', 'NA' = NA)
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
  
  data <- df.manip
 
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
    geom_segment(data = grid_data, 
                 aes(x = end, y = 30, xend = start, yend = 30), 
                 colour = "grey", 
                 alpha = 1,
                 size = 0.5,
                 inherit.aes = FALSE ) +
    annotate("text", x = rep(max(data$id),6), 
             y = c(5, 10, 15, 20, 25, 30), 
             label = c('5','10','15','20','25', '30'), 
             color = "grey", 
             size=4, 
             angle = -5, 
             fontface = "bold", 
             hjust=0.5) +
    
    # setting limitations of actual plot
    ylim(-50,40) +
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
                 aes(x = start, y = 35, xend = end, yend = 35, color = individual), 
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
    )
 
  return(p)
}  