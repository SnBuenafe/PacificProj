# used to pivot the features into presence and absence

create_object <- function(x, cost) {
  library(tidyverse)
  species <- x %>% 
    dplyr::select(c(new_features, cellsID)) %>% 
    mutate(Presence = 1) %>% # if the feature has the particular cellsID, it is present
    pivot_wider(names_from = new_features, values_from = Presence) %>% 
    replace(is.na(.), 0)
  
  object <- x %>%
    dplyr::select(-new_features) %>% 
    distinct(cellsID, .keep_all = TRUE) %>% 
    left_join(species, by = "cellsID") %>% 
    right_join(cost, by = c("cellsID","geometry")) %>% 
    st_as_sf(sf_column_name = "geometry")
  
  return(object)
}