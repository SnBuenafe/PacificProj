# Helper functions for tuna script

### PlotVisreg
PlotVisreg <- function(Model, Variable, Ylab, Xlab, MaxPA){
  p1 <- visreg(Model, Variable, partial = FALSE, scale = "response", ylab = Ylab, xlab = Xlab, gg = TRUE) + 
    theme_bw() + ylim(0, MaxPA)
return(p1)
}

### fOrganizedf (organizing dataframe for plotting using PlotMap() )
fOrganizedf <- function(species, predictions){
  library(MBA)
  library(reshape2)
  
  Surface <- mba.surf(cbind(dat %>% 
                              filter(Species == species) %>% 
                              select("Longitude", "Latitude"), predictions), 1000, 1000)
  
  # This is just to organise dataframe for plotting
  dimnames(Surface$xyz.est$z) <- list(Surface$xyz.est$x, Surface$xyz.est$y)
  df <- melt(Surface$xyz.est$z, varnames = c('Longitude', 'Latitude'), value.name = 'Preds') %>% 
    mutate(Preds2 = (Preds >= median(predictions))*1)
  return(df)
}

### PlotMap
library(tidyverse)
WorldData <- map_data('world')
WorldData %>% filter(region != "Antarctica") -> WorldData
WorldData <- fortify(WorldData)

PlotMap <- function(df, Y){
  p <- ggplot(data = df, aes(x = Longitude, y = Latitude)) +
    geom_raster(aes_string(fill = Y)) +
    scale_fill_gradientn(colours = matlab.like(7), na.value = "white") +
    theme(legend.position="right") + 
    geom_map(data = WorldData, map = WorldData, 
             aes(x = long, y = lat, group = group, map_id = region), 
             fill = "grey", colour = "grey", size = 0.5) + 
    theme_minimal()
}

