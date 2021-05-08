fCreateRobinsonBoundary <- function(west, east, north, south, ...) {
  library(proj4)
  library(tidyverse)
  
  test<-cbind(c(east, -west, -west, east), #TopLeft, TopRight, BottomRight, BottomLeft
              c( north, north, -south, -south))
  Cnr <- proj4::project(test, proj = rob_pacific)
  
  Bndry <- tibble(x = Cnr[1:2,1] , y = Cnr[1:2,2]) %>% # Start with N boundary (51N)
    bind_rows(as_tibble(project(as.matrix(tibble(x = -west, y = seq(north, -south, by = -1))), proj = rob_pacific))) %>% # Then bind to E boundary (-78E)
    bind_rows(as_tibble(project(as.matrix(tibble(x = east, y = seq(-south, north, by = 1))), proj = rob_pacific))) %>% # Then W boundary (140E) - reverse x order
    as.matrix() %>%
    list() %>%
    st_polygon() %>%
    st_sfc(crs = rob_pacific)
  
  return(Bndry)
}