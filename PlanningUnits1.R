#written by Tin Buenafe, 2021

library(tidyverse)
library(sf)
library(rgdal)

#calling shape file; converting to sf object and transforming it to mollweide projection
LandMass<-readOGR(dsn="~/GitHub/PacificProject_data/data/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp",layer="EEZ_Land_v3_202030") %>% 
  st_as_sf() %>% 
  st_transform(crs="+proj=moll +datum=WGS84 +ellps=WGS84 +units=m +no_defs")

print(po_pu1)

#plotting the high seas
ggplot(data=LandMass,aes=(geometry=geometry))+
  geom_sf(color="black",fill="cadetblue2")+
  theme_bw()

#i'm not sure about the projections...i know they have to be the same but i don't know which one to use (I'm using Robinson's because I saw an example on the net that used it to centralize the map to the PO)

#setting boundaries for the Pacific Ocean (code from Jase); using (very) approximate coordinates for PO
#still experimenting but it's not working out :(
Bndry <- tibble(x = seq(-135, 135, by = 1), y = 60) %>% # Start with N boundary (-20N)
  bind_rows(tibble(x = 135, y = seq(60, -60, by = -1))) %>% # Then bind to E boundary (160E)
  bind_rows(tibble(x = seq(135, -135, by = -1), y = -60)) %>% # Then S boundary (-40N) - reverse y order
  bind_rows(tibble(x = -135, y = seq(-60, 60, by = 1))) %>% # Then W boundary (140E) - reverse x order
  as.matrix() %>%
  list() %>%
  st_polygon() %>%
  st_sfc(crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% # Current crs
  st_transform(crs = "+proj=moll +datum=WGS84 +units=m +no_defs") # Transform crs to m

plot(Bndry)

ggplot()+
  geom_sf(data=LandMass,color="black",fill="cadetblue2")+
  geom_sf(data=Bndry,color="blue",fill=NA)+
  theme_bw()


