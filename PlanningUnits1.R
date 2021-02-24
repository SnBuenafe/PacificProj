library(tidyverse)
library(sf)
library(rgdal)

#calling shape file; converting to sf object and transforming it to mollweide projection
po_pu1<-readOGR(dsn="~/GitHub/PacificProject/data/World_High_Seas_v1/High_Seas_v1.shp",layer="High_Seas_v1") %>% 
  st_as_sf() %>% 
  st_transform(crs="+proj=robin +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

print(po_pu1)
head(po_pu1)

#plotting the high seas
ggplot(data=po_pu1,aes=(geometry=geometry))+
  geom_sf(color="black",fill="cadetblue2")+
  theme_bw()

#i'm not sure about the projections...i know they have to be the same but i don't know which one to use (I'm using Robinson's because I saw an example on the net that used it to centralize the map to the PO)

#setting boundaries for the Pacific Ocean (code from Jase); using (very) approximate coordinates for PO
#still experimenting but it's not working out :(
Bndry <- tibble(x = seq(140, 160, by = 1), y = 60) %>% # Start with N boundary (-20N)
  bind_rows(tibble(x = 160, y = seq(60, -40, by = -1))) %>% # Then bind to E boundary (160E)
  bind_rows(tibble(x = seq(160, 140, by = -1), y = -40)) %>% # Then S boundary (-40N) - reverse y order
  bind_rows(tibble(x = 140, y = seq(-40, 60, by = 1))) %>% # Then W boundary (140E) - reverse x order
  as.matrix() %>%
  list() %>%
  st_polygon() %>%
  st_sfc(crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% # Current crs
  st_transform(crs = "+proj=utm +zone=56 +south +datum=WGS84 +units=m +no_defs") # Transform crs to m

ggplot()+
  geom_sf(data=po_pu1,color="black",fill="cadetblue2")+
  geom_sf(data=Bndry,color="blue",fill=NA)+
  theme_bw()
