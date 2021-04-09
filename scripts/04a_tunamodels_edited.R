library(tidyverse)
library(effects)
library(splines)
library(ggplot2)
library(devtools)
library(ggiraphExtra)
library(ggiraph)
library(mgcv)
library(visreg)
library(patchwork)

# calling data

tuna_data <- read.csv("inputs/mercer/TunaData.txt", sep="")
tuna_data <- na.omit(tuna_data)

# Very high chlorophyll max

#Very high chlorophyll max
hist(tuna_data$Chl)
tuna_data <- tuna_data %>% mutate(Chl = replace(Chl, Chl > 5, 5)) # Set max Chl to 5

# Reorder Seasons so they are in order
tuna_data <- tuna_data %>% mutate(Season2 = replace(Season2, Season2 == "summer", 2)) %>% 
  mutate(Season2 = replace(Season2, Season2 == "winter", 4)) %>% 
  mutate(Season2 = replace(Season2, Season2 == "autumn", 3)) %>% 
  mutate(Season2 = replace(Season2, Season2 == "spring", 1))

#subset based on species
yft <- subset(tuna_data, species == "yellowfin tuna")
alba <- subset(tuna_data, species == "albacore")
sword <- subset(tuna_data, species == "swordfish")
skip <- subset(tuna_data, species == "skipjack tuna")

###################################
# Doing GAMs for each subset
###################################

###################################
# Yellowfin Tuna
###################################

# Look at the data by plotting a global map of YFT
WorldData <- map_data('world')
WorldData %>% filter(region != "Antarctica") -> WorldData
WorldData <- fortify(WorldData)
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "grey", colour = "grey", size = 0.5) +
  geom_point(data = yft, aes(x = Longitude, y = Latitude), size = 0.2) + 
  facet_wrap(~pa)

# Creating GAM for all variables
m1 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = yft, family = "binomial")
summary(m1)

# Plotting response of all variables
par(mfrow = c(3,3))
visreg(m1, "SST", partial = FALSE)
visreg(m1, "Season2", partial = FALSE)
visreg(m1, "MLD", partial = FALSE)
visreg(m1, "Latitude", partial = FALSE)
visreg(m1, "Longitude", partial = FALSE)
visreg(m1, "Bathymetry", partial = FALSE)
visreg(m1, "Dist2Coast", partial = FALSE) # Dist2Coast seems to make more sense that Bathymetry
visreg(m1, "Nitrate", partial = FALSE)
visreg(m1, "Chl", partial = FALSE)
dev.copy2pdf(file = "outputs/commercial/plots/YFT_FullModel.pdf", paper = "A4r")

# Some of the relationships look too wiggly, and might not make sense
# First, let's see what we can drop using BIC
summary(m1) # Everything significant because lots of df, but least sig is Bathymetry, then Nitrate, then Chl
# Deviance explained = 14.4%, R-squared (adjusted) = 0.1

# Removing Bathymetry
m2 <- update(m1, ~ . -s(Bathymetry))
BIC(m1, m2) # m2 has lower BIC (i.e. Bathymetry n.s.)

# Removing Nitrate
m3 <- update(m2, ~ . -s(Nitrate))
BIC(m2, m3) # m3 has lower BIC (i.e. Nitrate n.s.)

# Removing Chl
m4 <- update(m3, ~ . -s(Chl))
BIC(m3, m4) # m3 has lower BIC (i.e. Chl should be retained)

# Let's use Chl with fewer dfs though, so not so wiggly
m5 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Dist2Coast) + s(Chl, k = 4), data = yft, family = "binomial")
# NOTE: m5 will have higher BIC (less wiggly), but probably more realistic

par(mfrow = c(2,3))
visreg(m5, "SST", partial = FALSE)
visreg(m5, "Season2", partial = FALSE)
visreg(m5, "MLD", partial = FALSE)
visreg(m5, "Dist2Coast", partial = FALSE)
visreg(m5, "Chl", partial = FALSE)
vis.gam(m5, c("Latitude", "Longitude"), type = "response", ticktype = "detailed", xlab = "\nLatitude (oC)", 
        ylab = "Longitude", zlab = "\nPresence/Absence", color = "cm", theta = 45, phi = 10, r = 100) # also "grey"
summary(m5) # 13.4% Deviance Explained; R^2: 0.0927

#######################################
# Plotting best model as a map
#######################################

# Plot m5 as a map
yft$Preds <- predict.gam(m5, type = "response")
# Writing the data and predictions into a .csv
write_csv(yft, file = "inputs/mercer/yft.csv")

library(MBA) # Does bilinear interpolation
library(reshape2) # For melt
library(colorRamps) # for Matlab like colour scheme
library(ggthemes) # for theme_minimal()

Surface <- mba.surf(yft[, c("Longitude", "Latitude", "Preds")], 1000, 1000)

# This is just to organise dataframe for plotting
dimnames(Surface$xyz.est$z) <- list(Surface$xyz.est$x, Surface$xyz.est$y)
df3 <- melt(Surface$xyz.est$z, varnames = c('Longitude', 'Latitude'), value.name = 'Preds')

# Plot the map
x11(width = 14, height = 7)
p <- ggplot(data = df3, aes(Longitude, Latitude)) +
  geom_raster(aes(fill = Preds)) +
  scale_fill_gradientn(colours = matlab.like(7), na.value = "white") +
  theme_minimal() +
  theme(legend.position="right")

p <- p + geom_map(data = WorldData, map = WorldData,
                  aes(x = long, y = lat, group = group, map_id = region),
                  fill = "grey", colour = "grey", size = 0.5)
p 
ggsave("outputs/commercial/plots/YFT_map.png", p, dpi = 1200)

###################################
# Albacore
###################################

# Look at the data by plotting a global map of Alba
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "grey", colour = "grey", size = 0.5) +
  geom_point(data = alba, aes(x = Longitude, y = Latitude), size = 0.2) + 
  facet_wrap(~pa)

# Creating GAM for all variables
m10 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = alba, family = "binomial")
summary(m10) # Everything significant except Bathymetry and Distance to Coast

# Plotting response of all variables
par(mfrow = c(3,3))
visreg(m10, "SST", partial = FALSE)
visreg(m10, "Season2", partial = FALSE)
visreg(m10, "MLD", partial = FALSE)
visreg(m10, "Latitude", partial = FALSE)
visreg(m10, "Longitude", partial = FALSE)
visreg(m10, "Bathymetry", partial = FALSE)
visreg(m10, "Dist2Coast", partial = FALSE) # Dist2Coast seems to make more sense that Bathymetry
visreg(m10, "Nitrate", partial = FALSE)
visreg(m10, "Chl", partial = FALSE)
# Some of the relationships look too wiggly, and might not make sense

# NOTE: There are wide SEs for SST and Chl
# Fix these cutting off data (not removing any points though)
alba$SST[alba$SST<10] <- 10 # Few data <10oC, so make them 10oC
alba$Chl[alba$Chl>2] <- 2 # Few data >2, so make them 2

# First, let's see what we can drop using BIC
m10 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = alba, family = "binomial")
summary(m10) # Everything significant except Bathymetry and Dist2Coast
# Deviance explained = 30.9%, R-sq. (adjusted) = 0.185

# Removing Bathymetry
m11 <- update(m10, ~ . -s(Bathymetry))
BIC(m11, m10) # m11 has lower BIC (i.e. Bathymetry n.s.)
summary(m11)

# Dist2Coast n.s. but looks important in plot - let's see
m12 <- update(m11, ~ . -s(Dist2Coast))
BIC(m12, m11) # m12 has lower BIC (Dist2Coast n.s.)
summary(m12)

# Removing Nitrate
m13 <- update(m12, ~ . -s(Nitrate))
BIC(m13, m12) # m13 has lower BIC (Nitrate n.s.)
summary(m13)

# Removing MLD
m14 <- update(m13, ~ . -s(MLD))
BIC(m14, m13) # m14 has lower BIC (MLD n.s.)
summary(m14)

# Try removing SST
m15 <- update(m14, ~ . -s(SST))
BIC(m15, m14) # m14 has lower BIC (i.e. SST should be retained)

# Plotting retained variables
par(mfrow = c(2,3))
visreg(m14, "SST", partial = FALSE, ylim = c(-50, 0))
visreg(m14, "SST", partial = FALSE, ylim = c(0, 0.02), scale = "response")
visreg(m14, "Season2", partial = FALSE)
visreg(m14, "Chl", partial = FALSE, ylim = c(-20, 0))
visreg(m14, "Chl", partial = FALSE, ylim = c(0, 0.1), scale = "response")
vis.gam(m14, c("Latitude", "Longitude"), ticktype = "detailed", xlab = "\nLatitude (oC)", 
        ylab = "Longitude", zlab = "\nPresence/Absence", color = "cm", theta = 45, phi = 10, r = 100) # also "grey"
summary(m14) # Deviance explained = 29.3%; R-sq (adjusted) = 0.174

#######################################
# Plotting best model as a map
#######################################

# Plot m14 as a map
alba$Preds <- predict.gam(m14, type = "response")
write_csv(alba, file = "inputs/mercer/alba.csv")

Surface <- mba.surf(alba[, c("Longitude", "Latitude", "Preds")], 1000, 1000)

# This is just to organise dataframe for plotting
dimnames(Surface$xyz.est$z) <- list(Surface$xyz.est$x, Surface$xyz.est$y)
df3 <- melt(Surface$xyz.est$z, varnames = c('Longitude', 'Latitude'), value.name = 'Preds')

# Plot the map
x11(width = 14, height = 7)
p <- ggplot(data = df3, aes(Longitude, Latitude)) +
  geom_raster(aes(fill = Preds)) +
  scale_fill_gradientn(colours = matlab.like(7), na.value = "white") +
  theme_minimal() +
  theme(legend.position="right")

p <- p + geom_map(data = WorldData, map = WorldData,
                  aes(x = long, y = lat, group = group, map_id = region),
                  fill = "grey", colour = "grey", size = 0.5)
p 
ggsave("outputs/commercial/plots/Alba_map.png", p, dpi = 1200)

###################################
# Skipjack Tuna
###################################

# Look at the data by plotting a global map of Alba
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "grey", colour = "grey", size = 0.5) +
  geom_point(data = skip, aes(x = Longitude, y = Latitude), size = 0.2) + 
  facet_wrap(~pa)

# Creating GAM for all variables
m16 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = skip, family = "binomial")
summary(m16) # Everything significant except Bathymetry and Distance to Coast
# Deviance explained = 15.6%, R-sq.(adj) = 0.123

# Plotting response of all variables
par(mfrow = c(3,3))
visreg(m16, "SST", partial = FALSE)
visreg(m16, "Season2", partial = FALSE)
visreg(m16, "MLD", partial = FALSE)
visreg(m16, "Latitude", partial = FALSE)
visreg(m16, "Longitude", partial = FALSE)
visreg(m16, "Bathymetry", partial = FALSE)
visreg(m16, "Dist2Coast", partial = FALSE) # Dist2Coast seems to make more sense that Bathymetry
visreg(m16, "Nitrate", partial = FALSE)
visreg(m16, "Chl", partial = FALSE) # Chl has a lot of peaks

# Select best model based on BIC
m17 <- update(m16, ~ . -s(Bathymetry)) # Remove Bathymetry as it is not significant
BIC(m16, m17) # BIC is lower for m17; remove bathymetry
summary(m17) # Deviance explained = 15.5%; R-sq(adj.) = 0.122

# All the variables are significant, so let's just try to remove some of the variables one by one

# Try removing SST
m18 <- update(m17, ~ . -s(SST))
BIC(m17, m18) # BIC is lower for m17; SST is important

# Try removing MLD
m19 <- update(m17, ~. -s(MLD))
BIC(m17, m19)
summary(m19) # BIC is a little bit smaller, but not by much. It also decreases the deviance explained by a bit.
# Try to retain MLD. (Looks like it's important according the the plots)

# Try removing Dist2Coast (but this looks important according to the plots)
m20 <- update(m17, ~ . -s(Dist2Coast))
BIC(m17, m20) #BIC is larger; retain Dist2Coast
summary(m20)

# Try removing Nitrate (also looks important).
m21 <- update(m17, ~ . -s(Nitrate))
BIC(m17, m21) # Not much change in BIC; a little bit smaller.
summary(m21) # Also decreases the deviance explained by a bit.
# Try to retain Nitrate

# Try changing the df of Chlorophyll because there are too many peaks
m22 <- update(m17, ~. -s(Chl) +s(Chl, k = 4))
BIC(m17, m22) # Made the BIC smaller.
summary(m22) # Deviance explained = 15.2%, R-sq. (adj.) = 0.119
# Retain Chl with 4 degrees of freedom.
# Probably the best model.

#######################################
# Plotting best model as a map
#######################################

# Plot m22 as a map
skip$Preds <- predict.gam(m22, type = "response")
write_csv(skip, file = "inputs/mercer/skip.csv")

Surface <- mba.surf(skip[, c("Longitude", "Latitude", "Preds")], 1000, 1000)

# This is just to organise dataframe for plotting
dimnames(Surface$xyz.est$z) <- list(Surface$xyz.est$x, Surface$xyz.est$y)
df3 <- melt(Surface$xyz.est$z, varnames = c('Longitude', 'Latitude'), value.name = 'Preds')

# Plot the map
x11(width = 14, height = 7)
p <- ggplot(data = df3, aes(Longitude, Latitude)) +
  geom_raster(aes(fill = Preds)) +
  scale_fill_gradientn(colours = matlab.like(7), na.value = "white") +
  theme_minimal() +
  theme(legend.position="right")

p <- p + geom_map(data = WorldData, map = WorldData,
                  aes(x = long, y = lat, group = group, map_id = region),
                  fill = "grey", colour = "grey", size = 0.5)
p 
ggsave("outputs/commercial/plots/Skip_map.png", p, dpi = 1200)

###################################
# Swordfish
###################################

# Look at the data by plotting a global map of Swordfish
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "grey", colour = "grey", size = 0.5) +
  geom_point(data = sword, aes(x = Longitude, y = Latitude), size = 0.2) + 
  facet_wrap(~pa)

# Creating GAM for all variables
m23 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = sword, family = "binomial")
summary(m23) # Everything significant except Bathymetry.
# Deviance explained = 22.2%, R-sq.(adj) = 0.118

# Plotting response of all variables
par(mfrow = c(3,3))
visreg(m23, "SST", partial = FALSE)
visreg(m23, "Season2", partial = FALSE)
visreg(m23, "MLD", partial = FALSE)
visreg(m23, "Latitude", partial = FALSE)
visreg(m23, "Longitude", partial = FALSE)
visreg(m23, "Bathymetry", partial = FALSE)
visreg(m23, "Dist2Coast", partial = FALSE)
visreg(m23, "Nitrate", partial = FALSE)
visreg(m23, "Chl", partial = FALSE) # Chl has a lot of peaks

# Select best model based on BIC
m24 <- update(m23, ~ . -s(Bathymetry)) # Remove Bathymetry as it is not significant
BIC(m23, m24) # BIC is lower for m24; remove bathymetry
summary(m24) # Deviance explained = 21.9%; R-sq(adj.) = 0.116

# Try removing Dist2Coast
m25 <- update(m24, ~. -s(Dist2Coast))
BIC(m24,m25) # BIC is lower for m25
summary(m25) # Deviance explained = 21.7%; R-sq (adj.) = 0.115
# Remove Dist2Coast?
# Everything else is significant.

#######################################
# Plotting best model as a map
#######################################

# Plot m25 as a map
sword$Preds <- predict.gam(m25, type = "response")
write_csv(sword, file = "inputs/mercer/sword.csv")

Surface <- mba.surf(sword[, c("Longitude", "Latitude", "Preds")], 1000, 1000)

# This is just to organise dataframe for plotting
dimnames(Surface$xyz.est$z) <- list(Surface$xyz.est$x, Surface$xyz.est$y)
df3 <- melt(Surface$xyz.est$z, varnames = c('Longitude', 'Latitude'), value.name = 'Preds')

# Plot the map
x11(width = 14, height = 7)
p <- ggplot(data = df3, aes(Longitude, Latitude)) +
  geom_raster(aes(fill = Preds)) +
  scale_fill_gradientn(colours = matlab.like(7), na.value = "white") +
  theme_minimal() +
  theme(legend.position="right")

p <- p + geom_map(data = WorldData, map = WorldData,
                  aes(x = long, y = lat, group = group, map_id = region),
                  fill = "grey", colour = "grey", size = 0.5)
p 
ggsave("outputs/commercial/plots/Sword_map.png", p, dpi = 1200)

############################################################
# Doing GAMs for each subset (Fitting using Pacific Data)
############################################################
# Yellowfin Tuna
yft_pacific <- subset(yft, ocean =="pacific")

# Creating GAM for all variables
m26 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = yft_pacific, family = "binomial")
summary(m26) # Everything significant except Nitrate & low significance on Bathymetry
# Deviance explained = 17.2%; R-sq.(adj) = 0.13

# Plotting response of all variables
par(mfrow = c(3,3))
visreg(m26, "SST", partial = FALSE)
visreg(m26, "Season2", partial = FALSE)
visreg(m26, "MLD", partial = FALSE)
visreg(m26, "Latitude", partial = FALSE)
visreg(m26, "Longitude", partial = FALSE)
visreg(m26, "Bathymetry", partial = FALSE)
visreg(m26, "Dist2Coast", partial = FALSE)
visreg(m26, "Nitrate", partial = FALSE)
visreg(m26, "Chl", partial = FALSE)
visreg(m26, "Chl", partial = FALSE, scale = "response")

# Remove nitrate because it's not significant
m27 <- update(m26, ~. -s(Nitrate))
BIC(m26, m27) # BIC of m27 is smaller; remove nitrate
summary(m27)

m28 <- update(m27, ~. -s(Chl) +s(Chl, k = 4)) # changing df of Chl because it's too wiggly and doesn't really make sense
BIC(m27, m28) # BIC of m28 is smaller; keep these df.
summary(m28)
# Would retain other variables because everything is significant.

# Plotting response to the variables included in the final model.
par(mfrow = c(3,3))
visreg(m28, "SST", partial = FALSE)
visreg(m28, "MLD", partial = FALSE)
visreg(m28, "Bathymetry", partial = FALSE)
visreg(m28, "Dist2Coast", partial = FALSE)
visreg(m28, "Chl", partial = FALSE)
visreg(m28, "Season2", partial = FALSE)
vis.gam(m28, c("Latitude", "Longitude"), type = "response", ticktype = "detailed", xlab = "\nLatitude (oC)", 
        ylab = "Longitude", zlab = "\nPresence/Absence", color = "cm", theta = 45, phi = 10, r = 100) # also "grey"

#######################################
# Plotting best model as a map
#######################################

# Plot m28 as a map
yft_pacific$Preds <- predict.gam(m28, type = "response")
write_csv(yft_pacific, file = "inputs/mercer/yft_pacific.csv")

Surface <- mba.surf(yft_pacific[, c("Longitude", "Latitude", "Preds")], 1000, 1000)

# This is just to organise dataframe for plotting
dimnames(Surface$xyz.est$z) <- list(Surface$xyz.est$x, Surface$xyz.est$y)
df3 <- melt(Surface$xyz.est$z, varnames = c('Longitude', 'Latitude'), value.name = 'Preds')

# Plot the map
x11(width = 14, height = 7)
p <- ggplot(data = df3, aes(Longitude, Latitude)) +
  geom_raster(aes(fill = Preds)) +
  scale_fill_gradientn(colours = matlab.like(7), na.value = "white") +
  theme_minimal() +
  theme(legend.position="right")

p <- p + geom_map(data = WorldData, map = WorldData,
                  aes(x = long, y = lat, group = group, map_id = region),
                  fill = "grey", colour = "grey", size = 0.5)
p 
ggsave("outputs/commercial/plots/Yft_Pacific_map.png", p, dpi = 1200)

# Albacore
alba_pacific <- subset(alba, ocean =="pacific")

# Creating GAM for all variables
m29 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = alba_pacific, family = "binomial")
summary(m29) # The only significant variables are SST, Lat, Long and Chl
# Deviance explained = 33.3%; R-sq.(adj) = 0.206

# Plotting response of all variables
par(mfrow = c(3,3))
visreg(m29, "SST", partial = FALSE)
visreg(m29, "Season2", partial = FALSE)
visreg(m29, "MLD", partial = FALSE)
visreg(m29, "Latitude", partial = FALSE)
visreg(m29, "Longitude", partial = FALSE)
visreg(m29, "Bathymetry", partial = FALSE)
visreg(m29, "Dist2Coast", partial = FALSE)
visreg(m29, "Nitrate", partial = FALSE)
visreg(m29, "Chl", partial = FALSE)

# Remove MLD as it is not significant
m30 <- update(m29, ~. -s(MLD))
BIC(m29, m30) # BIC is smaller. Remove MLD.
summary(m30) 

# Remove Bathymetry as it is not significant
m31 <- update(m30, ~. -s(Bathymetry))
BIC(m30, m31) # BIC is a little bit higher.
summary(m31)
# Try to retain bathymetry

# Remove Nitrate as it is not significant.
m32 <- update(m30, ~. -s(Nitrate))
BIC(m30, m32) # BIC is smaller. Remove Nitrate.
summary(m32)

# Try removing Dist2Coast as it is not significant.
m33 <- update(m32, ~. -s(Dist2Coast))
BIC(m32, m33) # BIC is smaller. Remove Dist2Coast
summary(m33) # Best model?
# Deviance explained = 32.3%; R-sq.(adj) = 0.198

# Plotting response to the variables included in the final model.
par(mfrow = c(3,2))
visreg(m33, "SST", partial = FALSE)
visreg(m33, "Bathymetry", partial = FALSE)
visreg(m33, "Chl", partial = FALSE)
visreg(m33, "Season2", partial = FALSE)
vis.gam(m33, c("Latitude", "Longitude"), type = "response", ticktype = "detailed", xlab = "\nLatitude (oC)", 
        ylab = "Longitude", zlab = "\nPresence/Absence", color = "cm", theta = 45, phi = 10, r = 100) # also "grey"

#######################################
# Plotting best model as a map
#######################################

# Plot m33 as a map
alba_pacific$Preds <- predict.gam(m33, type = "response")
write_csv(alba_pacific, file = "inputs/mercer/alba_pacific.csv")

Surface <- mba.surf(alba_pacific[, c("Longitude", "Latitude", "Preds")], 1000, 1000)

# This is just to organise dataframe for plotting
dimnames(Surface$xyz.est$z) <- list(Surface$xyz.est$x, Surface$xyz.est$y)
df3 <- melt(Surface$xyz.est$z, varnames = c('Longitude', 'Latitude'), value.name = 'Preds')

# Plot the map
x11(width = 14, height = 7)
p <- ggplot(data = df3, aes(Longitude, Latitude)) +
  geom_raster(aes(fill = Preds)) +
  scale_fill_gradientn(colours = matlab.like(7), na.value = "white") +
  theme_minimal() +
  theme(legend.position="right")

p <- p + geom_map(data = WorldData, map = WorldData,
                  aes(x = long, y = lat, group = group, map_id = region),
                  fill = "grey", colour = "grey", size = 0.5)
p 
ggsave("outputs/commercial/plots/Alb_Pacific_map.png", p, dpi = 1200)

# Skipjack Tuna
skip_pacific <- subset(skip, ocean =="pacific")

# Creating GAM for all variables
m34 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = skip_pacific, family = "binomial")
summary(m34) # Dist2Coast is not significant
# Deviance explained = 18.2%; R-sq. adj.

# Plot the variables of the full model.
par(mfrow = c(3,3))
visreg(m34, "SST", partial = FALSE)
visreg(m34, "Season2", partial = FALSE)
visreg(m34, "MLD", partial = FALSE)
visreg(m34, "Latitude", partial = FALSE)
visreg(m34, "Longitude", partial = FALSE)
visreg(m34, "Bathymetry", partial = FALSE)
visreg(m34, "Dist2Coast", partial = FALSE)
visreg(m34, "Nitrate", partial = FALSE)
visreg(m34, "Chl", partial = FALSE)

# Try to remove Dist2Coast as it is not significant.
m35 <- update(m34, ~. -s(Dist2Coast))
BIC(m34, m35) # BIC is smaller. Remove Dist2Coast.
summary(m35)

# Try to remove Nitrate.
m36 <- update(m35, ~. -s(Nitrate))
BIC(m35, m36)
summary(m36) # BIC is smaller. Remove Nitrate.
# R-sq.(adj) = 0.147; Deviance explained = 18%
# Best model?

# Plotting response to the variables included in the final model.
par(mfrow = c(3,2))
visreg(m36, "SST", partial = FALSE)
visreg(m36, "MLD", partial = FALSE)
visreg(m36, "Bathymetry", partial = FALSE)
visreg(m36, "Chl", partial = FALSE)
visreg(m36, "Season2", partial = FALSE)
vis.gam(m36, c("Latitude", "Longitude"), type = "response", ticktype = "detailed", xlab = "\nLatitude (oC)", 
        ylab = "Longitude", zlab = "\nPresence/Absence", color = "cm", theta = 45, phi = 10, r = 100) # also "grey"

#######################################
# Plotting best model as a map
#######################################

# Plot m33 as a map
skip_pacific$Preds <- predict.gam(m36, type = "response")
write_csv(skip_pacific, file = "inputs/mercer/skip_pacific.csv")

Surface <- mba.surf(skip_pacific[, c("Longitude", "Latitude", "Preds")], 1000, 1000)

# This is just to organise dataframe for plotting
dimnames(Surface$xyz.est$z) <- list(Surface$xyz.est$x, Surface$xyz.est$y)
df3 <- melt(Surface$xyz.est$z, varnames = c('Longitude', 'Latitude'), value.name = 'Preds')

# Plot the map
x11(width = 14, height = 7)
p <- ggplot(data = df3, aes(Longitude, Latitude)) +
  geom_raster(aes(fill = Preds)) +
  scale_fill_gradientn(colours = matlab.like(7), na.value = "white") +
  theme_minimal() +
  theme(legend.position="right")

p <- p + geom_map(data = WorldData, map = WorldData,
                  aes(x = long, y = lat, group = group, map_id = region),
                  fill = "grey", colour = "grey", size = 0.5)
p 
ggsave("outputs/commercial/plots/Skip_Pacific_map.png", p, dpi = 1200)

# Swordfish
sword_pacific <- subset(sword, ocean =="pacific")

# Creating GAM for all variables
m37 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = sword_pacific, family = "binomial")
summary(m37) # Everything significant except Bathymetry and Dist2Coast
# Deviance explained = 20.1%; R-sq.(adj) = 0.0897

# Plot the variables from the full model.
par(mfrow = c(3,3))
visreg(m37, "SST", partial = FALSE)
visreg(m37, "Season2", partial = FALSE)
visreg(m37, "MLD", partial = FALSE)
visreg(m37, "Latitude", partial = FALSE)
visreg(m37, "Longitude", partial = FALSE)
visreg(m37, "Bathymetry", partial = FALSE)
visreg(m37, "Dist2Coast", partial = FALSE)
visreg(m37, "Nitrate", partial = FALSE)
visreg(m37, "Chl", partial = FALSE)

# Remove Dist2Coast.
m38 <- update(m37, ~. -s(Dist2Coast))
BIC(m37, m38) # BIC is smaller; Remove Dist2Coast
summary(m38)

# Remove Bathymetry.
m39 <- update(m38, ~. -s(Bathymetry))
BIC(m38, m39) # BIC is smaller; 
summary(m39) # Deviance explained = 19.7%

# Try removing MLD
m40 <- update(m39, ~. -s(MLD))
BIC(m39, m40) # BIC is smaller; Remove MLD.
summary(m40) # Deviance explained = 19.1%; R-sq.(adj) = 0.0841
# Best model?

# Try removing Chl
m41 <- update(m40, ~. -s(Chl))
BIC(m40, m41) # BIC change is not big. Retain Chl.

# Plotting response to the variables included in the final model.
par(mfrow = c(3,2))
visreg(m40, "SST", partial = FALSE)
visreg(m40, "Nitrate", partial = FALSE)
visreg(m40, "Chl", partial = FALSE)
visreg(m40, "Season2", partial = FALSE)
vis.gam(m40, c("Latitude", "Longitude"), type = "response", ticktype = "detailed", xlab = "\nLatitude (oC)", 
        ylab = "Longitude", zlab = "\nPresence/Absence", color = "cm", theta = 45, phi = 10, r = 100) # also "grey"

#######################################
# Plotting best model as a map
#######################################

# Plot m40 as a map
sword_pacific$Preds <- predict.gam(m40, type = "response")
write_csv(sword_pacific, file = "inputs/mercer/sword_pacific.csv")

Surface <- mba.surf(sword_pacific[, c("Longitude", "Latitude", "Preds")], 1000, 1000)

# This is just to organise dataframe for plotting
dimnames(Surface$xyz.est$z) <- list(Surface$xyz.est$x, Surface$xyz.est$y)
df3 <- melt(Surface$xyz.est$z, varnames = c('Longitude', 'Latitude'), value.name = 'Preds')

# Plot the map
x11(width = 14, height = 7)
p <- ggplot(data = df3, aes(Longitude, Latitude)) +
  geom_raster(aes(fill = Preds)) +
  scale_fill_gradientn(colours = matlab.like(7), na.value = "white") +
  theme_minimal() +
  theme(legend.position="right")

p <- p + geom_map(data = WorldData, map = WorldData,
                  aes(x = long, y = lat, group = group, map_id = region),
                  fill = "grey", colour = "grey", size = 0.5)
p 
ggsave("outputs/commercial/plots/Sword_Pacific_map.png", p, dpi = 1200)
