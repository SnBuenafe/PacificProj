# All models (global-fitted) are from James Mercer's code.
# 05a creates the GAMs for each of the species.
# It saves the predictions (with the environmental variables and the coordinates) as a .csv file. (dir: input/mercer/)
# Saves the visreg plots and maps. (dir: outputs/05_Commercial/05a_GAMPlots/)
# There are 8 parts to 05a; the first 4 of which are global-fitted data:
# 1. 05a1: yellowfin
# 2. 05a2: albacore
# 3. 05a3: swordfish
# 4. 05a4: skipjack
# The code must be run one after the other.

source("scripts/commercial/Tuna_Helpers.R") # Load helper functions

####################################################################################
####### Defining packages needed
####################################################################################
# List of pacakges that we will use
list.of.packages <- c("tidyverse", "effects", "splines", "devtools", "ggiraphExtra",
                      "ggiraph", "mgcv", "visreg", "patchwork", "MBA", "reshape2",
                      "colorRamps", "ggthemes")
# MBA: for bilinear interpolation; reshape2: for melt
# If is not installed, install the pacakge
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)

dat <- read.csv("inputs/mercer/TunaData_final.txt", sep="") %>% 
  na.omit()

# Very high chlorophyll max
hist(dat$Chl)

###################################################################
# Pre-processing before GAMs to reduce wide SEs (max of the data)
# THIS IS NOT REALLY WHY YOU DO IT. IT IS BECAUSE THERE IS VERY FEW DATA ABOVE THESE VALUES AND THIS IS WHAT
# LEADS TO THE WIDE SEs
# And reorder Seasons so they are in order
##################################################################

MinSST <- floor(min(dat$SST[dat$pa == 1])) # Minimum SST for the GAMs. Binomial regressions
# work poorly when all zero values

MaxNitrate <- ceiling(max(dat$Nitrate[dat$pa == 1])) # Maximum Nitrate for the GAMs. Binomial regressions
# work poorly when all zero values
# MaxNitrate is 7, but set to 6 because Swordfish CIs huge when set to 7
MaxNitrate <- 6

dat <- dat %>% 
  mutate(Season2 = replace(Season2, Season2 == "summer", 2), 
         Season2 = replace(Season2, Season2 == "winter", 4), 
         Season2 = replace(Season2, Season2 == "autumn", 3), 
         Season2 = replace(Season2, Season2 == "spring", 1), 
         SST = replace(SST, SST < 10, 10), 
         Chl = replace(Chl, Chl > 2, 2), 
         MLD = replace(MLD, MLD > 100, 100), 
         Nitrate = replace(Nitrate, Nitrate > MaxNitrate, MaxNitrate)) %>% 
  rename(Species = species, 
         Season = season, 
         Ocean = ocean,
         PA = pa) %>% 
  filter(SST > MinSST) # Only warmer temperatures

###################################
# GAMs
###################################

###################################
# Yellowfin Tuna
###################################

# Chl-a is often very wiggly in the models (partly because very few high values of Chl-a), so constrain it using log10
m1 <- gam(PA ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + 
            s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + log10(Chl), 
          data = dat %>% filter(Species == "yellowfin tuna"), family = "binomial")
summary(m1)
par(mfrow = c(2,2))
gam.check(m1) # Not that informative for binomial error structures
graphics.off() # Reset graphics device

# Try removing Bathymetry (it's got the lowest p-level)
m2 <- update(m1, ~ . -s(Bathymetry))
BIC(m1, m2) # m2 has lower BIC (i.e. Bathymetry n.s.)
summary(m2)

# Try removing Chl - it's now got the lowest p-level
m3 <- update(m2, ~ . -log10(Chl))
BIC(m2, m3) # m3 has lower BIC (i.e. Remove Chl)
summary(m3)

# Try removing Nitrate - it's now got the lowest p-level
m4 <- update(m3, ~ . -s(Nitrate))
BIC(m3, m4) # m4 has lower BIC (i.e. Nitrate n.s.)
summary(m4)

# Try removing MLD - it's now got the lowest p-level
m5 <- update(m4, ~ . -s(MLD))
BIC(m4, m5) # m4 has lower BIC, so retain MLD, and m4 is the best model
summary(m5)

# Saving predictions
yft_preds <- as.numeric(predict.gam(m4, type = "response"))
median(yft_preds)

# Plot model effects
MaxPA <- 0.20
p1 <- PlotVisreg(m4, "SST", Ylab = " Probability occurrence", Xlab = "SST", MaxPA)
p1

p2 <- PlotVisreg(m4, "Season2", Ylab = " ", Xlab = "Season", MaxPA)
p2

p3 <- PlotVisreg(m4, "MLD", Ylab = " ", Xlab = "MLD", MaxPA)
p3

p4 <- PlotVisreg(m4, "Dist2Coast", Ylab = " Probability occurrence", Xlab = "Distance to coast", MaxPA)
p4

# p5 <- visreg2d(m4, yvar = "Latitude", xvar = "Longitude", scale = "response", 
#          plot.type = "persp", theta = 45, phi = 10, r = 100, 
#          ticktype = "detailed", xlab = "\nLongitude (º)", 
#          ylab = "\nLatitude (º)", zlab = "\nPresence", 
#          color = "deepskyblue2")
# p5
# NOTE: Can't convert visreg2d to a gg object (which is needed to use patchwork easily) when using plot.type = "persp", so use contour plot
p5 <- visreg2d(m4, yvar = "Latitude", xvar = "Longitude", scale = "response", plot.type = "gg")
p5

# Combine plots for yft
(p1 | p2 | p3) / (p4 | plot_spacer() | p5) &
  theme_bw(base_size = 18)
ggsave("YFT_Final.png", width = 15, height = 10, dpi = 600)


#######################################
# Plotting best model as a map
#######################################

# Defining generalities (used for all subsequent 04a)
WorldData <- map_data('world')
WorldData %>% filter(region != "Antarctica") -> WorldData
WorldData <- fortify(WorldData)
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "grey", colour = "grey", size = 0.5) +
  geom_point(data = dat %>% filter(Species == "yellowfin tuna"), aes(x = Longitude, y = Latitude), size = 0.2) + 
  facet_wrap(~PA)

Surface <- mba.surf(cbind(dat %>% 
                            filter(Species == "yellowfin tuna") %>% 
                            select("Longitude", "Latitude"), yft_preds), 1000, 1000)

# This is just to organise dataframe for plotting
dimnames(Surface$xyz.est$z) <- list(Surface$xyz.est$x, Surface$xyz.est$y)
df <- melt(Surface$xyz.est$z, varnames = c('Longitude', 'Latitude'), value.name = 'Preds') %>% 
  mutate(Preds2 = (Preds >= median(yft_preds))*1)

# Plot the map
x11(width = 14, height = 7)

p <- PlotMap(df, "Preds")
p
# ggsave("outputs/05_Commercial/05a_GAMPlots/05a1_YFT/YFT_map.png", p, dpi = 1200)
ggsave("YFT_map.png", p, dpi = 600)

p <- PlotMap(df, "Preds2")
p
ggsave("YFT_map_presence.png", p, dpi = 600)

# Cleanup
rm(p, p1, p2, p3, p4, p5, m1, m2, m3, m4, m5, Surface, df, yft_preds)

###################################
# Albacore
###################################
m1 <- gam(PA ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + 
            s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + log10(Chl), 
          data = dat %>% filter(Species == "albacore"), family = "binomial")
summary(m1)

# Try removing Bathymetry (it's got the lowest p-level)
m2 <- update(m1, ~ . -s(Bathymetry))
BIC(m1, m2) # m2 has lower BIC (i.e. Bathymetry n.s.)
summary(m2)

# Try removing Dist2Coast - it's now got the lowest p-level
m3 <- update(m2, ~ . -s(Dist2Coast))
BIC(m2, m3) # m3 has lower BIC (i.e. Remove Dist2Coast)
summary(m3)

# Try removing Nitrate - it's now got the lowest p-level
m4 <- update(m3, ~ . -s(Nitrate))
BIC(m3, m4) # m4 has lower BIC (i.e. Remove Nitrate)
summary(m4)

# Try removing MLD - it's now got the lowest p-level
m5 <- update(m4, ~ . -s(MLD))
BIC(m4, m5) # m5 has lower BIC (i.e. Remove MLD)
summary(m5)

MaxPA <- 0.20
p1 <- PlotVisreg(m5, "SST", Ylab = " Probability occurrence", Xlab = "SST", MaxPA)
p1

p2 <- PlotVisreg(m5, "Season2", Ylab = " ", Xlab = "Season", MaxPA)
p2

p3 <- PlotVisreg(m5, "Chl", Ylab = " ", Xlab = "Chlorophyll", MaxPA)
p3

p4 <- visreg2d(m4, yvar = "Latitude", xvar = "Longitude", scale = "response", plot.type = "gg")
p4

# Combine plots for albacore
x11(width = 15, height = 10)
(p1 | p2 | plot_spacer()) / (p3 | plot_spacer() | p4) &
  theme_bw(base_size = 18)
ggsave("Albacore_Final.png", width = 15, height = 10, dpi = 600)
rm(m1, m2, m3, m4, m5, p1, p2, p3, p4)

### Set min SST as 15oC?

###################################
# Swordfish
###################################
m1 <- gam(PA ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + 
            s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + log10(Chl), 
          data = dat %>% filter(Species == "swordfish"), family = "binomial")
summary(m1)

# Try removing Bathymetry (it's got the lowest p-level)
m2 <- update(m1, ~ . -s(Bathymetry))
BIC(m1, m2) # m2 has lower BIC (i.e. Bathymetry n.s.)
summary(m2)

# Try removing Dist2Coast - it's now got the lowest p-level
m3 <- update(m2, ~ . -s(Dist2Coast))
BIC(m2, m3) # m3 has lower BIC (i.e. Remove Dist2Coast)
summary(m3)

# Try removing MLD - it's now got the lowest p-level
m4 <- update(m3, ~ . -s(MLD))
BIC(m3, m4) # m5 has lower BIC (i.e. Remove MLD)
summary(m4)

# Try removing Chl- it's now got the lowest p-level
m5 <- update(m4, ~ . -log10(Chl))
BIC(m4, m5) # m4 has lower BIC (i.e. Keep Chl)
summary(m4)

MaxPA <- 0.50
p1 <- PlotVisreg(m4, "SST", Ylab = " Probability occurrence", Xlab = "SST", MaxPA)
p1

p2 <- PlotVisreg(m4, "Season2", Ylab = " ", Xlab = "Season", MaxPA)
p2

p3 <- PlotVisreg(m4, "Chl", Ylab = " ", Xlab = "Chlorophyll", MaxPA)
p3

p4 <- PlotVisreg(m4, "Nitrate", Ylab = " ", Xlab = "Nitrate", MaxPA)
p4

p5 <- visreg2d(m4, yvar = "Latitude", xvar = "Longitude", scale = "response", plot.type = "gg")
p5

# Combine plots for albacore
x11(width = 15, height = 10)
(p1 | p2 | p3) / (p4 | plot_spacer() | p5) &
  theme_bw(base_size = 18)
ggsave("Swordfish_Final.png", width = 15, height = 10, dpi = 600)

rm(m1, m2, m3, m4, m5, p1, p2, p3, p4, p5)

###################################
# Skipjack
###################################

m1 <- gam(PA ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + 
            s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + log10(Chl), 
          data = dat %>% filter(Species == "skipjack tuna"), family = "binomial")
summary(m1)

# Try removing Bathymetry (it's got the lowest p-level)
m2 <- update(m1, ~ . -s(Bathymetry))
BIC(m1, m2) # m2 has lower BIC (i.e. Bathymetry n.s.)
summary(m2)

# Try removing Chl- it's now got the lowest p-level
m3 <- update(m2, ~ . -log10(Chl))
BIC(m2, m3) # m3 has lower BIC (i.e. drop Chl)
summary(m3)

# Try removing MLD - it's now got the lowest p-level
m4 <- update(m3, ~ . -s(MLD))
BIC(m3, m4) # m5 has lower BIC (i.e. Remove MLD)
summary(m4)

# Try removing Nitrate - it's now got the lowest p-level
m5 <- update(m4, ~ . -s(Nitrate))
BIC(m4, m5) # m4 has lower BIC (i.e. Keep Nitrate)
summary(m4)

MaxPA <- 0.20
p1 <- PlotVisreg(m4, "SST", Ylab = " Probability occurrence", Xlab = "SST", MaxPA)
p1

p2 <- PlotVisreg(m4, "Season2", Ylab = " ", Xlab = "Season", MaxPA)
p2

p3 <- PlotVisreg(m4, "Dist2Coast", Ylab = " ", Xlab = "Distance to coast", MaxPA)
p3

p4 <- PlotVisreg(m4, "Nitrate", Ylab = " ", Xlab = "Nitrate", 1)
p4

p5 <- visreg2d(m4, yvar = "Latitude", xvar = "Longitude", scale = "response", plot.type = "gg")
p5

# Combine plots for albacore
x11(width = 15, height = 10)
(p1 | p2 | p3) / (p4 | plot_spacer() | p5) &
  theme_bw(base_size = 18)
ggsave("Skipjack_Final.png", width = 15, height = 10, dpi = 600)

rm(m1, m2, m3, m4, m5, p1, p2, p3, p4, p5)

