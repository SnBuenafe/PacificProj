# All models (global-fitted) are from James Mercer's code.
# 04a creates the GAMs for each of the species.
# It saves the predictions (with the environmental variables and the coordinates) as a .csv file. (dir: input/mercer/)
# Saves the visreg plots and maps. (dir: outputs/04_Commercial/04a_GAMPlots/)
# There are 8 parts to 04a; the first 4 of which are global-fitted data:
# 1. 04a1: yellowfin (where the dataset is pre-processed & packages are defined)
# 2. 04a2: albacore
# 3. 04a3: swordfish
# 4. 04a4: skipjack
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
model1 <- gam(PA ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + 
            s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + log10(Chl), 
          data = dat %>% filter(Species == "yellowfin tuna"), family = "binomial")
summary(model1)
par(mfrow = c(2,2))
gam.check(model1) # Not that informative for binomial error structures
graphics.off() # Reset graphics device

# Try removing Bathymetry (it's got the lowest p-level)
model2 <- update(model1, ~ . -s(Bathymetry))
BIC(model1, model2) # m2 has lower BIC (i.e. Bathymetry n.s.)
summary(model2)

# Try removing Chl - it's now got the lowest p-level
model3 <- update(model2, ~ . -log10(Chl))
BIC(model2, model3) # m2 has lower BIC (i.e. Retain log10(Chl))
summary(model3)

# Try removing Nitrate - it's now got the lowest p-level
model4 <- update(model2, ~ . -s(Nitrate))
BIC(model2, model4) # model4 has lower BIC (i.e. Nitrate n.s.) # model4 seems to be the best model
summary(model4)

# Saving predictions
yft_preds <- as.numeric(predict.gam(model4, type = "response"))
median(yft_preds)
# Writing the data and predictions into a .csv
yft <- dat %>% filter(Species == 'yellowfin tuna')
yft$Preds <- yft_preds
write_csv(yft, file = "inputs/mercer/yft.csv")

#######################################
# Plotting best model as a map
#######################################

# Plotting model effects
MaxPA <- 0.25
plot1 <- PlotVisreg(model4, "SST", Ylab = " Probability occurrence", Xlab = "SST", MaxPA) +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
plot1
ggsave('outputs/04_Commercial/04a_GAMPlots/04a1_YFT/bestmodel_SST.png', plot1, width = 10, height = 10, dpi = 600)

plot2 <- PlotVisreg(model4, "Season2", Ylab = " ", Xlab = "Season", MaxPA) +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
plot2
ggsave('outputs/04_Commercial/04a_GAMPlots/04a1_YFT/bestmodel_seasons.png', plot2, width = 10, height = 10, dpi = 600)


plot3 <- PlotVisreg(model4, "MLD", Ylab = " ", Xlab = "MLD", MaxPA) +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 
plot3
ggsave('outputs/04_Commercial/04a_GAMPlots/04a1_YFT/bestmodel_MLD.png', plot3, width = 10, height = 10, dpi = 600)


plot4 <- PlotVisreg(model4, "Dist2Coast", Ylab = " Probability occurrence", Xlab = "Distance to coast", MaxPA) +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 
plot4
ggsave('outputs/04_Commercial/04a_GAMPlots/04a1_YFT/bestmodel_dist2coast.png', plot4, width = 10, height = 10, dpi = 600)

plot5 <- PlotVisreg(model4, "Chl", Ylab = " ", Xlab = "Chl", MaxPA)  +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) 
plot5
ggsave('outputs/04_Commercial/04a_GAMPlots/04a1_YFT/bestmodel_Chl.png', plot5, width = 10, height = 10, dpi = 600)

# plot6 <- visreg2d(model4, yvar = "Latitude", xvar = "Longitude", scale = "response", 
#          plot.type = "persp", theta = 45, phi = 10, r = 100, 
#          ticktype = "detailed", xlab = "\nLongitude (º)", 
#          ylab = "\nLatitude (º)", zlab = "\nPresence", 
#          color = "deepskyblue2")
# plot6
# NOTE: Can't convert visreg2d to a gg object (which is needed to use patchwork easily) when using plot.type = "persp", so use contour plot
plot6 <- visreg2d(model4, yvar = "Latitude", xvar = "Longitude", scale = "response", plot.type = "gg") +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key.width = unit(1,"cm"),
        legend.text = element_text(size = 20),
        legend.title = element_blank()) 
plot6
ggsave('outputs/04_Commercial/04a_GAMPlots/04a1_YFT/bestmodel_LatLong.png', plot6, width = 10, height = 10, dpi = 600)


# Combine plots for yft
(plot1 | plot2 | plot3) / (plot4 | plot5 | plot6) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & theme_bw(base_size = 18)
ggsave("outputs/04_Commercial/04a_GAMPlots/04a1_YFT/YFT_BestModel.pdf", width = 15, height = 10, dpi = 600)

# Plot the map
x11(width = 14, height = 7)

df <- fOrganizedf('yellowfin tuna', yft_preds)
p <- PlotMap(df, "Preds")
p
ggsave("outputs/04_Commercial/04a_GAMPlots/04a1_YFT/YFT_map.png", p, dpi = 1200)

p <- PlotMap(df, "Preds2") +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key.width = unit(1,"cm"),
        legend.text = element_text(size = 15),
        legend.title = element_blank()) 
p
ggsave("outputs/04_Commercial/04a_GAMPlots/04a1_YFT/YFT_map_presence.png", p, dpi = 600)

# Cleanup
rm(p, plot1, plot2, plot3, plot4, plot5, plot6, model1, model2, model3, model4, m5, Surface, df, yft_preds, yft)
