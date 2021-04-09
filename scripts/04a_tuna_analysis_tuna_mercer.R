#James Mercer 2019
#Larval Tuna Data Analysis progression
#
# Questions: 
# 1. What are the optimum conditions (SST, Chl, MLD, season, latitude) for each fish species.
# 2. To find out which species is most affected by temperature and bathymetry.
# 3. Make a model based on optimum conditions
library(tidyverse)
library(effects)
library(splines)
library(ggplot2)
library(devtools)
library(ggiraphExtra)
library(ggiraph)

#Import Data Set
# complete_new5 <- read.csv("~/R working directory/complete_new5.txt", sep="")
complete_new5 <- read.csv("inputs/mercer/TunaData.txt", sep="")
complete_new5 <- na.omit(complete_new5)

#Very high chlorophyll max
hist(complete_new5$Chl)
complete_new5 <- complete_new5 %>% mutate(Chl = replace(Chl, Chl > 5, 5)) # Set max Chl to 5

# Reorder Seasons so they are in order
complete_new5 <- complete_new5 %>% mutate(Season2 = replace(Season2, Season2 == "summer", 2)) %>% 
  mutate(Season2 = replace(Season2, Season2 == "winter", 4)) %>% 
  mutate(Season2 = replace(Season2, Season2 == "autumn", 3)) %>% 
  mutate(Season2 = replace(Season2, Season2 == "spring", 1))

#subset based on species
yft <- subset(complete_new5, species == "yellowfin tuna")
alba <- subset(complete_new5, species == "albacore")
sword <- subset(complete_new5, species == "swordfish")
skip <- subset(complete_new5, species == "skipjack tuna")

### Ant - YFT ###
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

# Do a gam
library(mgcv)
library(visreg)
# Look at all variables
m1 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = yft, family = "binomial")
summary(m1)

# Plot it
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
# Some of the relationships look too wiggly, and might not make sense
# First, let's see what we can drop using BIC
summary(m1) # Everything significant because lots of df, but least sig is Bathymetry, then Nitrate, then Chl

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
dev.copy2pdf(file = "pdfs/YFT_Model.pdf", paper = "A4r")
summary(m5) # r2 = 13.6%

# Plot m5 as a map
yft$Preds <- predict.gam(m5, type = "response")
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

### Ant - Albacore ###
# Look at the data by plotting a global map of Alba
ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "grey", colour = "grey", size = 0.5) +
  geom_point(data = alba, aes(x = Longitude, y = Latitude), size = 0.2) + 
  facet_wrap(~pa)

# Do a gam
# Look at all variables
m10 <- gam(pa ~ s(SST) + Season2 + s(MLD) + s(Latitude, Longitude) + s(Bathymetry) + s(Dist2Coast) + s(Nitrate) + s(Chl), data = alba, family = "binomial")
summary(m10) # Everything significant except Bathymetry

# Plot it
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
summary(m10) # Everything significant except Bathymetry

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

par(mfrow = c(2,3))
visreg(m14, "SST", partial = FALSE, ylim = c(-50, 0))
visreg(m14, "Season2", partial = FALSE)
visreg(m14, "Chl", partial = FALSE, ylim = c(-20, 0))
vis.gam(m14, c("Latitude", "Longitude"), ticktype = "detailed", xlab = "\nLatitude (oC)", 
        ylab = "Longitude", zlab = "\nPresence/Absence", color = "cm", theta = 45, phi = 10, r = 100) # also "grey"
summary(m14) # r2 = 29.3%

dev.copy2pdf(file = "Alba_Model.pdf", paper = "A4r")

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
ggsave("Alba_map.png", p, dpi = 1200)




# James' Code for testing
#############################################################
#make logistic regression
#########################
#step one, just SST and season - bathymetry, bathymetry SD and dist to coast don't make a whole lot of biological sense
yftlogit <- glm(pa ~ SST + season, data = yft, family = "binomial") 
plot(allEffects(yftlogit))
summary(yftlogit)

#Doesn't work, I'll try latitude instead
yftlogit <- glm(pa ~ (SST + MLD) by =season*Latitude, data = yft, family = "binomial") 

summary(yftlogit)

#Doesn't work, using Season2 makes more biological sense, esp. with SST and MLD

yftlogit <- glm(pa ~ SST + MLD + Season2, data = yft, family = "binomial") 
plot(allEffects(yftlogit))

#plot SST as a natural spline
library(splines)
yftlogit <- glm(pa ~ ns(SST, df=3) + MLD + Season2, data = yft, family = "binomial") 
summary(yftlogit)
#AIC is higher as a spline?

plot(allEffects(yftlogit))
yftnullmod <- glm(pa~1, data = yft, family="binomial") # For calculating McFaddens pseudo r2
1-logLik(yftlogit)/logLik(yftnullmod)

#chlorophyll a
yftlogit <- glm(pa ~ ns(SST, df=3) + MLD + Chl + Season2, data = yft, family = "binomial") 
summary(yftlogit)

plot(allEffects(yftlogit))

#not a hugely sensical effect of Chl

#nitrate
yftlogit <- glm(pa ~ ns(SST, df=3) + MLD + Nitrate + Season2, data = yft, family = "binomial") 
summary(yftlogit)

plot(allEffects(yftlogit))

#Nitrate shouldn't have a positive effect? 
#subset only catches for species
yftcatches <- subset(yft, pa == "1")
#Doesn't make sense against histogram of yft nitrate
hist(yftcatches$Nitrate, breaks=50)
#Leave Nitrate out of model
#Chlorophyll a
yftlogit <- glm(pa ~ ns(SST, df=3) + MLD + Chl + Season2, data = yft, family = "binomial") 
plot(allEffects(yftlogit))
#Chl doesn't seem to have a sig effect

yftlogit <- glm(pa ~ ns(SST, df=3) + MLD + Season2, data = yft, family = "binomial") 
summary(yftlogit)
#Best model
#add latitude?
#latitude will not have a linear effect, should make a natural spline
#adding more degrees of freedom as peak latitude should be equatorial for yellowfin tuna (around 0)

#best GLM for YFT
yftlogit <- glm(pa ~ ns(SST, df=3) + MLD + Season2 + ns(Latitude, df=12) + Bathymetry, data = yft, family = "binomial") 
yftlogit <- glm(pa ~ SST + Season2 + MLD, data = yft, family = "binomial") 

summary(yftlogit)
anova(yftlogit, test = "Chi")

plot(allEffects(yftlogit))
#this should be made into a GAM, latitude not well represented in this model
nullmod <- glm(pa~1, data = yft, family="binomial") # For calculating McFaddens pseudo r2
1-logLik(yftlogit)/logLik(yftnullmod) # 9% Deviance explained, the lowest of the GLMs?


#plot predictions with Season and Hemisphere interacting
ggPredict(yftlogit, interactive=TRUE, colorn=100, jitter = FALSE)


##### Albacore model testing ####
albalogit <- glm(pa ~ SST + Season2, data = alba, family = "binomial") 
summary(albalogit)

#SST is strong, add as a spline and add MLD

albalogit <- glm(pa ~ ns(SST, df=3)  + MLD + Season2, data = alba, family = "binomial")
summary(albalogit)
plot(allEffects(albalogit))

#add chlorophyll
albalogit <- glm(pa ~ ns(SST, df=3) + Chl + MLD + Season2, data = alba, family = "binomial")
summary(albalogit)
plot(allEffects(albalogit))
# strong effect of chlorophyll, add latitude as spline
albalogit <- glm(pa ~ ns(SST, df=3) + Chl + MLD + Season2 + ns(Latitude, df=3), data = alba, family = "binomial")

# increase degrees of freedom of latitude because it doesn't make sense for albacore to be spawning >20 and <-20
albalogit <- glm(pa ~ ns(SST, df=3) + Chl + MLD + Season2 + ns(Latitude, df=5), data = alba, family = "binomial")
#pattern for latitude looks much more realistic now
#increase df for SST as well
albalogit <- glm(pa ~ ns(SST, df=5) + Chl + MLD + Season2 + ns(Latitude, df=5), data = alba, family = "binomial")

#add nitrate to see effect
albalogit <- glm(pa ~ ns(SST, df=5) + Chl + MLD + Season2 + Nitrate + ns(Latitude, df=5), data = alba, family = "binomial")
#Nitrate doesn't have a great effect and doesn't make a lot of sense, take it out
albalogit <- glm(pa ~ ns(SST, df=5) + Chl + MLD + Season2 + ns(Latitude, df=5), data = alba, family = "binomial")
#add bathymetry
albalogit <- glm(pa ~ ns(SST, df=5) + Chl + MLD + Season2 + Bathymetry + ns(Latitude, df=5), data = alba, family = "binomial")
#bathymetry doesn't have great effect, take it out
#best GLM model
albalogit <- glm(pa ~ ns(SST, df=5) + Chl + MLD + Season2 + ns(Latitude, df=5), data = alba, family = "binomial")

summary(albalogit)
plot(allEffects(albalogit))
nullmod <- glm(pa~1, data = alba, family="binomial") # For calculating McFaddens pseudo r2
1-logLik(albalogit)/logLik(nullmod) # 24.5% deviance explained, not bad, perhaps could get higher with GAM?

############SWORDFISH ############
nullmod <- glm(pa~1, data = sword, family="binomial") # For calculating McFaddens pseudo r2

#SST and Season2 to begin
swordlogit <- glm(pa ~ SST + Season2, data = sword, family = "binomial")
summary(swordlogit)
plot(allEffects(swordlogit))

#add SST as a spline

swordlogit <- glm(pa ~ ns(SST, df=5) + Season2, data = sword, family = "binomial")
summary(swordlogit)
plot(allEffects(swordlogit))

#better represents data
#add latitude as a spline and increase dfs until makes sense

swordlogit <- glm(pa ~ ns(SST, df=5) + Season2 + ns(Latitude, df=6), data = sword, family = "binomial")
summary(swordlogit)
plot(allEffects(swordlogit))

#add Chl because it seems to have a big effect with swordfish
swordlogit <- glm(pa ~ ns(SST, df=5) + Season2 + ns(Latitude, df=6) + Chl, data = sword, family = "binomial")
summary(swordlogit)
plot(allEffects(swordlogit))

#remove Chl, not significant or effect that makes sense
# add MLD

swordlogit <- glm(pa ~ ns(SST, df=5) + Season2 + ns(Latitude, df=6) + MLD, data = sword, family = "binomial")
summary(swordlogit)
plot(allEffects(swordlogit))

# mixed layer depth makes sense, but not significant, improves AIC?

#try Nitrate
swordlogit <- glm(pa ~ ns(SST, df=5) + Season2 + ns(Latitude, df=6) + MLD + Nitrate, data = sword, family = "binomial")

#nitrate also makes sense but not significant, also doesn't improve AIC, so I'll take it out
#Best glm model for swordfish
swordlogit <- glm(pa ~ ns(SST, df=5) + Season2 + ns(Latitude, df=6) + MLD, data = sword, family = "binomial")

1-logLik(albalogit)/logLik(nullmod) # 21.3% deviance explained, not bad, perhaps could get higher with GAM?


######SKIPJACK ##########

#SST and Season2 to begin
skiplogit <- glm(pa ~ SST + Season2, data = skip, family = "binomial")
summary(skiplogit)
plot(allEffects(skiplogit))

#add SST as a spline, incr df until the curve makes sense

skiplogit <- glm(pa ~ ns(SST, df=9) + Season2, data = skip, family = "binomial")
summary(skiplogit)
plot(allEffects(skiplogit))

#better represents data
#add latitude as a spline and increase dfs until makes sense

skiplogit <- glm(pa ~ ns(SST, df=9) + Season2 + ns(Latitude, df=5), data = skip, family = "binomial")
summary(skiplogit)
plot(allEffects(skiplogit))
#best fit of latitude, still not perfect, doesn't represent data brilliantly


#add Chl
skiplogit <- glm(pa ~ ns(SST, df=9) + Season2 + ns(Latitude, df=5) + Chl, data = skip, family = "binomial")
summary(skiplogit)
plot(allEffects(skiplogit))

#remove Chl, not significant or effect that makes sense
# add MLD

skiplogit <- glm(pa ~ ns(SST, df=9) + Season2 + ns(Latitude, df=5) + MLD, data = skip, family = "binomial")
summary(skiplogit)
plot(allEffects(skiplogit))

# mixed layer depth makes sense and significant

#try Nitrate
skiplogit <- glm(pa ~ ns(SST, df=9) + Season2 + ns(Latitude, df=6) + MLD + Nitrate, data = skip, family = "binomial")
summary(skiplogit)
plot(allEffects(skiplogit))

#nitrate also makes sense and significant and improves AIC so left in
#Best glm model for skipjack
skiplogit <- glm(pa ~ ns(SST, df=9) + Season2 + ns(Latitude, df=6) + MLD, Nitrate, data = skip, family = "binomial")

nullmod <- glm(pa~1, data = skip, family="binomial") # For calculating McFaddens pseudo r2
1-logLik(albalogit)/logLik(nullmod) # 66% deviance explained, very good, better than my GAMs




#### BUBBLEPLOT AND VARIOGRAM SCRIPT 


library(ggplot2)
library(sp)
library(gstat)
yft <- na.omit(yft)
# Check for spatial auto-correllation
autocorData<-data.frame(yft$Longitude, yft$Latitude, resids=resid(yftlogit)) # Extract residuals from model
names(autocorData) <- c('Longitude','Latitude','resids')
autocorData$signres<- sign(autocorData$resids)

## Plot the residuals in space
world <- map_data("world")

# Bubble plot (2000 * 1000)
ggplot(data = autocorData, aes(x = Longitude, y = Latitude)) + 
  geom_map(data=world, map=world, aes(x = long, y = lat, map_id=region), color="white", fill="gray94", size=0.08) + 
  geom_point(aes(size = abs(resids), color = sign(autocorData$resids)), shape = 1) + 
  theme_bw() + scale_size_continuous(range=c(.1,4)) + scale_colour_gradient(low = "springgreen3", high = "magenta3") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks = element_blank(), legend.title = element_text(size=12, face="bold"), 
        legend.text = element_text(size = 10), legend.background = element_rect(color="black", size=.3),
        legend.position=c(.93, .6)) + ylab(NULL) + xlab(NULL) + guides(colour = "none", size = guide_legend(title = "Magnitude"))

# Data format for variograms
autocorData<-data.frame(yft$Longitude, yft$Latitude, resids=resid(yftlogit)) # Extract residuals
names(autocorData) <- c('Longitude','Latitude','resids')
coordinates(autocorData)<-c('Longitude','Latitude') # Convert lon and lat to coordinates

# Look at variogram in all directions together
varmodel<-variogram(resids~1,data=autocorData, cutoff = 10)
plot(varmodel, main = "Variogram of all directions combined")

# Look at variogram in all directions separately
varmodel<-variogram(resids~1,data=autocorData, alpha=c(0,45,90,135), cutoff = 10)
plot(varmodel, main = "Variogram of all directions separately") ## There is spatial autocorrelation as the lines are not flat




######Histogram plotting, extracting values from it
#subset only catches for species
yftcatches <- subset(yft, pa == "1")
albacatches <- subset(alba, pa == "1")
swordcatches <- subset(sword, pa == "1")
skipcatches <- subset(skip, pa == "1")

#make histograms
#SST for yft
yftSST <- hist(yftcatches$SST, breaks = 50)

#extract mean
c(yftSST$breaks[var(yftSST$counts)])
#extract highest value
c(yftSST$breaks[which.max(yftSST$counts)])

#SST for albacore
albaSST <- hist(albacatches$SST, breaks = 100)

#extract highest value
c(albaSST$breaks[which.max(albaSST$counts)])


#SST for skipjack
skipSST <- hist(skipcatches$SST, breaks = 100)

#extract highest value
c(skipSST$breaks[which.max(skipSST$counts)])


##################### Plotting predictions for yft and graphing#########################

#testing if season is statistically significant
wald.test(b = coef(yftlogit), Sigma = vcov(yftlogit), Terms = 6:8) # 73.1 chi squaed with v significant p value, so overall effect of season is significant

#test if seasons are statistically significant from eachother
l <- cbind(0, 0, 1, -1, 0)
wald.test(b = coef(yftlogit), Sigma = vcov(yftlogit), L = l) # janmarch and julsept statistically diff  X2 = 23.8, df = 1, P(> X2) = 1.1e-06

l <- cbind(0, 0, 0, 1, -1)
wald.test(b = coef(yftlogit), Sigma = vcov(yftlogit), L = l) # julsept and octoberdec different X2 = 51.9, df = 1, P(> X2) = 5.9e-13

l <- cbind(0, 0, 1, 0, -1)
wald.test(b = coef(yftlogit), Sigma = vcov(yftlogit), L = l) # janmarch and octoberdec different X2 = 6.1, df = 1, P(> X2) = 0.014

#plotting SST probabilities for each season, using (    )^2 / SST:season to give all two way interactions
yftlogit <- glm(pa ~ SST + season + SST:season, data = yft, family = "binomial") 
summary(yftlogit)

#select range for x-axis
X1_range <- seq(from=min(yft$SST), to=max(yft$SST), by=.1)

#generate probabilities#
#make new dataframe
generated_data <- as.data.frame(expand.grid(SST=X1_range, season=c("janmarch", "apriljune", "julsept", "octoberdec") ))
head(generated_data)
#get probs
generated_data$prob <- predict(yftlogit, newdata=generated_data, type = 'response')
head(generated_data) 

#get standard error
## grad the inverse link function
fam <- family(yftlogit)
ilink <- fam$linkinv
#add fit and se.fit on the link scale 
ndata <- bind_cols(generated_data, setNames(as_tibble(predict(yftlogit, generated_data, se.fit = TRUE)[1:2]), c('fit_link','se_link')))
head(ndata)
## create the interval and backtransform
ndata <- mutate(ndata, fit_resp = ilink(fit_link), right_upr = ilink(fit_link + (2*se_link)), right_lwr = ilink(fit_link - (2*se_link)))
head(ndata)

#PLOT
#load ggplot
library(ggplot2)

#plot the probabilities
mycolors<-c("#FFD1E3","#FF8782","#EE0000","#940000")

ggplot(ndata, aes(x=SST, y=prob, color=season)) + geom_line(lwd=2) +
  geom_ribbon(aes(ymin = right_lwr,
                  ymax = right_upr, fill = season), alpha=0.6)


#step two, SST, season and MLD

yftlogit <- glm(pa ~ SST + MLD + season, data = yft, family = "binomial") 
#MLD significant with negative effect
summary(yftlogit)

#select range for x-axis, SST
X1_range <- seq(from=min(yft$SST), to=max(yft$SST), by=.1)

#select range for x-axis, MLD
X2_range <- seq(from=min(yft$MLD), to=max(yft$MLD), by=.1)


#new range for new-axis variable (MLD)
generated_data <- as.data.frame(expand.grid(SST=X1_range, MLD=X2_range, season=c("janmarch", "apriljune", "julsept", "octoberdec") ))

generated_data$prob <- predict(yftlogit, newdata=generated_data, type = 'response')
head(generated_data) 

#get standard error
## grad the inverse link function
fam <- family(yftlogit)
ilink <- fam$linkinv
#add fit and se.fit on the link scale 
ndata <- bind_cols(generated_data, setNames(as_tibble(predict(yftlogit, generated_data, se.fit = TRUE)[1:2]), c('fit_link','se_link')))
head(ndata)
## create the interval and backtransform
ndata <- mutate(ndata, fit_resp = ilink(fit_link), right_upr = ilink(fit_link + (2*se_link)), right_lwr = ilink(fit_link - (2*se_link)))
head(ndata)

#PLOT
#load ggplot
library(ggplot2)

#plot the probabilities

ggplot(ndata, aes(x=SST, y=prob, color=season)) + geom_line(lwd=2) +
  geom_ribbon(aes(ymin = right_lwr,
                  ymax = right_upr, fill = season), alpha=0.6)


ggplot(ndata, aes(x=MLD, y=prob, color=season)) + geom_line(lwd=2) +
  geom_ribbon(aes(ymin = right_lwr,
                  ymax = right_upr, fill = season), alpha=0.6)

library(devtools)
library(ggiraphExtra)
library(ggiraph)


#make inteaction of MLD, SST for each season
ggPredict(yftlogit, interactive=TRUE, colorn=100, jitter = FALSE)
#This makes sense, but will be revealing if I could show seasonal cycles by restricting hemisphere, or incorporating new variable as hemisphere. Season2 variable is
yftlogit <- glm(pa ~ SST + MLD + season, data = yft, family = "binomial") 
yftlogit2 <- glm(pa ~ SST + MLD + Season2, data = yft, family = "binomial") 

summary(yftlogit)
summary(yftlogit2)
#season is a better variable, AIC better than Season2, maybe should try hemisphere
#making new variable, hemisphere
complete_new5$Hemisphere <- ifelse(complete_new5$Latitude >= 0, 'North', 'South')
#subset based on species again
yft <- subset(complete_new5, species == "yellowfin tuna")
alba <- subset(complete_new5, species == "albacore")
sword <- subset(complete_new5, species == "swordfish")
skip <- subset(complete_new5, species == "skipjack tuna")
#add hemisphere into the model
yftlogit <- glm(pa ~ SST + MLD + season*Hemisphere, data = yft, family = "binomial") 
summary(yftlogit)
#plot predictions with Season and Hemisphere interacting
ggPredict(yftlogit, interactive=TRUE, colorn=100, jitter = FALSE)




