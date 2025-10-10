
AR1sim<-function(n, a){
  x = rep(NA, n)
  x[1] = 0
  for(i in 2:n){
    x[i] = a * x[i-1] + (1-a) * rnorm(1)
  }
  return(x)
}

set.seed(123)
obs = AR1sim(1000, 0.9)
plot(obs)
time = 1:1000

fit = lm(obs~1)
summary(fit)

plot(residuals(fit))

acf(residuals(fit))

pacf(residuals(fit))

library(DHARMa)
testTemporalAutocorrelation(fit, time = 1:1000)



yOnlyTrend = 0.2 * sqrt(time) + rnorm(1000)
plot(yOnlyTrend ~ time)

fit = lm(yOnlyTrend~1)
plot(residuals(fit))
acf(residuals(fit))
testTemporalAutocorrelation(fit, time = 1:1000)

fit = lm(yOnlyTrend~time)
plot(residuals(fit))
acf(residuals(fit))
testTemporalAutocorrelation(fit, time = 1:1000)

library(mgcv)
fit = gam(yOnlyTrend~s(time))
testTemporalAutocorrelation(fit, time = 1:1000)


library(nlme)

fitGLS = gls(obs~1, corr = corAR1(0.771, form = ~ 1))
summary(fitGLS)
acf(residuals(fitGLS))


library(glmmTMB)

time <- factor(1:1000) # time variable
group = factor(rep(1,1000)) # group (for multiple time series)

fitGLMMTMB = glmmTMB(obs ~ ar1(time + 0 | group))
summary(fitGLMMTMB)

acf(predict(fitGLMMTMB, re.form = NULL) - obs)

library(EcoData)
plot(thick ~ soil, data = thickness)

fit = lm(thick ~ soil, data = thickness)
summary(fit)

res = simulateResiduals(fit)

testSpatialAutocorrelation(res, 
                           x = thickness$north, 
                           y = thickness$east)


library(gstat)
tann.dir.vgm = variogram(residuals(fit) ~ 1,
                         loc =~ east + north, data = thickness,
                         alpha = c(0, 45, 90, 135))
plot(tann.dir.vgm)


library(mgcv)

fit1 = gam(thick ~ soil + te(east, north) , data = thickness)
summary(fit1)
plot(fit1, pages = 0, lwd = 2)

res = simulateResiduals(fit1)
testSpatialAutocorrelation(res, x = thickness$north, y = thickness$east)


fit2 = gls(thick ~ soil, 
           correlation = corExp(form = ~ east + north),
           data = thickness)
summary(fit2)


thickness$pos <- numFactor(thickness$east, 
                           thickness$north)
thickness$group <- factor(rep(1, nrow(thickness)))

fit3 = glmmTMB(thick ~ soil + exp(pos + 0 | group) , data = thickness)
summary(fit3)


# obs per farm, 15 farms, 

Performance ~ mean(histology) + stocking density + season 



plants_sf <- plantcounts
str(plants_sf)
plants_sf$agrarea_scaled <- scale(plants_sf$agrarea)

plants_sf$longitude <- plants_sf$lon
plants_sf$latitude <- plants_sf$lat
library(sf)
plants_sf <- sf::st_as_sf(plants_sf, coords = c('longitude', 'latitude'), crs
                          = st_crs("+proj=longlat +ellps=bessel
                                   +towgs84=606,23,413,0,0,0,0 +no_defs"))

library(mapview)
mapview(plants_sf["richness"], map.types = "OpenTopoMap")

fit <-  glmmTMB::glmmTMB(richness ~ agrarea_scaled + offset(log(area)),
                         family = nbinom1, data = plants_sf)
summary(fit)

library(DHARMa)
res <- simulateResiduals(fit)
plot(res)
testSpatialAutocorrelation(res, x = plants_sf$lon, y =  plants_sf$lat)

fit2<-mgcv::gam(richness ~ agrarea_scaled + te(lon, lat),
                offset(log(area)), family = nb, data = plants_sf)
summary(fit2)
plot(fit2)

library(mgcViz)


res <- simulateResiduals(fit2)
plot(res)
testSpatialAutocorrelation(res, x = plants_sf$lon, y =  plants_sf$lat)


# nlme 
# in principle, they change residuals, because you can condition on predictions
# 


# 1) Detrend
# 2) Model residual correlation 











library(EcoData)
library(ape)
library(geiger)
library(nlme)
library(phytools)
library(DHARMa)

plot(anolisTree)

# Check whether names are matching in both files.
name.check(anolisTree, anolisData)

# Plot traits.
plot(anolisData[, c("awesomeness", "hostility")])

plot(hostility ~ awesomeness, data = anolisData)
fit = lm(hostility ~ awesomeness, data = anolisData)
summary(fit)
abline(fit)

# Calculate weight matrix for phylogenetic distance.
w = 1/cophenetic(anolisTree)
diag(w) = 0

Moran.I(residuals(fit), w)

res = simulateResiduals(fit)
testSpatialAutocorrelation(res, 
                           distMat = cophenetic(anolisTree))


# Extract columns.
host = anolisData[, "hostility"]
awe = anolisData[, "awesomeness"]

# Give them names.
names(host) = names(awe) = rownames(anolisData)

# Calculate PICs.
hPic = pic(host, anolisTree)
aPic = pic(awe, anolisTree)

# Make a model.
picModel = lm(hPic ~ aPic - 1)

summary(picModel) # Yes, significant.

# plot results.
plot(hPic ~ aPic)
abline(a = 0, b = coef(picModel))


pglsModel = gls(hostility ~ awesomeness,
                correlation = corBrownian(phy = anolisTree, form =~ species),
                data = anolisData, method = "ML")
summary(pglsModel)
coef(pglsModel)
plot(hostility ~ awesomeness, data = anolisData)
abline(pglsModel, col = "red")


GLMM(hostility ~ awesomeness + (1|Family) )


# 1. First create anomaly
# 2. Then scale the anomaly
# 3. either linear, abs, or linear + quadratic


CV/mean ~ nicheDist + nichtDist^2 + (nichtDist + nichtDist^2 | Species) )


library(EcoData)
dat = scoutingAnts[scoutingAnts$first.visit == 0,]
dat$ant_group = as.factor(dat$ant_group)
dat$ant_group_main = as.factor(dat$ant_group_main)

fit <- glm(went.phero ~ ant_group_main, data = dat)
summary(fit)


dat$directionConst = ifelse(dat$Treatment %in% c("LL", "RR"), T, F)
dat$directionPhero = as.factor(ifelse(dat$Treatment %in% c("LL", "RL"), "left", "right"))


library(lme4)
fit1<-glmer(went.phero ~ ant_group_main
            + directionConst
            + directionPhero
            + Orientation
            + (1|Colony),family="binomial", 
            data=dat)
summary(fit1)

res<-simulateResiduals(fit1)
plot(res, asFactor = T)

library(lme4)
fit1<-glmer(went.phero ~ ant_group_main * (
            + directionConst
            + directionPhero
            + Orientation)
            + (1|Colony),family="binomial", 
            data=dat)
summary(fit1)

res<-simulateResiduals(fit1)
plot(res, asFactor = T)

plot(allEffects(fit1))




