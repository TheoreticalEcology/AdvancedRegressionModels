library(DHARMa)
library(glmmTMB)


m1 = glmmTMB(alldeaths ~ scale(MasFem)*
               (scale(Minpressure_Updated_2014) + scale(NDAM)),
             data = hurricanes, family = nbinom2)
summary(m1)

plot(allEffects(m1))


res<- simulateResiduals(m1)
plot(res)

plotResiduals(res, form = hurricanes$MasFem)
plotResiduals(res, form = hurricanes$Minpressure_Updated_2014)
plotResiduals(res, form = hurricanes$NDAM)

hist(hurricanes$NDAM, breaks = 50)


m2 = glmmTMB(alldeaths ~ scale(MasFem)*
               (scale(Minpressure_Updated_2014) + scale(NDAM^0.25)),
             data = hurricanes, family = nbinom2)
res<- simulateResiduals(m2)
plot(res)

plotResiduals(res, form = hurricanes$MasFem)
plotResiduals(res, form = hurricanes$Minpressure_Updated_2014)
plotResiduals(res, form = hurricanes$NDAM)

summary(m2)

plot(allEffects(m2))


m3 = glmmTMB(alldeaths ~ scale(MasFem) * scale(NDAM^0.2) 
             + Year + (1|Year),
             data = hurricanes, family = nbinom2)
res<- simulateResiduals(m3)
plot(res)

plotResiduals(res, form = hurricanes$MasFem)
plotResiduals(res, form = hurricanes$Minpressure_Updated_2014)
plotResiduals(res, form = hurricanes$NDAM)

summary(m3)

obs2 = rnorm(1000)
acf(obs2)
pacf(obs2)

plot(obs2)

plot(obs2[1:999], obs2[2:1000])
cor(obs[1:999], obs[2:1000])
acf(obs)


# simulate temporally autocorrelated data
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
plot(obs, type = "l")


fit = lm(obs~1)
summary(fit)

plot(residuals(fit))

acf(residuals(fit))
pacf(residuals(fit))

library(DHARMa)

testTemporalAutocorrelation(fit, time = 1:1000)

res <- simulateResiduals(fit)

pacf(residuals(res))

library(nlme)
fitGLS = gls(obs~1, corr = corAR1(form = ~ 1))
summary(fitGLS)

acf(residuals(fitGLS))



library(glmmTMB)

time <- factor(1:1000) # time variable
group = factor(rep(1,1000)) # group (for multiple time series)

fitGLMMTMB = glmmTMB(obs ~ ar1(time + 0 | group))

summary(fitGLMMTMB)


acf(residuals(fitGLMMTMB))



library(EcoData)
plot(thick ~ soil, data = thickness)

fit = lm(thick ~ soil, data = thickness)
summary(fit)
abline(fit)

plot(north ~ east, data = thickness)





library(gstat)
tann.dir.vgm = variogram(residuals(fit) ~ 1,
                         loc =~ east + north, data = thickness,
                         alpha = c(0, 45, 90, 135))
plot(tann.dir.vgm)


res = simulateResiduals(fit)
testSpatialAutocorrelation(res,
                           x = thickness$north, 
                           y = thickness$east)


library(mgcv)
fit = gam(thick ~ soil + te(thickness$north, thickness$east) , data = thickness)
summary(fit)
plot(fit)

res = simulateResiduals(fit)
testSpatialAutocorrelation(res,
                           x = thickness$north, 
                           y = thickness$east)




thickness$pos <- numFactor(thickness$east, 
                           thickness$north)
thickness$group <- factor(rep(1, nrow(thickness)))

fit3 = glmmTMB(thick ~ soil + exp(pos + 0 | group) , data = thickness)
summary(fit3)


?EcoData::plantcounts

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
res <- simulateResiduals(fit)
plot(res)
testSpatialAutocorrelation(res, x = plants_sf$lon, y = plants_sf$lat)

library(mgcViz)

plants_sf = as.data.frame(plants_sf)

fit2<-mgcv::gam(richness ~ agrarea_scaled + te(lon, lat) + offset(log(area)), family = nb(), data = plants_sf)

res <- simulateResiduals(fit2)
plot(res)
testSpatialAutocorrelation(res, x = plants_sf$lon, y = plants_sf$lat)

plot(fit2)


plants_sf$pos <- numFactor(plants_sf$lon, 
                           plants_sf$lat)
plants_sf$group <- factor(rep(1, nrow(plants_sf)))

fit3 = glmmTMB(richness ~ agrarea_scaled + exp(pos + 0 | group) + offset(log(area)), family = nbinom1, data = plants_sf)


res <- simulateResiduals(fit3)
plot(res)

testSpatialAutocorrelation(residuals(res), x = plants_sf$lon, y = plants_sf$lat)



library(EcoData)
library(ape)
library(geiger)
library(nlme)
library(phytools)
library(DHARMa)

plot(anolisTree)

name.check(anolisTree, anolisData)

plot(anolisData[, c("awesomeness", "hostility")])

plot(hostility ~ awesomeness, data = anolisData)
fit = lm(hostility ~ awesomeness, data = anolisData)
summary(fit)

# Calculate weight matrix for phylogenetic distance.
w = 1/cophenetic(anolisTree)
diag(w) = 0

Moran.I(residuals(fit), w)

pglsModel = gls(hostility ~ awesomeness,
                correlation = corBrownian(phy = anolisTree, form = ~ species),
                data = anolisData, method = "ML")
summary(pglsModel)


tempTree = anolisTree
tempTree$edge.length = tempTree$edge.length * 100
pglsModelLambda = gls(hostility ~ awesomeness,
                      correlation = corPagel(1, phy = tempTree, fixed = FALSE,
                                             form =~ species),
                      data = anolisData, method = "ML")
summary(pglsModelLambda)

?barbet
Ecod
