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

par(mfrow = c(2,2))
plot(fit)

acf(residuals(fit))
pacf(residuals(fit))

library(DHARMa)
testTemporalAutocorrelation(fit, time = 1:1000)

library(nlme)
fitGLS = gls(obs~1, corr = corAR1(form = ~ 1))
summary(fitGLS)

library(glmmTMB)

time <- factor(1:1000) 
group <- factor(rep(1,1000)) # this would be if we have several time series
fitGlmmTMB = glmmTMB(obs ~ ar1(time + 0|group))

summary(fitGlmmTMB)

library(EcoData)
plot(thick ~ soil, data = thickness)

fit = lm(thick ~ soil, data = thickness)
summary(fit)

plot(east ~ north, data = thickness)

res = simulateResiduals(fit)
testSpatialAutocorrelation(res, x = thickness$north, y = thickness$east)
testSpatialAutocorrelation(res, x = runif(nrow(thickness)), y = runif(nrow(thickness)))

library(gstat)
var = variogram(residuals(fit)~1,
                loc = ~ east + north,
                data = thickness,
                alpha = c(0, 45, 90, 135))
plot(var)

fit = lm(thick ~ soil + east + I(east^2) + north + I(north^2), data = thickness)
summary(fit)

res = simulateResiduals(fit)
testSpatialAutocorrelation(res, x = thickness$north, y = thickness$east)

fit = gam(thick ~ soil + te(east , north), data = thickness)
summary(fit)
plot(fit)

res = simulateResiduals(fit)
testSpatialAutocorrelation(res, x = thickness$north, y = thickness$east)

fit2 = gls(thick ~ soil, correlation = corExp(form=~east + northnorth),
           data = thickness)
summary(fit2)

thickness$pos <- numFactor(thickness$east, thickness$north)
thickness$group <- factor(rep(1, nrow(thickness)))  

fit3 = glmmTMB(thick ~ soil + exp(pos + 0|group), data = thickness)
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

res = simulateResiduals(fit)
testSpatialAutocorrelation(res, x = plants_sf$lon, y = plants_sf$lat)

fit =  gam(richness ~ agrarea_scaled + te(lon, lat) + offset(log(area)),
                        family = nb, data = plants_sf)
plot(fit)
summary(fit)

res = simulateResiduals(fit)
testSpatialAutocorrelation(res, x = plants_sf$lon, y = plants_sf$lat)


plants_sf$pos <- numFactor(plants_sf$lon, plants_sf$lat)
plants_sf$group <- factor(rep(1, nrow(plants_sf)))  

fit <-  glmmTMB::glmmTMB(richness ~ agrarea_scaled 
                         + exp(pos + 0|group)+ offset(log(area)),
                         family = nbinom1, data = plants_sf)




library(DHARMa)
library(mgcv)
?hurricanes
str(hurricanes)

plot(hurricanes$MasFem, hurricanes$NDAM, cex = 0.5, pch = 5)
points(hurricanes$MasFem, hurricanes$NDAM, cex = hurricanes$alldeaths/20, pch = 4, col= "red")

originalModelGAM = gam(alldeaths ~ MasFem * (Minpressure_Updated_2014 + NDAM), data = hurricanes, family = nb, na.action = "na.fail")
summary(originalModelGAM)

library(DHARMa)
library(glmmTMB)

m1 = glmmTMB(alldeaths ~ MasFem*
               (Minpressure_Updated_2014 + scale(NDAM)),
             data = hurricanes, family = nbinom2)
summary(m1)

m2 = glmmTMB(alldeaths ~ scale(MasFem) *
               (scale(Minpressure_Updated_2014) + scale(NDAM^0.2)),
             data = hurricanes, family = nbinom2)
summary(m2)

plot(allEffects(m2))

res<-simulateResiduals(m2, plot = T)

plotResiduals(res, form = hurricanes$MasFem)
plotResiduals(res, form = hurricanes$Minpressure_Updated_2014)
plotResiduals(res, form = hurricanes$NDAM)

# plot(residuals(m2, type = "pearson") ~ log(hurricanes$NDAM + 1))


m2 = glmmTMB(alldeaths ~ scale(MasFem) * scale(NDAM^0.2) + Year + (1|Year) , data = hurricanes, family = nbinom2)
summary(m2)


m2 = gam(alldeaths ~ scale(MasFem) + s(NDAM) + s(Year) , data = hurricanes, family = nb)
summary(m2)
plot(m2)


