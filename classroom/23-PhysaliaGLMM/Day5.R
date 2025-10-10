
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

plot(residuals(fit), type = "l")

acf(residuals(fit))

pacf(residuals(fit))

library(DHARMa)

res <- simulateResiduals(fit)
residuals(res) # extract scaled DHARMa residuals

testTemporalAutocorrelation(res, time = 1:1000)

library(nlme)

fitGLS = gls(obs~1, corr = corAR1(form = ~ 1))
summary(fitGLS)
acf(residuals(fitGLS))


library(glmmTMB)
time <- factor(1:1000) # time variable
group = factor(rep(1,1000)) # group (for multiple time series)

fitGLMMTMB = glmmTMB(obs ~ ar1(time + 0 | group))

summary(fitGLMMTMB)

ranef(fitGLMMTMB) # can extract temporal correlated residauls and see if anything is left


library(EcoData)
plot(thick ~ soil, data = thickness)

fit = lm(thick ~ soil, data = thickness)
summary(fit)

plot(east ~ north, data = thickness)

res = simulateResiduals(fit)
testSpatialAutocorrelation(res, x = thickness$north, y = thickness$east)


library(gstat)
tann.dir.vgm = variogram(residuals(fit) ~ 1,
                         loc =~ east + north, data = thickness,
                         alpha = c(0, 45, 90, 135))
plot(tann.dir.vgm)



fit = lm(thick ~ soil + north + I(north^2), data = thickness)
summary(fit)

res = simulateResiduals(fit)
testSpatialAutocorrelation(res, x = thickness$north, y = thickness$east)


library(mgcv)

fit1 = gam(thick ~ soil + te(east, north) , data = thickness)
summary(fit1)

plot(fit1, pages = 0)

res = simulateResiduals(fit1)
testSpatialAutocorrelation(res, x = thickness$north, y = thickness$east)


fit2 = gls(thick ~ soil , 
           correlation = corExp(form =~ east + north) , data = thickness)
summary(fit2)


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

lm(log(richness) ~ agrarea)



library(EcoData)
library(ape)
library(geiger)
library(nlme)
library(phytools)
library(DHARMa)

plot(anolisTree)

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
testSpatialAutocorrelation(res, distMat = cophenetic(anolisTree))


pglsModel = gls(hostility ~ awesomeness,
                correlation = corBrownian(phy = anolisTree, form =~ species),
                data = anolisData, method = "ML")
summary(pglsModel)


# eDNA = abundance / presence of the entire community
# check residuals for correlation between species
# jSDMs -> 

# Power analysis 

load(file = "../AdvancedBiostatistics/0_Data/Data/elk_data.RData")

library(MASS)

fit <- glm(presence ~ dist_roads  + dem , data = elk_data, family = "binomial")
summary(fit)

simulate(fit)


n = 100
distRoads = runif(n)
dem = runif(n)
Linpred = 1 + 0.2 * distRoads + 0.5 * dem
response = rbinom(n, 1, plogis(Linpred))

fit <- glm(response ~ distRoads + dem, family = "binomial" )
summary(fit)        



        