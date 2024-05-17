n <- 100                                              
x <- MASS::mvrnorm(mu = rep(0,n),
                   Sigma = .9 ^ as.matrix(dist(1:n)) )   
y <- rep(x, each = 5) + 0.2 * rnorm(5*n)                       
times <- factor(rep(1:n, each = 5), levels=1:n)
levels(times)
group <- factor(rep(1,n*5))
dat0 <- data.frame(y,times,group)

# fit model
model = glmmTMB(y ~ ar1(times + 0 | group), data=dat0)

# Standard residuals show spurious problems because of autocorrelation
res <- simulateResiduals(model)
plot(res)

# grouped according to times, unrotated shows autocorrelation
res2 <- recalculateResiduals(res, group=dat0$times) 
testTemporalAutocorrelation(res2, time = 1:length(res2$scaledResiduals))

# extract estimated AR1 covariance
cov <- VarCorr(model)
cov <- cov$cond$group # extract covariance matrix of REs

# grouped according to times, rotated with estimated Cov - how all fine!
res3 <- recalculateResiduals(res, group=dat0$times, rotation=cov) 
plot(res3)
testTemporalAutocorrelation(res3, time = 1:length(res2$scaledResiduals))



# Spatial autocorrelation 


library(EcoData)
plot(thick ~ soil, data = thickness, cex = thick/10)


fit = lm(thick ~ soil, data = thickness)
summary(fit)

res = simulateResiduals(fit)
testSpatialAutocorrelation(res,  x = thickness$east, y = thickness$north)

library(gstat)
tann.dir.vgm = variogram(residuals(fit) ~ 1,
                         loc =~ east + north, data = thickness,
                         alpha = c(0, 45, 90, 135))
plot(tann.dir.vgm)


fit = lm(thick ~ soil + east + I(east^2) + north + I(north^2), data = thickness)
summary(fit)

res = simulateResiduals(fit)
testSpatialAutocorrelation(res, x = thickness$east, y = thickness$north)

# Before you fit spatial models, try out to de-trend spatially using a GAM

library(mgcv)

fit = gam(thick ~ soil + te(east, north), data = thickness)
summary(fit)
plot(fit)

res = simulateResiduals(fit)
testSpatialAutocorrelation(res, x = thickness$east, y = thickness$north)

# Alternatively, here is how you would fit an autoregressive model 

fit = lm(thick ~ soil, data = thickness)
summary(fit)

res = simulateResiduals(fit)
testSpatialAutocorrelation(res, x = thickness$east, y = thickness$north)


fit2 = gls(thick ~ soil , 
           correlation = corExp(form =~ east + north) 
           , data = thickness)
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


fit <-  glm(richness ~ agrarea_scaled + offset(log(area)) , 
            data = plants_sf, family = "poisson")
summary(fit)

res = simulateResiduals(fit)
plot(res)


fit <-  glmmTMB(richness ~ agrarea + offset(log(area)) , 
            data = plants_sf, family = nbinom2)

res = simulateResiduals(fit)
plot(res)

testSpatialAutocorrelation(res, x = plants_sf$lon, y = plants_sf$lat)

summary(fit)


fit <-  gam(richness ~ agrarea + offset(log(area)) + te(lon, lat) , 
                data = plants_sf, family = nb)

summary(fit)

res = simulateResiduals(fit)
plot(res)
testSpatialAutocorrelation(res, x = plants_sf$lon, y = plants_sf$lat)
plot(fit)


library(EcoData)
library(ape)
library(geiger)
library(nlme)
library(phytools)
library(DHARMa)

plot(anolisTree)
name.check(anolisTree, anolisData)

plot(hostility ~ awesomeness, data = anolisData)
fit = lm(hostility ~ awesomeness, data = anolisData)
summary(fit)
abline(fit)

# Calculate weight matrix for phylogenetic distance.
w = 1/cophenetic(anolisTree)
diag(w) = 0
image(log(w))

Moran.I(residuals(fit), w)

res = simulateResiduals(fit)
testSpatialAutocorrelation(res, distMat = cophenetic(anolisTree))

# PGLS = Phylogenetic generalized least squared 

pglsModel = gls(hostility ~ awesomeness,
                correlation = corBrownian(phy = anolisTree, form =~ species),
                data = anolisData, method = "ML")
summary(pglsModel)


pglsModel = gls(hostility ~ ecomorph * awesomeness,
                weights = 
                correlation = corBrownian(phy = anolisTree, form =~ species),
                data = anolisData, method = "ML")
summary(pglsModel)

Moran.I(residuals(pglsModel), w)


library(ape)
library(EcoData)

dat = barbetData
tree = barbetTree

dat$species = row.names(dat)
plot(tree)

# dropping species in the phylogeny for which we don't have data
obj<-geiger::name.check(tree,dat)
reducedTree<-drop.tip(tree, obj$tree_not_data)
geiger::name.check(reducedTree,dat)

plot(log(Lnote)~Lnalt, data = dat)

fit <- lm(log(Lnote)~ Lnalt + I(Lnalt^2), data = dat)
summary(fit)

par(mfrow = c(2,2))
plot(fit)
res <- simulateResiduals(fit, plot = T)

w = 1/cophenetic(reducedTree)
diag(w) = 0
Moran.I(residuals(fit), w)

fit <- gls(Lnote~ Lnalt + I(Lnalt^2), 
           correlation = corBrownian(phy = reducedTree, 
                                     form =~ species), data = dat, 
           method = "ML")
summary(fit)


library(brms)

prior1 <- prior(normal(0,10), class = b) +
  prior(cauchy(0,2), class = sd)
fit1 <- brm(count ~ zAge + zBase * Trt + (1|patient),
            data = epilepsy, family = poisson(), prior = prior1)

# generate a summary of the results
summary(fit1)


