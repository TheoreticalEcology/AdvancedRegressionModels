
library(EcoData)
plot(thick ~ soil, data = thickness)

fit = lm(thick ~ soil, data = thickness)
summary(fit)

plot(north ~ east, data = thickness, cex = scale(thick))
plot(north ~ east, data = thickness, cex = scale(soil))
plot(north ~ east, data = thickness, cex = scale(residuals(fit)))

library(DHARMa)
testSpatialAutocorrelation(fit, 
                           x = thickness$east, 
                           y = thickness$north )


library(gstat)
tann.dir.vgm = variogram(residuals(fit) ~ 1,
                         loc = ~ east + north, 
                         data = thickness,
                         alpha = c(0, 45, 90, 135))
plot(tann.dir.vgm)



fit = lm(thick ~ soil + east + north + I(east^2) + I(north^2), data = thickness)
summary(fit)
testSpatialAutocorrelation(fit, 
                           x = thickness$east, 
                           y = thickness$north )


library(mgcv)

fit = gam(thick ~ soil + te(east, north), data = thickness)
summary(fit)
plot(fit)
testSpatialAutocorrelation(fit, 
                           x = thickness$east, 
                           y = thickness$north)



fit = lm(thick ~ soil + east + north + I(east^2) + I(north^2), data = thickness)
summary(fit)
testSpatialAutocorrelation(fit, 
                           x = thickness$east, 
                           y = thickness$north )


library(nlme)

fit = gls(thick ~ soil + east + north + I(east^2) + I(north^2), 
          data = thickness,
          correlation = corExp(form = ~ east + north) )
summary(fit)


# same as yesterday - normalized residuals are de-correlated by rotating 
# according to the fitted model 
residuals(fit)
residuals(fit, type = "normalized")


library(glmmTMB)

thickness$pos <- numFactor(thickness$east, 
                           thickness$north)
thickness$group <- factor(rep(1, nrow(thickness)))

fit = glmmTMB(thick ~ soil + east + north + I(east^2) + I(north^2)
              + exp(pos + 0 | group), 
          data = thickness,
         )
summary(fit)

# to check, same as yesterday, can choose DHARMa with rotation argument
# to rotate out 

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



fit <-lm(log(richness) ~ agrarea, data = plantcounts)
summary(fit)
plot(allEffects(fit))
res<-simulateResiduals(fit, plot = T)

testSpatialAutocorrelation(res, x = plantcounts$lon, y = plantcounts$lat)


fit <-gam(log(richness) ~ agrarea + te(lon, lat), data = plantcounts)
summary(fit)
plot(fit)

res<-simulateResiduals(fit, plot = T)
testSpatialAutocorrelation(res, x = plantcounts$lon, y = plantcounts$lat)

fit <-gls(log(richness) ~ agrarea, data = plantcounts, 
          correlation=corExp(form = ~ lat + lon))
summary(fit)
plot(fit)





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

w = 1/cophenetic(anolisTree)
diag(w) = 0
image(log(w))

Moran.I(residuals(fit), w)

res = simulateResiduals(fit)
testSpatialAutocorrelation(res, distMat = cophenetic(anolisTree))


fit = gls(hostility ~ awesomeness, data = anolisData,
          correlation = corBrownian(phy = anolisTree, form =~ species))

summary(fit)


library(ape)
library(EcoData)

dat = barbetData
tree = barbetTree

dat$species = row.names(dat)
plot(tree)

plot(Lnote~Lnalt, data = dat)

fit <- lm(Lnote~ Lnalt + I(scale(Lnalt)^2), data = dat)
summary(fit)
plot(allEffects(fit,partial.residuals = T))


obj<-geiger::name.check(tree,dat)
reducedTree<-drop.tip(tree, obj$tree_not_data)
geiger::name.check(reducedTree,dat)

w = 1/cophenetic(reducedTree)
diag(w) = 0
Moran.I(residuals(fit), w)


fit <- gls(Lnote~ scale(Lnalt) * I(scale(Lnalt)^2), 
           correlation = corBrownian(phy = reducedTree, 
                                     form =~ species), data = dat, 
           method = "ML")
summary(fit)

