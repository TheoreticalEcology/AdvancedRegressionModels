devtools::install_github(repo = "TheoreticalEcology/EcoData", 
                         dependencies = F, build_vignettes = F)

library(EcoData)

str(elephant$occurenceData)

library(BayesianTools)
correlationPlot(elephant$occurenceData[,2:10])


model = glm(Presence ~ bio1 + I(bio1^2), data = elephant$occurenceData, family = binomial()) 

newDat = data.frame(bio1 = seq(-3,3, length.out = 100))

x = predict(model, 
            type = "response", 
            se.fit = T, 
            newdata = newDat)

plot(newDat$bio1, x$fit, type = "l", ylim = c(0,0.8), lwd = 2)
lines(newDat$bio1, x$fit + 1.96 * x$se.fit, lty = 2)
lines(newDat$bio1, x$fit - 1.96 * x$se.fit, lty = 2)

polygon(c(newDat$bio1, rev(newDat$bio1)), 
        c(x$fit - 1.96 * x$se.fit, rev(x$fit + 1.96 * x$se.fit) ), col = "#DD000022" )

library(effects)

plot(allEffects(model))


model = glm(Presence ~ bio1 + I(bio1^2), data = elephant$occurenceData, family = binomial()) 

AIC(model) 

library(pROC)
auc(elephant$occurenceData$Presence, predict(model, type = "response")) # 0.5 = random, 1 = perfect

library(boot)
res = cv.glm(elephant$occurenceData, 
             model, cost = auc, K=5)
res$delta

library(sp)
library(raster)
plot(elephant$predictionData$bio1)

predictions =  predict(elephant$predictionData, model = model, type = "response")

plot(predictions)



library(EcoData)


plot(feeding ~ attractiveness, data = birdfeeding)

fit = glm(feeding ~ attractiveness, data = birdfeeding, family = "poisson")
summary(fit)

res <- simulateResiduals(fit, plot = T)

library(effects)
plot(allEffects(fit))
exp(1.47459 )


library(DHARMa)
library(glmmTMB)

m1 = glm(count ~ spp + mined, family = poisson, data = Salamanders)
summary(m1)

res <- simulateResiduals(m1, plot = T)

plot(allEffects(m1, partial.residuals = T))

testDispersion(res)
summary(m1)

# Overdispersion - first address misfit if possible!

plot(res)

plotResiduals(res, Salamanders$spp)
plotResiduals(res, Salamanders$mined)

m1 = glm(count ~ spp + mined + cover + site, family = poisson, data = Salamanders)
summary(m1)
res <- simulateResiduals(m1, plot = T)

# only if that does not help, correct overdispersion!

# -> switch to neg binom distribution 

library(MASS)
m1 = glm.nb(count ~ spp + mined + cover + site, data = Salamanders)
res <- simulateResiduals(m1, plot = T)


library(glmmTMB)
m1 = glmmTMB(count ~ spp + mined + cover , data = Salamanders,
              family = nbinom1)
res <- simulateResiduals(m1, plot = T)
testDispersion(m1)

summary(m1)



m1 = glm(SiblingNegotiation ~ 
           FoodTreatment*SexParent + 
           offset(log(BroodSize)),
           data = Owls , family = poisson)
summary(m1)

res = simulateResiduals(m1)
plot(res)
testDispersion(res)


m1 = glmmTMB(SiblingNegotiation ~ 
           FoodTreatment*SexParent + 
           offset(log(BroodSize)),
         data = Owls , family = nbinom1)
summary(m1)

res = simulateResiduals(m1)
plot(res)
testDispersion(res)
testZeroInflation(res) # not reliable as a diagnostic with variable dispersion count data regressions

m2 = glmmTMB(SiblingNegotiation ~ 
               FoodTreatment*SexParent + 
               offset(log(BroodSize)),
             data = Owls , 
             family = nbinom1,
             ziformula = ~ 1)
summary(m2)

# in principle this should work as well simulateLRT(m1, m2, n = 2000)

# Poisson residuals checks: 

# 1) all normal residual checks, in partiuclar res ~ predictors
# 2) Important: need to check for overdispersion and zero-inflation 



fit = glmmTMB(alldeaths ~ MasFem*
               (Minpressure_Updated_2014 + scale(NDAM)),
             data = hurricanes, family = nbinom2)
summary(fit)

res <- simulateResiduals(fit)
plot(res)
plotResiduals(res, hurricanes$MasFem)
plotResiduals(res, hurricanes$Minpressure_Updated_2014)
plotResiduals(res, hurricanes$NDAM)

plot(residuals(fit, type = "pearson") ~ log(hurricanes$NDAM))



correctedModel = glmmTMB(alldeaths ~ scale(MasFem) *
                           (scale(Minpressure_Updated_2014) + scale(NDAM^0.25)),
                         data = hurricanes, family = nbinom2)

res <- simulateResiduals(correctedModel, plot = T)
plotResiduals(res, hurricanes$NDAM)
plotResiduals(res, hurricanes$Minpressure_Updated_2014)
plotResiduals(res, hurricanes$NDAM)

summary(correctedModel)
car::Anova(correctedModel)



newModel = glmmTMB(alldeaths ~ scale(MasFem) * scale(NDAM^0.2) + Year , data = hurricanes, family = nbinom2)
summary(newModel)

# can also use a gam

library(mgcv)

newModel = gam(alldeaths ~ MasFem + s(NDAM) + s(Year) ,
                   data = hurricanes, family = nb)


summary(newModel)
plot(newModel)



https://theoreticalecology.github.io/AdvancedRegressionModels/4C-CorrelationStructures.html#spatial-correlation-structures

library(EcoData)
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



fit <-  glmmTMB(richness ~ agrarea_scaled ,
                         family = nbinom1, 
                         data = plants_sf)
summary(fit)


library(DHARMa)
res <- simulateResiduals(fit)
plot(res)

plotResiduals(res, form = plants_sf$agrarea, rank = F)



testSpatialAutocorrelation(res, 
                           x = plants_sf$lon, 
                           y =  plants_sf$lat)


fit <-  glmmTMB(richness ~ agrarea_scaled + 
                           lon + I(lon^2) + I(lon^3) + 
                           lat + I(lat^2) + I(lon^3)  ,
        family = nbinom1, 
        data = plants_sf)

summary(fit)

testSpatialAutocorrelation(fit, 
                           x = plants_sf$lon, 
                           y =  plants_sf$lat)


fit2<-mgcv::gam(richness ~ s(agrarea_scaled) , family = nb, data = plants_sf)
plot(fit2)

fit2<-mgcv::gam(richness ~ s(agrarea_scaled) + te(lon, lat), family = nb, data = plants_sf)
summary(fit2)

library(mgcViz)
b <- getViz(fit2)
print(plot(b, allTerms = F), pages = 1) 

res <- simulateResiduals(fit2)

testSpatialAutocorrelation(res, x = plants_sf$lon, y =  plants_sf$lat)
simulateResiduals(fit2, plot = T)


testData = createData(sampleSize = 40, family = gaussian(), 
                      randomEffectVariance = 0)
fittedModel <- lm(observedResponse ~ Environment1, data = testData)
res = simulateResiduals(fittedModel)

# Standard use
testTemporalAutocorrelation(res, time =  testData$time)


# continous positive 

library(faraway)

fit <- lm(log(resist) ~ x1 + x2 + x3 + x4, data = wafer)
summary(fit)

hist(rgamma(1000, 1,1))

fit <- glm(formula = resist ~ x1 + x2 + x3 + x4,
           family  = Gamma(link = "log"),
           data    = wafer)
summary(fit)


# continous proportions
# typically between 0-1, but can also be 1-20, important is that it is really continous

?elemental
m1 <- lm(N_arc ~ Year + Site , 
         data = elemental[elemental$Species == "ABBA", ])
summary(m1)

hist(elemental$N_dec, xlim = c(0,1))

m2 <- glmmTMB(N_dec ~ Year + Site, 
              family = beta_family, 
              data = elemental[elemental$Species == "ABBA", ] )
summary(m2)





