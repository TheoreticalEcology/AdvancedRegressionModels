set.seed(125)

data = data.frame(treatment = factor(rep(c("A", "B", "C"), each = 15)))
data$observation = c(7, 2 ,4)[as.numeric(data$treatment)] +
  rnorm( length(data$treatment), 
         sd = as.numeric(data$treatment)^2 )

boxplot(weight ~ group, data = PlantGrowth)
boxplot(observation ~ treatment, data = data)

fit <- lm(observation ~ treatment, data = data)
summary(fit)
par(mfrow = c(2,2))
plot(fit)

res <- simulateResiduals(fit)
plot(res)

# Transformation? 
boxplot(observation ~ treatment, data = data)

library(glmmTMB)
fit <- glmmTMB(observation ~ treatment, data = data,
               dispformula = ~ treatment)
summary(fit)


# R regression packages:
# lm / glm 
# nlme -> dispersion, correlations, RE
# lme4 -> improved RE, but didnt include dispersion / correlations
# glmmTMB -> includes everything, but some functions limited 

library(nlme)

fit = gls(observation ~ treatment, data = data, 
          weights = varIdent(form = ~ 1 | treatment))
summary(fit)


plot(Ozone ~ Solar.R, data = airquality)
fit = lm(Ozone ~ Solar.R, data = airquality)

par(mfrow = c(2,2))
plot(fit)

res <- simulateResiduals(fit)
plot(res, quantreg = T)


fit = glmmTMB(sqrt(Ozone) ~ sqrt(Solar.R) , data = airquality,
              dispformula = ~ sqrt(Solar.R))

res <- simulateResiduals(fit)
plot(res, quantreg = T)


summary(fit)
plot(allEffects(fit, partial.residuals = T))


fit = gls(Ozone ~ Solar.R , 
          data = airquality[complete.cases(airquality),],
          weights = varPower(0.2, form = ~ Solar.R))
summary(fit)
plot(fit)


m3 = glmmTMB(count ~ spp + mined + (1|site), 
             family = nbinom1, 
             dispformula = ~ spp + mined ,  data = Salamanders)
summary(m3)



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
plot(rnorm(1000), type = "l")

# don't check autocorrelation on the response 
# acf(obs)

acf(residuals(fit))
pacf(residuals(fit))
arima(residuals(fit), order = c(5L, 0L, 0L))

res = simulateResiduals(fit)
testTemporalAutocorrelation(res, time = 1:1000)

library(nlme)
fitGLS = gls(obs~1, corr = corAR1(0.771, form = ~ 1))
summary(fitGLS)

library(glmmTMB)

time <- factor(1:1000) # time variable
group = factor(rep(1,1000)) # group (for multiple time series)

fitGLMMTMB = glmmTMB(obs ~ ar1(time + 0 | group))
summary(fitGLMMTMB)



library(DHARMa)

fit = glmmTMB(alldeaths ~ scale(MasFem) *
                (scale(Minpressure_Updated_2014) + scale(NDAM)),
              data = hurricanes, family = nbinom2)

# Residual checks with DHARMa.
res = simulateResiduals(fit)

# Checking for temporal autocorrelation
res2 = recalculateResiduals(res, group = hurricanes$Year)
testTemporalAutocorrelation(res2, time = unique(hurricanes$Year))

pacf(residuals(res))

time <- factor(hurricanes$Year) # time variable
group = factor(rep(1,nrow(hurricanes))) # group (for multiple 

fit = glmmTMB(alldeaths ~ scale(MasFem) *
                (scale(Minpressure_Updated_2014) + scale(NDAM)) 
              + ar1(time + 0 | group),
              data = hurricanes, family = nbinom2)
summary(fit)

fit = glmmTMB(alldeaths ~ scale(MasFem) 
              + ar1(time + 0 | group),
              data = hurricanes, family = nbinom2)
summary(fit)


library(EcoData)
plot(thick ~ soil, data = thickness)
fit = lm(thick ~ soil, data = thickness)
plot(allEffects(fit, partial.residuals = T))
summary(fit)

plot(thickness$east, thickness$north, cex = scale(thickness$thick) + 3)
plot(thickness$east, thickness$north, cex = scale(thickness$soil) + 2)

res = simulateResiduals(fit)
testSpatialAutocorrelation(fit, x = thickness$east, y = thickness$north)

library(gstat)
tann.dir.vgm = variogram(residuals(fit) ~ 1,
                         loc =~ east + north, data = thickness,
                         alpha = c(0, 45, 90, 135))
plot(tann.dir.vgm)

plot(thick ~ soil, data = thickness)

fit = lm(thick ~ soil + north + I(north^2), data = thickness)

res = simulateResiduals(fit)
testSpatialAutocorrelation(fit, x = thickness$east, y = thickness$north)

fit = gam(thick ~ soil + s(north) + s(east), data = thickness)
res = simulateResiduals(fit)
testSpatialAutocorrelation(fit, x = thickness$east, y = thickness$north)
plot(fit)

fit = gam(thick ~ soil + te(north, east), data = thickness)
res = simulateResiduals(fit)
testSpatialAutocorrelation(fit, x = thickness$east, y = thickness$north)
plot(fit)
summary(fit)



fit = lm(thick ~ soil + north + I(north^2), data = thickness)

res = simulateResiduals(fit)
testSpatialAutocorrelation(fit, x = thickness$east, y = thickness$north)

fit = gls(thick ~ soil + north + I(north^2), data = thickness,
          correlation = corExp(form = ~ east + north))
summary(fit)


thickness$pos <- numFactor(thickness$east, 
                           thickness$north)
thickness$group <- factor(rep(1, nrow(thickness)))

fit = glmmTMB(thick ~ soil + north + I(north^2)
              + exp(pos + 0 | group), data = thickness)
summary(fit)

# WARNING: Standard residuals are still autocorrelated! 



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


fit <-  glmmTMB::glmmTMB(richness ~ agrarea_scaled + 
                           log(area),
                          family = nbinom1, data = plants_sf)
summary(fit)

res <- simulateResiduals(fit)
plot(res)
testDispersion(res)


counts = exp(ax + b + offset(log(Area)))
      = exp(ax + b) * exp( log (Area))

hist(plants_sf$area)

testSpatialAutocorrelation(res, x = plants_sf$lon, y =  plants_sf$lat)

fit2<-mgcv::gam(richness ~ agrarea_scaled + te(lon, lat),
                offset(log(area)), family = nb, data = plants_sf)
summary(fit2)

res = simulateResiduals(fit2)

testSpatialAutocorrelation(res, x = plants_sf$lon, y =  plants_sf$lat)
plot(res)
testDispersion(res)

library(qgam)
plot(richness ~ agrarea_scaled, data = plants_sf)
fit2 <-qgam(richness ~ agrarea_scaled + te(lon, lat),
            qu = 0.5, data = as.data.frame(plants_sf))
summary(fit2)



library(EcoData)
?elephant
str(elephant)
table(elephant$occurenceData$Presence)

model = glm(Presence~bio1, 
            data = elephant$occurenceData, 
            family = binomial()) 

library(effects)
plot(allEffects(model))

library(DHARMa)
res = simulateResiduals(model)
plot(res)

# Task: fit a "good" Niche model for the African Elephant

library(sp)
library(raster)
plot(elephant$predictionData$bio1)

predictions =  predict(elephant$predictionData, 
                       model = model, 
                       type = "response")
spplot(predictions, colorkey = list(space = "left") )



x = elephant$occurenceData[,-1]
data.pca <- princomp(x)
data.pca$scores
summary(data.pca)
biplot(data.pca)

library(corrplot)
cor_matrix <- cor(x)
corrplot(cor_matrix, 
         method = "ellipse", 
         order = "hclust", 
         addrect = 5)

# BIO1 = Annual Mean Temperature
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (×100)
# BIO4 = Temperature Seasonality (standard deviation ×100)
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO7 = Temperature Annual Range (BIO5-BIO6)
# BIO8 = Mean Temperature of Wettest Quarter
# BIO9 = Mean Temperature of Driest Quarter
# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO12 = Annual Precipitation
# BIO13 = Precipitation of Wettest Month
# BIO14 = Precipitation of Driest Month
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# BIO16 = Precipitation of Wettest Quarter
# BIO17 = Precipitation of Driest Quarter
# BIO18 = Precipitation of Warmest Quarter
# BIO19 = Precipitation of Coldest Quarter


model = glm(Presence~ poly(bio1,2) + 
                      poly(bio12,2) + 
                      poly(bio19,2) + 
                      poly(bio2,2) + 
                      poly(bio5,2), 
            data = elephant$occurenceData, 
            family = binomial()) 

plot(allEffects(model))
summary(model)

res = simulateResiduals(model)
plot(res)
plotResiduals(res, form = ~ bio1)
plotResiduals(res, form = ~ bio12)
plotResiduals(res, form = ~ bio19)
plotResiduals(res, form = ~ bio2)
plotResiduals(res, form = ~ bio5)

library(mgcv)
model = gam(Presence~ s(bio1) + 
              s(bio12) + 
              s(bio19) + 
              s(bio2) + 
              s(bio5), 
            data = elephant$occurenceData, 
            family = binomial()) 

res = simulateResiduals(model)
plot(res)
plotResiduals(res, form = ~ bio1)
plotResiduals(res, form = ~ bio12)
plotResiduals(res, form = ~ bio3)
plotResiduals(res, form = ~ bio2)
plotResiduals(res, form = ~ bio5)

plot(model, ylim = c(-5,5))

predictions =  predict(elephant$predictionData, 
                       model = model, 
                       type = "response")
spplot(predictions, colorkey = list(space = "left") )


xy <- as.data.frame(elephant$prediction)
head(xy)
pts <- rasterToPoints(elephant$prediction)
nrow(pts)
elephant$occurenceData = rbind(elephant$occurenceData, xy)


model = gam(elephant$occurenceData$Presence ~ 
              s(data.pca$scores[,1])+
              s(data.pca$scores[,2])+
              s(data.pca$scores[,3]), 
            family = binomial()) 

plot(model, ylim = c(-5,5))


x = runif(100, -2,2)
y = x^2 + rnorm(100)
plot(x,y)
fit= gam(y ~ s(x))
plot(fit)

