summary(airquality)

image(is.na(t(airquality)), axes = F)
axis(3, at = seq(0,1, len = 6), labels = colnames(airquality))

fit2 = lm(Ozone ~ Wind + Temp + Solar.R, data = airquality)
summary(fit2)

str(airquality)


fit1 = lm(Ozone ~ Wind + Temp, data = airquality)
summary(fit1)

# This is problematic because you are comparing models with different 
# size of the data
AIC(fit1)
AIC(fit2)

# This is valid, because it uses the data of the full model (with NAs dropped)
summary(aov(fit2))


options("na.action")

fit1 = lm(Ozone ~ Wind + Temp, data = airquality, na.action = "na.fail")
summary(fit1)

options(na.action = "na.fail")
options(na.action = "na.omit")

# NAs - missing at random or not? 
# Usually: biological / experimental setup question?
# dropping data only valid if missing at random

# Imputation 

# Simplest solution: fill with mean or random draws of the variable itself, outdated! 

pairs(airquality)

# state-of-the art solution: predict missing values from other predictor variables

library(missRanger)
airqualityImp<- missRanger(airquality[,-1])
airqualityImp$Ozone = airquality$Ozone

fit2 = lm(Ozone ~ Wind + Temp + Solar.R, data = airqualityImp)
summary(fit2)

# GLMs

library(EcoData)
#str(birdfeeding)
plot(feeding ~ attractiveness, data = birdfeeding)


fit <- lm(feeding ~ attractiveness, data = birdfeeding)
summary(fit)
par(mfrow = c(2,2))
plot(fit)


fit <- glm(feeding ~ attractiveness, data = birdfeeding,
           family = "poisson")

hist(rpois(1000, lambda =2), breaks = 100)

summary(fit)
plot(allEffects(fit, partial.residuals = T))

# link(prediction) = ax + b ; here: log(pred) = ax + b
# prediction = link^-1 (ax + b) here pred = exp(ax + b)

# purpose of link: bring the predictions to the right range. 
# log-link makes the predictions positive 
# possion distribution requires positive values 

dpois(1, lambda = -0.4)

predict(fit) # predictions based on linear predictor = regression table
predict(fit, type = "response") # transformed with link function

newdat = data.frame(attractiveness = -15:5)
  
newdat$pred1 = predict(fit, newdata = newdat)
plot(pred1 ~ attractiveness, data = newdat, type = "l")
newdat$pred2 = predict(fit, newdata = newdat, type = "response")
plot(pred2 ~ attractiveness, data = newdat, type = "l")

# binomial glm 

library(EcoData)
str(titanic)
titanic$pclass = as.factor(titanic$pclass)

fit = lm(survived ~ sex * age, data = titanic)
summary(fit)
par(mfrow = c(2,2))
plot(fit)

fit = glm(survived ~ sex * age, data = titanic,
          family = "binomial")
summary(fit)

curve(plogis, -5,5)
plogis(0.493381)

plot(allEffects(fit))

# standard diagnostics plots work but don't make sense because they are designed for lm
par(mfrow = c(2,2))
plot(fit)
class(fit)

library(DHARMa)
res = simulateResiduals(fit)
plot(res)
plotResiduals(res, form = ~ age)
plotResiduals(res, form = ~ sex)




library(EcoData)
library(effects)
fit <- glm(presence ~ dist_roads, family = binomial, data = elk)
summary(fit)
plot(allEffects(fit))


?elk

fit <- glm(presence ~ dist_roads, family = binomial, data = elk)

pairs(elk)
library(corrplot)
cor_matrix <- cor(elk[,1:4])
corrplot(cor_matrix, method = "ellipse")


fit <- glm(presence ~ dist_roads*habitat + ruggedness + dem + NDVI , family = binomial, data = elk)

plot(allEffects(fit))

res <- simulateResiduals(fit)
plot(res)
plotResiduals(res, form = ~ dist_roads)
plotResiduals(res, form = ~ habitat)
plotResiduals(res, form = ~ ruggedness, rank = F)
plotResiduals(res, form = ~ dem, rank = F)
plotResiduals(res, form = ~ NDVI)


fit <- glm(presence ~ dist_roads*habitat + 
             ruggedness + 
             dem + 
             NDVI , family = binomial, data = elk)

library(mgcv)
fit <- gam(presence ~ dist_roads * habitat + 
             s(ruggedness) + 
             s(dem) + 
             s(NDVI) , family = binomial, data = elk)

plot(fit)

res <- simulateResiduals(fit)
plot(res)
plotResiduals(res, form = ~ dist_roads)
plotResiduals(res, form = ~ habitat)
plotResiduals(res, form = ~ ruggedness, rank = F)
plotResiduals(res, form = ~ dem, rank = F)
plotResiduals(res, form = ~ NDVI)

summary(fit)



fit <- lm(feeding ~ attractiveness, data = birdfeeding,
           family = "poisson")
summary(fit)


fit <- glm(feeding ~ attractiveness, data = birdfeeding,
           family = "poisson")
summary(fit)

# Pseudo-R2 , McFadden 1 - ResDeviance / Null Deviance
1 - 18.320 / 25.829 # 30% pseudo R2 (McFadden)
1 - residualdVariance / dataVariance

car::Anova(fit)

res <- simulateResiduals(fit)
plot(res)
testDispersion(res)
plot(allEffects(fit, partial.residuals = T))


library(glmmTMB)
library(DHARMa)
m1 = glm(count ~ spp + mined, family = poisson, data = Salamanders)
summary(m1)
plot(allEffects(m1))

res <- simulateResiduals(m1)
plot(res)
testDispersion(res)
plot(allEffects(m1, partial.residuals = T))

library(lme4)
m1 = glmer(count ~ spp * mined + (spp|site), family = poisson, data = Salamanders)
summary(m1)
res <- simulateResiduals(m1)
plot(res)
testDispersion(res)


library(glmmTMB)
m1 = glmmTMB(count ~ spp * mined + (spp|site), family = nbinom1, data = Salamanders)
summary(m1)
res <- simulateResiduals(m1)
plot(res)
testDispersion(res)

# test for zero-inflation
testZeroInflation(res)

m2 = glmmTMB(count ~ spp + mined + (1|site), family = nbinom1, data = Salamanders,
             ziformula = ~ mined)
summary(m2)



x































library(corrplot)
cor_matrix <- cor(x)
corrplot(cor_matrix, method = "ellipse")


fit = glm(Presence~ poly(bio1,2) +  poly(bio2,2) , data = elephant$occurenceData, family = binomial())
plot(allEffects(fit))



library(glmmTMB)

m1 = glm(SiblingNegotiation ~ FoodTreatment*SexParent + offset(log(BroodSize)),
         data = Owls , family = poisson)
res = simulateResiduals(m1)
plot(res)


m1 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + ArrivalTime +
                        + (1|Nest) + offset(log(BroodSize)),
         data = Owls , family = nbinom1)
res = simulateResiduals(m1)
plot(res)
plotResiduals(res, form = ~ ArrivalTime)
summary(m1)
testDispersion(res)
testZeroInflation(res)


m2 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent +
               + (FoodTreatment |Nest) + offset(log(BroodSize)), 
             ziformula = ~ FoodTreatment*SexParent, data = Owls , family = nbinom1)

AIC(m1)
AIC(m2)

res = simulateResiduals(m2)
plot(res)
testDispersion(res)
testZeroInflation(res)

summary(m2)



library(DHARMa)
library(glmmTMB)

m1 = glmmTMB(alldeaths ~ 
               (scale(Minpressure_Updated_2014) 
                + scale(I(NDAM^0.2))) * scale(MasFem),
             data = hurricanes, family = nbinom2)
summary(m1)
plot(allEffects(m1))

res <- simulateResiduals(m1)
plotResiduals(res, form = hurricanes$MasFem)
plotResiduals(res, form = ~ MasFem)
plotResiduals(res, form = ~ Minpressure_Updated_2014)
plotResiduals(res, form = ~ ZNDAM)


m1 = glmmTMB(alldeaths ~ ZNDAM + ZMasFem,
             data = hurricanes, family = nbinom2)
summary(m1)

res <- simulateResiduals(m1)
plotResiduals(res, form = ~ MasFem)
plotResiduals(res, form = ~ ZNDAM)

m1 = gam(alldeaths ~ s(ZNDAM) + ZMasFem,
             data = hurricanes, family = nb)
summary(m1)

res <- simulateResiduals(m1)
plotResiduals(res, form = ~ MasFem)
plotResiduals(res, form = ~ ZNDAM)

plot(m1)


