plot(Ozone ~ Wind, data = airquality)

airquality
str(airquality)

airquality$Month
airquality$fMonth = factor(airquality$Month, labels = c("May", "June", "July", "August", "Sept"))
as.numeric(airquality$fMonth)

plot(Ozone ~ Wind, data = airquality, col = as.numeric(airquality$fMonth))
plot(Ozone ~ fMonth, data = airquality)

summary(airquality)

fit = lm(Ozone ~ Wind , data = airquality)

summary(fit)
abline(fit)

options("na.action")
# change the global setting: options(na.action = "na.fail")

# tomorrow: how to best deal with missing data
fit = lm(Ozone ~ Wind + I(Wind^2) , data = airquality, na.action = "na.fail")

library(effects)

plot(allEffects(fit, partial.residuals = T))

opar = par(mfrow = c(2,2))
plot(fit)
par(opar)

fit = lm(Ozone ~ scale(Wind) , data = airquality)

library(DHARMa)
res <- simulateResiduals(fit, plot = T)

# linear regression
# y ~ a0 + a1 * x + a2 * x2^2 + ... 
# polynomial 


# quadratic function y ~ a0 + a1 * x + a2 * x2^2
fit = lm(Ozone ~ Wind + I(Wind^2) + I(Wind^3) , data = airquality)
plot(allEffects(fit, partial.residuals = T))

opar = par(mfrow = c(2,2))
plot(fit)
par(opar)
summary(fit)


fit = lm(log(Ozone) ~ I(Wind^0.6) , data = airquality)
plot(allEffects(fit, partial.residuals = T))
opar = par(mfrow = c(2,2))
plot(fit)
par(opar)
summary(fit)


fit = lm(Ozone ~ Wind , data = airquality)
library(MASS)
MASS::boxcox(fit)
fit = lm(Ozone^0.3 ~ Wind , data = airquality)
plot(allEffects(fit, partial.residuals = T))
opar = par(mfrow = c(2,2))
plot(fit)
par(opar)


# GAMs = generalized additive models 
library(mgcv)

fit = gam(Ozone ~ s(Wind) , data = airquality)
summary(fit)
plot(fit)

boxplot(weight ~ group, data = PlantGrowth)

fit = lm(weight ~ group, data = PlantGrowth)
summary(fit)
summary(aov(fit))

fit = lm(weight ~ group - 1, data = PlantGrowth)
summary(fit)

plot(allEffects(fit, partial.residuals = T))

opar = par(mfrow = c(2,2))
plot(fit)
par(opar)

summary(aov(fit))

library(multcomp)
tuk = glht(fit, linfct = mcp(group = "Tukey"))
summary(tuk)          # Standard display.

tuk.cld = cld(tuk)    # Letter-based display.
plot(tuk.cld)

library(EcoData)
str(plantHeight)
plot(height ~ temp, data = plantHeight)
plot(loght ~ temp, data = plantHeight)

boxplot(loght ~ growthform, data = plantHeight)


# Multiple regression 

fit = lm(Ozone ~ Temp + Wind + Solar.R + fMonth, data = airquality)
summary(fit)

# fits the effect of Temp adjusted / corrected for the effects ofthe other variables and vice versa

plot(allEffects(fit, partial.residuals = T))

fit = lm(Ozone ~ Temp , data = airquality)
summary(fit)

# fits the raw / unadjusted effect of Temp 

# centering / scaling / standardised effects 

airquality$sTemp = scale(airquality$Temp)
airquality$sWind = scale(airquality$Wind)
airquality$sSolar.R = scale(airquality$Solar.R)

fit = lm(Ozone ~ sTemp + sWind + sSolar.R + fMonth, data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

plot(Ozone ~ Temp, data = airquality, xlim = c(-10, 110), ylim = c(-200, 170))
abline(fit)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

# Interactions 

fit = lm(Ozone ~ Temp + Wind + Temp:Wind, data = airquality)
fit = lm(Ozone ~ Wind * Temp , data = airquality)
summary(fit)
plot(allEffects(fit))

fit = lm(Ozone ~ sWind * sTemp , data = airquality)
summary(fit)
plot(allEffects(fit))

# Therefore: always center if you deal with interactions!

fit = lm(Ozone ~ sWind * fMonth , data = airquality)
summary(fit)
plot(allEffects(fit))

# Predictions 

fit = lm(Ozone ~ Wind, data = airquality)

predict(fit)
predict(fit, newdata = X) # predicts on new data
predict(fit, se.fit = T)

Wind = seq(0,10,0.1)
newData = data.frame(Wind = Wind)
pred = predict(fit, newdata = newData, se.fit = T)
plot(Wind, pred$fit, type = "l")
lines(Wind, pred$fit - 1.96 * pred$se.fit, lty = 2)
lines(Wind, pred$fit + 1.96 * pred$se.fit, lty = 2)

anova(fit)
summary(aov(fit))
car::Anova(fit)



fit = lm(Ozone ~ sTemp + sWind + sSolar.R + fMonth, data = airquality)
summary(fit)

plot(allEffects(fit, partial.residuals = T))
par(mfrow=c(2,2))
plot(fit)

usedData = model.frame(fit)
str(usedData)
plot(residuals(fit)~usedData$sTemp)


# solution exercise

library(EcoData)
fit = lm(loght ~ scale(temp) + scale(NPP), data = plantHeight)
plot(allEffects(fit, partial.residuals = T))
summary(fit)

par(mfrow = c(2,2))
plot(fit)

plot(temp ~ NPP, data = plantHeight)
cor(plantHeight$temp, plantHeight$NPP, use = "complete.obs", method = "kendall")

plantHeight$sTemp = scale(plantHeight$temp)
plantHeight$sLat = scale(plantHeight$lat)
plantHeight$sNPP = scale(plantHeight$NPP)

# relevel 
plantHeight$growthform2 = relevel(as.factor(plantHeight$growthform), "Herb")

fit = lm(loght ~ sTemp * growthform2 , data = plantHeight)
summary(fit)

plot(allEffects(fit, partial.residuals = T))

summary(aov(fit))

fit = lm(loght ~ sTemp * sLat , data = plantHeight)
summary(fit)

plot(allEffects(fit, partial.residuals = T))


plot(sTemp ~ sLat , data = plantHeight)

