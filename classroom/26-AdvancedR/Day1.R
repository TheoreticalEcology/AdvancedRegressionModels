

airquality

plot(Ozone ~ Wind,data = airquality)

# y = a1 * x + a0
fit = lm(Ozone ~ Wind,data = airquality)
summary(fit)
abline(fit, col = "red")

# Effect of Wind:  -5.5509 +/- 0.6904 SE +/- 1.96 * 0.6904 95% CI 

library(effects)
plot(allEffects(fit))

# R2 = 1 - var(residuals) / variance(response)
# R2 = 0 = residuals are as variable as the raw data  
# R2 = 1 = all data is on the regression line

# F-statistic: compares fitted model against intercept-only model. 

plot(allEffects(fit, partial.residuals = T))

par(mfrow = c(2,2))
plot(fit)

fit = lm(Ozone ~ Wind + I(Wind^2), data = airquality)
fit = lm(Ozone ~ poly(Wind,2), data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

par(mfrow = c(2,2))
plot(fit)


par(mfrow = c(1,1))
plot(log(Ozone) ~ sqrt(Wind),data = airquality)
fit = lm(log(Ozone) ~ sqrt(Wind) , data = airquality)
plot(allEffects(fit, partial.residuals = T))
summary(fit)


plot(log(Ozone) ~ log(Wind), data=airquality)
fit <- lm(log(Ozone) ~ log(Wind), data=airquality)
plot(allEffects(fit, partial.residuals = T))
par(mfrow = c(2,2))
plot(fit)


fit <- lm(Ozone^.3 ~ log(Wind), data=airquality)
plot(allEffects(fit, partial.residuals = T))
par(mfrow = c(2,2))
plot(fit)

fit <- lm(Ozone ~ log(Wind), data=airquality)
library(MASS)
boxcox(fit)

fit1 = lm(Ozone^0.35 ~ poly(Wind, 8), data = airquality)
plot(allEffects(fit1, partial.residuals = T), selection = 1)
par(mfrow = c(2,2))
plot(fit1)
summary(fit1)


library(mgcv)
fit = gam(Ozone ~ s(Wind) , data = airquality)
summary(fit)
plot(fit) # more complicated spline effect plots: mgcViz


# Categorical predictors

boxplot(weight ~ group, data = PlantGrowth)

str(PlantGrowth)

# default R: treatment contrasts
fit = lm(weight ~ group, data = PlantGrowth)
summary(fit)

# simple alternative R: mean contrasts
fit = lm(weight ~ group - 1, data = PlantGrowth)
summary(fit)


fit = lm(weight ~ group, data = PlantGrowth)
summary(fit)

summary(aov(fit))

# R2 calculation by hand
# 3.766 / (3.766 + 10.492)

library(multcomp)
tuk = glht(fit, linfct = mcp(group = "Tukey"))
summary(tuk)          # Standard display.

tuk.cld = cld(tuk)    # Letter-based display.
plot(tuk.cld)


par(mfrow = c(2,2))
plot(fit)


library(EcoData)
plot(loght ~ temp, data = plantHeight)
fit = lm(loght ~ temp, data = plantHeight)
plot(allEffects(fit, partial.residuals = T))
par(mfrow = c(2,2))
plot(fit)

fit = gam(loght ~ s(temp), data = plantHeight)
plot(fit)

plantHeight$fGrowthform = factor(plantHeight$growthform)
plantHeight$fGrowthform2 = relevel(plantHeight$fGrowthform, ref = "Tree")

boxplot(loght ~ fGrowthform2, data = plantHeight)
fit = lm(loght ~ fGrowthform2, data = plantHeight)
summary(fit)


library(multcomp)
tuk = glht(fit, linfct = mcp(fGrowthform2 = "Tukey"))
summary(tuk)          # Standard display.

tuk.cld = cld(tuk)    # Letter-based display.

par(mar = c(3,3,8,3))
plot(tuk.cld)



pairs(airquality)

# Effect of Wind ADJUSTED for the effect of Temp
# Idea: artificial statistical control for Temp 

fit = lm(Ozone ~ Wind + Temp, data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

fit = gam(Ozone ~ Wind + s(Temp), data = airquality)
summary(fit)


airquality$sWind = scale(airquality$Wind)
airquality$sTemp = scale(airquality$Temp)

fit = lm(Ozone ~ sWind + sTemp, data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))


# DANGEROUS - calculates main effect at natural zero of all other variables
fit = lm(Ozone ~ Wind * Temp, data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

# Interactions ALWAYS with centered variables
fit = lm(Ozone ~ sWind * sTemp, data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))


# artifically calculating main effects at other values
airquality$mTemp = airquality$Temp - 2 * max(airquality$Temp)
fit = lm(Ozone ~ sWind * mTemp, data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))


plantHeight$sTemp = scale(plantHeight$temp)
plantHeight$sLat = scale(plantHeight$lat)
plantHeight$sNPP = scale(plantHeight$NPP)

# relevel 
plantHeight$growthform2 = relevel(as.factor(plantHeight$growthform), "Herb")

# Task 1

fit = lm(loght ~ sTemp + sNPP, data = plantHeight)
summary(fit)

# Task 2

fit = lm(loght ~ sTemp * growthform2, data = plantHeight)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

# suppress comparison to reference group
fit = lm(loght ~ sTemp * growthform2 - sTemp - 1, data = plantHeight)
summary(fit)

# Task 3

fit = lm(loght ~ sTemp * sLat, data = plantHeight)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

plot(sTemp ~ sLat, data = plantHeight)


# ANOVA (sequential or type I)

fit = lm(Ozone ~ Wind + Temp, data = airquality)
summary(aov(fit))

summary(lm(Ozone ~ 1, data = airquality))
summary(lm(Ozone ~ Wind, data = airquality))
summary(lm(Ozone ~ Wind + Temp, data = airquality))

fit = lm(Ozone ~ Temp * Wind, data = airquality)
summary(aov(fit))

library(car)
car::Anova(fit, type = "II")
car::Anova(fit, type = "III")




library(EcoData)
fit = lm(loght ~ temp * lat, data = plantHeight)
summary(fit)

print(car::Anova(fit, type = "II"))
print(car::Anova(fit, type = "III"))


library(EcoData)
fit = lm(loght ~ temp * growthform2, data = plantHeight)
summary(fit)

car::Anova(fit, type = "II")
