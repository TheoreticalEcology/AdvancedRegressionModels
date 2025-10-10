dat = airquality
dat$fMonth = as.factor(dat$Month)

pairs(dat)

plot(Ozone ~ Wind, data = airquality)

# lm = linear model -> normal distribution
# glm = generalized linear model -> other distributions



# Ozone ~ Wind = Ozone = intercept + a * Wind
fit <- lm(Ozone ~ Wind, data = airquality)
summary(fit)

abline(fit)


fit <- lm(Ozone ~ Wind - 1, data = airquality)
summary(fit)

abline(fit, col = "red")


library(effects)

plot(allEffects(fit, partial.residuals = T))

par(mfrow = c(2,2))
plot(fit)


fit <- lm(Ozone ~ Wind + I(Wind^2) + I(Wind^3), data = airquality)
summary(fit)


plot(allEffects(fit, partial.residuals = T))


fit <- lm(Ozone^0.2 ~ log(Wind) , data = airquality)
summary(fit)

plot(allEffects(fit, partial.residuals = T))

par(mfrow = c(2,2))
plot(fit)


library(mgcv)

fit = gam(Ozone ~ s(Wind) , data = airquality)
plot(fit)
summary(fit)

str(PlantGrowth)
boxplot(weight ~ group, data = PlantGrowth)


fit <- lm(weight ~ group, data = PlantGrowth)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

summary(aov(fit))


fit <- lm(weight ~ group - 1, data = PlantGrowth)
summary(fit)


summary(aov(fit))


library(multcomp)

fit = lm(weight ~ group, data = PlantGrowth)
summary(fit)

tuk = glht(fit, linfct = mcp(group = "Tukey"))

summary(tuk)          # Standard display.

tuk.cld = cld(tuk)    # Letter-based display.
plot(tuk.cld)



library(EcoData)
plot(loght ~ temp, data = plantHeight)

fit = lm(loght ~ temp, data = plantHeight)

plot(allEffects(fit, partial.residuals = T))

par(mfrow = c(2, 2))
plot(fit)

boxplot(loght ~ growthform, data = plantHeight)

table(plantHeight$growthform)

model2 = lm(loght ~ growthform, data = plantHeight)
summary(model2)

plantHeight$growthform2 = relevel(as.factor(plantHeight$growthform), "Herb")

plantHeight2 = droplevels(subset(plantHeight, ! growthform %in% c("Fern", "Herb/Shrub")))


aq <- 
table(           aq $Month)
table(droplevels(aq)$Month)

model2 = lm(loght ~ growthform2, data = plantHeight)
summary(model2)

summary(aov(model2))


tuk = glht(model2, linfct = mcp(growthform2 = "Tukey"))

summary(tuk)          # Standard display.

tuk.cld = cld(tuk)    # Letter-based display.
plot(tuk.cld)

########## Multiple regression ##############


airquality$fMonth = factor(airquality$Month)

fit = lm(Ozone ~ Temp + Wind + Solar.R + fMonth, data = airquality)
summary(fit)

plot(allEffects(fit, partial.residuals = T))
par(mfrow = c(2,2))
plot(fit)

fit = lm(Ozone ~ Temp , data = airquality)
summary(fit)

fit = lm(Ozone ~ Wind , data = airquality)
summary(fit)

plot(Temp ~ Wind, data = airquality)
plot(Ozone ~ Wind, data = airquality)
pairs(airquality)


fit = lm(Ozone ~ Temp + Wind , data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))



# scale = scale + center per default
# rule: center: always!
# scale: depends on the purpose, scale if you want to quickly compare effect importance
airquality$sWind = scale(airquality$Wind)
airquality$sTemp = scale(airquality$Temp)

fit = lm(Ozone ~ sTemp + sWind , data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))



fit = lm(Ozone ~ Temp * Wind, data = airquality)
plot(allEffects(fit))
summary(fit)


fit = lm(Ozone ~ sTemp + sWind, data = airquality)
plot(allEffects(fit))
summary(fit)



model2 = lm(loght ~ growthform2 * temp, data = plantHeight)
summary(model2)


plantHeight2 = droplevels(subset(plantHeight, ! growthform %in% c("Fern", "Herb/Shrub")))


model2 = lm(loght ~ growthform2 * temp - 1 - temp, data = plantHeight2)
summary(model2)
plot(allEffects(model2))


# data preparation
plantHeight$sTemp = scale(plantHeight$temp)
plantHeight$sLat = scale(plantHeight$lat)
plantHeight$sNPP = scale(plantHeight$NPP)
plantHeight2 = droplevels(subset(plantHeight, ! growthform %in% c("Fern", "Herb/Shrub")))


fit = lm(loght ~ sTemp + sNPP, data = plantHeight2)
summary(fit)
# NPP more important, alternative would be an ANOVA
Anova(fit, type = "II")


fit = lm(loght ~ growthform2 *  sTemp , data = plantHeight2)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

Anova(fit, type = "III")

# yes, could also use ANOVA, see later

fit = lm(loght ~ growthform2 *  sTemp - 1 - sTemp, data = plantHeight2)
summary(fit)



fit = lm(loght ~ sTemp * sLat, data = plantHeight2)
summary(fit)
plot(sTemp ~ sLat, data = plantHeight2)


### ANOVA ###


fit = lm(Ozone ~ Wind + Temp, data = airquality)
summary(fit)
summary(aov(fit))

fit = lm(Ozone ~ Temp + Wind,  data = airquality)
summary(fit)
summary(aov(fit))

library(car)

car::Anova(fit, type = "II")


