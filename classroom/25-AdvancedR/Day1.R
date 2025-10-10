plot(Ozone ~ Temp, data = airquality)
str(airquality)
View(airquality)

cor(airquality$Ozone, airquality$Temp, use = "complete.obs")
cor.test(airquality$Ozone, airquality$Temp, use = "complete.obs")

fit = lm(Ozone ~ Temp, data = airquality)
fit
abline(fit, col = "red")
summary(fit)

library(effects)
plot(allEffects(fit, partial.residuals = F))


## Effect ## 95% CI / se (63% CI) ## p-value 

# There was a significant effect of Temp on Ozone (effect size 2.42 +/- 0.23 (se), p-value < 2E-16. 

plot(allEffects(fit, partial.residuals = T))

airquality

1 - var(residuals(fit), na.rm = T) / var(airquality$Ozone, na.rm = T)

# r2 1 = perfect fit 
# r2 0 = no explanation 

fit = lm(Ozone ~ Temp, data = airquality)
plot(allEffects(fit, partial.residuals = T))

par(mfrow = c(2,2))
plot(fit)

airquality[117, ]


fit = lm(sqrt(Ozone) ~ I(Temp^2) , data = airquality)
plot(allEffects(fit, partial.residuals = T))
par(mfrow = c(2,2))
plot(fit)
summary(fit)

fit = lm(sqrt(Ozone) ~ Temp, data = airquality)
library(MASS)
x = boxcox(fit)


fit = lm(Ozone ~ Temp, data = airquality)
par(mfrow = c(2,2))
plot(fit)


fit = lm(Ozone^0.2 ~ Temp + I(Temp^2)
, data = airquality)
par(mfrow = c(2,2))
plot(fit)

fit = lm(log(Ozone) ~ I(Temp^3), data = airquality)
plot(allEffects(fit, partial.residuals = T))
par(mfrow = c(2,2))
plot(fit)

summary(fit)

fit = lm(Ozone^0.2 ~ Temp + I(Temp^2) + I(Temp^3), data = airquality)
plot(allEffects(fit, partial.residuals = T))
par(mfrow = c(2,2))
plot(fit)
summary(fit)



library(mgcv)
fit = gam(Ozone ~ s(Solar.R), data = airquality)
summary(fit)
plot(fit)

# categorical predictors

boxplot(weight ~ group, data = PlantGrowth)

fit <- lm(weight ~ group, data = PlantGrowth)
summary(fit)

PlantGrowth$newGroup = relevel(PlantGrowth$group, ref = "trt1")
boxplot(weight ~ newGroup, data = PlantGrowth)
fit <- lm(weight ~ newGroup, data = PlantGrowth)
summary(fit)

fit <- lm(weight ~ group-1, data = PlantGrowth)
summary(fit)

fit <- lm(weight ~ group, data = PlantGrowth)
summary(fit)

anov = aov(fit)
summary(anov)

totalSumSq = 3.766 + 10.492

3.766 / totalSumSq

TukeyHSD(anov)

library(multcomp)

fit = lm(weight ~ group, data = PlantGrowth)
tuk = glht(fit, linfct = mcp(group = "Tukey"))
summary(tuk)          # Standard display.

tuk.cld = cld(tuk)    # Letter-based display.
plot(tuk.cld)



library(EcoData)

plot(loght ~ temp, data = plantHeight)
fit = lm(loght ~ temp, data = plantHeight)
opar = par(mfrow = c(2,2))
plot(fit)
summary(fit)

# There was a significant positive relationship between plant height and Temperature. Plants tended to increase in their log height with 0.04 +/- 0.006 (se) per degree temperature. 

plantHeight$fGrowthform = factor(plantHeight$growthform)
par(opar)
plot(loght ~ fGrowthform, data = plantHeight)
plantHeight$newGrowthForm = relevel(plantHeight$fGrowthform, ref = "Tree")
plot(loght ~ newGrowthForm, data = plantHeight, las = 2)

fit = lm(loght ~ newGrowthForm, data = plantHeight)
summary(fit)

opar = par(mfrow = c(2,2))
plot(fit)
par(opar)

anova = aov(fit)
summary(anova)

plot(loght ~ newGrowthForm, data = plantHeight, las = 2, notch = T)
points(as.numeric(plantHeight$newGrowthForm) + runif(nrow(plantHeight), -0.1, 0.1), 
       plantHeight$loght,
       pch = 16, cex = 0.7)

library(multcomp)
tuk = glht(fit, linfct = mcp(newGrowthForm = "Tukey"))
summary(tuk)          # Standard display.

tuk.cld = cld(tuk)    # Letter-based display.
par(mar = c(6, 4,10,4))
plot(tuk.cld, las = 2, notch = T)
points(as.numeric(plantHeight$newGrowthForm) + runif(nrow(plantHeight), -0.1, 0.1), 
       plantHeight$loght,
       pch = 16, cex = 0.5)


# Multiple regressions 

str(airquality)
dat = airquality
dat$fMonth = factor(dat$Month, labels = c("May", "June", "July", "August", "Sept"))

plot(Ozone ~ Temp, data = dat)
fit = lm(Ozone ~ Temp, data = dat)
summary(fit)

fit = lm(Ozone ~ Temp + Wind + Solar.R + fMonth, data = dat)
summary(fit)

par(mfrow = c(2,2))
plot(fit)

plot(allEffects(fit, partial.residuals = T))



par(mfrow = c(1,3))
plot(Ozone ~ Temp, data = dat)
plot(Ozone ~ Wind, data = dat)
plot(Temp ~ Wind, data = dat)

fit = lm(Ozone ~ Temp, data = dat)
summary(fit)


set.seed(123)
x1 = runif(1000)
x2 = - 0.7 *x1 + 0.3 * runif(1000)

y = 0.3 * x1 + x2 + rnorm(1000)
summary(lm(y ~ x1 + x2))
m1 = summary(lm(y ~ x1))

plot(y ~ x1)
abline(m1)



fit = lm(Ozone ~ Temp + Wind + Solar.R + fMonth, data = dat)
summary(fit)
plot(allEffects(fit, partial.residuals = T))


# center = subtract mean, scale = divide by sd
dat$sSolar.R = scale(dat$Solar.R)
dat$sTemp = scale(dat$Temp)
dat$sWind = scale(dat$Wind)

fit = lm(Ozone ~ sTemp + sWind + sSolar.R + fMonth, data = dat)
summary(fit)
plot(allEffects(fit, partial.residuals = T))


fit = lm(Ozone ~ sWind * sTemp , data = dat)
summary(fit)
plot(allEffects(fit, partial.residuals = F))


fit = lm(Ozone ~ sWind * fMonth, data = dat)
summary(fit)
plot(allEffects(fit, partial.residuals = F))



model = lm(loght ~ temp, data = plantHeight)

#  If temp or NPP (net primary productivity) is a more important predictor.

plantHeight$sTemp = scale(plantHeight$temp)
plantHeight$sNPP = scale(plantHeight$NPP)
plantHeight$sLat = scale(plantHeight$lat)

m1 = lm(loght ~ sTemp + sNPP, data = plantHeight)
summary(m1)

m1b = lm(loght ~ sNPP, data = plantHeight)
summary(m1b)

# If growth forms (variable growthform) differ in their temperature effects.

m2 = lm(loght ~ growthform2 * sTemp, data = plantHeight)
summary(m2)
plot(allEffects(m2, partial.residuals = T))

# Switch from treatment to mean contrasts for both intercept and slope
m2b = lm(loght ~ growthform2 * sTemp - sTemp - 1, data = plantHeight)
summary(m2b)

summary(aov(m2))


# If the effect of temp remains significant if we include latitude and an interaction of latitude with temp. If not, why? Tip: plot temp ~ lat.


m3 = lm(loght ~ sLat * sTemp, data = plantHeight)
summary(m3)
plot(sLat ~ sTemp, data = plantHeight)

m3b = lm(loght ~ sTemp, data = plantHeight)
summary(m3b)

# ANOVA 

fit1 = lm(Ozone ~ Wind + Temp, data = airquality)
summary(fit1)

summary(aov(fit1))

sumSq = c(45284,25886,53973)
sumSq / sum(sumSq)

fit2 = lm(Ozone ~ Temp + Wind, data = airquality)
summary(fit2)

summary(aov(fit2))
sumSq = c(61033,10137,53973)
sumSq / sum(sumSq)


m0 = lm(Ozone ~ 1, data = airquality)
summary(m0)

m1 = lm(Ozone ~ Wind, data = airquality)
summary(m1)

m1b = lm(Ozone ~ Temp, data = airquality)
summary(m1b)

m2 = lm(Ozone ~ Wind + Temp, data = airquality)
summary(m2)


# Type I, II, III ANOVA 

fit1 = lm(Ozone ~ Wind * Temp, data = airquality)

summary(aov(fit1)) # Type I 
car::Anova(fit1, type = "II")
car::Anova(fit1, type = "III")






