dat = airquality

str(dat)
summary(dat)
dat$fMonth = factor(dat$Month, labels = c("May", "June", "July",  "August", "Sept"))

str(dat)

dat$sOzone = scale(dat$Ozone)
dat$logOzone = log(dat$Ozone)

dat[1:3,]
dat[complete.cases(dat), ]

plot(Ozone~Temp, data = dat)

plot(Ozone~Wind, data = dat)

fit <- lm(Ozone~Wind, data = dat)
fit
abline(fit)
summary(fit)

library(effects)
plot(allEffects(fit, partial.residuals = T))

par(mfrow = c(2,2))
plot(fit)

hist(dat$Ozone)
hist(residuals(fit))

library(DHARMa)
res <- simulateResiduals(fit, plot = T)

# Strategies to improve the fit 

fit <- lm(log(Ozone)~sqrt(Wind), data = dat)
summary(fit)
plot(allEffects(fit, partial.residuals = T))


fit <- lm(Ozone~ Wind + I(Wind^2), data = dat)
summary(fit)
plot(allEffects(fit, partial.residuals = T))


library(MASS)
fit <- lm(Ozone~Wind, data = dat)
boxcox(fit)
fit <- lm(Ozone^0.3 ~ Wind, data = dat)
plot(allEffects(fit, partial.residuals = T))

library(mgcv)

fit <- gam(Ozone~s(Wind), data = dat)
summary(fit)
plot(fit)

fit = lm(Ozone ~ Temp, data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

par(mfrow = c(2,2))
plot(fit)
res <- simulateResiduals(fit, plot = T)

fit = lm(log(Ozone) ~ Temp , data = airquality)
plot(allEffects(fit, partial.residuals = T))
par(mfrow = c(2,2))
plot(fit)
res <- simulateResiduals(fit, plot = T)

summary(fit)

par(mfrow = c(1,1))
boxplot(weight~group, data = PlantGrowth)
str(PlantGrowth)

# default is treatment contrasts
fit = lm(weight ~ group, data = PlantGrowth)
summary(fit)

# mean contrasts
fit = lm(weight~group -1 , data = PlantGrowth)
summary(fit)

# plots and checks 
fit = lm(weight ~ group, data = PlantGrowth)
plot(allEffects(fit, partial.residuals = T))
par(mfrow = c(2,2))
plot(fit)

anovaResult = aov(fit)
summary(anovaResult)

TukeyHSD(anovaResult)

library(multcomp)
fit = lm(weight ~ group, data = PlantGrowth)
tuk = glht(fit, linfct = mcp(group = "Tukey"))
summary(tuk)          # Standard display.

tuk.cld = cld(tuk)    # Letter-based display.
plot(tuk.cld)

# multiple regression 

airquality$fMonth = factor(airquality$Month)

fit = lm(Ozone ~ Temp + Wind + fMonth, data = airquality)
summary(fit)

par(mfrow =c(2,2))
plot(fit)

# very good advise to plot also residuals against all predictors

x = model.frame(fit)
plot(residuals(fit) ~ x$Wind)
plot(residuals(fit) ~ x$Temp)
plot(residuals(fit) ~ x$fMonth)

plot(allEffects(fit, partial.residuals = T) )


# Effect of collinearity on estimates 
fit = lm(Ozone ~ Temp + Wind + fMonth, data = airquality)
summary(fit)
# effect of Wind: -2.7

fit = lm(Ozone ~ Wind , data = airquality)
summary(fit)
# effect of Wind: -5.5

plot(Temp ~ Wind, data = airquality)
pairs(airquality)


# centering and scaling 

fit = lm(Ozone ~ Temp + Wind + fMonth, data = airquality)
summary(fit)

plot(Ozone ~ Temp, data = airquality, xlim = c(-10, 110), ylim = c(-200, 170))
abline(fit)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

airquality$cTemp = airquality$Temp - mean(airquality$Temp)

airquality$cTemp = scale(airquality$Temp, 
                         center = T, 
                         scale = F)

fit = lm(Ozone ~ cTemp, data = airquality)
plot(Ozone ~ cTemp, data = airquality)
abline(fit)
abline(v = 0, lty = 2)

summary(fit)

fit = lm(Ozone ~ Temp + Wind + fMonth, data = airquality)
summary(fit)

plot(allEffects(fit))

airquality$sTemp = scale(airquality$Temp)
airquality$sWind = scale(airquality$Wind)

fit = lm(Ozone ~ sTemp + sWind + fMonth, data = airquality)
summary(fit)


fit = lm(Ozone ~ Wind * Temp, data = airquality)
fit = lm(Ozone ~ Wind + Temp + Wind:Temp, data = airquality)
plot(allEffects(fit))
summary(fit)

# interactions and centoring

fit = lm(Ozone ~ Wind + Temp, data = airquality)
summary(fit) # main effect of Wind is negative

fit = lm(Ozone ~ Wind * Temp, data = airquality)
summary(fit) # main effect of Wind is negative

# same with centered variables 

airquality$cWind = airquality$Wind - mean(airquality$Wind)

fit = lm(Ozone ~ cWind + cTemp, data = airquality)
summary(fit) # main effect of Wind is negative

fit = lm(Ozone ~ cWind * cTemp, data = airquality)
summary(fit) # main effect of Wind is negative

# now it's the same! Therefore, I always recommend to center so that you can't forget it!

# Exercise https://theoreticalecology.github.io/AdvancedRegressionModels/2A-LinearRegression.html#case-studies


library(EcoData)
plot(loght ~ temp, data = plantHeight)
fit = lm(loght ~ temp, data = plantHeight)
par(mfrow = c(2,2))
plot(fit)
plot(allEffects(fit, partial.residuals = T))

boxplot(loght ~ growthform, data = plantHeight, las = 2)

table(plantHeight$growthform)

plantHeight$fgrowthform = factor(plantHeight$growthform)
plantHeight$fgrowthform = relevel(plantHeight$fgrowthform, ref = "Tree")

fit = lm(loght ~ fgrowthform, data = plantHeight)
summary(fit)


fit = lm(loght ~ scale(temp) + scale(NPP), data = plantHeight)
summary(fit)

fit = lm(loght ~ temp * fgrowthform, data = plantHeight)
summary(fit)
plot(allEffects(fit, partial.residuals = T))


fit = lm(loght ~ scale(temp) * scale(lat), data = plantHeight)
summary(fit)

plot(temp~lat , data = plantHeight)
