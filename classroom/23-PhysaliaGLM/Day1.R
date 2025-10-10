
dat<- airquality
str(dat)
summary(dat)

dat$fMonth = as.factor(dat$Month)
levels(dat$fMonth)

dat$sWind = scale(dat$Wind)

dat$logTemp = log(dat$Temp)

dat2 = dat[1:100, ]

dat3 = dat2[complete.cases(dat2), ]


plot(dat3$Ozone, dat3$Temp)
plot(Temp ~ Ozone, data = dat3)

cor(dat3$Ozone, dat3$Temp, method = "spearman")


plot(Ozone ~ Temp, data = dat3)


plot(Ozone ~ Wind, data = airquality)

fit <- lm(Ozone ~ Wind, data = airquality)

# y ~ a0 + a1 * Wind + NormalError

abline(fit)
summary(fit)

residuals(fit)

summary(airquality)
str(airquality)

library(effects)

plot(allEffects(fit, partial.residuals = T))

par(mfrow = c(2,2))
plot(fit)

# RESIDUALS CHECKS: 

# 1) First get the mean right
# 2) Then care about distribution

library(DHARMa)

res <- simulateResiduals(fit, plot = T)

airquality$logOzone = log(airquality$Ozone)




plot(Ozone ~ Wind, data = airquality)



fit <- lm(Ozone ~ log(Wind +1), data = airquality)

plot(allEffects(fit, partial.residuals = T))


fit <- lm(Ozone ~ Wind + I(Wind^2), data = airquality)

plot(allEffects(fit, partial.residuals = T))


fit <- lm(Ozone^1.4 ~ Wind, data = airquality)
plot(allEffects(fit, partial.residuals = T))

library(MASS)
boxcox(fit)


fit1 = lm(Ozone^0.35 ~ Wind + I(Wind^2), data = airquality)
plot(allEffects(fit1, partial.residuals = T), selection = 1)


fit2 = lm(Ozone^0.35 ~ Wind + I(Wind^2) + I(Wind^3), data = airquality)
plot(allEffects(fit2, partial.residuals = T), selection = 1)
summary(fit2)

fit3 = lm(Ozone^0.3 ~ log(Wind +1), data = airquality)
plot(allEffects(fit3, partial.residuals = T), selection = 1)
summary(fit3)


plot(Ozone ~ Wind, data = airquality)

library(mgcv)

fit <- gam(Ozone ~ s(Wind), data = airquality)
summary(fit)
plot(fit)


fit3 = lm(Ozone^0.3 ~ log(Wind +1), data = airquality)
plot(allEffects(fit3, partial.residuals = T), selection = 1)
summary(fit3)

par(mfrow = c(2,2))
plot(fit3)


plot(weight ~ group, data = PlantGrowth)

fit<-lm(weight ~ group, data = PlantGrowth)
summary(fit)
plot(allEffects(fit, partial.residuals = T))


fit<-lm(weight ~ group + 0, data = PlantGrowth)
summary(fit)


summary(aov(weight ~ group, data = PlantGrowth))


fit<-lm(weight ~ group, data = PlantGrowth)
summary(fit)

fit<-lm(weight ~ group, data = PlantGrowth)
summary(aov(fit))

library(multcomp)

tuk = glht(fit, linfct = mcp(group = "Tukey"))
summary(tuk)         

tuk.cld = cld(tuk)    # Letter-based display.
plot(tuk.cld)


library(EcoData)

str(plantHeight)

plot(loght ~ temp, data = plantHeight)

abline(fit)

model = lm(loght ~ temp, data = plantHeight)
par(mfrow = c(2, 2))
plot(model)
abline(model)
summary(model)

# There was a significant postive effect of temperature on global plant height (based on a linear regression, p = 1.87 E-12, effect estimate 0.0424 +/- 0.0056). Residual checks were performed on the regression and showed no concerning patterns (online supplement S1)


model2 = lm(loght ~ growthform, data = plantHeight)
summary(model2)

plot( loght ~ as.factor(growthform), data = plantHeight)

plantHeight$growthform2 = relevel(as.factor(plantHeight$growthform), "Herb")

plot( loght ~ growthform2, data = plantHeight)

model2 = lm(loght ~ growthform2, data = plantHeight)
summary(model2)

summary(aov(model2))

tuk = glht(model2, linfct = mcp(growthform2 = "Tukey"))
summary(tuk)         

par(mar = c(8,8,8,8))
tuk.cld = cld(tuk)    # Letter-based display.
plot(tuk.cld)



airquality$fMonth = factor(airquality$Month)
fit = lm(Ozone ~ Temp + Wind + Solar.R + fMonth, data = airquality)
summary(fit)

plot(allEffects(fit, partial.residuals = T))

par(mfrow = c(2,2))
plot(fit)




fit = lm(Ozone ~ scale(Wind) + scale(Temp), data = airquality)
summary(fit)

fit = lm(Ozone ~ Wind , data = airquality)
summary(fit)


fit = lm(Ozone ~ Wind * Temp , data = airquality)
summary(fit)

plot(allEffects(fit))

fit = lm(Ozone ~ Wind + Temp, data = airquality)
summary(fit)

hist(scale(airquality$Temp))


fit = lm(Ozone ~ scale(Wind) * scale(Temp) , data = airquality)
summary(fit)
plot(allEffects(fit))

# Bottomline: in doubt, center and scale all numeric variables!

# Replicates -> it depends, see model selection!



