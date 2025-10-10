# test
# Linear regression basics

str(airquality)

plot(Ozone ~ Wind, data = airquality)

fit<-lm(Ozone ~ Wind, data = airquality)

fit

summary(fit)

abline(fit)

library(effects)
plot(allEffects(fit, partial.residuals = T))

par(mfrow = c(2,2))
plot(fit)

library(DHARMa)

res <- simulateResiduals(fit, plot = T)

shapiro.test(residuals(fit))


fit<-lm(log10(Ozone) ~ Wind , data = airquality)
plot(allEffects(fit, partial.residuals = T))

par(mfrow = c(2,2))
plot(fit)

library(mgcv)
fit<-gam(Ozone ~ s(Wind), data = airquality)
plot(fit)
summary(fit)


boxplot(weight ~ group, data = PlantGrowth)

# treatment contrasts
fit <- lm(weight ~ group, data = PlantGrowth)
summary(fit)

summary(aov(fit))

TukeyHSD(aov(fit))


library(multcomp)

fit = lm(weight ~ group, data = PlantGrowth)
tuk = glht(fit, linfct = mcp(group = "Tukey"))
summary(tuk)          # Standard display.
tuk.cld = cld(tuk)    # Letter-based display.
plot(tuk.cld)


par(mfrow = c(2,2))
plot(fit)



library(EcoData)


model = lm(loght ~ temp, data = plantHeight)
summary(model)

par(mfrow = c(2,2))
plot(model)

model = lm(loght ~ growthform, data = plantHeight)
summary(model)

plot(allEffects(model))

table(plantHeight$growthform)

plantHeight$growthform2 = relevel(as.factor(plantHeight$growthform), "Herb")
model2 = lm(loght ~ growthform2, data = plantHeight)
summary(model2)

plot(allEffects(model2))



airquality$fMonth = factor(airquality$Month)


fit = lm(Ozone ~ Temp + Solar.R + Wind + fMonth , data = airquality)
summary(fit)


plot(allEffects(fit, partial.residuals = T))

fit = lm(Ozone ~ Wind , data = airquality)
summary(fit)

plot(Temp ~ Wind, data = airquality)


set.seed(123)
x1 = runif(100)
x2 = 0.95 *x1 + 0.05 * runif(100)
y = x1 + x2 + rnorm(100)

summary(lm(y ~ x1 + x2))

summary(lm(y ~ x1))


res <- simulateResiduals(fit, plot = T)
plotResiduals(res, quantreg = T)

fit = lm(Ozone ~ scale(Temp) + scale(Solar.R), data = airquality)
summary(fit)

# centering = always
# scale: depends


fit = lm(Ozone ~ Temp , data = airquality)
plot(Ozone ~ Temp , data = airquality, xlim = c(0,100), ylim = c(-150, 150))
abline(fit)

summary(fit)

airquality$cTemp = airquality$Temp - mean(airquality$Temp)

fit = lm(Ozone ~ cTemp , data = airquality)
summary(fit)

airquality$csTemp = airquality$cTemp / sd(airquality$cTemp)


fit = lm(Ozone ~ Temp + Wind , data = airquality)
summary(fit)

fit = lm(Ozone ~ scale(Temp) + scale(Wind) , data = airquality)
summary(fit)


fit = lm(Ozone ~  Wind * Temp , data = airquality)
summary(fit)


airquality$sTemp = scale(airquality$Temp)
airquality$sWind = scale(airquality$Wind)
fit = lm(Ozone ~  sWind * sTemp , data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))


fit = lm(Ozone ~  fMonth *  Wind , data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

fit = lm(Ozone ~ Wind, data = airquality)

Wind = seq(0,10,0.1)
newData = data.frame(Wind = Wind)
pred = predict(fit, newdata = newData, se.fit = T)
plot(Wind, pred$fit, type = "l")
lines(Wind, pred$fit - 1.96 * pred$se.fit, lty = 2)
lines(Wind, pred$fit + 1.96 * pred$se.fit, lty = 2)








plantHeight$sTemp = scale(plantHeight$temp)
plantHeight$sLat = scale(plantHeight$lat)
plantHeight$sNPP = scale(plantHeight$NPP)
plantHeight$growthform2 = relevel(as.factor(plantHeight$growthform), "Herb")


fit = lm(loght ~ sTemp + sNPP, data = plantHeight)
summary(fit)

fit = lm(loght ~ sTemp * growthform2 , data = plantHeight)
summary(fit)

plot(allEffects(fit))


fit = lm(loght ~ sTemp * sLat, data = plantHeight)
summary(fit)

cor(plantHeight$temp, plantHeight$lat)

fit = lm(loght ~ sTemp, data = plantHeight)
summary(fit)


# ANOVA

fit = lm(Ozone ~ scale(Temp) + scale(Wind), data = airquality)
summary(fit)

# type I ANOVA - takes in one variable after the other
summary(aov(fit))

summary(aov(Ozone ~ Temp + Wind, data=airquality))
summary(aov(Ozone ~ Wind + Temp, data=airquality))

10137 / (61033 + 10137 + 53973)

# likeklihood ratio test

print(car::Anova(fit, type = "II"))
print(car::Anova(fit, type = "III"))


m0 = lm(Ozone ~ 1, data = airquality)
m1 = lm(Ozone ~ Wind + I(Wind^2) , data = airquality)

plot(allEffects(m1))

summary(m1)
anova(m0, m1)

# MIXED MODELS 

library(mlmRev)
library(effects)

mod0 = lm(normexam ~ standLRT + sex , data = Exam)
plot(allEffects(mod0))
summary(mod0)

mod1 = lm(normexam ~ standLRT + sex + school, data = Exam)
plot(allEffects(mod1))
summary(mod1)

library(lme4)
mod1 = lmer(normexam ~ standLRT + sex + (1|school), data = Exam)
plot(allEffects(mod1))
summary(mod1)

x = ranef(mod1)

0.51399 + x$school$`(Intercept)`

hist(x$school$`(Intercept)`)

mf = lm(normexam ~ 0 + school, data = Exam)
mr = lmer(normexam ~ (1 | school), data = Exam)

ef = coef(mf)
er = ranef(mr)$school$`(Intercept)`
plot(ef, er, xlab = "fixed", ylab = "random")
abline(0,1)

library(lmerTest)

mod0 = lmer(normexam ~ standLRT + sex + (1|school) , data = Exam)
summary(mod0)


mod1 = lm(normexam ~ standLRT * school + sex, data = Exam)
plot(allEffects(mod1))
summary(mod1)

library(lme4)
mod2 = lmer(normexam ~ standLRT + sex + (standLRT|school), data = Exam)
plot(allEffects(mod2))
summary(mod2)

ranef(mod1)


plantHeight


fit <- lm(loght ~ sTemp * growthform2 , data = plantHeight)
summary(fit)

fit <- lmer(loght ~ sTemp + (sTemp | growthform2) , data = plantHeight)
summary(fit)

ranef(fit)


Random intercept: (1|group)
Random slope + intercept: (fixed|group)
Random slope + intercept without correlation: (fixed||group)

Crossed RE: (1|time) + (1|location)
Nested RE:  (1|group/subgroup)

Group Subgroup SubUnique

A      1        A1
A      2        A2
B      1        B1
B      2        B2

(1|group/subgroup)
(1|group/SubUnique) identical to (1|group) + (1|SubUnique) 





