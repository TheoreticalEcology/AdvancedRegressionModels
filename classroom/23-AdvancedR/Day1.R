
head(airquality)
str(airquality)
summary(airquality)

airquality$fMonth = as.factor(airquality$Month)

plot(Ozone ~ fMonth, data = airquality)


airquality$cTemp = airquality$Temp - mean(airquality$Temp)




plot(Ozone ~ cTemp, data = airquality) 

fit <- lm(Ozone ~ cTemp, data = airquality)
summary(fit)

abline(fit)
abline(h = 0)
abline(v = 0)

# effect plots
library(effects)
plot(allEffects(fit, partial.residuals = T))


# Standard residual plots
par(mfrow = c(2,2))
plot(fit)

res = residuals(fit)
hist(res)
shapiro.test(res)


fit <- lm(Ozone ~ cTemp + I(cTemp^2), data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))


fit <- lm(Ozone^0.25 ~ cTemp + I(cTemp^2) , data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))
par(mfrow = c(2,2))
plot(fit)


library(MASS)
x = boxcox(fit)

library(mgcv)
fit <- gam(Ozone ~ s(cTemp) , data = airquality)
summary(fit)
plot(fit)





boxplot(weight ~ group, data = PlantGrowth)


fit = lm(log(weight) ~ group , data = PlantGrowth)
summary(fit)

plot(allEffects(fit))

anov = aov(fit)
summary(anov)

library(multcomp)

tuk = glht(fit, linfct = mcp(group = "Tukey"))
summary(tuk)          # Standard display.


tuk.cld = cld(tuk)    # Letter-based display.
plot(tuk.cld)

par(mfrow = c(2,2))
plot(fit)



plot(Ozone ~ Temp, data = airquality)



fit <- lm(Ozone ~ Temp + Wind, data = airquality)
summary(fit)



x1 <- runif(500)
x2 <- 0.90 * x1 + 0.1 *  runif(500)

plot(x1, x2)

y <- 1*x1 - 1*x2 + rnorm(500, sd = 0.05)

plot(y ~ x1)

fit<- lm(y ~ x1)
summary(fit)

fit<- lm(y ~ x1 + x2)
summary(fit)



fit <- lm(Ozone ~ Temp, data = airquality)
summary(fit)

fit <- lm(Ozone ~ Temp + Wind + fMonth, data = airquality)
summary(fit)

plot(allEffects(fit, partial.residuals = T))

airquality$sWind = scale(airquality$Wind)
airquality$sTemp = scale(airquality$Temp)
airquality$sSolar = scale(airquality$Solar.R)

fit <- lm(Ozone ~ sTemp + sWind + fMonth, data = airquality)
summary(fit)

plot(allEffects(fit, partial.residuals = T))


fit <- lm(Ozone ~ Wind * Temp , data = airquality)
summary(fit)

plot(allEffects(fit, partial.residuals = T))

# center = always
# scale = up to you


fit <- lm(Ozone ~ sTemp * sWind , data = airquality)
summary(fit)

anov = aov(fit)
summary(anov)


car::Anova(fit, type = "II")
car::Anova(fit, type = "III")











