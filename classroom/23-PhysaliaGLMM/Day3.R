set.seed(123)

x = runif(100)
y = 0.25 * x + rnorm(100, sd = 0.3)

plot(x,y)
fit = lm(y~x)
summary(fit)
abline(fit)

xNoise = matrix(runif(8000), ncol = 80)
dat = data.frame(y=y,x=x, xNoise)

fullModel = lm(y~ . , data = dat)
summary(fullModel)


set.seed(42)
X = runif(100)
P = runif(100)
Y = 0.8*X + 10*P + rnorm(100, sd = 0.5)

summary(lm(Y~X))
summary(lm(Y~X+P))



# Model 1
m1 = lm(Ozone ~ Wind  , data = airquality)

# Model 2
m2 = lm(Ozone ~ Wind + Temp , data = airquality)

library(DHARMa)
simulateLRT(m1,m2)


# LRT
anova(m1, m2)

AIC(m1)
AIC(m2)


set.seed(123)
x1 = runif(100)
x2 = 0.8 * x1 + 0.2 *runif(100)
y = x1 + x2 + rnorm(100)

m1 = lm(y ~ x1 + x2)
summary(m1)

m2 = MASS::stepAIC(m1)
summary(m2)



set.seed(123)
x1 = runif(100)
x2 = 0.95 * x1 + 0.05 *runif(100)
x3 = runif(100)
y = x1 + x2 + x3 + rnorm(100)
m1 = lm(y ~ x1)
AIC(m1)

m2 = lm(y ~ x1 + x2)
anova(m1, m2)
AIC(m2)

m3 = lm(y ~ x1 + x3)
anova(m1, m3)
AIC(m3)
summary(m3)


set.seed(123)
x = runif(100)
y = 0.25 * x + rnorm(100, sd = 0.3)
xNoise = matrix(runif(8000), ncol = 80)
dat = data.frame(y=y,x=x, xNoise)
fullModel = lm(y~., data = dat)
summary(fullModel)

library(MASS)
reduced = stepAIC(fullModel)
summary(reduced)



m2 = lm(Ozone ~ Wind + Temp + Day , data = airquality)
summary(m2)


m2 = lm(Ozone ~ (Wind + Temp + Day)^2 , data = airquality)
summary(m2)

par(mfrow = c(2,2))
plot(m2)

# GLMs 

library(EcoData)
library(effects)
#str(birdfeeding)
plot(feeding ~ attractiveness, data = birdfeeding)


fit <- glm(feeding ~ attractiveness, data = birdfeeding, family = "poisson")
summary(fit)
plot(allEffects(fit, partial.residuals = T))

res <- simulateResiduals(fit, plot = T)



library(EcoData)
#str(titanic)
#mosaicplot( ~ survived + sex + pclass, data = titanic)
titanic$pclass = as.factor(titanic$pclass)
str(titanic)

fit = lm(survived ~ sex * age, data = titanic)
summary(fit)

par(mfrow = c(2, 2))
plot(fit)

fit = glm(survived ~ sex * age, data = titanic, family = "binomial")
summary(fit)

plogis(0.493381)

plot(allEffects(fit))

predict(fit, type = "response")

newDat = data.frame(sex = as.factor(c("female", "male")), age = c(20,20))
predict(fit, newdata = newDat) # Linear predictor.
predict(fit, newdata = newDat, type = "response") # Linear predictor.
predict(fit, newdata = newDat, type = "response", se.fit = T) # Linear predictor.


dat = titanic[complete.cases(titanic[,c(1,2,4,5)]), ]

fit = glm(survived ~ sex , data = dat, family = "binomial")
summary(fit)

par(mfrow = c(2, 2))
plot(fit)

# can't use standard residual plots of non-normal GLMs or GLMMs 

fit = glm(survived ~ sex + age, data = dat, family = "binomial")
summary(fit)

library(DHARMa)
res <- simulateResiduals(fit, plot = T)

plotResiduals(res, dat$pclass)
plotResiduals(res, dat$age)

load(file = "../AdvancedBiostatistics/0_Data/Data/elk_data.RData")

library(MASS)

fit <- glm(presence ~ (dist_roads  + dem + ruggedness + habitat + NDVI)^2, data = elk_data, family = "binomial")
predictive_model = MASS::stepAIC(fit, direction = "both")


fit <- glm(presence ~ dist_roads, data = elk_data, family = "binomial")
summary(fit)
plot(allEffects(fit))


fit <- glm(presence ~ dist_roads + dem + ruggedness + habitat , data = elk_data, family = "binomial")
summary(fit)
plot(allEffects(fit))

res <- simulateResiduals(fit, plot = T)
plotResiduals(res, elk_data$dist_roads, quantreg = T)
plotResiduals(res, elk_data$NDVI, quantreg = T)
plotResiduals(res, elk_data$dem, quantreg = T)

library(mgcv)
fit <- gam(presence ~ dist_roads + s(dem) + s(ruggedness) + habitat , data = elk_data, family = "binomial")
plot(fit)

res <- simulateResiduals(fit, plot = T)
plotResiduals(res, elk_data$dem, quantreg = T)
summary(fit)

pairs(elk_data)

library(glmmTMB)
library(lme4)

library(DHARMa)

m1 = glm(count ~ spp + mined, family = poisson, data = Salamanders)
summary(m1)
plot(allEffects(m1))

res <- simulateResiduals(m1, plot = T)
testDispersion(res)

m2 = glmer(count ~ spp + mined + (1|site), family = poisson, data = Salamanders)
summary(m2)
res <- simulateResiduals(m2, plot = T)
testDispersion(res)

m3 = glmmTMB(count ~ spp + mined + (1|site), family = nbinom2, data = Salamanders)
res <- simulateResiduals(m3, plot = T)
testDispersion(res)
summary(m3)



