library(EcoData)

plot(feeding ~ attractiveness, data = birdfeeding)

fit <- lm(feeding ~ attractiveness, 
          data = birdfeeding)
abline(fit)
summary(fit)

predict(fit)

fit <- glm(feeding ~ attractiveness, 
           data = birdfeeding, 
           family = "poisson")
summary(fit)

exp(1.47459 + 2 * 0.14794)

attractiveness = seq(1,5,length.out = 50)

x = predict(fit, type = "response", newdata = data.frame(attractiveness = attractiveness))

lines(attractiveness, x, col = "red")
plot(allEffects(fit))



library(EcoData)
str(titanic)
titanic$pclass = as.factor(titanic$pclass)

fit <- lm(survived ~ age*sex, data = titanic)
summary(fit)
par(mfrow = c(2,2))
plot(fit)

fit <- glm(survived ~ age*sex, data = titanic, family = binomial)
summary(fit)

curve(plogis, -5, 5)

plogis(0.49 + 0.022516 * 20)
plogis(0.49 - 1.15 + (0.022516 -0.046276) *20)
plot(allEffects(fit))

par(mfrow = c(2,2))
plot(fit)

# residual checks for GLMs

plot(allEffects(fit, partial.residuals = T))

?residuals.glm

# raw residuals = distance between predictions and observations 
# not useful because most GLM distributions have non-constant variance 

# Therefore: two other residuals
# Pearson = raw residual / expected sd of the distribution 
# Deviance residuals = weigh residual by likelihood, effectively similar to Pearson 

summary(fit)

library(DHARMa)

res <- simulateResiduals(fit, plot = T)

plot(res)
i = as.numeric(rownames(model.frame(fit)))
plotResiduals(res, form = titanic$pclass[i])
plotResiduals(res, form = model.frame(fit)$age)


# Find out if elks ~ roads! 

plot(factor(presence) ~ dist_roads, data = elk_data)


fit <- glm(presence ~ dist_roads, data = elk_data, family = "binomial")
summary(fit)
plot(allEffects(fit))


fit <- glm(presence ~ dist_roads*habitat + ruggedness + dem , data = elk_data, family = "binomial")
summary(fit)
plot(allEffects(fit))

res = simulateResiduals(fit)
plot(res, quantreg = T)

plotResiduals(res, form = elk_data$dist_roads, quantreg = T)
plotResiduals(res, form = elk_data$ruggedness, quantreg = T)
plotResiduals(res, form = elk_data$dem, quantreg = T)

library(mgcv)
fit <- gam(presence ~ dist_roads*habitat + s(ruggedness) + s(dem) , data = elk_data, family = "binomial")

res = simulateResiduals(fit)
plot(res, quantreg = T)

plotResiduals(res, form = elk_data$dist_roads, quantreg = T)
plotResiduals(res, form = elk_data$ruggedness, quantreg = T)
plotResiduals(res, form = elk_data$dem, quantreg = T)

summary(fit)
plot(fit)

# to check if there is additional nonlinearity in dist_roads
fit <- gam(presence ~ s(dist_roads, by =habitat) + s(ruggedness) + s(dem) , data = elk_data, family = "binomial")
plot(fit)

library(glmmTMB)
m1 = glm(count ~ mined, family = poisson, data = Salamanders)
summary(m1)
plot(allEffects(m1))
exp(-1.2)

res = simulateResiduals(m1)
plot(res)

# for comparison linear model
# m1 = lm(count ~ mined, data = Salamanders)
# summary(m1)
# res = simulateResiduals(m1)
# plot(res)

testDispersion(res)

m1b = glm(count ~ mined + spp, family = poisson, data = Salamanders)
res = simulateResiduals(m1b)
plot(res)
testDispersion(res)

m1b = glmmTMB(count ~ mined + spp + (1|site), 
              family = poisson, 
              data = Salamanders)
res = simulateResiduals(m1b)
plot(res)
testDispersion(res)

m1b = glmmTMB(count ~ mined + spp + (1|site), 
              family = poisson, 
              data = Salamanders)
res = simulateResiduals(m1b)
plot(res)
testDispersion(res)

# no need here, but if there was still overdispersion change the distribution 

m1b = glmmTMB(count ~ mined + spp + (1|site), 
              family = nbinom1, 
              data = Salamanders)
res = simulateResiduals(m1b)
plot(res)
testDispersion(res)
summary(m1b)

testZeroInflation(m1b)


m1c = glmmTMB(count ~ mined + spp + (1|site), 
              family = nbinom1, 
              ziformula = ~ mined,
              data = Salamanders)
summary(m1c)


library(glmmTMB)

m1 = glm(SiblingNegotiation ~ FoodTreatment*SexParent + offset(log(BroodSize)),
         data = Owls , family = poisson)
res = simulateResiduals(m1)
plot(res)

# explanation for offset log size
# y = exp(intercept + food + log(broodsize) )
# y = exp(intercept + food) * broodsize

m2 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)),
         data = Owls , family = poisson)
res = simulateResiduals(m2)
plot(res)


m3 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)),
             data = Owls , family = nbinom1)
res = simulateResiduals(m3)
plot(res)

testDispersion(res)
testZeroInflation(res)

m4 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)),
             data = Owls , family = nbinom1,
             ziformula = ~ FoodTreatment*SexParent)
summary(m4)

res = simulateResiduals(m4)
plot(res)
testDispersion(res)




m4 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)),
             data = Owls , family = nbinom1,
             ziformula = ~ FoodTreatment*SexParent)
summary(m4)

# Model selection 

mFull <- lm(Ozone ~ Temp + Wind + Solar.R + as.factor(Month), data = airquality)
summary(mFull)
summary(aov(mFull))

dat = model.frame(mFull)

m1 = lm(Ozone ~ Temp, data = dat)
m2 <- lm(Ozone ~ Temp + Wind, data = dat)
m3 <- lm(Ozone ~ Temp + Wind + Solar.R, data = dat)

anova(m1, m2, m3)
anova(m1, m3)

# Principle: Likelihood-Ratio-Test: Improvement in Fit / Likelihood greater
# than what you would expect if the variable has no effect 
# Prerequisite: Models need to be nested, so more complex model neeeds to be 
# an extension of the simpler model 

# WARNING: FOR ANYTHING COMPLEX, NEED TO MAKE SURE THAT THE LRT IS PROPERLY 
# IMPLEMENTED 

library(lme4)
m4 <- lm(Ozone ~ Temp + Wind + Solar.R , data = airquality)
m4b <- lmer(Ozone ~ Temp + Wind + Solar.R + (1|Month), data = airquality)

summary(m4)
?lmerTest::ranova # RANDOM EFFECTS 
?anova.lmerModLmerTest # Fixed Effects


library(DHARMa)
# define Null and alternative model (should be nested)
dat <- createData(sampleSize = 200, randomEffectVariance = 1)
m1 = glmer(observedResponse ~ Environment1 + (1|group), data = dat, family = "poisson")
m0 = glm(observedResponse ~ Environment1 , data = dat, family = "poisson")
out = simulateLRT(m0, m1, n = 10)

# Information Criteria

m1 = lm(Ozone ~ Temp, data = airquality)
summary(m1)
logLik(m1)
AIC(m1)

m2 <- lm(Ozone ~ Temp + Wind, data = airquality)
summary(m2)
logLik(m2)
AIC(m2)

# AIC = - 2 LogLik + 2 * df
# WARNING: AIC in R is not properly calculating DF of random effects 

library(glmmTMB)
m2 <- glmmTMB(Ozone ~ Temp + Wind, data = airquality)
summary(m2)


set.seed(123)
x1 = runif(100)
x2 = 0.8 * x1 + 0.2 *runif(100)
y = x1 + x2 + rnorm(100)

m1 = lm(y ~ x1 + x2)
summary(m1)

m2 = MASS::stepAIC(m1)


set.seed(123)
x1 = runif(100)
x2 = 0.95 * x1 + 0.05 *runif(100)
x3 = runif(100)
y = x1 + x2 + x3 + rnorm(100)
m1 = lm(y ~ x1)
summary(m1)

m2 = lm(y ~ x1 + x2)
AIC(m2)

m3 = lm(y ~ x1 + x3)
AIC(m3)

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


