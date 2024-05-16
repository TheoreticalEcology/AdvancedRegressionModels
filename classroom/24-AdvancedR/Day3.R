set.seed(123)
x1 = runif(100)
x2 = 0.8 * x1 + 0.2 *runif(100)
y = 1 * x1 + 1 * x2 + rnorm(100)

m1 = lm(y ~ x1 + x2)
summary(m1)

m2 = MASS::stepAIC(m1)
summary(m2)

# For this reason, don't apply model selection on your confounders! 

set.seed(123)
x = runif(100)
y = 0.25 * x + rnorm(100, sd = 0.3)
xNoise = matrix(runif(8000), ncol = 80)
dat = data.frame(y=y,x=x, xNoise)
fullModel = lm(y~., data = dat)
summary(fullModel)

simpleModel = lm(y~x, data = dat)
summary(simpleModel)

m2 = MASS::stepAIC(fullModel)
library(MuMIn)

summary(m2)

# AIC selection or step-wise or global model selection in general has a multiplicity (multiple testing) problem, inflated type I error!

# If you do this, you would have to correct for this, post-selection inference

fit <- lm(lebensz_org ~ (einkommenj1 + 
            sex + 
            alter + 
            gesund_org + 
            syear + 
            anz_kind + 
            anz_pers +
            bildung +
            erwerb +
            branche)^2, data = soep)  
summary(fit)

m2 = MASS::stepAIC(fullModel)


library(EcoData)
#str(birdfeeding)
plot(feeding ~ attractiveness, data = birdfeeding)

fit <- glm(feeding ~ attractiveness, data = birdfeeding, family = poisson)
summary(fit)
res <- simulateResiduals(fit, plot = T)

exp(1.47459)

predict(fit, type = c("link")) # linear predictor
predict(fit, type = c("response")) # linear predictor
exp(predict(fit, type = c("link")))

plot(allEffects(fit))
plot(allEffects(fit, partial.residuals = T))


estimate = exp(1.47 + 0.14 * attractiveness)


library(EcoData)
#str(titanic)
#mosaicplot( ~ survived + sex + pclass, data = titanic)
titanic$pclass = as.factor(titanic$pclass)

fit = lm(survived ~ sex * age, data = titanic)
summary(fit)
par(mfrow = c(2,2))
plot(fit)

fit = glm(survived ~ sex * scale(age) + pclass, data = titanic, family = binomial)
summary(fit)

curve(plogis, -5,5)
plogis(1.1662)
plogis(1.1662-2.5369)

plot(allEffects(fit))

# These plots can be drawn, but are only valid for glm with non-gaussian
par(mfrow = c(2,2))
plot(fit)

library(DHARMa)
res <- simulateResiduals(fit, plot = T)

fit = glm(survived ~ sex + pclass + age, data = titanic, family = binomial)
summary(fit)
plot(allEffects(fit))

res <- simulateResiduals(fit, plot = T)
x = model.frame(fit)
plotResiduals(res, x$pclass)
plotResiduals(res, x$age)


str(elk_data)

# Task: perform an analysis of the data to understand if roads have a causal effect on elk presence. Correct for appropriately for confounders! Check the residuals of your model to make sure it's properly specified!

fit = glm(presence ~ dist_roads, data = elk_data, family = binomial)

1) Which other variables to include, can also consider interactions
2) Check residuals, could have nonlinearities?
3) Interpret your model, are roads bad for elks?
4) Present predict of the model as you would present them in a paper or for the public 

fit = glm(presence ~ dist_roads , data = elk_data, family = binomial)
summary(fit)
plot(allEffects(fit))


pairs(elk_data)

fit = glm(presence ~ dist_roads * habitat + NDVI + ruggedness * dem , data = elk_data, family = binomial)
summary(fit)
plot(allEffects(fit))

res <- simulateResiduals(fit)
plot(res, quantreg = T)
plotResiduals(res, elk_data$dist_roads, quantreg = T, rank = F)
plotResiduals(res, elk_data$habitat, quantreg = T)
plotResiduals(res, elk_data$NDVI, quantreg = T)
plotResiduals(res, elk_data$ruggedness, quantreg = T)
plotResiduals(res, elk_data$dem, quantreg = T)


fit = gam(presence ~ dist_roads * habitat + NDVI + ruggedness + dem , data = elk_data, family = binomial)
summary(fit)

fit = gam(presence ~ dist_roads * habitat + s(NDVI) + s(ruggedness) + s(dem) , data = elk_data, family = binomial)
summary(fit)
plot(fit)

res <- simulateResiduals(fit)
plot(res, quantreg = T)
plotResiduals(res, elk_data$dist_roads, quantreg = T, rank = F)
plotResiduals(res, elk_data$habitat, quantreg = T)
plotResiduals(res, elk_data$NDVI, quantreg = T)
plotResiduals(res, elk_data$ruggedness, quantreg = T)
plotResiduals(res, elk_data$dem, quantreg = T)

# Collinear variables 

# Check for correlation, 0.7 -> Dormann, Carsten F., et al. "Collinearity: a review of methods to deal with it and a simulation study evaluating their performance." Ecography 36.1 (2013): 27-46.

# 0.7 for predictive performance in species distribution models 

set.seed(123)
x1 = runif(100000)
x2 = 0.95 * x1 + 0.05 *runif(100000)
y = 1 * x1 + 1 * x2 + rnorm(100000)

cor(x1, x2)

m1 = lm(y ~ x1 + x2)
summary(m1)

m1 = lm(y ~ x1)
summary(m1)



m1 = lm(y ~ humans + protected + forest)

1) Option 1: show one or all, and say there is an effect but you cant say which is the causal variable 

m1 = lm(y ~ humans)
m1 = lm(y ~ protected)
m1 = lm(y ~ forest)

2) Option 2: PCA regression 

m1 = lm(y ~ PC1)

PCA = prcomp(elk_data[,1:4], scale. = FALSE)
biplot(PCA)

fit = glm(presence ~ PCA$x[,1], data = elk_data, family = binomial)
summary(fit)


# Interaction categorical spline 

fit = gam(presence ~ s(dist_roads, by = habitat), data = elk_data, family = binomial)
summary(fit)
par(mfrow = c(1,2))
plot(fit)

# Spline interaction between 2 contiuous variables

fit = gam(presence ~ te(dist_roads, NDVI), data = elk_data, family = binomial)
summary(fit)
par(mfrow = c(1,2))
plot(fit)

library(mgcViz)

library(glmmTMB)
library(lme4)

m1 = glm(count ~ spp + mined, family = poisson, data = Salamanders)
summary(m1)
plot(allEffects(m1, partial.residuals = T))

res <- simulateResiduals(m1, plot = T)

# Overdispersion Problem 

hist(rpois(1000,lambda = 10), xlim = c(0,30))

# Change distribution -> negativeBinomial 

m1 = glm(count ~ spp + mined, family = poisson, data = Salamanders)
summary(m1)

table(Salamanders$site)

library(lme4)

m1 = glmer(count ~ spp * mined + spp *cover + (1|site), family = poisson, data = Salamanders)
summary(m1)

res <- simulateResiduals(m1, plot = T)
testDispersion(res)
testZeroInflation(res)

library(glmmTMB)

m1 = glmmTMB(count ~ mined * spp + cover * spp + (1|site), family = nbinom1, data = Salamanders)
summary(m1)

res <- simulateResiduals(m1, plot = T)
testZeroInflation(res)

m1 = glmmTMB(count ~ mined + cover + spp + (1|site), 
             family = nbinom1,
             ziformula = ~  cover,
             data = Salamanders)
summary(m1)

