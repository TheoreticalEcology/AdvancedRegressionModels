# Question: do the elks react positively or negatively to roads? CAUSAL!

fit<-glm(presence ~ scale(dist_roads) * scale(dem) + NDVI + ruggedness + habitat , data = elk_data, family = "binomial")
summary(fit)
plot(allEffects(fit))

res <- simulateResiduals(fit)
plot(res, quantreg = T)
plot(res, quantreg = T, form = elk_data$dist_roads)
plot(res, quantreg = T, form = elk_data$NDVI)
plot(res, quantreg = T, form = elk_data$dem, rank = F)
plot(res, quantreg = T, form = elk_data$ruggedness)
plot(res, quantreg = T, form = elk_data$habitat)

library(mgcv)
fit<-gam(presence ~ dist_roads + s(dem) + s(NDVI) + s(ruggedness) + habitat , data = elk_data, family = "binomial")
summary(fit)

plot(fit)

res <- simulateResiduals(fit)
plot(res, quantreg = T)
plot(res, quantreg = T, form = elk_data$dist_roads)
plot(res, quantreg = T, form = elk_data$NDVI)
plot(res, quantreg = T, form = elk_data$dem, rank = F)
plot(res, quantreg = T, form = elk_data$ruggedness)
plot(res, quantreg = T, form = elk_data$habitat)


#### Dispersion problems in GLMs #### 

hist(rnorm(5000, mean = 3, sd = 10))

barplot(table(rpois(5000, lambda = 10)))

fit <- glm(feeding ~ attractiveness, data = birdfeeding, family = "poisson")
summary(fit)


m0 = glm(count ~ cover, family = "poisson", data = Salamanders)
res <- simulateResiduals(m0, plot = T)

library(glmmTMB)
str(Salamanders)

library(lme4)

m1 = glmer(count ~ cover + mined + spp + (1|site), family = "poisson", data = Salamanders)

res <- simulateResiduals(m1, plot = T)
plotResiduals(res, form = Salamanders$cover)
plotResiduals(res, form = Salamanders$spp)
plotResiduals(res, form = Salamanders$mined)

# for Poisson, the main family to switch to with OD is neg.binomial 

library(glmmTMB)
m2 = glmmTMB(count ~ cover + mined + spp + (1|site), family = "nbinom1", data = Salamanders)
summary(m2)

res <- simulateResiduals(m2, plot = T)

# OD can also occur in the binomial, but only with k/n data

hist(Salamanders$count, breaks = 50)

testZeroInflation(m2)

m3 = glmmTMB(count ~ cover + mined + spp + (1|site), 
             family = "nbinom1", 
             data = Salamanders, 
             ziformula = ~ 1)
summary(m3)

AIC(m2)
AIC(m3)


library(glmmTMB)

https://theoreticalecology.github.io/AdvancedRegressionModels/6C-CaseStudies.html#owls


m1 = glm(SiblingNegotiation ~ FoodTreatment*SexParent 
         + offset(log(BroodSize)),
         data = Owls , family = poisson)
res = simulateResiduals(m1)
plot(res)
testDispersion(res)
summary(m1)

# improve them model!
# Consider RE, neg.binomial. zero-inflation

m1 = glmer(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest)
         + offset(log(BroodSize)),
         data = Owls , family = poisson)
res = simulateResiduals(m1)
plot(res)
testDispersion(res)

m1 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest)
           + offset(log(BroodSize)),
           data = Owls , family = nbinom1)
res = simulateResiduals(m1)
plot(res)
testDispersion(res)
testZeroInflation(res)
summary(m1)

m2 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest)
             + offset(log(BroodSize)),
             data = Owls , 
             family = nbinom1,
             ziformula = ~ FoodTreatment + SexParent)
res = simulateResiduals(m2)
plot(res)
testDispersion(res)
testZeroInflation(res)
summary(m2)

AIC(m1)
AIC(m2)

# Dispersion 

set.seed(125)

data = data.frame(treatment = factor(rep(c("A", "B", "C"), each = 15)))
data$observation = c(7, 2 ,4)[as.numeric(data$treatment)] +
  rnorm( length(data$treatment), sd = as.numeric(data$treatment)^2 )
boxplot(observation ~ treatment, data = data)

fit = lm(observation ~ treatment, data = data)
summary(fit)
summary(aov(fit))
AIC(fit)

par(mfrow = c(2,2))
plot(fit)

# violates iid normal assumption -> heteroskedasticity, means the variance changes with predictors or the mean prediction 

# regressions that model also the dispersion

library(nlme)
fit = gls(observation ~ treatment, data = data,
          weights = varIdent(form = ~ 1 | treatment))
summary(fit)
AIC(fit)

anova(fit)

fit = glmmTMB(observation ~ treatment, data = data,
              dispformula = ~ treatment)
summary(fit)

plot(Ozone ~ Solar.R , data = airquality)
m1 = lm(Ozone ~ sqrt(Solar.R) +  Solar.R + I(Solar.R^2), data = airquality)
par(mfrow = c(2, 2))
plot(m1)
res <- simulateResiduals(m1, plot = T)


plot(Ozone ~ Solar.R , data = airquality)
m1 = lm(log(Ozone) ~ Solar.R + I(Solar.R^2) , data = airquality)
par(mfrow = c(2, 2))
plot(m1)
plot(allEffects(m1, partial.residuals = T))
res <- simulateResiduals(m1, plot = T)

library(nlme)
m1 = gls(Ozone ~ Solar.R , data = airquality[complete.cases(airquality),],
         weights = varPower(form = ~ Solar.R))
summary(m1)
plot(m1)

library(glmmTMB)

m1 = glmmTMB(Ozone ~ Solar.R , data = airquality,
          dispformula = ~ Solar.R)
res <- simulateResiduals(m1, plot = T)

summary(m1)




m3 = glmmTMB(count ~ spp + mined, family = nbinom1 ,  data = Salamanders,
             dispformula = ~ spp + mined)
summary(m3)

library(DHARMa)
res = simulateResiduals(m3, plot = T)

par(mfrow = c(1, 2))
plotResiduals(res, Salamanders$spp)
plotResiduals(res, Salamanders$mined)

