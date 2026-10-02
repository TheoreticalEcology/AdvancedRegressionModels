m1 = glm(survived ~ sex*age, data = titanic)
par(mfrow = c(2,2))
plot(m1)

m1 = glm(survived ~ sex*age, family = "binomial", data = titanic)
par(mfrow = c(2,2))
plot(m1) # DANGER - THESE PLOTS ONLY CHECK FOR NORMAL DISTRIBUTION!
class(m1)

hist(rpois(1000,100)) # GLM distributions difficult to check by looking at them
# because distributional shape changes with the mean 

library(DHARMa)
res = simulateResiduals(m1, plot=T)
plotResiduals(res, form = ~ age + sex + pclass)
plotResiduals(res, form = ~ age, rank = F)
plotResiduals(res, form = ~ sex)

https://theoreticalecology.github.io/AdvancedRegressionModels/4A-GLMs.html#elks

library(EcoData)
library(effects)
fit <- glm(presence ~ dist_roads, family = binomial, data = elk)
summary(fit)
plot(allEffects(fit))

fit <- gam(presence ~ dist_roads + s(dem) + s(ruggedness), family = binomial, data = elk)
summary(fit)
plot(allEffects(fit)) # does not work for gam unfortunately
plot(fit)

res = simulateResiduals(fit, plot = T)
plotResiduals(res, form = ~ dem)
plotResiduals(res, form = ~ dem, rank = F)
plotResiduals(res, form = ~ ruggedness)
plotResiduals(res, form = ~ dist_roads)

plot(dist_roads ~ habitat, data = elk)


library(glmmTMB)
Salamanders
library(lme4)

m1 = glmer(count ~ mined * spp + Wtemp * spp + (1|site), family = poisson, data = Salamanders)
res = simulateResiduals(m1, plot = T)
summary(m1)
testDispersion(m1)

library(glmmTMB)

m1 = glmmTMB(count ~ mined + Wtemp  + (1|site), family = nbinom1, data = Salamanders)
res = simulateResiduals(m1, plot = T)
summary(m1)
testDispersion(m1)
testZeroInflation(m1)

m2 = glmmTMB(count ~ mined + Wtemp + (1|site), 
             family = nbinom1, 
             ziformula = ~ mined + Wtemp ,
             data = Salamanders)
summary(m2)

AIC(m1)
AIC(m2) # model 1 is better, absolutely no indication of zero-inflation!

https://theoreticalecology.github.io/AdvancedRegressionModels/6C-CaseStudies.html#owls

library(glmmTMB)

m1 = glm(SiblingNegotiation ~ FoodTreatment*SexParent + offset(log(BroodSize)),
         data = Owls , family = poisson)
res = simulateResiduals(m1)
plot(res)

m2 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)),
         data = Owls , family = poisson)
res = simulateResiduals(m2)
plot(res)

m3 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)),
             data = Owls , family = nbinom1)
res = simulateResiduals(m3)
summary(m3)
plot(res)
testDispersion(res)
testZeroInflation(res)

m4 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)),
             ziformula = ~ FoodTreatment*SexParent,
             data = Owls , family = nbinom1)
summary(m4)

res = simulateResiduals(m4)
plot(res)
testDispersion(res)
testZeroInflation(res)
plotResiduals(res, form = ~ FoodTreatment)
plotResiduals(res, form = ~ SexParent)


# Dispersion models 

set.seed(125)
data = data.frame(treatment = factor(rep(c("A", "B", "C"), each = 15)))
data$observation = c(7, 2 ,4)[as.numeric(data$treatment)] +
  rnorm( length(data$treatment), sd = as.numeric(data$treatment)^2 )
boxplot(observation ~ treatment, data = data)


fit = lm(observation ~ treatment, data = data)
summary(fit)
summary(aov(fit))
res = simulateResiduals(fit, plot = T)
plotResiduals(res, form = ~ treatment)


library(nlme)
fit = gls(observation ~ treatment, data = data, 
          weights = varIdent(form = ~ 1 | treatment))
summary(fit)


# glmmTBM - works for LMs and GLMs with variable dispersion 
library(glmmTMB)
fit = glmmTMB(observation ~ treatment, data = data, 
              dispformula = ~ treatment)
summary(fit)

# example for the owl data
m4 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)),
             ziformula = ~ FoodTreatment*SexParent,
             dispformula = ~ FoodTreatment*SexParent,
             data = Owls , family = nbinom1)
summary(m4)



# Dealing with autocorrelation 

# simulate temporally autocorrelated data
AR1sim<-function(n, a){
  x = rep(NA, n)
  x[1] = 0
  for(i in 2:n){
    x[i] = a * x[i-1] + (1-a) * rnorm(1)
  }
  return(x)
}

set.seed(123)
obs = AR1sim(1000, 0.9)
plot(obs)

fit = lm(obs~1)
summary(fit)

acf(residuals(fit))
pacf(residuals(fit))
library(DHARMa)
res = simulateResiduals(fit)
testTemporalAutocorrelation(fit, 1:1000)

library(nlme)
fitGLS = gls(obs~1, corr = corAR1(0.771, form = ~ 1))
summary(fitGLS)

library(glmmTMB)

time <- factor(1:1000) # time variable
group = factor(rep(1,1000)) # group (for multiple time series)

fitGLMMTMB = glmmTMB(obs ~ ar1(time + 0 | group))
summary(fitGLMMTMB)


library(EcoData)
plot(thick ~ soil, data = thickness)

fit = lm(thick ~ soil, data = thickness)
summary(fit)

res = simulateResiduals(fit)
testSpatialAutocorrelation(res, x = thickness$north, y = thickness$east)

library(mgcv)
fit1 = gam(thick ~ soil + te(east, north) , data = thickness)
summary(fit1)
plot(fit1)

res = simulateResiduals(fit1)
testSpatialAutocorrelation(res, x = thickness$north, y = thickness$east)

fit2 = gls(thick ~ soil , 
           correlation = corExp(form =~ east + north) , data = thickness)
summary(fit2)


thickness$pos <- numFactor(thickness$east, 
                           thickness$north)
thickness$group <- factor(rep(1, nrow(thickness)))
fit3 = glmmTMB(thick ~ soil + exp(pos + 0 | group) , data = thickness)
summary(fit3)


