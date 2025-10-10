library(DHARMa)
library(mgcv)
?hurricanes
str(hurricanes)

summary(hurricanes)

plot(hurricanes$MasFem, hurricanes$NDAM, cex = 0.5, pch = 5)
points(hurricanes$MasFem, hurricanes$NDAM, cex = hurricanes$alldeaths/20,
       pch = 4, col= "red")


plot(hurricanes$Year, hurricanes$MasFem, cex = 0.5, pch = 5)
points(hurricanes$Year, hurricanes$MasFem, cex = hurricanes$alldeaths/20,
       pch = 4, col= "red")



originalModelGAM = gam(alldeaths ~ MasFem * (Minpressure_Updated_2014 + NDAM),
                       data = hurricanes, family = nb, na.action = "na.fail")
summary(originalModelGAM)


library(DHARMa)
library(glmmTMB)
library(effects)


m1 = glmmTMB(alldeaths ~ ZMasFem * (ZMinPressure_A + ZNDAM),
             data = hurricanes, family = nbinom2)
summary(m1)
plot(allEffects(m1))


# moving the zero to max values
hurricanes$pressureMaxZero = hurricanes$Minpressure_Updated_2014 - max(hurricanes$Minpressure_Updated_2014)
hurricanes$NDAMMaxZero = hurricanes$NDAM - max(hurricanes$NDAM)
hist(hurricanes$NDAMMaxZero)

m1 = glmmTMB(alldeaths ~ ZMasFem * (pressureMaxZero + NDAMMaxZero),
             data = hurricanes, family = nbinom2)
summary(m1)
plot(allEffects(m1))



m1 = glmmTMB(alldeaths ~ ZMasFem * (ZMinPressure_A + ZNDAM),
             data = hurricanes, family = nbinom2)

res = simulateResiduals(m1)
plot(res)
plotResiduals(res, form = hurricanes$MasFem)
plotResiduals(res, form = hurricanes$ZMinPressure_A)
plotResiduals(res, form = hurricanes$ZNDAM)
plotResiduals(res, form = hurricanes$ZNDAM, rank = F)



m1 = glmmTMB(alldeaths ~ ZMasFem * (ZMinPressure_A + scale(NDAM^0.2)),
             data = hurricanes, family = nbinom2)

res = simulateResiduals(m1)
plot(res)
plotResiduals(res, form = hurricanes$MasFem)
plotResiduals(res, form = hurricanes$ZMinPressure_A)
plotResiduals(res, form = hurricanes$ZNDAM)
plotResiduals(res, form = hurricanes$ZNDAM, rank = F)

summary(m1)
plot(allEffects(m1))


m1 = glmmTMB(alldeaths ~ ZMasFem * scale(NDAM^0.2) + Year + (1|Year) ,
             data = hurricanes, family = nbinom2)
summary(m1)
plot(allEffects(m1))

res = simulateResiduals(m1)
plot(res)
plotResiduals(res, form = hurricanes$MasFem)
plotResiduals(res, form = hurricanes$ZMinPressure_A)
plotResiduals(res, form = hurricanes$ZNDAM)


# R2 for GLMs

library(EcoData)
m1 = glm(survived ~ sex*age, family = "binomial", data = titanic)
summary(m1)
plot(allEffects(m1))

# sequential ANOVA for GLMs 
anova(m1, test = "Chisq")
car::Anova(m1)

# R2 for GLMs -> complicated -> pseudo-R2

summary(m1)

# deviance = - 2 logL

logLik(m1)
m0 = glm(survived ~ 1, family = "binomial", data = titanic)
logLik(m0)

# McFadden PseudoR1 
1 - logLik(m1) / logLik(m0)

summary(m1)
# residual deviance = 1083.4
anova(m1, test = "Chisq")

shares = c(312.612 , 0.669, 17.903, 1083.4)
shares / sum(shares)

library(faraway)
library(lme4)
detach("package:lmerTest", unload=TRUE)
data(irrigation)
fit <- lmer(yield ~ irrigation + variety + (1|field), data = irrigation)
summary(fit)
anova(fit)

library(lmerTest)
fit <- lmer(yield ~ irrigation + variety + (1|field), data = irrigation)
anova(fit)
summary(fit)

library(MuMIn)
r.squaredGLMM(fit) 

m1 = glm(survived ~ sex*age, family = "binomial", data = titanic)
r.squaredGLMM(m1) 



m1 = glm(survived ~ sex*age, family = "binomial", data = titanic)

res = simulateResiduals(m1)
plot(res)
plotResiduals(res, form = model.frame(m1)$sex)
plotResiduals(res, form = model.frame(m1)$age)

m1 = glm(survived ~ sex, family = "binomial", data = titanic)
ind = as.numeric(rownames(model.frame(m1)))
plotResiduals(m1, form = titanic$age[ind])

# 

set.seed(125)
data = data.frame(treatment = factor(rep(c("A", "B", "C"), each = 15)))
data$observation = c(7, 2 ,4)[as.numeric(data$treatment)] +
  rnorm( length(data$treatment), sd = as.numeric(data$treatment)^2 )
boxplot(observation ~ treatment, data = data)


fit <- lm(observation ~ treatment, data = data)
summary(fit)
par(mfrow = c(2,2))
plot(fit)
res = simulateResiduals(fit, plot = T)

# lm = iid normal = identical independent normal 
# issue - identical - distributional variance changes between groups 

summary(aov(fit))

boxplot(log(observation+1) ~ treatment, data = data)

# switch to a model with heterogenous variance 

library(nlme)

fit <- gls(observation ~ treatment, data = data,
           weight = varIdent(form = ~ 1 | treatment))
summary(fit)
anova(fit)

fit <- glmmTMB(observation ~ treatment, data = data,
               dispformula = ~ treatment)
summary(fit)


plot(Ozone ~ Solar.R, data = airquality)
m1 = lm(Ozone ~ Solar.R, data = airquality)
abline(m1)
par(mfrow = c(2, 2))
plot(m1)

dat = airquality[complete.cases(airquality), ]
dat = model.frame(m1)
dat = model.frame(Ozone ~ Solar.R, data = airquality)

#options("na.action")
#options(na.action = na.omit)

fit <- gls(Ozone ~ Solar.R, data = dat,
           weight = varPower(form = ~ Solar.R))

summary(fit) # var = solarR^53
plot(fit)

plot(Ozone ~ Solar.R, data = airquality)
abline(fit)


fit <- glmmTMB(Ozone^0.2 ~ Solar.R , data = dat,
           dispformula = ~ Solar.R)
summary(fit)

plot(allEffects(fit, partial.residuals = T))
res <- simulateResiduals(fit, plot = T)


m3 = glmmTMB(count ~ spp + mined + (1|site), family = nbinom1,
             dispformula = ~ spp + mined ,  data = Salamanders)
summary(m3)


# Robust regression

set.seed(123)

n = 100
concentration = runif(n, -1, 1)
growth = 2 * concentration + rnorm(n, sd = 0.5) +
  rbinom(n, 1, 0.05) * rnorm(n, mean = 6*concentration, sd = 10)
plot(growth ~ concentration)

fit = lm(growth ~ concentration)
par(mfrow = c(2, 2))
plot(fit)

# Robust regression 

library(MASS)

fit = rlm(growth ~ concentration) 
summary(fit)

library(qgam)
dat = data.frame(growth = growth, concentration = concentration)
fit = qgam(growth ~ concentration, data = dat, qu = 0.5) 
summary(fit)


# Temporal correlation structure 


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
plot(obs, type ="l")


fit = lm(obs~1)
summary(fit)

par(mfrow = c(2,2))
plot(fit)

# lm = iid normal = identical normal, BUT not indepedent 

plot(residuals(fit)[1:50])

acf(residuals(fit))
pacf(residuals(fit))

testTemporalAutocorrelation(fit, time = 1:1000)

fit = lm(obs[seq(1, 1000, by = 25)]~1)
summary(fit)
acf(residuals(fit))

library(nlme)

fit <- gls(obs~1, correlation = corAR1(form = ~ 1))
summary(fit)

acf(residuals(fit))
acf(residuals(fit, type = "normalized"))


library(glmmTMB)

time <- factor(1:1000) # time variable
group = factor(rep(1,1000)) # group (for multiple time series)

fitGLMMTMB = glmmTMB(obs ~ ar1(time + 0 | group))
summary(fitGLMMTMB)

res <- simulateResiduals(fitGLMMTMB)
testTemporalAutocorrelation(res, time = 1:1000)

res <- simulateResiduals(fitGLMMTMB, rotation = "estimated")
testTemporalAutocorrelation(res, time = 1:1000)


library(DHARMa)

fit = glmmTMB(alldeaths ~ scale(MasFem) *
                (scale(Minpressure_Updated_2014) + scale(NDAM)),
              data = hurricanes, family = nbinom2)

# Residual checks with DHARMa.
res = simulateResiduals(fit)

# Checking for temporal autocorrelation
res2 = recalculateResiduals(res, group = hurricanes$Year)
testTemporalAutocorrelation(res2, time = unique(hurricanes$Year))


hurricanes$yearF <- factor(hurricanes$Year)
hurricanes$group = factor(rep(1,nrow(hurricanes)))

fit = glmmTMB(alldeaths ~ scale(MasFem) *
                (scale(Minpressure_Updated_2014) + scale(NDAM))
              + ar1(yearF + 0 | group),
              data = hurricanes, family = nbinom2)

summary(fit)

fit = glmmTMB(alldeaths ~ scale(MasFem) 
              + ar1(yearF + 0 | group),
              data = hurricanes, family = nbinom2)

summary(fit)





