data = EcoData::snails
data$sTemp_Water = scale(data$Temp_Water)
data$spH = scale(data$pH)
data$swater_speed_ms = scale(data$water_speed_ms)
data$swater_depth = scale(data$water_depth)
data$sCond = scale(data$Cond)
data$swmo_prec = scale(data$wmo_prec)
data$syear = scale(data$year)
data$sLat = scale(data$Latitude)
data$sLon = scale(data$Longitude)
data$sTemp_Air = scale(data$Temp_Air)
# Remove NAs
rows = rownames(model.matrix(~sTemp_Water + spH + sLat + sLon + sCond + seas_wmo+ swmo_prec + swater_speed_ms + swater_depth +sTemp_Air+ syear + duration + locality + site_irn + coll_date, data = data))

data = data[rows, ]

model1 = glmer(bt_pres~ sTemp_Water + spH + sLat + sLon + sCond + seas_wmo + swmo_prec + (1|locality/site_irn) , data = data,  family = binomial)
summary(model1)

plot(allEffects(model1))

res <- simulateResiduals(model1)
plot(res)
plotResiduals(res, data$sLon, quantreg = T)

# nested RE 
(1|locality/site_irn)

Locality       site

Regensburg    1
Regensburg    2
Regensburg    3
Munich        1
Munich        2
Munich        3
Augsburg      1
Augsburg      2
Augsburg      3


(1|locality) + (1|site) # doesnt work if data is recorded like this

(1|locality/site_irn) basically ensures that each site is taken as a unique site by internally transforming the site to

Regensburg1
Regensburg2
... 

# in our data, the sites already have unique names 
unique(paste(data$locality, data$site_irn))
unique(data$site_irn)

# how to decide on home many REs to add? Should we add a RE on year?

model1 = glmer(bt_pres~ sTemp_Water + spH + sLat + sLon + sCond + seas_wmo + swmo_prec + (1|locality/site_irn) , data = data,  family = binomial)

model2 = glmer(bt_pres~ sTemp_Water + spH + sLat + sLon + sCond + seas_wmo + swmo_prec +  (1|locality/site_irn) + (1|syear) , data = data,  family = binomial)


# AIC comparison doesn't work 
AIC(model1) 
AIC(model2)

# let's calculate how many parameters AIC function is calcuating for the RE on year 

(AIC(model1) + 2 * logLik(model1))/2
(AIC(model2) + 2 * logLik(model2))/2

# conclusion: AIC is calculting in df per RE, super anti-conservative, will usually always tell you to add the RE! 
# Basically, AIC is not correctly implemented for mixed models in R

# how to select RE?

# 1 option: just put everything in that seems sensible to you!

# 2nd option: use model selection tools that account for df in mixed models

lmerTest::ranova()
DHARMa::simulateLRT(model1, model2, n = 20)


set.seed(123)
dat <- createData(sampleSize = 200, randomEffectVariance = 0.1)

# define Null and alternative model (should be nested)
m1 = glmer(observedResponse ~ Environment1 + (1|group), data = dat, family = "poisson")
m0 = glm(observedResponse ~ Environment1 , data = dat, family = "poisson")

## Not run: 
# run LRT - n should be increased to at least 250 for a real study
out = simulateLRT(m0, m1, n = 100)
out


k/n = 3 out of 5 infected 


model2 = glm(cbind(BT_pos_tot, BT_tot - BT_pos_tot) ~ sTemp_Water 
             data = data[data$BT_tot > 0, ],  family = binomial)

summary(model2)

# alternative syntax, I don't recommend it!
model2b = glm(BT_pos_tot / BT_tot  ~ sTemp_Water ,
             data = data[data$BT_tot > 0, ],  family = binomial,
             weight = BT_tot)

summary(model2b)


hist(rbinom(1000,40,0.5), xlim = c(0,40))
hist(rbinom(1000,40,0.95), xlim = c(0,40))


res <- simulateResiduals(model2)
plot(res)
testDispersion(res)

# Distributions with variable dispersion 

Poisson -> negBinomial
Binomial -> betabinomial

# Model variance 


set.seed(125)

data = data.frame(treatment = factor(rep(c("A", "B", "C"), each = 15)))
data$observation = c(7, 2 ,4)[as.numeric(data$treatment)] +
  rnorm( length(data$treatment), sd = as.numeric(data$treatment)^2 )
boxplot(observation ~ treatment, data = data)

fit = lm(observation ~ treatment, data = data)
summary(fit)
summary(aov(fit))
par(mfrow = c(2,2))
plot(fit)
plot(allEffects(fit, partial.residuals = T))
res <- simulateResiduals(fit, plot = T)
car::leveneTest(fit)

library(nlme)
fit = gls(observation ~ treatment, data = data, 
          weights = varIdent(form = ~ 1 | treatment))
summary(fit)


library(glmmTMB)
fit = glmmTMB(observation ~ treatment, data = data, 
              dispformula = ~ treatment)
summary(fit)


m1 = lm(Ozone ~ Solar.R, data = airquality)
par(mfrow = c(2, 2))
plot(m1)
plot(allEffects(m1, partial.residuals = T))
res <- simulateResiduals(m1, plot = T)


m1 = gam(Ozone ~ s(Solar.R), data = airquality)
res <- simulateResiduals(m1, plot = T)

m2 = gls(Ozone ~ Solar.R, 
         weights = varPower(0.2, form = ~ Solar.R),
         data = airquality[complete.cases(airquality),])
summary(m2)

?nlme

plot(m2)
plot(allEffects(m2, partial.residuals = T))
res <- simulateResiduals(m2, plot = T)

m3 = glmmTMB(Ozone ~ Solar.R, 
             dispformula = ~ Solar.R,
             data = airquality)
summary(m3)
res <- simulateResiduals(m3, plot = T)


m3 = glmmTMB(count ~ spp + mined + (1|site), 
             family = nbinom1,
             dispformula = ~ spp + mined ,  
             data = Salamanders)
summary(m3)


# solution exercise

library(DHARMa)
library(mgcv)
?hurricanes
str(hurricanes)

plot(hurricanes$MasFem, hurricanes$NDAM, cex = 0.5, pch = 5)
points(hurricanes$MasFem, hurricanes$NDAM, cex = hurricanes$alldeaths/20,
       pch = 4, col= "red")


originalModelGAM = gam(alldeaths ~ scale(MasFem) * (scale(Minpressure_Updated_2014) + scale(NDAM)),
                       data = hurricanes, family = nb, na.action = "na.fail")
summary(originalModelGAM)


max(hurricanes$NDAM)

hurricanes$maxCenteredNDAM = hurricanes$NDAM - max(hurricanes$NDAM)

originalModelGAM = gam(alldeaths ~ MasFem * hurricanes$maxCenteredNDAM,
                       data = hurricanes, family = nb)
summary(originalModelGAM)



m1 = glmmTMB(alldeaths ~ MasFem*
               (Minpressure_Updated_2014 + scale(NDAM)),
             data = hurricanes, family = nbinom2)
summary(m1)

m2 = glmmTMB(alldeaths ~ scale(MasFem) *
               (scale(Minpressure_Updated_2014) + scale(NDAM)),
             data = hurricanes, family = nbinom2)
summary(m2)


res <- simulateResiduals(m2)
plot(res)
plotResiduals(res, hurricanes$MasFem)
plotResiduals(res, hurricanes$Minpressure_Updated_2014)
plotResiduals(res, hurricanes$NDAM)

m2 = glmmTMB(alldeaths ~ scale(MasFem) *
               (scale(Minpressure_Updated_2014) + I(NDAM^0.2)),
             data = hurricanes, family = nbinom2)
summary(m2)

res <- simulateResiduals(m2)
plot(res)
plotResiduals(res, hurricanes$MasFem)
plotResiduals(res, hurricanes$Minpressure_Updated_2014)
plotResiduals(res, hurricanes$NDAM)

summary(m2)

plotResiduals(res, hurricanes$MasFem, quantreg = F)

x = runif(100)
y = runif(100)
plot(x,y)


m3 = glmmTMB(alldeaths ~ scale(MasFem) + 
               scale(Minpressure_Updated_2014) + 
               scale(NDAM) + 
               factor(Category) +
               scale(Year) + 
               (1|Year),
             data = hurricanes, family = nbinom2)
summary(m3)


pairs(data.frame(hurricanes)[,c(1,3,5,7,8,9)])



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
plot(obs, type = "l")


fit = lm(obs~1)
summary(fit)

#par(mfrow = c(2,2))
#plot(fit)

#plot(residuals(fit), type = "l")

acf(residuals(fit))
pacf(residuals(fit))

testTemporalAutocorrelation(fit, time = 1:1000)

# Conclusion: we have a AR1 autocorrelation structure of strength roughly 0.9 in the residuals 


fit = lm(obs[seq(0,1000, by = 50)]~1)
summary(fit)

acf(residuals(fit))

library(nlme)
fitGLS = gls(obs~1, 
            corr = corAR1(form = ~ 1))
summary(fitGLS)

acf(residuals(fitGLS))

library(glmmTMB)
time <- factor(1:1000) # time variable
group = factor(rep(1,1000)) # group (for multiple time series)

fitGLMMTMB = glmmTMB(obs ~ ar1(time + 0 | group))
summary(fitGLMMTMB)


time = 1:1000/100
y = time +2*(sin(time/0.4)) + rnorm(1000)
data = 
  data.frame(y = y, time = time, timeF = as.factor(1:1000), group = as.factor(1))
plot(y ~time)

fit1 = glmmTMB(y~time, data = data)
res = simulateResiduals(fit1, plot = TRUE)

testTemporalAutocorrelation(res, time = data$time)




# Exercise


library(DHARMa)

fit = glmmTMB(alldeaths ~ scale(MasFem) *
                (scale(Minpressure_Updated_2014) + scale(NDAM)),
              data = hurricanes, family = nbinom2)

# Residual checks with DHARMa.
res = simulateResiduals(fit)

# Checking for temporal autocorrelation
res2 = recalculateResiduals(res, group = hurricanes$Year)
plot(res2)
testTemporalAutocorrelation(res2, time = unique(hurricanes$Year))


hurricanes$yearF <- factor(hurricanes$Year)
hurricanes$group = factor(rep(1,nrow(hurricanes)))

fit = glmmTMB(alldeaths ~ scale(MasFem) 
              + ar1(yearF + 0 | group),
              data = hurricanes, family = nbinom2)

summary(fit)

# removing the model complexity solves the convergence problem but not what we want 
# better solution: go Bayesian and use the brms package 






