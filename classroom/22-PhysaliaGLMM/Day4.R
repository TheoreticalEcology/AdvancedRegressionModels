dat <- read.csv("~/Downloads/data4modelling.csv")

dat$landuse = dat$landuse.cluster
dat$tair = scale(dat$tair_anom_apr.median)
dat$precip = scale(dat$precip_anom_jul1.median)
dat$PC1 = scale(dat$PC1)

# I think is that climatic anomalies will cause defoliation depending on the land use (which determines tree density, and hence water competition) and the topographical features of the terrain expressed in PC1 (i.e. negative vs positive PC1 equals to lower vs higher potential terrain-driven soil moisture). For instance, I would expect that those places with higher occurrence of drier summers will show higher defoliation increase rate but only when tree density is high and PC1 is low.

# I did 3-way interactions because I want to test if defoliation trend is a consequence of the interaction between topography,  land use, and climate anomalies. For me these are not confounders but mediators.

library(lme4)

fit <- lm(defol.trend ~ tair*landuse*PC1 + precip*landuse*PC1, data = dat)

plot(allEffects(fit))
summary(fit)

res <- simulateResiduals(fit)
testSpatialAutocorrelation(fit, x = dat$lat, y = dat$long)



# I have 29 cytokines (immune response) measured over time in 14 individuals (4 controls, 10 infected with a virus). The first timestep is a baseline level, before infection. The sampling scheme is such that, after infection, a given cohort (2 controls, 5 infected) is sampled on even days and the other cohort on odd days. The cohort are sampled at 3 and 4 different timesteps after infection, respectively. This experiment was done with 2 different viruses, and 2 different animal species. I'll post those questions separately so you can answer in dedicated threads.


[ (concentration - baselineI) / baselineI ] ~ appliedInfection * baseline + (species*virus) 

+ time * treatment

(treatment | ID)


lm(concentration ~ treatment + offset(baseline))
lm(concentration - baseline ~ treament)


glm(cars/time ~ weather, family = "poisson")

exp(intercep + weather + offset(log(time)))
exp(intercep + weather) * exp(logtime))






fit = glm(survived ~ sex * age, data = titanic, 
          family = "binomial")

fitRE = lme4::glmer(survived ~ sex * age + (age|pclass), 
              data = titanic, family = "binomial")

library(glmmTMB)
fitRETMB = glmmTMB(survived ~ sex * age + (age|pclass), 
                data = titanic, family = "binomial")


summary(fit)
summary(fitRE)
summary(fitRETMB)

# GLM -> make sure it's a ANOVA based on chi2 tests

anova(fit)
anova(fitRE)
anova(fitRETMB)

car::Anova(fit)
car::Anova(fitRE)
car::Anova(fitRETMB)

predict(fit, se = T, type = "response")
predict(fitRE, se = T, type = "response")
predict(fitRETMB, se = T, type = "response")


library(glmmTMB)
library(lme4)
library(DHARMa)

m1 = glm(count ~ spp * mined, 
         family = poisson, data = Salamanders)
summary(m1)

res <- simulateResiduals(m1, plot = T)

plot(residuals(res, quantileFunction = qnorm) ~ predict(m1))
testDispersion(m1)


m2 = glmer(count ~ spp * mined + (1|site), 
         family = poisson, data = Salamanders,
         control = glmerControl(optimizer = "bobyqa"))

res <- simulateResiduals(m2, plot = T)
testDispersion(m2)

m3 = glmmTMB(count ~ spp * mined + (1|site), 
           family = nbinom1, 
           dispformula = ~ spp + mined,
           data = Salamanders)

summary(m3)

res <- simulateResiduals(m3, plot = T)
testDispersion(m3)

m4 = glmmTMB(count ~ spp * mined + (1|site), 
             family = nbinom1, 
             dispformula = ~ spp ,
             ziformula = ~ 1 ,
             data = Salamanders)
summary(m4)

?Owls

m1 = glm(SiblingNegotiation ~ FoodTreatment*SexParent + 
           offset(log(BroodSize)),
         data = Owls , family = poisson)
res = simulateResiduals(m1)
plot(res)




hist(Salamanders$count, breaks=50)



PlantGrowth
boxplot(weight~group, data = PlantGrowth)

fit<-lm(weight~group, data = PlantGrowth)
summary(fit)

fit<-glmmTMB(weight~group, 
             dispformula = ~ group,
             data = PlantGrowth)
summary(fit)


library(glmmTMB)

m0 = glm(SiblingNegotiation ~ FoodTreatment*SexParent + 
           offset(log(BroodSize)),
         data = Owls , family = poisson)
res = simulateResiduals(m0)
plot(res)
summary(m0)


m1 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + 
           offset(log(BroodSize)),
           data = Owls ,
           dispformula = ~ FoodTreatment,
           ziformula = ~ FoodTreatment*SexParent,
           family = nbinom1)

simulateResiduals(m1, plot = T)

summary(m1)

predict.glmmTMB


plot(allEffects(m1))


m0 = glmmTMB(SiblingNegotiation ~ FoodTreatment + 
               offset(log(BroodSize)),
             family = nbinom1, data = Owls)

m1 = glmmTMB(SiblingNegotiation ~ FoodTreatment + 
               offset(log(BroodSize)),
             dispformula = ~ FoodTreatment,
             family = nbinom1,
             data = Owls)

simulateLRT(m0,m1, n = 10)



m0 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)),
             data = Owls , family = nbinom1,
             ziformula = ~ FoodTreatment + SexParent)

m1 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)),
             data = Owls , family = nbinom1,
             ziformula = ~ FoodTreatment + SexParent,
             dispformula = ~ FoodTreatment + SexParent)

simulateLRT(m0,m1, n =)


resampledParameters = function(){
  newData = Owls
  newData$observedResponse = unlist(simulate(m0))
  mNew0 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)),
                  data = Owls , family = nbinom1,
                  ziformula = ~ FoodTreatment + SexParent, REML = FALSE)
  mNew1 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)),
                  data = Owls , family = nbinom1,
                  ziformula = ~ FoodTreatment + SexParent,
                  dispformula = ~ FoodTreatment + SexParent, REML = FALSE)
  return(logLik(mNew1) - logLik(mNew0))
}
nullDistribution = replicate(10, resampledParameters())
print(nullDistribution)




x = as.factor(rep(c("ctrl", "t1", "t2"), each = 20))
od = c(rep(0, 20), rnorm(20), rnorm(20,sd=3))
y = rpois(60, exp(as.numeric(x) + od))

plot(log(y) ~ x)

fit<- glmmTMB(y ~ x, family = nbinom1,
              dispformula = ~ x)

summary(fit)
res <- simulateResiduals(fit, plot = T)




res = simulateResiduals(m1)
plot(res)

simulateResiduals(m1)


summary(m1)

testDispersion(m1)
testZeroInflation(m1)




library(DHARMa)
?hurricanes

gam()

# this is the model fit by Jung et al., fith with glmmTMB
library(glmmTMB)

originalModelGAM = glmmTMB(alldeaths ~ scale(MasFem) *
                             (scale(Minpressure_Updated_2014) + scale(NDAM^0.2)) + (1|Year),
                           data = hurricanes, family = nbinom2,
                           control = glmmTMBControl()
                           )
summary(originalModelGAM)
res <- simulateResiduals(originalModelGAM, plot = T)

plotResiduals(res, hurricanes$NDAM)
plotResiduals(res, hurricanes$MasFem)
plotResiduals(res, hurricanes$Minpressure_Updated_2014)
plotResiduals(res, scale(hurricanes$MasFem)*scale(hurricanes$NDAM))
plotResiduals(res, scale(hurricanes$MasFem)*scale(hurricanes$Minpressure_Updated_2014))



m1 = glmmTMB(alldeaths ~ scale(MasFem) + Year + scale(NDAM) + (1|Year),
                           data = hurricanes, 
                           family = nbinom2,
                           control = glmmTMBControl()
)
summary(m1)




