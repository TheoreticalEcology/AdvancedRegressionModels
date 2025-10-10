
m3 = glmmTMB(count ~ spp + mined + (1|site), family = nbinom1, data = Salamanders)
res <- simulateResiduals(m3, plot = T)
testDispersion(res)
summary(m3)

testZeroInflation(m3)

m4 = glmmTMB(count ~ spp + mined + (1|site), 
             ziformula = ~ spp + mined,
             family = nbinom1, data = Salamanders)
summary(m4)

simulateLRT(m3, m4)



m5 = glmmTMB(count ~ spp + mined +  (1|site), 
             ziformula = ~ 1,
             family = nbinom1, data = Salamanders)
summary(m5)

simulateLRT(m3, m5, n = 50)


library(glmmTMB)

fit = glm(SiblingNegotiation ~ FoodTreatment*SexParent + offset(log(BroodSize)),
         data = Owls , family = poisson)
res = simulateResiduals(fit, plot = T)
testDispersion(res)
testZeroInflation(res)


fit = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)),
          data = Owls , family = poisson)
res = simulateResiduals(fit, plot = T)
testDispersion(res)
testZeroInflation(res)


fit = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)),
              data = Owls , family = nbinom1)
res = simulateResiduals(fit, plot = T)
testDispersion(res)
testZeroInflation(res)
AIC(fit)


fit = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + 
                (1|Nest) + offset(log(BroodSize)),
              ziformula = ~ FoodTreatment*SexParent,
              data = Owls , family = nbinom1)
res = simulateResiduals(fit, plot = T)
testDispersion(res)
testZeroInflation(res)
AIC(fit)

summary(fit)


library(EcoData)
?elephant

elephant$occurenceData

model = glm(Presence~bio1, data = elephant$occurenceData, family = binomial()) 

model = glm(Presence~ . + I(bio1^2) + I(bio2^2) + I(bio3^2) + I(bio4^2) + I(bio5^2) + I(bio6^2) + I(bio7^2) + I(bio8^2) + I(bio9^2) + I(bio10^2), data = elephant$occurenceData, family = binomial()) 

model = stepAIC(model)

model = gam(Presence~ s(bio1) + s(bio2) + s(bio3) + s(bio4) + s(bio5) + s(bio6) + s(bio7) + s(bio8) + s(bio9) + s(bio10) + s(bio11) , data = elephant$occurenceData, family = binomial()) 

plot(model)

AIC(model)  

library(pROC)
auc(elephant$occurenceData$Presence, predict(model, type = "response"))

library(boot)
res = cv.glm(elephant$occurenceData, model, cost = auc, K=5)
res$delta

library(sp)
library(raster)
plot(elephant$predictionData$bio1)

str(elephant$predictionData$bio1)

predictions =  predict(elephant$predictionData, model = model, type = "response")
head(as.data.frame(predictions))

spplot(predictions, colorkey = list(space = "left") )


# dispersion models 


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


library(nlme)

fit = gls(observation ~ treatment, data = data, 
          weights = varIdent(form = ~ 1 | treatment))
summary(fit)

library(glmmTMB)

fit = glmmTMB(observation ~ treatment, data = data, 
              dispformula = ~ treatment)
summary(fit)


plot(Ozone ~ Solar.R, data = airquality)

m1 = lm(Ozone ~ Solar.R, data = airquality)
par(mfrow = c(2, 2))
plot(m1)

res <- simulateResiduals(m1, plot = T)


fit = glmmTMB(Ozone ~ Solar.R, data = airquality, 
              dispformula = ~ Solar.R)
summary(fit)

plot(residuals(fit) ~ predict(fit))
plot(residuals(fit, type = "pearson") ~ predict(fit))

res <- simulateResiduals(fit, plot = T)


m3 = glmmTMB(count ~ spp + mined + (1|site), family = nbinom1,
             dispformula = ~ spp + mined ,  data = Salamanders)
summary(m3)

library(DHARMa)
res = simulateResiduals(m3, plot = T)


library(DHARMa)
library(mgcv)
?hurricanes
str(hurricanes)


originalModelGAM = gam(alldeaths ~ MasFem * (Minpressure_Updated_2014 + NDAM),
                       data = hurricanes, family = nb, na.action = "na.fail")
summary(originalModelGAM)


m1 = glmmTMB(alldeaths ~ scale(MasFem) *
               (scale(Minpressure_Updated_2014) + scale(NDAM)),
             data = hurricanes, family = nbinom2)
summary(m1)

plot(allEffects(m1))


res <- simulateResiduals(m1, plot = T)

plotResiduals(res, hurricanes$MasFem)
plotResiduals(res, hurricanes$Minpressure_Updated_2014)
plotResiduals(res, hurricanes$NDAM)

m1 = glmmTMB(alldeaths ~ scale(MasFem) *
               (scale(Minpressure_Updated_2014) + scale(NDAM^0.2)),
             data = hurricanes, family = nbinom2)
summary(m1)

res <- simulateResiduals(m1, plot = T)

plotResiduals(res, hurricanes$MasFem)
plotResiduals(res, hurricanes$Minpressure_Updated_2014)
plotResiduals(res, hurricanes$NDAM)




m1 = glmmTMB(alldeaths ~ scale(MasFem) * scale(NDAM^0.2) + 
                         scale(Category)  + scale(Year) * scale(MasFem) + (1|Year), 
             data = hurricanes, family = nbinom1)
summary(m1)


newModel <- glmmTMB(alldeaths ~ scale(MasFem) * scale(NDAM^0.2) 
                      + (1 + MasFem|Year),
                    data = hurricanes, family = nbinom2)
summary(newModel)
resCorrected <- simulateResiduals(newModel, plot = T)

plotResiduals(resCorrected, hurricanes$MasFem)
plotResiduals(resCorrected, hurricanes$Minpressure_Updated_2014)
plotResiduals(resCorrected, hurricanes$NDAM)


