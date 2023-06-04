library(mlmRev)
library(effects)

mod0 = lm(normexam ~ standLRT + sex , data = Exam)
#plot(allEffects(mod0))
summary(mod0)


mod0b = lm(normexam ~ standLRT + sex + school , data = Exam)
summary(mod0b)

# lme4 / glmmTMB

# random intercept model
library(lme4)
mod1 = lmer(normexam ~ standLRT + sex + (1|school), data = Exam)
summary(mod1)



# random slope model
mod2 = lmer(normexam ~ standLRT + sex +  (standLRT | school), data = Exam)
summary(mod2)

# equivalent fixed effect model
mod2 = lm(normexam ~ standLRT * school + sex, data = Exam)
summary(mod2)


library(lmerTest)
library(lme4)
library(EcoData)

oldModel <- lm(loght ~ temp , data = plantHeight)
summary(oldModel)


plantHeight = plantHeight[complete.cases(plantHeight),]

m1 <- lmer(loght ~ temp + (1|growthform) , data = plantHeight)
summary(m1)

plot(m1)

qqnorm(residuals(mod1))
qqline(residuals(mod1))

x = ranef(m1)
qqnorm(x$Family$`(Intercept)`)
qqline(x$Family$`(Intercept)`)

plot(m1, resid(., scaled=TRUE) ~ temp | growthform , abline = 0)

m1 <- lmer(loght ~ temp + (1|growthform) , data = plantHeight)
summary(m1)

m2 <- lmer(loght ~ temp + (temp|growthform) , data = plantHeight)
summary(m2)

AIC(m1)
AIC(m2)




plot(log(Spobee + 1) ~ Infection + FIXED + RANDOM, data = bees)

# fit a mixed model

# perform some residual checks 



library(lme4)
fit <- lmer(log(Spobee + 1) ~ Infection + BeesN + (1|Hive), data = bees)
summary(fit)

plot(fit, 
     resid(., scaled=TRUE) ~ fitted(.) | Hive, 
     abline = 0)





plot(Fertility ~ Education, data = swiss)




Fertility ~ Education + Agriculture

Education <- Agriculture
Fertility <- Agriculture


fit <- lm(lebensz_org ~ einkommenj1, data = soep)
summary(fit)
plot(allEffects(fit))


fit <- lm(lebensz_org ~ scale(einkommenj1) + sex + scale(alter) + anz_kind, data = soep)
summary(fit)
plot(allEffects(fit))


boxplot(einkommenj1 ~ anz_kind, data = soep)
cor.test(soep$einkommenj1, soep$anz_kind, method = "spearman")


library(mgcv)

fit <- gam(lebensz_org ~ scale(einkommenj1) + sex + s(alter) + anz_kind, data = soep)

summary(fit)


m1 = lm(Ozone ~ Wind , data = airquality)
summary(airquality)
summary(m1)

# Likelihood ratio tests

airqualityC = airquality[complete.cases(airquality),]


# Model 1
m1 = lm(Ozone ~ Wind , data = airqualityC)

# Model 2
m2 = lm(Ozone ~ Wind + Temp, data = airqualityC)

# LRT
anova(m1, m2)

AIC(m1)
AIC(m2)


m2 = lm(Ozone ~ (Wind + Temp + Solar.R)^2, data = airqualityC)
summary(m2)

library(MASS)
stepOut <- stepAIC(m2)

library(MuMIn)

options(na.action = "na.fail")

dd <- dredge(m2)
subset(dd, delta < 4)

summary(model.avg(dd, subset = delta < 4))



summary(stepOut)


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


library(EcoData)
model = lm(loght ~ temp * lat, data = plantHeight)
summary(model)


cor(plantHeight$temp, plantHeight$lat)

model = lm(loght ~ temp , data = plantHeight)
summary(model)

model = lm(loght ~ lat, data = plantHeight)
summary(model)



options(na.action = "na.omit")

library(mgcv)
m2 = gam(Ozone ~ Temp + te(x,y), data = airquality)
summary(m2)

plot(m2)



fullModel <- lm(loght ~ (growthform + Family + lat + long + alt + temp + NPP )^2, data = plantHeight)

fullModel <- lm(loght ~ growthform + lat + long + alt + temp + NPP , data = plantHeight)

selectedModel = stepAIC(fullModel, trace = 0)
summary(selectedModel)


options(na.action = "na.fail")
dd <- dredge(fullModel)
subset(dd, delta < 4)


m0 <- lm(loght ~ alt + temp , data = plantHeight)
m1 <- lmer(loght ~ alt + temp + (1|Country), data = plantHeight)
m2 <- lmer(loght ~ alt + temp + (temp|Country), data = plantHeight)

# ranova function in lmerTest to test on RE structure
lmerTest::ranova(m2)

DHARMa::simulateLRT(m0,m1)
DHARMa::simulateLRT(m1,m2)


m0 <- lm(loght ~ lat , data = plantHeight)
summary(m0)

m0 <- lm(loght ~ lat + temp , data = plantHeight)
summary(m0)




library(EcoData)
#str(birdfeeding)
plot(feeding ~ attractiveness, data = birdfeeding)


fit = glm(feeding ~ attractiveness, 
          data = birdfeeding, 
          family = "poisson")

summary(fit)

plot(allEffects(fit, partial.residuals = T))

predict(fit, type = "response")[1]


options(na.action = "na.omit")

library(EcoData)
#str(titanic)
#mosaicplot( ~ survived + sex + pclass, data = titanic)
titanic$pclass = as.factor(titanic$pclass)


fit = lm(survived ~ sex * age, data = titanic)
summary(fit)
par(mfrow = c(2,2))
plot(fit)



titanicC = titanic[complete.cases(titanic[,c(1,2,4,5)]),]

fit = glm(survived ~ sex * age, 
          data = titanic, 
          family = "binomial")
summary(fit)


library(DHARMa)
res <- simulateResiduals(fit, plot = T)
plotResiduals(res, form = titanicC$pclass)

library(mgcv)

fit = gam(survived ~ (sex + age + pclass)^2, data = titanic, family = "binomial")

res <- simulateResiduals(fit, plot = T)


plotResiduals(res, form = age | pclass)
plotResiduals(res, form = titanicC$sex)
plotResiduals(res, form = titanicC$age)

titanicC$res =  residuals(res)
plot(res ~ age, data = titanicC[titanicC$sex == "male", ] )
plot(res ~ age, data = titanicC[titanicC$sex == "female", ] )


# Causal -> how do elks react to roads?
# Predictive analysis -> best predict where we find elks

# 0/1 data -> family = "binomial"


fit <- glm(presence ~ dist_roads + dem + ruggedness, data = elk_data, family = "binomial")
summary(fit)
plot(allEffects(fit))


fit <- gam(presence ~ dist_roads + s(dem) + s(ruggedness), data = elk_data, family = "binomial")
summary(fit)

plot(fit)


res <- simulateResiduals(fit, plot = T)
plot(res, quantreg = T)
plotResiduals(res, form = elk_data$dem, quantreg = T)




fit <- glm(presence ~ (dist_roads + dem + ruggedness + NDVI + habitat)^2, data = elk_data)

out <- stepAIC(fit)


library(glmmTMB)
library(lme4)



m1 = glm(count ~ spp + mined, family = poisson, data = Salamanders)
summary(m1)

res <- simulateResiduals(m1, plot = T)
testDispersion(res)


m1 = glmmTMB(count ~ spp + mined + (1|site), 
             family = poisson, 
             data = Salamanders)

res <- simulateResiduals(m1, plot = T)
testDispersion(res)

m1 = glmmTMB(count ~ spp + mined + (1|site), 
             family = nbinom1, 
             data = Salamanders)

summary(m1)

res <- simulateResiduals(m1, plot = T)
testDispersion(res)
testZeroInflation(res)


m2 = glmmTMB(count ~ spp + mined + (1|site), 
             family = nbinom1, 
             ziformula = ~ spp + mined,
             data = Salamanders)

summary(m2)
plogis(-2.8804)

DHARMa::simulateLRT(m1,m2)




library(glmmTMB)

m1 = glm(SiblingNegotiation ~ FoodTreatment*SexParent + 
           offset(log(BroodSize)),
         data = Owls , family = poisson)
res = simulateResiduals(m1)
plot(res)


m1 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + 
           offset(log(BroodSize)),
         data = Owls , family = poisson)
res = simulateResiduals(m1)
plot(res)

m1 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + 
               offset(log(BroodSize)),
             data = Owls , family = nbinom1)
res = simulateResiduals(m1)
plot(res)
testDispersion(m1)
summary(m1)


m1 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + 
               offset(log(BroodSize)),
             data = Owls , 
             dispformula = ~ FoodTreatment + SexParent,
             ziformula = ~ FoodTreatment + SexParent,
             family = nbinom1)
res = simulateResiduals(m1)
plot(res)
summary(m1)

plotResiduals(res, form = Owls$FoodTreatment)
plotResiduals(res, form = Owls$SexParent)






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


library(glmmTMB)

fit = glmmTMB(observation ~ treatment, data = data, 
              dispformula = ~ treatment)
summary(fit)


plot(Ozone ~ Solar.R, data = airquality)


plot(Ozone ~ Solar.R, data = airquality)

fit <- lm(Ozone ~ Solar.R, data = airquality)
plot(allEffects(fit, partial.residuals = T))
res <- simulateResiduals(fit)
plot(res, quantreg = F)


fit <- glmmTMB(Ozone ~ Solar.R, 
               dispformula = ~ Solar.R, 
               data = airquality)
summary(fit)

plot(allEffects(fit, partial.residuals = T))

res <- simulateResiduals(fit)
plot(res, quantreg = F)



DHARMa::simulateLRT(m1, m2)






