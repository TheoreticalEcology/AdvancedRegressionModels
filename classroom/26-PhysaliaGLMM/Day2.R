
# Exercise

# preparations
library(EcoData)
plantHeight$sTemp = as.numeric(scale(plantHeight$temp))
plantHeight$sLat = as.numeric(scale(plantHeight$lat))
plantHeight$sNPP = as.numeric(scale(plantHeight$NPP))
plantHeight2 = plantHeight[! plantHeight$growthform %in% c("Herb/Shrub", "Shrub/Tree", "Fern"), ]
plantHeight2$growthform2 = relevel(as.factor(plantHeight2$growthform), "Herb")




# Task 1
fit = lm(loght ~ sTemp + sNPP, data = plantHeight)
summary(fit)

# Task 2
fit = lm(loght ~ sTemp * growthform2  , data = plantHeight2 )
summary(fit)
plot(allEffects(fit))

fit = lm(loght ~ sTemp, data = plantHeight[plantHeight$growthform == "Tree",])
summary(fit)

fit = lm(loght ~ sTemp*growthform2 - sTemp -1, data = plantHeight2)
summary(fit)

-0.31075 + 1.58601

# Task 3
fit = lm(loght ~ sTemp * sLat, data = plantHeight)
summary(fit)
plot(sTemp ~ sLat, data = plantHeight)

# Missing data

fit = lm(Ozone ~ Wind + Temp, data = airquality)
summary(fit)

options("na.action")

fit = lm(Ozone ~ Wind + Temp, data = airquality, na.action = "na.fail")
options(na.action = "na.fail")

summary(airquality)

image(is.na(t(airquality)), axes = F)
axis(3, at = seq(0,1, len = 6), labels = colnames(airquality))

airquality[complete.cases(airquality), ]

library(missRanger)
airqualityImp<- missRanger(airquality)

fit = lm(Ozone ~ Wind + Temp, data = airqualityImp)
summary(fit)

plot(Ozone ~ Wind, data = airquality)

options(na.action = "na.omit")


## ANOVA ##

fit = lm(Ozone ~ Wind + Temp, data = airquality)
summary(aov(fit))

x = summary(aov(fit))
x = x[[1]]$`Sum Sq`
x/sum(x)

summary(fit)

fit = lm(Ozone ~ Temp + Wind, data = airquality)
summary(aov(fit))

x = summary(aov(fit))
x = x[[1]]$`Sum Sq`
x/sum(x)

m1 = lm(Ozone ~ 1, data = airquality)
summary(m1)
m2 = lm(Ozone ~ Temp, data = airquality)
summary(m2)
m2b = lm(Ozone ~ Wind, data = airquality)
summary(m2b)
m3 = lm(Ozone ~ Temp + Wind, data = airquality)
summary(m3)

library(car)
fit = lm(Ozone ~ Temp + Wind, data = airquality)
car::Anova(fit, type="II")

fit = lm(Ozone ~ Wind + Temp, data = airquality)
car::Anova(fit, type="II")



library(EcoData)
fit = lm(loght ~ temp * lat, data = plantHeight)
summary(fit)

summary(aov(fit))
car::Anova(fit, type = "II")
car::Anova(fit, type = "III")


## Random / mixed models ##

install.packages("mlmRev")
library(mlmRev)
library(effects)

mod0 = lm(normexam ~ standLRT + sex , data = Exam)
plot(allEffects(mod0))

mod0 = lm(normexam ~ standLRT + sex + school, data = Exam)
plot(allEffects(mod0))
summary(mod0)

library(lme4)
library(lmerTest)

mod1 = lmer(normexam ~ standLRT + sex + (1|school), data = Exam)
plot(allEffects(mod1))
summary(mod1)
ranef(mod1)

str(bees)
table(bees$Infection)
table(bees$Spobee)

library(lme4)
bees$fInfection = as.factor(bees$Infection)
fit <- lmer(log(Spobee+1)  ~ Infection + scale(BeesN) + (1|Hive) , data = bees)
summary(fit)
plot(allEffects(fit, partial.residuals = T))


# Random slope (+ intercept) model 
mod2 = lmer(normexam ~ standLRT + sex + (standLRT|school), data = Exam)
plot(allEffects(mod2))
summary(mod2)
ranef(mod2)

plot(mod1)
plot(mod2, resid(., scaled = T) ~ standLRT | school, abline = 0)



library(lme4)
library(glmmTMB)
library(EcoData)


gpa$sOccasion = scale(gpa$occasion)
gpa$nJob = as.numeric(gpa$job)

fit <- lmer(gpa ~ sOccasion*sex + nJob + (1|student), data = gpa)
summary(fit)
table(gpa$nJob)



library(lme4)
library(glmmTMB)
library(EcoData)
gpa$sOccasion = scale(gpa$occasion)
gpa$nJob = as.numeric(gpa$job)

fit <- lmer(gpa ~ sOccasion * sex + nJob + (1|student), data = gpa)
plot(allEffects(fit))
summary(fit)

# plot seems to show a lot of differences still, so add random slope
plot(fit, 
     resid(., scaled=TRUE) ~ fitted(.) | student, 
     abline = 0)

# slope + intercept model
fit <- lmer(gpa ~ sOccasion*sex + nJob + (sOccasion|student), data = gpa)

# checking residuals - heteroskedasticity 
plot(fit)

summary(fit)

fit = lm(loght ~ sTemp + growthform2 , data = plantHeight2 )
plot(allEffects(fit))
