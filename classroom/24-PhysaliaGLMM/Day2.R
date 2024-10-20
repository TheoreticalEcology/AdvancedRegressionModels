fit = lm(Ozone ~ Wind + Temp , data = airquality)
fit
summary(fit)
library(effects)
plot(allEffects(fit, partial.residuals = T))

summary(aov(fit))

m0 = lm(Ozone ~ 1 , data = airquality)
m1 = lm(Ozone ~ Wind + I(Wind^2), data = airquality)
m2 = lm(Ozone ~ Wind + I(Wind^2) + Temp + I(Temp^2), data = airquality)

anova(m0, m1, m2)
anova(m0, m2)

library(DHARMa)
simulateLRT(m0, m1)

# What we have done so far is sequential or type I ANOVA

m1 = lm(Ozone ~ Wind + Temp , data = airquality)
m2 = lm(Ozone ~ Temp + Wind , data = airquality)

summary(m1)
summary(m2)

summary(aov(m1))
summary(aov(m2))

# results are different - reason is sequential ANOVA + collinearity
# CAREFUL: order matters in sequential ANOVA
# Solution: type II/III ANOVA 

car::Anova(m1)

m1 = lm(Ozone ~ scale(Wind) + scale(Temp) , data = airquality)
summary(m1)


library(EcoData)
fit = lm(loght ~ scale(temp) * scale(lat), data = plantHeight)
summary(fit)


# collinearity can be reduced by centering! 

summary(aov(fit))

car::Anova(fit, type = "II")
car::Anova(fit, type = "III")


# question about difference 2-way and type II ANOVA
# fit <- lm(response ~ (A + B + C + D)^3 )
# ^2 is adding all 2-way interactions between the factors / variables 
# I(x^2)

# Linear Mixed Models 

library(mlmRev)
library(effects)

mod0 = lm(normexam ~ standLRT + sex , data = Exam)
plot(allEffects(mod0))
summary(mod0)

str(Exam)

mod0b = lm(normexam ~ standLRT + sex + school , data = Exam)
plot(allEffects(mod0b))
summary(mod0b)

library(lme4)
library(lmerTest)

# random intercept model
mod1 = lmer(normexam ~ standLRT + sex + (1|school) , data = Exam)
summary(mod1)
plot(allEffects(mod1))
ranef(mod1)

# Differences in the slopes between schools 

mod0b = lm(normexam ~ sex + school * standLRT , data = Exam)
plot(allEffects(mod0b))
summary(mod0b)

# random intercept + slope 
mod1 = lmer(normexam ~ standLRT + sex + (standLRT|school) , data = Exam)
summary(mod1)
plot(allEffects(mod1))
ranef(mod1)

library(EcoData)
oldModel <- lm(loght ~ temp, data = plantHeight)
summary(oldModel)
plot(allEffects(oldModel, partial.residuals = T))

str(plantHeight)

library(lme4)
library(lmerTest)

m1 <- lmer(loght ~ temp + (1|Family), data = plantHeight)
summary(m1)

# if you are really interested in if temp or Family is more important, use FE model
m1b <- lm(loght ~ temp + Family, data = plantHeight)
summary(m1b)
car::Anova(m1b)

# ANOVA for RE models 

# default ANOVA function is overwritten by lmerTest, works on FE only
anova(m1)

# ANOVA on REs can be done (in pricn) via ranova in lmerTest
# have to throw out NA values

m1 <- lmer(loght ~ temp + (1|Family), data = plantHeight[complete.cases(plantHeight),])
lmerTest::ranova(m1)

# This is the parametric version of the simulated LRT test in DHARMa

m0 <- lm(loght ~ temp , data = plantHeight[complete.cases(plantHeight),])
m1 <- lmer(loght ~ temp + (1|Family), data = plantHeight[complete.cases(plantHeight),])
simulateLRT(m0,m1)

# R2 for mixed models 
library(MuMIn)
r.squaredGLMM(m1) 


m1 <- lmer(loght ~ scale(temp) + (scale(temp)|Family), data = plantHeight)
summary(m1)


m1 <- lm(loght ~ temp*Family, data = plantHeight)
summary(m1)
car::Anova(m1)


m1 <- lmer(loght ~ temp + (1|Family), data = plantHeight, REML = F)
summary(m1)

mod1 = lmer(normexam ~ standLRT + sex +  (1 | school), data = Exam)
summary(mod1)
plot(mod1)
qqnorm(residuals(mod1))
qqline(residuals(mod1))

library(DHARMa)
res<- simulateResiduals(mod1, plot = T)
plot(res, quantreg = T)

# otherwise you can do by hand any of the other plots we discussed for lm 

x = ranef(mod1)
qqnorm(x$school$`(Intercept)`)
shapiro.test(x$school$`(Intercept)`)

y = aggregate(cbind(standLRT, sex) ~ school, FUN = mean, data = Exam)
plot(x$school$`(Intercept)` ~ y$standLRT)

# against predictors
plot(mod1, resid(., scaled=TRUE) ~ standLRT)
# against predictors per group 
plot(mod1, resid(., scaled=TRUE) ~ standLRT | school, abline = 0)


mod0 = lmer(normexam ~ standLRT + sex +  (1 | school), data = Exam)
mod1 = lmer(normexam ~ standLRT + sex +  (standLRT | school), data = Exam)
simulateLRT(mod0, mod1)


library(EcoData)
str(gpa)

fit <- lm(gpa ~ occasion, data = gpa)
plot(allEffects(fit, partial.residuals = T))
summary(fit)

# 1) decide on FE structure as a group 
# 2) add RE 
# 3) check residuals 
# 4) do final interpretation - what is your result?

gpa$sOccasion = scale(gpa$occasion)
gpa$nJob = as.numeric(gpa$job)-3

fit <- lmer(gpa ~ occasion * sex + nJob + (1|student), data = gpa)
plot(allEffects(fit, partial.residuals = T))
summary(fit)

plot(fit)
library(DHARMa)
simulateResiduals(fit, plot = T)

x = ranef(fit)
qqnorm(x$student$`(Intercept)`)
qqline(x$student$`(Intercept)`)

plot(fit, resid(., scaled=TRUE) ~ occasion | student, abline = 0)

fit2 <- lmer(gpa ~ occasion * sex + nJob + (occasion|student), data = gpa)
summary(fit2)

plot(fit2, resid(., scaled=TRUE) ~ occasion | student, abline = 0)

summary(fit2)

# year = 1990, 1991, ...
# lmer(growth ~ year + (1|year))

m1 = lmer(gpa ~ occasion 
                + (1|student) 
                + (1|college) 
                + (1|homestate))

m1 = lmer(gpa ~ occasion 
          + (1|college/student) 
          + (1|homestate))

site | quadrat | obs

A      1
A.     2
A.     3
A      4
B.     1
B.     2
B      3
B      4
C
C
C

(1|site) + (1|quadrat) 
(1|site/quadrat) 












