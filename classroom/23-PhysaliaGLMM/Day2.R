
fit = lm(Ozone ~ Wind + Temp, data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

# Mixed 

# grouping (categorical) variable that we are not intested, but could have an effect?
# -> not including them could inflate type I error, weird distributions, could also cost power!

library(mlmRev)
library(effects)

mod0 = lm(normexam ~ standLRT + sex , data = Exam)
plot(allEffects(mod0))

mod0b = lm(normexam ~ standLRT + sex + school , data = Exam)
summary(mod0b)
plot(allEffects(mod0b))

library(lme4)
mod1= lmer(normexam ~ standLRT + sex + (1|school) , data = Exam)
summary(mod1)
plot(allEffects(mod1))
x = ranef(mod1)
str(x)
hist(x$school$`(Intercept)`)

shapiro.test(x$school$`(Intercept)`)


# mean per school = 0.07639 + x$school$`(Intercept)`

# when loading lmerTest, you also have p-values
library(lmerTest)
mod1= lmer(normexam ~ standLRT + sex + (1|school) , data = Exam)  
summary(mod1)  

with(Exam, {
  randcoef = ranef(mod1)$school[,1]
  fixedcoef = fixef(mod1)
  plot(standLRT, normexam)
  for(i in 1:65){
    abline(a = fixedcoef[1] + randcoef[i], b = fixedcoef[2], col = i)
  }
})

# RE Shrinkage

mf = lm(normexam ~ 0 + school, data = Exam)
mr = lmer(normexam ~ (1 | school), data = Exam)

ef = coef(mf)
er = ranef(mr)$school$`(Intercept)`
plot(ef, er, xlab = "fixed", ylab = "random")
abline(0,1)


# fixed effect model
mod2a = lm(normexam ~ standLRT * school + sex  , data = Exam)
summary(mod0b)



mod2 = lmer(normexam ~ standLRT + sex + (1 + standLRT|school) , data = Exam)
summary(mod2)



with(Exam, {
  randcoefI = ranef(mod2)$school[,1]
  randcoefS = ranef(mod2)$school[,2]
  fixedcoef = fixef(mod2)
  plot(standLRT, normexam)
  for(i in 1:65){
    abline(a = fixedcoef[1] + randcoefI[i] , b = fixedcoef[2] + randcoefS[i], col = i)
  }
})


# Random intercept model

library(lme4)
library(lmerTest)

m1 <- lm(loght ~ temp , data = plantHeight)
summary(m1)

m1 <- lm(loght ~ temp + Family , data = plantHeight)
summary(m1)

randomInterceptModel <- lmer(loght ~ temp + (1|Family), data = plantHeight[complete.cases(plantHeight),])
summary(randomInterceptModel)



plot(randomInterceptModel, resid(., scaled=TRUE) ~ temp | Family, abline = 0)


# Random slope model

randomSlopeModel <- lmer(loght ~ temp + (temp | Family), data = plantHeight)
summary(randomSlopeModel)

# scaling helps the optimizer
plantHeight$sTemp = scale(plantHeight$temp)

randomSlopeModel <- lmer(loght ~ sTemp + (sTemp | Family), data = plantHeight)
summary(randomSlopeModel)

randomSlopeModel <- lmer(loght ~ sTemp + (sTemp || Family), data = plantHeight)
summary(randomSlopeModel)



mod1 = lmer(normexam ~ standLRT + sex +  (1 | school), data = Exam)
summary(mod1)

# if it didn't converge, scale all predictors, if that doesn't help go to optimizer settings and increase iterations and set to "bobyqa"

plot(mod1)

qqnorm(residuals(mod1))
qqline(residuals(mod1))

class(mod1)

residuals(mod1, re.form = NULL) # conditional, default
predict(mod1, re.form = ~ 0) # marginal

x = ranef(mod1)
qqnorm(x$school$`(Intercept)`)
qqline(x$school$`(Intercept)`)

y = aggregate(cbind(standLRT, sex) ~ school, FUN = mean, data = Exam)
plot(x$school$`(Intercept)` ~ y$standLRT)

plot(mod1, resid(., scaled=TRUE) ~ standLRT | school, abline = 0)

# don't use this on the RE structure because doesn't count DF properly
AIC(mod1)

# If using AIC or anything else depending on likelihood, set REML = F

mod1 = lmer(normexam ~ standLRT + sex +  (1 | school), data = Exam, 
            REML = F)
summary(mod1)

library(MuMIn)
r.squaredGLMM(mod1) 



library(EcoData)
str(bees)

library(lme4)
fit <- lmer(log(Spobee + 1) ~ Infection + BeesN + (1|Hive), data = bees)
summary(fit)

plot(fit)

x = ranef(fit)
qqnorm(x$Hive$`(Intercept)`)
qqline(x$Hive$`(Intercept)`)

shapiro.test(x$Hive$`(Intercept)`)

plot(fit, 
     resid(., scaled=TRUE) ~ fitted(.) | Hive, 
     abline = 0)


library(lme4)
library(lmerTest)
library(glmmTMB)
library(EcoData)
# initial model with a random intercept and fixed effect structure based on
# causal assumptions

gpa$sOccasion = scale(gpa$occasion)
gpa$nJob = as.numeric(gpa$job)

fit <- lmer(gpa ~ sOccasion*sex + nJob + (1|student), data = gpa)
summary(fit)

# normal residuals
plot(fit)
qqnorm(residuals(fit))
plot(allEffects(fit, partial.residuals = T))

# RE distribution 

x = ranef(fit)
qqnorm(x$student$`(Intercept)`)

plot(fit, 
     resid(., scaled=TRUE) ~ fitted(.) | student, 
     abline = 0)

fit2 <- lmer(gpa ~ sOccasion*sex + scale(nJob) + (sOccasion|student), data = gpa)

plot(fit2, 
     resid(., scaled=TRUE) ~ fitted(.) | student, 
     abline = 0)

summary(fit2)


library(EcoData)
?soep


fit <- lm(lebensz_org ~ einkommenj1 + syear + sex + alter + anz_kind + gesund_org, data = soep)

summary(fit)


  

