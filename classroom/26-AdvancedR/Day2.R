
library(ggdag)
library(ggplot2)
theme_set(theme_dag())
dag = confounder_triangle(x = "Coffee", y = "Lung Cancer", z = "Smoking") 
ggdag(dag, text = FALSE, use_labels = "label")

smoking <- runif(50)
Coffee <- smoking + rnorm(50, sd = 0.2)
LungCancer <- smoking + rnorm(50, sd =0.2)

fit <- lm(LungCancer ~ Coffee)
plot(LungCancer ~ Coffee)
abline(fit)
summary(fit)

ggdag_dconnected(dag, text = FALSE, use_labels = "label")

ggdag_dconnected(dag, text = FALSE, use_labels = "label", 
                 controlling_for = "z")


smoking <- rep(0.5, 50)
Coffee <- smoking + rnorm(50, sd = 0.2)
LungCancer <- smoking + rnorm(50, sd =0.2)

fit <- lm(LungCancer ~ Coffee)
plot(LungCancer ~ Coffee)
abline(fit)
summary(fit)


smoking <- runif(100)
Coffee <- smoking + rnorm(100, sd = 0.2)
LungCancer <- smoking + rnorm(100, sd =0.2)
fit1 <- lm(LungCancer ~ Coffee)
fit2 <- lm(LungCancer ~ Coffee + smoking)
plot(LungCancer ~ Coffee)
abline(fit1)
abline(fit2, col = "red")
legend("topleft", c("simple regression", "multiple regression"), col = c(1,2), lwd = 1)


dag = collider_triangle(x = "Coffee", y = "Lung Cancer", m = "Nervousness") 
ggdag(dag, text = FALSE, use_labels = "label")

set.seed(123)
Coffee <- runif(100)
LungCancer <- runif(100)
nervousness = Coffee + LungCancer + rnorm(100, sd = 0.1)

fit1 <- lm(LungCancer ~ Coffee + nervousness)
summary(fit1)

fit1 <- lm(LungCancer ~ Coffee)
summary(fit1)


library(EcoData)
?soep

fit <- lm(lebensz_org^2 ~ scale(sqrt(einkommenj1)) 
          + scale(syear) + sex + scale(alter) + scale(anz_pers) + 
            scale(bildung) + scale(gesund_org), data = soep)
summary(fit)

par(mfrow=c(2,2))
plot(fit)

fit <- lm(lebensz_org ~ scale(sqrt(einkommenj1)) , data = soep)
summary(fit)

x =  model.frame(fit)
plot(residuals(fit) ~ x$`scale(sqrt(einkommenj1))`, cex = 0.2)
plot(residuals(fit) ~ x$`scale(alter)`)



library(piecewiseSEM)
theme_set(theme_dag())

dag <- dagify(rich ~ distance + elev + abiotic + age + hetero + firesev + cover,
              firesev ~ elev + age + cover,
              cover ~ age + elev + abiotic ,
              exposure = "cover",
              outcome = "rich"
)

ggdag(dag)

ggdag_adjustment_set(dag)
ggdag_adjustment_set(dag, effect="direct")

library(piecewiseSEM)

mod = psem(
  lm(rich ~ distance + elev + abiotic + age + hetero + firesev + cover, data = keeley),
  lm(firesev ~ elev + age + cover, data = keeley), 
  lm(cover ~ age + elev + hetero + abiotic, data = keeley)
)

summary(mod)
plot(mod)


# Model selection 

set.seed(123)

x = runif(100)
y = 0.25 * x + rnorm(100, sd = 0.3)

plot(x, y)
fit = lm(y~x)
summary(fit)
abline(fit, col = "red")

xNoise = matrix(runif(8000), ncol = 80)
dat = data.frame(y=y,x=x, xNoise)

fit = lm(y~x, data = dat)
summary(fit)

fullModel = lm(y ~ ., data = dat)
summary(fullModel)


set.seed(42)
X = runif(100)
P = runif(100)
Y = 0.8*X + 10*P + rnorm(100, sd = 0.5)

summary(lm(Y~X))
summary(lm(Y~X+P))


# Model 1
m1 = lm(Ozone ~ Wind , data = airquality)

# Model 2
m2 = lm(Ozone ~ Wind + Temp, data = airquality)


logLik(m1) ~ R2
logLik(m2)

anova(m1, m2)

library(DHARMa)
simulateLRT(m1, m2)


# AIC - 2 logLikelihood + 2*NumParameters

AIC1 = -2 * - 543.5937 + 2*3
AIC2 = -2 * -520.8705 + 2*4

AIC(m1)
AIC(m2)
# lower is better, rule of thumb: AIC difference of 2 is meaningful
# although AIC differeces tend to get larger with data size

# AIC is an adjsted log-likelihood, similar to adjusted R2
summary(m1)

# IMPORTANT INFO: 

# Overadjusting to fit data = overfitting
# Test: split the data, part used for training, part for validation

m3 = lm(Ozone ~ Wind + Temp + Solar.R, data = airquality)
AIC(m3)

airquality$fDay = factor(airquality$Day)
airquality$fMonth = factor(airquality$Month)

m4 = lm(Ozone ~ Wind + Temp + Solar.R + fMonth, data = airquality)
summary(m4)
AIC(m4)


m5 = lm(Ozone ~ Wind + Temp + Solar.R + fMonth+ fDay, data = airquality)
summary(m5)
AIC(m5)

summary(aov(m5))
final = MASS::stepAIC(m5)

# !!!!!!!!!! WARNING !!!!!!!!!!

# MS Does not recognize causality 

set.seed(123)
x1 = runif(100)
x2 = 0.8 * x1 + 0.2 *runif(100)
y = x1 + x2 + rnorm(100)

m1 = lm(y ~ x1 + x2)
summary(m1)

m2 = MASS::stepAIC(m1)
summary(m2)

# p-values after MS needs to be corrected 

set.seed(123)
x = runif(100)
y = 0.25 * x + rnorm(100, sd = 0.3)
xNoise = matrix(runif(8000), ncol = 80)
dat = data.frame(y=y,x=x, xNoise)
fullModel = lm(y~., data = dat)


fit = lm(y~x, data = dat)
summary(fit)

fullModel = lm(y ~ ., data = dat)
summary(fullModel)

library(MASS)
reduced = stepAIC(fullModel)

summary(reduced)


library(mlmRev)
library(effects)
`%||%` <- function(x, y) if (!is.null(x)) x else y

mod0 = lm(normexam ~ standLRT + sex , data = Exam)
plot(allEffects(mod0))
summary(mod0)

mod0b = lm(normexam ~ standLRT + sex + school , data = Exam)
plot(allEffects(mod0b))
summary(mod0b)

library(lme4)
mod1 = lmer(normexam ~ standLRT + sex +  (1|school), data = Exam)
summary(mod1)

library(EcoData)
str(bees)

library(lme4)
fit <- lmer(log(Spobee + 1) ~ Infection + BeesN +  (1|Hive), data = bees)
summary(fit)

?plot
class(fit)
?plot.merMod

plot(fit)
plot(fit, resid(., scaled=TRUE) ~ fitted(.) | Hive, abline = 0)
plot(fit, resid(., scaled=TRUE) ~ Infection, abline = 0)
plot(fit, resid(., scaled=TRUE) ~ BeesN, abline = 0)

plot(allEffects(fit))
summary(fit)

library(lmerTest)
fit <- lmer(log(Spobee + 1) ~ Infection + BeesN +  (1|Hive), data = bees)
summary(fit)

# scaling the predictors is recommended for mixed models
fit <- lmer(log(Spobee + 1) ~ scale(Infection) + scale(BeesN) +  (1|Hive), data = bees)
summary(fit)

# REML settings to F for AIC selection or anything that depends on likelihood
fit <- lmer(log(Spobee + 1) ~ scale(Infection) + scale(BeesN) +  (1|Hive), data = bees, REML = F)
# CAREFUL: AIC only on fixed effect structure, doesnt cound
# DF correctly for mixed effects 

# How many RF to add? 
# Minimum: Random intercept on each grouping variable! 


mod0c = lm(normexam ~ standLRT * school + sex , data = Exam)
plot(allEffects(mod0c))
summary(mod0c)

Exam$sStandLRT = scale(Exam$standLRT)

mod2 = lmer(normexam ~ sStandLRT + sex + (sStandLRT | school) , data = Exam)
plot(allEffects(mod2))
summary(mod2)
ranef(mod2)

with(Exam, {
  randcoefI = ranef(mod2)$school[,1]
  randcoefS = ranef(mod2)$school[,2]
  fixedcoef = fixef(mod2)
  plot(standLRT, normexam)
  for(i in 1:65){
    abline(a = fixedcoef[1] + randcoefI[i] , b = fixedcoef[2] + randcoefS[i], col = i)
  }
})

# Adding random slope or not?
# Medical literature: need to add random slope on all predictors of interest else type I error inflation
# Ecology: too many predictors, just add random intercept
# My prectical recommendation: start with random intercept and add random slopes if you either 
# * see evidence in the residual checks
# * or model selection is in favor or random slope 

# residual checks

mod1 = lmer(normexam ~ standLRT + sex +  (1|school), data = Exam)

plot(mod1)
plot(mod1, resid(., scaled=TRUE) ~ standLRT | school, abline = 0)

# model selection, but don't use AIC
library(DHARMa)
simulateLRT(mod1, mod2, n = 25)

# ==> Conclusion: add random slope to the model 

# ANOVA implemented via lmerTest 
anova(mod1)
anova(mod1, type = "II")
anova(mod1, type = "I")

library(MuMIn)
r.squaredGLMM(mod1) 
r.squaredGLMM(mod2) 
summary(mod1)


library(lme4)
library(glmmTMB)
library(EcoData)
# initial model with a random intercept and fixed effect structure based on
# causal assumptions

gpa$sOccasion = scale(gpa$occasion)
gpa$nJob = as.numeric(gpa$job)

fit <- lmer(gpa ~ sOccasion*sex + nJob + (1|student), data = gpa)
summary(fit)
plot(allEffects(fit))


plot(fit)
plot(fit, 
     resid(., scaled=TRUE) ~ sOccasion, 
     abline = 0)

# plot seems to show a lot of differences still, so add random slope
plot(fit, 
     resid(., scaled=TRUE) ~ fitted(.) | student, 
     abline = 0)

# slope + intercept model
fit <- lmer(gpa ~ sOccasion*sex + nJob + (sOccasion|student), data = gpa)

# checking residuals - heteroskedasticity 
plot(fit)

x = residuals(fit)
qqnorm(x)
shapiro.test(x)

library(DHARMa)
res = simulateResiduals(fit)
plot(res)
