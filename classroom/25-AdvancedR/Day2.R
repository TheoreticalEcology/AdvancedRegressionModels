library(EcoData)

m2 = lm(loght ~ lat * sTemp, data = plantHeight)
summary(m2)

# type I (sequential) ANOVA

summary(aov(m2))

# ANOVA is doing a Likelihood Ratio Test 
m1 = lm(loght ~ lat, data = plantHeight)
m0 = lm(loght ~ 1, data = model.frame(m1))
library(DHARMa)
simulateLRT(m0, m1)

# type II (throwing away the shared fractions of the main effects)

car::Anova(m2, type = "II")
car::Anova(m2, type = "III")

summary(m2)


m2 = lm(loght ~ growthform2 * sTemp, data = plantHeight)
summary(m2)

# causal inference

library(ggplot2)
library(ggdag)

theme_set(theme_dag())
dag = confounder_triangle(x = "Coffee", y = "Lung Cancer", z = "Smoking") 
ggdag(dag, text = FALSE, use_labels = "label")

ggdag_dconnected(dag, text = FALSE, use_labels = "label")

ggdag_dconnected(dag, text = FALSE, use_labels = "label", controlling_for = "z")

smoking <- runif(50)

Coffee <- smoking + rnorm(50, sd = 0.2)
LungCancer <- smoking + rnorm(50, sd =0.2)
fit <- lm(LungCancer ~ Coffee)
plot(LungCancer ~ Coffee)
abline(fit)
summary(fit)



# experimental control 
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

summary(fit2)


library(mgcv)
fit = gam(Ozone ~ Wind + s(Temp) + s(Solar.R) , data = airquality)
summary(fit)


set.seed(123)
Coffee <- runif(100)
LungCancer <- runif(100)
nervousness = Coffee + LungCancer + rnorm(100, sd = 0.1)

fit1 <- lm(LungCancer ~ Coffee + nervousness)
summary(fit1)

fit1 <- lm(LungCancer ~ Coffee)
summary(fit1)


# case study 1 

plot(Fertility ~ Education, data = swiss)

fit = lm(Fertility ~ Education + Agriculture, data = swiss)
summary(fit)

fit = lm(Fertility ~ Education, data = swiss)
summary(fit)


library(EcoData)
?soep

plot(lebensz_org ~ einkommenj1, data = soep)
fit = lm(lebensz_org ~ log(einkommenj1+1), data = soep)
summary(fit)
abline(fit)


fit = lm(lebensz_org ~ log(einkommenj1+1) + syear + sex + alter + anz_kind + bildung + gesund_org , data = soep)
summary(fit)

library(mgcv)

fit = gam(lebensz_org ~ log(einkommenj1+1) + syear + sex + s(alter) + anz_kind + s(bildung) + gesund_org , data = soep)
summary(fit)

plot(fit)




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

# mixed models 


library(mlmRev)
library(effects)

mod0 = lm(normexam ~ standLRT + sex , data = Exam)
plot(allEffects(mod0))

plot(residuals(mod0) ~ school, data = Exam)

# linear regression - iid normal residuals - iid = identical independet residuals 

# clustered structure of residuals = inflated type I error on main predictors

# fixed effect on school variable to adjust for school 
mod0b = lm(normexam ~ standLRT + sex + school, data = Exam)
summary(mod0b)
plot(residuals(mod0b) ~ school, data = Exam)
summary(aov(mod0b))

library(lme4)
library(lmerTest)

mod1 = lmer(normexam ~ standLRT + sex + (1|school), data = Exam)
summary(mod1)



ranef(mod1)
with(Exam, {
  randcoef = ranef(mod1)$school[,1]
  fixedcoef = fixef(mod1)
  plot(standLRT, normexam)
  for(i in 1:65){
    abline(a = fixedcoef[1] + randcoef[i], b = fixedcoef[2], col = i)
  }
})

# -> random intercept model 


library(EcoData)
str(bees)

hist(bees$Spobee)
hist(log(bees$Spobee + 1))

bees$fInfection = factor(bees$Infection)

fit <- lm(Spobee ~ Infection + BeesN, data = bees)
par(mfrow = c(2,2))
plot(fit)


fit <- lmer(Spobee ~ Infection + (1|Hive) + BeesN, data = bees)
summary(fit)


fit <- lmer(log(Spobee + 1) ~ Infection + (1|Hive) + BeesN, data = bees)
qqnorm(residuals(fit))
plot(fit)

library(DHARMa)
res <- simulateResiduals(fit, plot = T)

plot(allEffects(fit, partial.residuals = T ))
summary(fit)


# random slopes

mod2 = lmer(normexam ~ standLRT + sex + (standLRT|school), data = Exam)
summary(mod2)
plot(allEffects(mod2))

with(Exam, {
  randcoefI = ranef(mod2)$school[,1]
  randcoefS = ranef(mod2)$school[,2]
  fixedcoef = fixef(mod2)
  plot(standLRT, normexam)
  for(i in 1:65){
    abline(a = fixedcoef[1] + randcoefI[i] , b = fixedcoef[2] + randcoefS[i], col = i)
  }
})


plot(mod1)

plot(mod1, resid(., scaled=TRUE) ~ standLRT)

plot(mod1, resid(., scaled=TRUE) ~ standLRT | school, abline = T)
plot(mod2, resid(., scaled=TRUE) ~ standLRT | school, abline = T)

summary(mod1)
summary(mod2)



library(EcoData)
str(gpa)

plot(gpa ~ occasion, data = gpa)
fit = lm(gpa ~ occasion, data = gpa)
summary(fit)

gpa$sOccasion = scale(gpa$occasion)


fit = lmer(gpa ~ occasion*sex + job + (occasion|student), data = gpa)
summary(fit)
plot(allEffects(fit))

plot(fit, resid(., scaled=TRUE) ~ occasion | student, abline = T)







