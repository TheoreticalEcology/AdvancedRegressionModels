fit <- lm(Ozone ~ sTemp + sWind + sSolar.R, data = airquality)
summary(fit)

summary(aov(fit))


airqualityReduced = airquality[complete.cases(airquality), ]

m1 = lm(Ozone ~ sTemp, data = airqualityReduced)
m2 = lm(Ozone ~ sTemp + sWind, data = airqualityReduced)
m3 = lm(Ozone ~ sTemp + sWind + sSolar.R, data = airqualityReduced)

anova (m1,m2,m3)

m2 = lm(Ozone ~ sTemp + sWind + I(sWind^2), data = airqualityReduced)
anova(m1,m2)

fit <- lm(Ozone ~ sTemp + sWind + sSolar.R, data = airquality)
x = summary(aov(fit))
print(x)
x[[1]]$`Sum Sq` / sum(x[[1]]$`Sum Sq`)

fit <- lm(Ozone ~ sWind + sTemp + sSolar.R, data = airquality)
summary(aov(fit))

m1 = lm(Ozone ~ sWind, data = airqualityReduced)
summary(m1)
m2 = lm(Ozone ~ sTemp + sWind, data = airqualityReduced)
summary(m2)

fit <- lm(Ozone ~ sTemp + sWind + sSolar.R, data = airquality)
summary(fit)

car::Anova(fit, type = "II")
car::Anova(fit, type = "III")


############### Mixed Models ####################################


library(mlmRev)
library(effects)

str(Exam)

mod0 = lm(normexam ~ standLRT + sex , data = Exam)
plot(allEffects(mod0))

boxplot(residuals(mod0) ~ Exam$school)

mod0 = lm(normexam ~ standLRT + sex + school , data = Exam)
plot(allEffects(mod0))
summary(mod0)
boxplot(residuals(mod0) ~ Exam$school)

library(lme4)
library(lmerTest)

mod1 = lmer(normexam ~ standLRT + sex + (1|school), data = Exam)
summary(mod1)
ranef(mod1)

FE = coef(mod0)
FE = FE[-c(2,3)]
FE[1] = 0

RE = ranef(mod1)
RE = RE$school$`(Intercept)` - 0.402927181

plot(FE, RE)
abline(0,1)

# slope differs between schools?

mod0 = lm(normexam ~ standLRT * school + sex , data = Exam)
summary(mod0)

mod1 = lmer(normexam ~ standLRT + sex + (standLRT|school), data = Exam)
summary(mod1)
ranef(mod1)


mod0 = lm(normexam ~ standLRT + sex , data = Exam)
plot(allEffects(mod0))

mod1 = lmer(normexam ~ standLRT + sex + (standLRT|school), data = Exam)
summary(mod1)
plot(allEffects(mod0))

str(plantHeight)
length(table(plantHeight$Family))

fit <- lm(loght ~ temp, data = plantHeight)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

fit <- lmer(loght ~ temp + (1|Family), data = plantHeight)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

fit <- lmer(loght ~ stemp  + (stemp |Family), data = plantHeight)
summary(fit)
plot(allEffects(fit, partial.residuals = T))


# residual checks

mod1 = lmer(normexam ~ standLRT + sex +  (1 | school), data = Exam)
summary(mod1)

plot(mod1)
qqnorm(residuals(mod1))
qqline(residuals(mod1))

library(DHARMa)
res <- simulateResiduals(mod1, plot = T)
plot(res, quantreg = T)
residuals(res)

x = ranef(mod1)
qqnorm(x$school$`(Intercept)`)
qqline(x$school$`(Intercept)`)

plot(mod1, resid(., scaled=TRUE) ~ standLRT | school, abline = 0)

mod2 = lmer(normexam ~ standLRT + sex +  (standLRT | school), data = Exam)

plot(mod2, resid(., scaled=TRUE) ~ standLRT | school, abline = 0)


###### ANOVA ##########

mod1 = lmer(normexam ~ standLRT + sex +  (1 | school), data = Exam)
summary(mod1)
plot(allEffects(mod1))

library(MuMIn)
r.squaredGLMM(mod1) 

# Careful: for p-values on RE ANOVA you need to approximate DF

library(ggdag)
library(ggplot2)
theme_set(theme_dag())
dag = confounder_triangle(x = "Coffee", y = "Lung Cancer", z = "Smoking") 
ggdag(dag, text = FALSE, use_labels = "label")

smoking <- runif(50)
Coffee <- smoking + rnorm(50, sd = 0.2)
LungCancer <- smoking + rnorm(50, sd =0.2)
fit <- lm(LungCancer ~ Coffee)
summary(fit)
plot(LungCancer ~ Coffee)
abline(fit)

ggdag_dconnected(dag, text = FALSE, use_labels = "label")

ggdag_dconnected(dag, text = FALSE, use_labels = "label", controlling_for = "z")

smoking <- rep(0.5, 50)
Coffee <- smoking + rnorm(50, sd = 0.2)
LungCancer <- smoking + rnorm(50, sd =0.2)
fit <- lm(LungCancer ~ Coffee)
summary(fit)
plot(LungCancer ~ Coffee)
abline(fit)


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

dag = collider_triangle(x = "Coffee", y = "Lung Cancer", m = "Nervousness") 
ggdag(dag, text = FALSE, use_labels = "label")

set.seed(123)
Coffee <- runif(100)
LungCancer <- runif(100)
nervousness = Coffee + LungCancer + rnorm(100, sd = 0.1)

fit1 <- lm(LungCancer ~ Coffee + nervousness)
summary(fit1)

# Collider bias / overadjustment 

fit1 <- lm(LungCancer ~ Coffee)
summary(fit1)

The following data set contains information about life satisfaction (lebensz_org) in Germany, based on the socio-economic panel.

library(EcoData)
?soep

plot(lebensz_org ~ einkommenj1, data = soep)
fit <- lm(lebensz_org ~ einkommenj1, data = soep)  
summary(fit)
abline(fit)


fit <- lm(lebensz_org ~ einkommenj1 + 
                        sex + 
                        alter + 
                        gesund_org + 
                        syear + 
                        anz_kind, data = soep)  
summary(fit)
abline(fit, col = "red")

car::Anova(fit)

# Main message: confounder in, collider out, mediator: depending on whether you want total or direct effects

# If a predictor variable doesn't have a relationhip with the other predictors, we can decide later if we put it 



# Model selection 


# Model 1
m1 = lm(Ozone ~ Wind , data = airquality)

# Model 2
m2 = lm(Ozone ~ Wind + Temp, data = airquality)

# LRT
anova(m1, m2)

AIC(m1)
AIC(m2)

data(mammals, package="MASS")
mammals.glm <- glm(log(brain) ~ log(body), data = mammals)
cv.err <- cv.glm(mammals, mammals.glm, K = 10)$delta

library(MASS)
stepAIC()

quine.hi <- aov(log(Days + 2.5) ~ .^4, quine)
quine.nxt <- update(quine.hi, . ~ . - Eth:Sex:Age:Lrn)
quine.stp <- stepAIC(quine.nxt,
                     scope = list(upper = ~Eth*Sex*Age*Lrn, lower = ~1),
                     trace = T)
quine.stp$anova



