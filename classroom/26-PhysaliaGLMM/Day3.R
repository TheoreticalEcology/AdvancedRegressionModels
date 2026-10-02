m = lmer(normexam ~ standLRT + sex +  (1 | school), data = Exam)

summary(m)

?predict.merMod
predict(m, re.form = NULL) # include all random effects, default
predict(m, re.form = ~0) # include no random effects, identical re.form = NA
predict(m, re.form = ~ (1|school)) # condition ONLY on 1|school


m = lm(normexam ~ standLRT + sex +  school, data = Exam)
summary(m)

# REML
m = lmer(normexam ~ standLRT + sex +  (1 | school), data = Exam)

# ANOVA 
library(MuMIn)
r.squaredGLMM(m) 
# 30% R2 fixed, 10% RE, 60% residual

lmerTest::ranova(m)

# ANOVA / R2: fit model, compare data variance against residual variance

var(Exam$normexam) # 0.9978891

var(Exam$normexam - predict(m, re.form = NULL))
1 - 55/100
var(Exam$normexam - predict(m, re.form = ~0))
1 - 64/100

ranef(m)


library(ggdag)
library(ggplot2)
theme_set(theme_dag())
dag = confounder_triangle(x = "Coffee", y = "Lung Cancer", z = "Smoking") 
ggdag(dag, text = FALSE, use_labels = "label")

ggdag_dconnected(dag, text = FALSE, use_labels = "label")

smoking <- runif(50)
Coffee <- smoking + rnorm(50, sd = 0.2)
LungCancer <- smoking + rnorm(50, sd =0.2)
fit <- lm(LungCancer ~ Coffee)
plot(LungCancer ~ Coffee)
abline(fit)
summary(fit)

ggdag_dconnected(dag, text = FALSE, use_labels = "label", controlling_for = "z")

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

library(piecewiseSEM)
theme_set(theme_dag())

dag <- dagify(rich ~ distance + elev + abiotic + age + hetero + firesev + cover,
              firesev ~ elev + age + cover,
              cover ~ age + elev + abiotic ,
              exposure = "cover",
              outcome = "rich"
)

ggdag(dag)


?soep


plot(lebensz_org  ~ einkommenj1, data = soep)
fit <- lm(lebensz_org  ~ einkommenj1, data = soep)
summary(fit)
plot(allEffects(fit))

# Lederer et al., 2018, “Control of Confounding and Reporting of Results in Causal Inference Studies. Guidance for Authors from Editors of Respiratory, Sleep, and Critical Care Journals” which is available here.

# Another great paper is Laubach, Z. M., Murray, E. J., Hoke, K. L., Safran, R. J., & Perng, W. (2021). A biologist’s guide to model selection and causal inference. Proceedings of the Royal Society B, 288(1943), 20202815.

fit <- lm(lebensz_org  ~ einkommenj1 + sex + alter + bildung, data = soep)
summary(fit)

fit <- lmer(lebensz_org  ~ einkommenj1 + sex + alter + bildung 
            + (1|id) + (1|syear), data = soep)
summary(fit)


# Model selection 

set.seed(123)

x = runif(100)
y = 0.25 * x + rnorm(100, sd = 0.3)

summary(lm(y~x))

xNoise = matrix(runif(8000), ncol = 80)
dat = data.frame(y=y,x=x, xNoise)

fullModel = lm(y~., data = dat)
summary(fullModel)


set.seed(42)
X = runif(100)
P = runif(100)
Y = 0.8*X + 10*P + rnorm(100, sd = 0.5)

summary(lm(Y~X))
summary(lm(Y~X+P))

# how to find out if a variable is important? 

m1 = lm(Ozone ~ Temp, data = airquality)
m2 = lm(Ozone ~ Temp + Wind, data = airquality)

anova(m1,m2)
library(DHARMa)
simulateLRT(m1,m2)

# AIC = - 2 LogLikelihood + 2*NumParameters
# Lower is better ! 
AIC(m1)
AIC(m2)
# no p-value, but AIC differences of 2-5 are meaningful 

# WARNINGS: 

m1 = lm(normexam ~ standLRT + sex , data = Exam)
m2 = lmer(normexam ~ standLRT + sex +  (1 | school), data = Exam, REML = F)

AIC(m1) + 2 * logLik(m1)
AIC(m2) + 2 * logLik(m2) # 1 df for RE
# This is wrong - can't use AIC to select on REs 

# Same is true for ANOVA - only valid if you have a specialized ANOVA for REs

# These are specialized ANOVAs that also work on RE structure
lmerTest::ranova(m2)
DHARMa::simulateLRT(m1,m2)

# generally considered OK to select on fixed effects in mixed models with AIC
m1 = lmer(normexam ~ standLRT + (1 | school), data = Exam, REML = F)
m2 = lmer(normexam ~ standLRT + sex + (1 | school), data = Exam, REML = F)
AIC(m1)
AIC(m2)
# BUT WARNING: RE sd should stay similar 

# GENERAL WARNINGS #

# Model selection goes against causal correction

set.seed(123)
x1 = runif(100)
x2 = 0.8 * x1 + 0.2 *runif(100)
y = x1 + x2 + rnorm(100)

m1 = lm(y ~ x1 + x2)
summary(m1)

m2 = MASS::stepAIC(m1)
summary(m2)

# Lesson: don't model select away important confounders

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

# lots of model selection leads to biased p-values and increased type I error
# p-values after MS need to be corrected for multiple testing
# post-selection inference

#  GLMs 

library(EcoData)
plot(feeding ~ attractiveness , data = birdfeeding)

lmfit = lm(feeding ~ attractiveness , data = birdfeeding)
summary(lmfit)

plot(allEffects(lmfit, partial.residuals = T))

glmfit = glm(feeding ~ attractiveness , data = birdfeeding, 
             family = poisson(link = "identity"))
summary(glmfit)

# linear predictor
y = 1.47 + 0.14 * attractiveness
# response
log(predictions) = y 
predictions = exp(y) # inverse link function 
# data distribution 
data = poisson(predictions)

y = 1.47 + 0.14 * 1
exp(1.61)
exp(1.47)

plot(allEffects(glmfit))



library(EcoData)
titanic$pclass = as.factor(titanic$pclass)

fit = lm(survived ~ sex * age, data = titanic)
summary(fit)

par(mfrow=c(2,2))
plot(fit)
plot(allEffects(fit, partial.residuals = T))

fit = glm(survived ~ sex * age - 1, data = titanic, family = binomial)
summary(fit)
curve(plogis, -5,5)

plogis(0.5)
plogis(0)

plot(allEffects(fit))

?predict.glm
predict(fit, type = "link")[1] # based on regression formula
predict(fit, type = "response")[1] # based on regression formula + link

# R2 for GLM
summary(fit)
# deviance = -2LogLik

# McFadden R2 = 1 - ResidualDeviance / NullDeviance
1 - 1083/1450



