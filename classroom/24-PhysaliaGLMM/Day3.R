# Example ANOVA where we include a variable without effect

m0 <- lm(Ozone ~ Temp + Wind , data = airquality)
summary(m0)
m1 <- lm(Ozone ~ Temp + Wind + Day, data = airquality)
summary(m1)

library(DHARMa)
simulateLRT(m0, m1)

summary(aov(m1))

# What to do with NAs?

summary(airquality)

fit = lm(Ozone ~ Wind + Temp , data = airquality)
summary(fit)

x = model.frame(fit)
str(x)
str(airquality)

options("na.action")
fit = lm(Ozone ~ Wind + Temp , data = airquality, na.action = "na.fail")

options(na.action = "na.fail") # if you want to be conservative in your analysis set this setting, then you have to deal with NAs yourself!

m1 = lm(Ozone ~ Wind  , data = airquality)
m2 = lm(Ozone ~ Wind + Temp + Solar.R, data = airquality)
anova(m1,m2)
AIC(m1)
AIC(m2)

options(na.action = "na.omit")

image(is.na(t(airquality)), axes = F)
axis(3, at = seq(0,1, len = 6), labels = colnames(airquality))

library(missRanger)

airqualityImp <- missRanger(airquality)

image(is.na(t(airqualityImp)), axes = F)
axis(3, at = seq(0,1, len = 6), labels = colnames(airqualityImp))

# Model selection - Causal inference 

# code for figures copied from https://theoreticalecology.github.io/AdvancedRegressionModels/3B-CausalInference.html

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


library(piecewiseSEM)
theme_set(theme_dag())

dag <- dagify(rich ~ distance + elev + abiotic + age + hetero + firesev + cover,
              firesev ~ elev + age + cover,
              cover ~ age + elev + abiotic ,
              exposure = "cover",
              outcome = "rich"
)

ggdag(dag)

ggdag_paths(dag)

ggdag_adjustment_set(dag)
ggdag_adjustment_set(dag, effect="direct")


fit = lm(rich ~ cover + abiotic + elev + firesev + age, data = keeley)
summary(fit)

library(piecewiseSEM)

mod = psem(
  lm(rich ~ distance + elev + abiotic + age + hetero + firesev + cover, data = keeley),
  lm(firesev ~ elev + age + cover, data = keeley), 
  lm(cover ~ age + elev + hetero + abiotic, data = keeley)
)

summary(mod)
plot(mod)

# First present the raw correlation, but note that this 
# should not be interpreted as the effect of income on
# happiness 

fit <- lm(lebensz_org^2 ~ sqrt(einkommenj1), data = soep)


fit <- lm(lebensz_org^2 ~ sqrt(einkommenj1) + syear + sex + alter + anz_pers + 
            bildung + erwerb + gesund_org, data = soep)
summary(fit)

par(mfrow = c(2,2))
plot(fit)

# https://theoreticalecology.github.io/AdvancedRegressionModels/3C-ModelSelection.html

set.seed(123)

x = runif(100)
y = 0.25 * x + rnorm(100, sd = 0.3)
fit = lm(y~x)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

# simulateResiduals(fit, plot = T)

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

# (automatic) model selection 

# Model 1
m1 = lm(Ozone ~ Wind , data = airquality)

# Model 2
m2 = lm(Ozone ~ Wind + Temp, data = airquality)

# LRT
anova(m1, m2)

# Akaike Information Criterion 
# AIC = - 2 LogLik + 2k (k = parameter)
# AIC asymptotically identical to cross-validation
# optimizes predictive error on independent data 
AIC(m1)
AIC(m2)

# AIC differences of 2-5-10 are considered meaningful!

# Note: there are other information criteria, such as BIC, ... 
# Advantage AIC: also works for non-nested models 

set.seed(123)
x1 = runif(100)
x2 = 0.8 * x1 + 0.2 *runif(100)
y = x1 + x2 + rnorm(100)

m1 = lm(y ~ x1 + x2)
summary(m1)

m2 = MASS::stepAIC(m1)
summary(m2)


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



# AIC on mixed models 

# as we don't know the parameters of RE, we can't do AIC selection on the REs, although R doesn't throw an error 

mod1 = lmer(normexam ~ standLRT + sex + (standLRT|school) , data = Exam)
mod0 = lmer(normexam ~ standLRT + sex + (1|school) , data = Exam)
AIC(mod0)
AIC(mod1)

# as we don't know the parameters of RE, we can't do AIC selection on the REs, although R doesn't throw an error 

mod0 = lmer(normexam ~ standLRT + (standLRT|school) , data = Exam)
mod1 = lmer(normexam ~ standLRT + sex + (standLRT|school) , data = Exam)

# if only fixed effects change, can use AIC with the argument that the RE structure doesn't change

library(EcoData)
library(MASS)
head(plantHeight)

fullModel <- lm(loght ~ growthform + lat + long + alt + temp + NPP + diurn.temp + rain.drym , data = plantHeight)
summary(fullModel)

selectedModel = stepAIC(fullModel, trace = 0)
summary(selectedModel)

plot(allEffects(fullModel, partial.residuals = T))

plot(allEffects(selectedModel, partial.residuals = T))

# GLMs 

library(EcoData)
str(birdfeeding)

plot(feeding ~ attractiveness, data = birdfeeding)


fit <- lm(feeding ~ attractiveness, data = birdfeeding)
summary(fit)

fit <- glm(feeding ~ attractiveness, data = birdfeeding, family = "poisson")
summary(fit)

# expectation value of bird at zero attractiveness 
exp(1.47459)

predict(fit) # linear predictor
predict(fit, type = "response")

plot(allEffects(fit, partial.residuals = T))

?titanic
titanic$pclass = as.factor(titanic$pclass)

fit <- lm(survived ~ sex * age, data = titanic)
summary(fit)

par(mfrow = c(2, 2))
plot(fit)

fit <- glm(survived ~ sex * age, data = titanic, family = "binomial")
summary(fit)

# female age 20

curve(plogis, -5,5)
plogis(0.493381 + 20* 0.022516)

plot(allEffects(fit))

par(mfrow = c(2, 2))
plot(fit)
plot(allEffects(fit, partial.residuals = T))

library(DHARMa)

x = model.frame(fit)

res <- simulateResiduals(fit, plot = T)
plotResiduals(res, form = titanic$pclass[as.numeric(rownames(x))])
plotResiduals(res, form = x$age)

anova(fit, test = "Chisq")
car::Anova(fit)

summary(fit)

# pseudo-R2

# McFadden = 1 - LogL(Model)/LogL(Null)
# 1-(1083.4/1414.6)


