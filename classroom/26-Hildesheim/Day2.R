

# RE Structures

+ (1|site) + (1|year) + (1|species) # crossed 
+ (1|site / subplot) # identical to (1|site) + (1|subplot)
+ (1|site/species) different to (1|site) + (1|species)

# assume 10 sites, 3 years
+ (1|site) + (1|year) # one intercept per year one per site
+ (1|site/year) # each site gets an intercept. And then each site/year compbination gets and intercept

# https://theoreticalecology.github.io/AdvancedRegressionModels/2C-RandomEffects.html#sec-re-honeybee

library(EcoData)
str(bees)
plot(Spobee ~ Infection, data = bees)

library(lme4)
fit = lmer(log(Spobee+1) ~ Infection + (1|Hive), data = bees)
summary(fit)
plot(fit)
plot(fit, resid(., scaled=TRUE) ~ Infection | Hive, abline = 0)
plot(allEffects(fit, partial.residuals = T))


# Does BeesN add something?
fit = lmer(log(Spobee+1) ~ Infection + BeesN + (1|Hive), data = bees)
summary(fit)

library(lme4)
mod1 = lmer(normexam ~ standLRT + sex +  (1 | school), data = Exam)
summary(mod1)
plot(allEffects(mod1))


# random slope model 
mod2 = lmer(normexam ~ standLRT + sex +  (standLRT | school), data = Exam)
summary(mod2)
plot(allEffects(mod2))


# residuals 
plot(mod1)
qqnorm(residuals(mod1))
# check evidence for random slopes
plot(mod1, resid(., scaled=TRUE) ~ standLRT | school, abline = 0)
x = ranef(mod1)
qqnorm(x$school[,1])

library(DHARMa)
res = simulateResiduals(mod1)
plot(res)


library(ggdag)
library(ggplot2)
theme_set(theme_dag())
dag = confounder_triangle(x = "Coffee", y = "Lung Cancer", z = "Smoking") 
ggdag(dag, text = FALSE, use_labels = "label")


smoking <- runif(50)
Coffee <- smoking + rnorm(50, sd = 0.2)
LungCancer <- smoking + rnorm(50, sd =0.2)
fit1 <- lm(LungCancer ~ Coffee)
summary(fit1)
plot(LungCancer ~ Coffee)
abline(fit1)

fit2 <- lm(LungCancer ~ Coffee + smoking)
summary(fit2)

abline(fit1)
xs <- seq(min(Coffee), max(Coffee), length.out = 50)
lines(xs, predict(fit2, data.frame(Coffee = xs, smoking = mean(smoking))), col = "red")
legend("topleft", c("simple regression", "multiple regression"), col = c(1,2), lwd = 1)


dag = collider_triangle(x = "Coffee", y = "Lung Cancer", m = "Nervousness") 
ggdag(dag, text = FALSE, use_labels = "label")

set.seed(123)
Coffee <- runif(100)
LungCancer <- runif(100)
nervousness = Coffee + LungCancer + rnorm(100, sd = 0.1)

plot(LungCancer ~ Coffee)

fit1 <- lm(LungCancer ~ Coffee )
summary(fit1)

fit2 <- lm(LungCancer ~ Coffee + nervousness)
summary(fit2)

library(EcoData)
?soep

plot(lebensz_org ~ einkommenj1 + sex + alter + bildung  , data = soep)
fit <- lm(lebensz_org ~ scale(sqrt(einkommenj1)) + sex + alter + bildung, data = soep)
summary(fit)
plot(allEffects(fit))


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

airquality2 = airquality[complete.cases(airquality), ]

m1 = lm(Ozone ~ Wind, data = airquality2)
m2 = lm(Ozone ~ Wind + Temp, data = airquality2)
m3 = lm(Ozone ~ Wind + Temp + Solar.R, data = airquality2)

# can't use R2 because R2 always better
summary(m2)
summary(m3)

# can use ANOVA forward 
anova(m2,m3)

# Information Criteria = Model Fit penalized by complexity
# AIC = -2 Log(Lik) + 2 * parameters
# AIC assymtotically equivalent to CV 
AIC(m2)
AIC(m3)

mFull = lm(Ozone ~ Wind + Temp + Solar.R + as.factor(Month), data = airquality2)
library(MASS)
stepAIC(mFull)
options(na.action = "na.fail")
library(MuMIn)
allModels = dredge(mFull)
allModels


library(EcoData); library(MASS)
dat = soep[complete.cases(soep[,c("lebensz_org","einkommenj1","syear","sex",
                                  "alter","anz_pers","bildung","erwerb","gesund_org")]), ]

fullModel = lm(lebensz_org ~ (sqrt(einkommenj1) + syear + sex + alter +
                                anz_pers + bildung + erwerb + gesund_org)^2, data = dat)

summary(fullModel)
predModel = stepAIC(fullModel)   # AIC-optimal predictive model
summary(predModel)

# Important Caveats of Model selection: 

set.seed(123)
x1 = runif(100)
x2 = 0.8 * x1 + 0.2 *runif(100)
y = x1 + x2 + rnorm(100)

m1 = lm(y ~ x1 + x2)
summary(m1)

m2 = MASS::stepAIC(m1)
summary(m2)

# Model selection destroys causality, don't select out confounders

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

# p-values of the final model have to be corrected (post-selection inference)


library(EcoData)
#str(birdfeeding)
plot(feeding ~ attractiveness, data = birdfeeding)

fit<-lm(feeding ~ attractiveness, data = birdfeeding)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

hist(rpois(1000, lambda = 0.2), breaks = 20)

fit<-glm(feeding ~ attractiveness, data = birdfeeding, family = "poisson")
summary(fit)

# y = link^-1 (1.47 * 0.15 * attractivenes)
exp(1.47459)
plot(allEffects(fit, partial.residuals = T))

library(DHARMa)
par(mfrow = c(2,2))
plot(fit) # dont do this , wrong for GLMs
res <- simulateResiduals(fit)
plot(res)
testDispersion(res)
testZeroInflation(res)

# logistic regression 

library(EcoData)
str(elk)

fit <- lm(presence ~ dist_roads, data = elk)
summary(fit)
par(mfrow = c(2,2))
plot(fit) # dont do this , wrong for GLMs


fit <- glm(presence ~ dist_roads + ruggedness + habitat + dem , data = elk, family = "binomial")
summary(fit)
plot(allEffects(fit))

curve(plogis,-5,5)
pairs(elk)

res = simulateResiduals(fit)
plot(res)
plotResiduals(fit, form = ~ .)


m1 <- glm(presence ~ dist_roads + poly(ruggedness,2) + habitat + poly(dem,2) , data = elk, family = "binomial")
summary(m1)
res = simulateResiduals(m1)
plot(res)
plotResiduals(res, form = ~ .)
summary(m1)
plot(allEffects(m1))

library(mgcv)
m1 <- gam(presence ~ dist_roads + s(ruggedness) + habitat + s(dem) , 
          data = elk, family = "binomial")
res = simulateResiduals(m1)
plot(res)
plotResiduals(res, form = ~ .)
summary(m1)


library(glmmTMB) # for the Owls data

m1 = glm(SiblingNegotiation ~ FoodTreatment*SexParent + offset(log(BroodSize)),
         data = Owls , family = poisson)
summary(m1)
plot(allEffects(m1))

res = simulateResiduals(m1)
plot(res)
testDispersion(res)

# nest as RE
table(Owls$Nest)


m2 = lme4::glmer(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)), data = Owls , family = poisson)

m2 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)), data = Owls , family = poisson)

summary(m2)

res = simulateResiduals(m2)
plot(res)
testDispersion(res)


m3 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)), data = Owls , family = nbinom1)
summary(m3)

res = simulateResiduals(m3)
plot(res)
plotResiduals(res, form = ~ FoodTreatment + SexParent, asFactor = F)
testDispersion(res)
testZeroInflation(res)

m4 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)),
             ziformula = ~ FoodTreatment*SexParent,
             data = Owls , family = nbinom1)

summary(m4)

res = simulateResiduals(m4)
plot(res)
testDispersion(res)
testZeroInflation(res)

# just for show, no indication in the residuals 
m5 = glmmTMB(SiblingNegotiation ~ FoodTreatment*SexParent + (1|Nest) + offset(log(BroodSize)),
             ziformula = ~ FoodTreatment,
             dispformula = ~ FoodTreatment,
             data = Owls , 
             family = nbinom1)

summary(m5)

plot(allEffects(m5))




