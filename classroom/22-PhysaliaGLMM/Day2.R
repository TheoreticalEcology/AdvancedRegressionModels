
mod1 = lmer(normexam ~ standLRT + sex +  (1 | school), data = Exam)
summary(mod1)

plot(mod1)

qqnorm(residuals(mod1))
qqline(residuals(mod1))

x = ranef(mod1)

qqnorm(x$school$`(Intercept)`)
qqline(x$school$`(Intercept)`)

# to check if this is noise or not, a test is useful
shapiro.test(x$school$`(Intercept)`)

# general comment: hypothesis tests are MAINLY useful to check if there is a pattern at all

# of course, all that was said for lm also applies, e.g. you shoudl fit residuals against predictor

plot(residuals(mod1) ~ Exam$standLRT)
boxplot(residuals(mod1) ~ Exam$sex)






# random intercept on hives
+ (1|Hive)



fit<-lm(log(Spobee + 1) ~ Infection  + BeesN, data = bees)
par(mfrow = c(2,2))
plot(fit)

hist(bees$Spobee)

bees$Infection

library(lme4)
fit <- lmer(log(Spobee + 1) ~ Infection + BeesN + (1|Hive), data = bees)
summary(fit)

plot(fit)
qqnorm(residuals(fit))


x = ranef(fit)
qqnorm(x$Hive$`(Intercept)`)

# to check if you should include a random slope, plot residuals ~ fitted or prdictor for each group

# to get help, ?plot.merMod

plot(fit, 
     resid(., scaled=TRUE) ~ fitted(.) | Hive, 
     abline = 0)

# predictions and residuals 

?predict.merMod

predict(fit, re.form = NULL) # conditional prediction
predict(fit, re.form = ~ 0) # unconditional or marginal
predict(fit, re.form = ~ (1|Hive)) # condition on Hive 

# DHARMa residuals can be calculated conditionally or unconditionally

library(DHARMa)
# unconditional residuals
res <- simulateResiduals(fit, plot = T)

# conditional residuals
res <- simulateResiduals(fit, plot = T, re.form = NULL)


library(EcoData)
library(lme4)
fit <- lmer(log(Spobee + 1) ~ Infection + BeesN + (1|Hive), data = bees)
summary(fit)

x = ranef(fit)
qqnorm(x$Hive$`(Intercept)`)

y = aggregate(bees$Infection ~ bees$Hive, FUN = mean)

plot(x$Hive$`(Intercept)` ~ y$`bees$Infection`)



library(EcoData)
library(lme4)
fit <- lmer(log(Spobee + 1) ~ Infection + BeesN + (1|Hive), data = bees)
summary(fit)
library(lmerTest)

### Fixed effects

# p-values on fixed effect estimates are fine
# AIC selection / model selection is also approximate fine as long as the RE structure doesn't change

### Random Effects

# p-values on RE structure / ANOVA 

bees$lSpobee = log(bees$Spobee + 1)

m1 = lm(lSpobee ~ Infection + BeesN , data = bees)
m2 = lmer(lSpobee ~ Infection + BeesN + (1|Hive), data = bees)

# default AIC and default ANOVA do not have correct df
AIC(m1)
AIC(m2) # 1 df for RE, 
logLik(m2)
(- 2 LogLik + 2 par)

# if you trust the Satterthwaite, you can use their ANOVA, this has df correction
lmerTest::ranova()

# general method: parameteric bootstrap 

# in general, we can get a CI / p-values on anything using the bootMer function in lme4

bootMer()

library(DHARMa)
simulateLRT(m1,m2)

# bootstrap predictions

pred <- function(m) predict(m,re.form = ~0)
pred(m2)

boot <- bootMer(m2, pred, nsim = 100, re.form = ~0)

# REML vs ML

m2 = lmer(lSpobee ~ Infection + BeesN + (1|Hive), data = bees)

# Switch it on, unless you want to do AIC or LRTs 

m2 = lmer(lSpobee ~ Infection + BeesN + (1|Hive), REML = F, data = bees)

# R2 partitioning 

library(MuMIn)
r.squaredGLMM(m2) 


bees$lSpobee

x = as.matrix(simulate(m2, 100))
hist(x[1,])
abline(v = bees$lSpobee[1], col = "red", lwd = 3)
mean(x[1,] < bees$lSpobee[1])

simulateResiduals(m2, plot = T)



Summary of the p-value strategies

# 1) load the lmerTest package
# 2) don't use AIC
# 3) exact CIs with lme4::bootMer
# 4) test on REs with DHARMa::simulatedLRT
# 5) R2 with MuMIn::r.squaredGLMM


library(lme4)
library(glmmTMB)
library(EcoData)

gpa$sOccasion = scale(gpa$occasion)
gpa$nJob = as.numeric(gpa$job)

fit <- lmer(gpa ~ sOccasion + (1|student), data = gpa)
fitML <- lmer(gpa ~ sOccasion + (1|student), REML = F, data = gpa)
summary(fit)

plot(fit, 
     resid(., scaled=TRUE) ~ fitted(.) | student, 
     abline = 0)

# slope + intercept model
fit2 <- lmer(gpa ~ sOccasion + (sOccasion|student), data = gpa)
fit2ML <- lmer(gpa ~ sOccasion + (sOccasion|student), REML = F, data = gpa)
summary(fit2)

simulateLRT(fitML, fit2ML)

plot(fit2, 
     resid(., scaled=TRUE) ~ fitted(.) | student, 
     abline = 0)


plot(fit2)

simulateResiduals(fit2, plot = T, re.form = NULL)


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

fit2 <- lm(LungCancer ~ Coffee + smoking)
summary(fit2)

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

fit = lm(rich ~ cover + abiotic + elev + age, data = keeley)
summary(fit)

fit = lm(rich ~ cover + abiotic + elev + age + firesev, data = keeley)
summary(fit)

# General strategy: 

# Start with your base question

plot(lebensz_org ~ sqrt(einkommenj1), data = soep)

fit <- lm(lebensz_org ~ sqrt(einkommenj1), data = soep)
summary(fit)
library(effects)
plot(allEffects(fit))

str(soep)

# General strategy:

# Start with your base question

1) lebensz_org ~ sqrt(einkommenj1)

# Go through the other variables, and decide if it's a confounder or collider or mediator 

# sex -> confounder -> adjust for it!






plot(gesund_org ~ einkommenj1, data = soep)
fit <- lm(gesund_org ~ einkommenj1, data = soep)
summary(fit)
