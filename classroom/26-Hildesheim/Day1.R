
# Startup 
# -> create ONE folder
# -> create Rstudio project and sensible project structure
# -> learn Git
# ( -> Setup AI reviewer ) <- only feedback, don't let it work alone !!!
# Reproducible workflow, for most setups, Rmd / qmd recommended

plot(Ozone ~ Wind, data = airquality)

fit = lm(Ozone ~ Wind, data = airquality)
summary(fit)

library(effects)
plot(allEffects(fit, partial.residuals = T))

par(mfrow = c(2,2))
plot(fit)

library(DHARMa)
res <- DHARMa::simulateResiduals(fit)
plot(res)
plot(res, quantreg = F)


fit = lm(log(Ozone) ~ Wind, data = airquality)
fit = lm(Ozone ~ poly(Wind, 2), data = airquality)
library(mgcv)
fit = gam(Ozone ~ s(Wind), data = airquality)
plot(fit)
summary(fit)

# gams adjust complexity to data size
fit = gam(Ozone ~ s(Wind), data = airquality[1:20, ])
plot(fit)

# in the end, check residual again!

### Categorical variables ### 

boxplot(weight ~ group, data = PlantGrowth)

fit = lm(weight ~ group, data = PlantGrowth)
summary(fit)
plot(allEffects(fit))

fit = lm(weight ~ group-1, data = PlantGrowth)
summary(fit)
plot(allEffects(fit))

fit = lm(weight ~ group, data = PlantGrowth)
anov = aov(fit)
summary(anov)
TukeyHSD(anov)


# residual checks
res <- DHARMa::simulateResiduals(fit)
plot(res)
plotResiduals(res, form = ~ group)

### multiple ###

airquality$fMonth = factor(airquality$Month)
fit = lm(Ozone ~ Temp + Wind + Solar.R + fMonth, data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))


plot(Ozone ~ Wind , data = airquality)
fit = lm(Ozone ~ Wind , data = airquality)
summary(fit)
abline(fit)

# The effect of Wind on Ozone was -5.5

fit = lm(Ozone ~ Wind +  Temp, data = airquality)
summary(fit)

# The effect of Wind, corrected / adjusted for Temperature, on Ozone was -3.0

plot(Temp ~  Wind, data = airquality)
cor(airquality$Temp, airquality$Wind)

pairs(airquality)

fit = lm(Ozone ~  Wind +  Temp, data = airquality)
plot(allEffects(fit, partial.residuals = T))

airquality$sWind = scale(airquality$Wind)
airquality$sTemp = scale(airquality$Temp)
fit = lm(Ozone ~  sWind +  sTemp, data = airquality)
plot(allEffects(fit, partial.residuals = T))
summary(fit)


fit = lm(Ozone ~  Wind * Temp, data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))
# Interactions on uncentered predictors dangerous!!!!
# because main effects are calculated at zero of the other variables

fit = lm(Ozone ~  sTemp * sWind, data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

# discussion - can / should you transform predictors or response
hist(airquality$Ozone, breaks = 50) # response tells you nothing
hist(airquality$Solar.R, breaks = 50) # predictors: no fundamental 
# requirement that predictors need to be symmetric / flat, but experience
# is that its often useful 


# residuals for the multiple regression
fit = lm(Ozone ~  Wind + Temp, data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

par(mfrow = c(2,2))
plot(fit)

res = simulateResiduals(fit)
plot(res)
plotResiduals(res, form = ~ Wind)
plotResiduals(res, form = ~ Wind, rank = F)
plotResiduals(res, form = ~ .)

# NAs

nrow(airquality)
nrow(airquality) - 37

options("na.action")
options(na.action = "na.fail")

fit = lm(Ozone ~  Wind + Temp, data = airquality)

summary(airquality)

rm(airquality)
image(is.na(t(airquality)), axes = F)
axis(3, at = seq(0,1, len = 6), labels = colnames(airquality))

plot(Ozone ~ Wind, data = airquality)

options(na.action = "na.omit")

# ANOVA (for the multiple regression)

fit = lm(Ozone ~ Wind + Temp, data = airquality)
anova(fit)

total = 45284 + 25886 + 53973
45284 / total
(45284 + 25886) / total

summary(fit)

# CAREFUL - this is a type I or sequential ANOVA 
anova(fit)

fit = lm(Ozone ~ Temp +  Wind, data = airquality)
anova(fit)
61033 / total

# Explanation: 

m0 = lm(Ozone ~ 1, data = airquality)
m1 = lm(Ozone ~ Wind, data = airquality)
summary(m1)
m2 = lm(Ozone ~ Wind + Temp, data = airquality)
summary(m2)

fit = lm(Ozone ~ Temp +  Wind, data = airquality)
car::Anova(fit, type = "II")
car::Anova(fit, type = "III")

fit = lm(Ozone ~ Temp *  Wind, data = airquality)
car::Anova(fit, type = "II")
car::Anova(fit, type = "III")

# IF you go for type III, scaling and coding of categorical variables changes result!

# discussion - Experiment of Michael 
fit = lm(Traits ~ (Raingfall + Water + Ontogeny)^2, data = airquality)
# if we want first the overall effect of a variable, and then the added effect of the interaction go for type II

library(EcoData)
str(plantHeight)

library(EcoData)
plot(loght ~ temp, data = plantHeight)

fit = lm(loght ~ temp, data = plantHeight)
summary(fit)
res = simulateResiduals(fit, plot = T)
plot(allEffects(fit, partial.residuals = T))


boxplot(loght ~ growthform, data = plantHeight)
table(plantHeight$growthform)

plantHeight2 = plantHeight[!plantHeight$growthform %in% c("Fern", "Herb/Shrub"), ]

table(plantHeight2$growthform)

boxplot(loght ~ growthform, data = plantHeight2)
fit = lm(loght ~ growthform, data = plantHeight2)
res = simulateResiduals(fit, plot = T)
summary(fit)
plot(allEffects(fit, partial.residuals = T))
TukeyHSD(aov(fit))


fit = lm(loght ~ scale(temp) + scale(NPP), data = plantHeight)
summary(fit)
car::Anova(fit, type = "II")

fit = lm(loght ~ scale(temp) * growthform, data = plantHeight2)
summary(fit)
plot(allEffects(fit, partial.residuals = T))
car::Anova(fit, type = "II")
car::Anova(fit, type = "III")

fit = lm(loght ~ scale(temp) * growthform - scale(temp) - 1, data = plantHeight2)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

# ANOVA DOESN WORK WITH THIS - temp

fit = lm(loght ~ temp * lat, data = plantHeight2)
summary(fit)


# Mixed models 

library(mlmRev)
library(effects)

mod0 = lm(normexam ~ standLRT + sex , data = Exam)
plot(allEffects(mod0))
summary(mod0)


mod1a = lm(normexam ~ standLRT + sex + school - 1, data = Exam)
plot(allEffects(mod1a))
summary(mod1a)

library(lme4)
library(lmerTest)
mod1b = lmer(normexam ~ standLRT + sex + (1|school), data = Exam)
plot(allEffects(mod1b))
summary(mod1b)
ranef(mod1b)

# simulated F/chi2 test
mod1 = lmer(normexam ~ standLRT  + (1|school), data = Exam)
mod2 = lmer(normexam ~ standLRT + sex + (1|school), data = Exam)
DHARMa::simulateLRT(mod1,mod2)

anova(mod2)
car::Anova(mod2, type = "III", test.statistic="F")






