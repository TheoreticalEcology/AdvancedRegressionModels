
# Prerequisites: install software as described here https://theoreticalecology.github.io/AdvancedRegressionModels/1A-GettingStarted.html

# Lecture notes openly available at https://theoreticalecology.github.io/AdvancedRegressionModels/

# Classroom scripts available at https://www.dropbox.com/scl/fo/lzijaez58vj530joz6dym/AFu5cJG5dWQUqzPQ671Zto0?rlkey=bo4ae2dmf50f81fp8axb2i9lc&e=1&dl=0

# https://theoreticalecology.github.io/AdvancedRegressionModels/2A-LinearRegression.html


str(airquality)
View(airquality)
?airquality
plot(Ozone ~ Temp, data = airquality)
plot(Ozone ~ Wind, data = airquality)

fit = lm(Ozone ~ Wind, data = airquality)
summary(fit)

library(effects)
plot(allEffects(fit, partial.residuals = T))

fit = lm(Ozone ~ Wind - 1, data = airquality)
summary(fit)

fit = lm(Ozone ~ poly(Wind,2), data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

# General principle
# fit = lm(Ozone ~ POLYNOMIAL(Wind), data = airquality)
# summary(fit)

fit = lm(Ozone ~ Wind, data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))

# paper: Effect of Wind is significant with - 5.5 (+/- 0.7 se) - translate this
# to 95% CI by multiplying with 1.96 ==> -5.5 with 95% CI 4.1 - 6.9

summary(airquality)
str(airquality)

# R2 = variance explained = 1 - Residual Variance / Raw variance

### Visualization ###

plot(Ozone ~ Wind, data = airquality)

fit = lm(Ozone ~ Wind, data = airquality)
summary(fit)

library(effects)
plot(allEffects(fit))
plot(allEffects(fit, partial.residuals = T))

### Regression Diagnostics ###

# in the summary()
# via the effects plots 

par(mfrow = c(2,2)) # opens a 2x2 plot panel for 4 plots
plot(fit) # regression diagnostics

# Effect of taking a data point out
fit = lm(Ozone ~ Wind, data = airquality[-1,])
summary(fit)

# Exercise 
fit = lm(log(Ozone) ~ log(Wind), data = airquality)
summary(fit)
plot(allEffects(fit, partial.residuals = T))
par(mfrow = c(2,2)) # opens a 2x2 plot panel for 4 plots
plot(fit) # regression diagnostics

# nonlinear fit through the data
library(mgcv)
fit = gam(Ozone ~ s(Wind), data = airquality)
summary(fit)
plot(fit)

# categorical predictors

str(PlantGrowth)
boxplot(weight ~ group, data = PlantGrowth)

fit = lm(weight ~ group, data = PlantGrowth)
summary(fit) # contrasts n.s., overall model significant

# Standard model is fitting treatment contrasts - compare to reference group

# switching to mean contrasts
fit = lm(weight ~ group - 1, data = PlantGrowth)
summary(fit) 

# back to treatement contrasts but with other reference group
PlantGrowth$newGroup = relevel(PlantGrowth$group, "trt1")
boxplot(weight ~ newGroup, data = PlantGrowth)
fit = lm(weight ~ newGroup, data = PlantGrowth)
summary(fit) 

anov = aov(fit)
summary(anov)

# Post-hoc tests

library(multcomp)
fit = lm(weight ~ group, data = PlantGrowth)
summary(fit)
tuk = glht(fit, linfct = mcp(group = "Tukey"))
summary(tuk)          # Standard display.

tuk.cld = cld(tuk)    # Letter-based display.
plot(tuk.cld)

p.adjust(0.08, method = "bonferroni", n = 3)

TukeyHSD()

library(EcoData)
plot(loght ~ temp, data = plantHeight)
plot(loght ~ as.factor(growthform), data = plantHeight)
relevel()

str(plantHeight)
model = lm(loght ~ temp, data = plantHeight)
plot(allEffects(model, partial.residuals =T ))
par(mfrow = c(2, 2))
plot(model)

par(mfrow = c(1, 1))
boxplot(loght ~ growthform, data = plantHeight)

plantHeight$fGrowthform = relevel(factor(plantHeight$growthform), ref = "Shrub")
boxplot(loght ~ fGrowthform, data = plantHeight)  
  
model2 = lm(loght ~ fGrowthform, data = plantHeight)
summary(model2)

library(multcomp)
tuk = glht(model2, linfct = mcp(fGrowthform = "Tukey"))
summary(tuk)          # Standard display.

tuk.cld = cld(tuk)    # Letter-based display.
par(mar = c(5,3,10,3))
plot(tuk.cld)


# multiple regression 

airquality$fMonth = factor(airquality$Month)
fit = lm(Ozone ~ Temp + Wind + Solar.R + fMonth, data = airquality)
plot(allEffects(fit, partial.residuals = T))
summary(fit)

fit = lm(Ozone ~ Wind , data = airquality)
summary(fit)

# -5.5. is different to -3.1 - effect estimates for Wind differ between
# multiple and simple regression

# Multiple regression wind effect in a paper: The effect of Wind, 
# ADJUSTED / CORRECTED for all other variables, is -3.1

# when is the adjusted effect different from the raw effect? 

plot(Temp ~ Wind, data =airquality)
pairs(airquality) # main collinearity between Temp and Wind

summary(airquality)
View(airquality)

# scaling variables for standardize effect sizes

airquality$sWind = scale(airquality$Wind)
airquality$sTemp = scale(airquality$Temp)
airquality$sSolar.R = scale(airquality$Solar.R)
fit = lm(Ozone ~ sTemp + sWind + sSolar.R + fMonth, data = airquality)
summary(fit)

# Interactions
fit = lm(Ozone ~ Temp * Wind, data = airquality)
plot(allEffects(fit))
summary(fit)

# Interactions should always be centered, scaling as above 
fit = lm(Ozone ~ sTemp * sWind, data = airquality)
plot(allEffects(fit))
summary(fit)

# visualization and predictions of multiple regression

# remember: for multiple regressions, use partial residuals not raw data 
# for comparison
plot(allEffects(fit, partial.residuals = T))
par(mfrow = c(2,2))
plot(fit)

fit = lm(Ozone ~ Temp + Wind + Solar.R + fMonth, data = airquality)
plot(allEffects(fit, partial.residuals = T))
par(mfrow = c(2,2))
plot(fit)

# to create your own plots, consider 
predict(fit)
predict(fit, newdata = X)
predict(fit, newdata = X, se.fit = T)

plot(residuals(fit) ~ model.frame(fit)$sWind)





        