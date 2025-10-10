plantHeight$sTemp = scale(plantHeight$temp)
plantHeight$sLat = scale(plantHeight$lat)
plantHeight$sNPP = scale(plantHeight$NPP)

# relevel 
table(plantHeight$growthform2)
plantHeight$growthform2 = relevel(as.factor(plantHeight$growthform), "Herb")
plantHeight2 = plantHeight[plantHeight$growthform2 %in% c("Herb" , "Shrub", "Tree"  ), ]


# Task 1

fit = lm(loght ~ sTemp + sNPP, data = plantHeight)
summary(fit)

fit = lm(loght ~ sTemp, data = plantHeight)
summary(fit)

plot(plantHeight$sTemp ~ plantHeight$sNPP)


# Task 2
fit = lm(loght ~ growthform2 *  sTemp , data = plantHeight2)
plot(allEffects(fit))
summary(fit)

anova(fit)

fit = lm(loght ~ growthform2 *  sTemp - sTemp , data = plantHeight2)
summary(fit)

fit = lm(loght ~  sTemp , 
         data = plantHeight2[plantHeight2$growthform == "Tree", ])
summary(fit)


fit = lm(loght ~ growthform2 *  sTemp - sTemp -1 , data = plantHeight2)
summary(fit)

res <- simulateResiduals(fit, plot = T)

x = model.frame(fit)

plotResiduals(res, form = x$growthform2)

# Task 3

fit = lm(loght ~  sTemp * sLat, data = plantHeight)
summary(fit)

plot(sTemp ~ sLat, data = plantHeight)


scale()

airquality$cTemp = airquality$Temp-mean(airquality$Temp)

fit = lm(Ozone ~ cTemp, data = airquality)
summary(fit)



# ANOVA

fit = lm(Ozone ~ scale(Wind) + scale(Temp), data = airquality)
plot(allEffects(fit))
summary(fit)


fit0 = lm(Ozone ~ 1, data = airquality)
summary(fit0)

fit1 = lm(Ozone ~ Wind, data = airquality)
summary(fit1)

fit2 = lm(Ozone ~ Wind + Temp, data = airquality)
summary(fit2)

summary(aov(fit))

total = 45284 + 25886 + 53973
45284 / total
25886 / total

simulateLRT(fit1, fit2)


fit = lm(Ozone ~ Wind + Temp, data = airquality)
summary(aov(fit))


fit = lm(Ozone ~ Temp +  Wind , data = airquality)
summary(aov(fit))



fit = lm(Ozone ~ scale(Temp) *  scale(Wind) , data = airquality)

summary(aov(fit)) # type I
car::Anova(fit,type = "II")
car::Anova(fit,type = "III")


m0 = lm(Ozone ~ 1 , data = airquality)
m1 = lm(Ozone ~ Wind + I(Wind^2) , data = airquality)
m2 = lm(Ozone ~ Wind + I(Wind^2) + Temp, data = airquality)
summary(m1)
anova(m0, m1, m2)


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

ggdag_paths(dag)

ggdag_adjustment_set(dag)
ggdag_adjustment_set(dag, effect="direct")


fit <- lm(rich ~ cover + age + abiotic, data =  keeley)
summary(fit)

# Table II fallacy: put all variables in one regression table and make no differenc between them.

library(piecewiseSEM)

mod = psem(
  lm(rich ~ distance + elev + abiotic + age + hetero + firesev + cover, data = keeley),
  lm(firesev ~ elev + age + cover, data = keeley), 
  lm(cover ~ age + elev + hetero + abiotic, data = keeley)
)

summary(mod)
plot(mod)

plot(Fertility ~ Education, data = swiss)
lm(Fertility ~ Education, data = swiss)


fit <- lm(Fertility ~ Education + Agriculture + Catholic, data = swiss)

summary(fit)



set.seed(123)

x = runif(100)
y = 0.25 * x + rnorm(100, sd = 0.3)

summary(lm(y~x))


xNoise = matrix(runif(8000), ncol = 80)
dat = data.frame(y=y,x=x, xNoise)

fullModel = lm(y~., data = dat)
summary(fullModel)



# Model 1
m1 = lm(Ozone ~ Wind , data = airquality)
summary(m1)

# Model 2
m2 = lm(Ozone ~ Wind + Temp, data = airquality)
summary(m2)

# LRT
anova(m1, m2)

AIC(m1)
AIC(m2)

dat = airquality[complete.cases(airquality), ]
mFull = lm(Ozone ~ .^2 , data = dat)

summary(mFull)


# stepwise automatic
library(MASS)
out = stepAIC(mFull)
print(out)


library(MuMIn)
options(na.action = "na.fail")

dd <- dredge(mFull, rank = AIC)
subset(dd, delta < 3)

options(na.action = "na.omit")



?soep

dat = soep[complete.cases(soep),]

fullModel <- lm(lebensz_org ~ (gesund_org + einkommenj1 + bildung + anz_kind + alter + sex)^2, data = dat )

out = stepAIC(fullModel)
summary(out)

plot(allEffects(out))

dd <- dredge(fullModel, rank = AIC)
subset(dd, delta < 3)



