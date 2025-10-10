

fit = lm(Ozone ~ Wind + Temp + Solar.R, data = airquality)
summary(fit)

summary(airquality)


image(is.na(t(airquality)), axes = F)
axis(3, at = seq(0,1, len = 6), labels = colnames(airquality))


options(na.action = "na.omit")

rows = rownames(model.matrix(Ozone ~ Wind + Temp + Solar.R, data = airquality))
airqualityRem = airquality[rows, ]

options(na.action = "na.fail")

fit = lm(Ozone ~ Wind + Temp + Solar.R, data = airqualityRem)
summary(fit)

fit$model


library(missRanger)
airqualityImp<- missRanger(airquality)


# run 20 imputations
airqualityMImp <- replicate(20, missRanger(airquality), simplify = FALSE)
# fit 20 models
models <- lapply(airqualityMImp, function(x) lm(Ozone ~ Wind + Temp, x))

# use mice package to compute corrected p-values
require(mice)
summary(pooled_fit <- pool(models)) 





plantHeight$sTemp = scale(plantHeight$temp)
plantHeight$sLat = scale(plantHeight$lat)
plantHeight$sRain = scale(plantHeight$rain)

set.seed(123)
plantSel = plantHeight[sample.int(178, 40),]

m1 = lm(loght ~ sLat * sTemp, data = plantSel)
summary(m1)

m2 = lm(loght ~ sLat + sTemp, data = plantSel)
summary(m2) 

m3 = lm(loght ~ sLat, data = plantSel)
summary(m3)

AIC(m1,m2,m3)
summary(aov(m1))

cor(plantSel$sLat, plantSel$sTemp)

m1 = lm(loght ~ sLat + sTemp, data = plantSel)
summary(m1)


# scientific inference
y ~ x1 * x2 + c1 + otherVariables + RE + distributions

# predict 
y ~ . AIC select 


m1 = lm(loght ~ sLat * sTemp, data = plantSel)
summary(m1)



set.seed(123)
x1 = runif(100)
x2 = 0.95 * x1 + 0.05 *runif(100)
x3 = runif(100)
y = x1 + x2 + x3 + rnorm(100)
m1 = lm(y ~ x1)
AIC(m1)

m2 = lm(y ~ x1 + x2)
anova(m1, m2)
AIC(m2)

m3 = lm(y ~ x1 + x3)
anova(m1, m3)
AIC(m3)


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



sex = factor(sample(c("m", "f"), 1000, replace = T))

income = runif(1000) + as.numeric(sex)

happyness = income + rnorm(1000) - as.numeric(sex)

boxplot(happyness ~ sex)

plot(happyness ~ income)
fit <- lm(happyness ~ income)
summary(fit)

fit <- lm(happyness ~ income + sex )
summary(fit)



library(EcoData)

hist(soep$einkommenj1, breaks = 50)

soep$sIncome = scale(soep$einkommenj1)
soep$fErwerb = as.factor(soep$erwerb)

options(na.action = "na.omit")

rows = rownames(model.matrix(lebensz_org ~ sIncome + sex + alter + bildung + anz_kind + gesund_org + fErwerb, data = soep))
soepR = soep[rows,]

options(na.action = "na.fail")

fit <- lmer(lebensz_org ~ sIncome + sex + alter + bildung + anz_kind + gesund_org + fErwerb + (1|id) + (1|syear), data = soepR )

summary(fit)


plot(fit)
plot(residuals(fit) ~ soepR$sIncome)

res <- simulateResiduals(fit, plot=T, re.form = NULL)



fit <- lmer(lebensz_org ~ scale(log(einkommenj1 + 1)) + scale(alter) + (1|id), data = soepR )

fit2 <- lmer(lebensz_org ~ scale(log(einkommenj1 + 1)) + scale(alter) + (scale(alter)|id) , data = soepR )
summary(fit2)

simulateLRT(fit, fit2)

fit <- lmer(lebensz_org ~ sIncome + sex + alter + bildung + anz_kind + gesund_org + fErwerb + (1|id) + (1|syear), data = soepR )

fit <- lmer(lebensz_org ~ sIncome + (1|id) + (1|syear), data = soepR )
summary(fit)

summary(fit)

fit <- lm(lebensz_org ~ (sIncome + sex + scale(alter) + scale(bildung) + scale(anz_kind) + scale(gesund_org) + fErwerb)^2 , data = soepR )

out <- stepAIC(fit)
summary(out)

car::Anova(out, type = "II")


library(MuMIn)

out = dredge(fit)
subset(out, delta < 4)







library(piecewiseSEM)
theme_set(theme_dag())

dag <- dagify(rich ~ distance + elev + abiotic + age + hetero + firesev + cover,
              firesev ~ elev + age + cover,
              cover ~ age + elev + abiotic ,
              exposure = "cover",
              outcome = "rich"
)

ggdag(dag)
ggdag_adjustment_set(dag, effect="direct")

library(piecewiseSEM)

mod = psem(
  lm(rich ~ distance + elev + abiotic + age + hetero + firesev + cover, data = keeley),
  lm(firesev ~ elev + age + cover, data = keeley), 
  lm(cover ~ age + elev + hetero + abiotic, data = keeley)
)

summary(mod)
plot(mod)



library(EcoData)
#str(birdfeeding)
plot(feeding ~ attractiveness, data = birdfeeding)

fit <- glm(feeding ~ attractiveness, data = birdfeeding, 
           family = poisson())
summary(fit)

exp(1.47)


library(EcoData)
#str(titanic)
#mosaicplot( ~ survived + sex + pclass, data = titanic)
titanic$pclass = as.factor(titanic$pclass)


fit = glm(survived ~ sex * age, data = titanic, family = "binomial")
summary(fit)

library(DHARMa)
res<-simulateResiduals(fit, plot = T)

plot(allEffects(fit))

curve(plogis, -5,5)

plogis(0.5)

library(lme4)

fitRE = glmer(survived ~ sex * age + (age|pclass), 
            data = titanic, family = "binomial")

fitRETMB = glmmTMB(survived ~ sex * age + (age|pclass), 
              data = titanic, family = "binomial")


summary(fit)

car::Anova(fit)
car::Anova(fitRE)
car::Anova(fitRETMB)

predict(fit, se = T, type = "response")
predict(fitRE, se = T, type = "response")
predict(fitRETMB, se = T, type = "response")




