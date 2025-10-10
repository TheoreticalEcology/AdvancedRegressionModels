set.seed(123)
x1 = runif(200)
x2 = 0.8 * x1 + 0.2 *runif(200)
y = 1.0 * x1 + 1.0 * x2 + rnorm(200, sd = 0.2)

m1 = lm(y ~ x1 + x2)
summary(m1)
library(effects)
plot(allEffects(m1, partial.residuals = T))

m2 = MASS::stepAIC(m1)
summary(m2)



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
summary(m2)

m3 = lm(y ~ x1 + x3)
anova(m1, m3)
AIC(m3)
summary(m3)


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


library(MASS)

fit <- lm(Ozone ~ Wind + Temp + Solar.R, data = airquality)
summary(fit)

fit <- lm.ridge(Ozone ~ Wind + Temp + Solar.R, data = airquality, lambda = 2)

plot(lm.ridge( Ozone ~ Wind + Temp + Solar.R, data = airquality,
                lambda = seq(0, 200, 0.1) ) )



set.seed(1)
dat = data.frame(matrix(rnorm(300), ncol = 10))
colnames(dat) = c("Performance", "Gen1", "Gen2", "soilC", "soilP", "Temp",
                  "Humidity", "xPos", "yPos", "Water")
fullModel <- lm(Performance ~ ., data = dat)

summary(lm(Performance ~ Gen1 * Humidity, data = dat[20:30,]))
summary(lm(Performance ~ Gen1 * Humidity, data = dat[20:30,]))





library(EcoData)
str(titanic)
titanic$pclass = as.factor(titanic$pclass)




m1 = lm(survived ~ sex + age + pclass, data = titanic)
summary(m1)
par(mfrow = c(2,2))
plot(m1)
plot(allEffects(m1))


x = rbinom(1000, 100, prob = 0.95)
hist(x)

m1 = glm(survived ~ sex + age + pclass, data = titanic,
         family = "binomial")

summary(m1)

curve(plogis, -5, 5)

plogis(3.5)

# 20 yr old male, p class 3

3.522074 -2.497845 -2.289661 + 20 * -0.034393
plogis(-1.953292)

plot(allEffects(m1))

predict(m1) # linear scale
predict(m1, type = "response") # response scale

new = model.frame(m1)[1,]
new[,3] = 20

predict(m1, 
        type = "response", 
        newdata = new)

anova(m1, test = "Chisq") # type 1, have to set Chisq
car::Anova(m1) # type II/III

plot(allEffects(m1, partial.residuals = T))
par(mfrow = c(2,2))
plot(m1)

residuals(m1) # raw residuals - not interpretable
residuals(m1, type = "pearson") # better but will note be homogenous

library(DHARMa)

res <- simulateResiduals(m1, plot = T)

x = model.frame(m1)

plotResiduals(res, form = x$age)
plotResiduals(res, form = x$pclass)
plotResiduals(res, form = x$sex)


m1 = glm(survived ~ sex * age + pclass, data = titanic,
         family = "binomial")
res <- simulateResiduals(m1, plot = T)

summary(m1)

str(elk_data)


fit<- glm(presence~dist_roads * habitat + 
            dem + I(dem^2) +
            scale(ruggedness) + 
            habitat
          , data=elk_data, 
          family = "binomial")

summary(fit)
plot(allEffects(fit))

res <- simulateResiduals(fit, plot = T)
plotResiduals(res, form = elk_data$dem, rank = F)

library(mgcv)
fit<- gam(presence~dist_roads + 
            s(dem) +
            s(ruggedness) + 
            habitat
          , data=elk_data, 
          family = "binomial")
summary(fit)

plot(fit)



fit<- glm(presence~ .^2 + I(dem^2)
          , data=elk_data, 
          family = "binomial")
summary(fit)
library(MASS)
out = stepAIC(fit)
summary(out)

# for predicting to a map
prefict(fit, 
        newdata = YOUR SPATIAL DATA, 
        type = response)



library(effects)
set.seed(123)
trt = as.factor(sample(c("ctrl", "trt"), 5000, replace= T))
concentration =  runif(5000)

response = plogis(0 + 1 * (as.numeric(trt) - 1) + 1*concentration)
survival = rbinom(5000, 1, prob = response)

dat = data.frame(trt = trt, 
                 concentration = concentration,
                 survival = survival)

m1 = glm(survival ~ trt * concentration, data = dat, family = "binomial")
summary(m1)

plot(allEffects(m1))


response = 0.45 * as.numeric(trt) + 0.1*concentration
survival = rbinom(5000, 1, response)

dat = data.frame(trt = trt, 
                 concentration = concentration,
                 survival = survival)

m2 = glm(survival ~ trt * concentration, 
         data = dat, family = "binomial")
summary(m2)

plot(allEffects(m2))



m2 = glm(survival ~ trt * concentration, 
         data = dat, family = binomial(link = "identity"))
summary(m2)
plot(allEffects(m2))






fit<- glm(presence~dist_roads * habitat + 
            dem + I(dem^2) +
            scale(ruggedness) + 
            habitat
          , data=elk_data, 
          family = "binomial")













data = EcoData::snails
data$sTemp_Water = scale(data$Temp_Water)
data$spH = scale(data$pH)
data$swater_speed_ms = scale(data$water_speed_ms)
data$swater_depth = scale(data$water_depth)
data$sCond = scale(data$Cond)
data$swmo_prec = scale(data$wmo_prec)
data$syear = scale(data$year)
data$sLat = scale(data$Latitude)
data$sLon = scale(data$Longitude)
data$sTemp_Air = scale(data$Temp_Air)
# Remove NAs
rows = rownames(model.matrix(~sTemp_Water + spH + sLat + sLon + sCond + seas_wmo+ swmo_prec + swater_speed_ms + swater_depth +sTemp_Air+ syear + duration + locality + site_irn + coll_date, data = data))
data = data[rows, ]




model1 = glm(bt_pres~ site_type + sTemp_Water + spH +
               sCond + swmo_prec + swater_speed_ms  + duration + 
               sTemp_Air + seas_wmo + syear + swater_depth,
             data = data,  family = binomial)
summary(model1)
plot(allEffects(model1))




model2 = glm(cbind(BT_pos_tot, BT_tot - BT_pos_tot )~ site_type + sTemp_Water + spH +
               sCond + swmo_prec + swater_speed_ms  + duration + 
               sTemp_Air + seas_wmo + syear + swater_depth ,
             data = data[data$BT_tot > 0, ],  family = binomial)


plot(allEffects(model2))
summary(model2)









library(EcoData)
#str(birdfeeding)
plot(feeding ~ attractiveness, data = birdfeeding)



fit <- lm(log(feeding + 1) ~ attractiveness, data = birdfeeding)
summary(fit)


fit <- glm(feeding ~ attractiveness, 
           data = birdfeeding,
           family = "poisson")
summary(fit)
plot(allEffects(fit))

# Bird with zero attractiveness
exp(1.47459)

plot(birdfeeding$attractiveness, 
     predict(fit, type = "response")
)

predict(fit, type = "response")

atr = 0:20

pred = predict(fit, type = "response", 
               newdata = data.frame(attractiveness = atr))


plot(pred ~ atr, type = "l")



barplot(table(rpois(10000, lambda = 20)))
var(rpois(10000, lambda = 20))


hist(rnorm(10000, mean = 0, sd = 0.1), xlim = c(-4,4))



