
library(EcoData)

plot(loght ~ temp, data = plantHeight)

model = lm(loght ~ temp, data = plantHeight)
summary(model)

par(mfrow = c(2, 2))
plot(model)


boxplot(loght ~ growthform, data = plantHeight)

model2 = lm(loght ~ growthform, data = plantHeight)
summary(model2)

plantHeight$growthform2 = relevel(as.factor(plantHeight$growthform), "Herb")
model2 = lm(loght ~ growthform2, data = plantHeight)
summary(model2)


library(EcoData)
model = lm(loght ~ temp, data = plantHeight)

plantHeight$sTemp = scale(plantHeight$temp)
plantHeight$sLat = scale(plantHeight$lat)
plantHeight$sNPP = scale(plantHeight$NPP)

fit = lm(loght ~ sTemp + sNPP, data = plantHeight)
summary(fit)

car::Anova(fit, type = "II")

fit = lm(loght ~ temp * growthform2   , data = plantHeight)
summary(fit)


fit = lm(loght ~ sTemp , data = plantHeight[plantHeight$growthform == "Tree",])
summary(fit)


newData = data.frame(temp = c(1,2,3), 
                     growthform2 = rep("Tree", 3))

x = predict(fit, newdata = newData, se.fit = T)

plot(newData$temp , x$fit, type = "l", ylim = c(1,2))
lines(newData$temp , x$fit + x$se.fit, lty = 2)
lines(newData$temp , x$fit - x$se.fit, lty = 2)


fit = lm(loght ~ sTemp * sLat, data = plantHeight)
summary(fit)


cor(plantHeight$temp, plantHeight$lat)


library(EcoData)
fit = lm(loght ~ sTemp * sLat, data = plantHeight)
summary(fit)

print(car::Anova(fit, type = "III"))



library(glmmTMB)

fit <- glmmTMB(loght ~ temp + (temp|growthform)   , data = plantHeight)
summary(fit)



