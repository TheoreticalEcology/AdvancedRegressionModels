
library(lme4)
library(lmerTest)
library(EcoData)
plantHeight$sTemp = scale(plantHeight$temp)
plantHeight$sLat = scale(plantHeight$lat)
plantHeight$growthform2 = relevel(as.factor(plantHeight$growthform), "Herb")


fit <- lm(loght ~ sTemp + Family, data = plantHeight)
summary(fit)

library(lme4)
library(lmerTest)

fit <- lmer(loght ~ sTemp + (1|Family), data = plantHeight)
summary(fit)
ranef(fit)


?Owls

library(glmmTMB)
?Salamanders

zipm3 = glmmTMB(count~ spp + mined + (1|site) , 
                Salamanders, family="poisson")


zipm3 = glmmTMB(count~ spp + mined + (1|plot/subplots) , 
                Salamanders, family="poisson")


glmmTMB(count~ spp + mined + (1|plot) + (1|subplots) , 
        Salamanders, family="poisson")


plot subplot

1   A          A1
1   B          B1
1   C.         C1
2   A          A2
2   B 
2   C
3   A
3   B
3   C

fit <- lm(loght ~ sTemp * Family , data = plantHeight)
summary(fit)

fit <- lmer(loght ~ sTemp + (sTemp | Family) , data = plantHeight)
summary(fit)

ranef(fit)


library(metafor)

fit<- lm(weight ~ group - 1, data = PlantGrowth)
summary(fit)


# Power analysis 

# pilot study
fit <- lm(loght ~ sTemp , data = plantHeight)
summary(fit)

# power simulations

n = 300
effectSize = 0.4

pVal = rep(NA, 1000)

for(i in 1:1000){
  temp = runif(n)
  lat = 0.9 * temp + 0.1 * runif(n)
  height = effectSize * temp + effectSize * lat + rnorm(n, sd = 0.7)
  fit <- lm(height ~ temp + lat)
  x = summary(fit)
  pVal[i] = x$coefficients[2,4]
}

hist(pVal)
mean(pVal < 0.05)



# reporting in a paper 


fit <- lmer(loght ~ sTemp*growthform2 + (sTemp | Family) , data = plantHeight)
summary(fit)


