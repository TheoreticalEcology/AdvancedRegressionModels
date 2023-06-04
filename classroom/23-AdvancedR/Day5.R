

library(ape)
library(EcoData)

dat = barbetData
tree = barbetTree

dat$species = row.names(dat)
plot(tree)

# dropping species in the phylogeny for which we don't have data
obj<-geiger::name.check(tree,dat)
reducedTree<-drop.tip(tree, obj$tree_not_data)
geiger::name.check(reducedTree,dat)



fit <- lm(Lnote~ scale(Lnalt), data = dat)
summary(fit)
par(mfrow = c(2,2))
plot(fit)

plot(allEffects(fit,partial.residuals = T))

res<- simulateResiduals(fit, plot = T)

fit <- lm(log(Lnote) ~ scale(Lnalt) + 
                       I(scale(Lnalt)^2) + 
                       I(scale(Lnalt)^3) , 
                       data = dat)

plot(allEffects(fit))


fit <- gam(log(Lnote) ~ s(scale(Lnalt)), 
          data = dat)
summary(fit)
plot(fit)




summary(fit)
par(mfrow = c(2,2))
plot(fit)

fit0 = lm(log(Lnote)~ 1, data = dat)
anova(fit0, fit)


w = 1/cophenetic(reducedTree)
diag(w) = 0
Moran.I(residuals(fit), w)










fit <- lm(Lnote~ scale(Lnalt), data = dat)
summary(fit)

w = 1/cophenetic(reducedTree)
diag(w) = 0
Moran.I(residuals(fit), w)


fit <- gls(Lnote~ scale(Lnalt), 
           correlation = corBrownian(phy = reducedTree, 
                                     form =~ species), data = dat, 
           method = "ML")
summary(fit)











fit = lm(Ozone ~ Wind + Temp, data = airquality)
summary(fit)

options("na.action")

fit = lm(Ozone ~ Wind + Temp, data = airquality, na.action = "na.fail")

options(na.action = "na.fail")

summary(airquality)

image(is.na(t(airquality)), axes = F)
axis(3, at = seq(0,1, len = 6), labels = colnames(airquality))


sum(complete.cases(airquality))


airquality[complete.cases(airquality), ]

rows = rownames(model.matrix(Ozone ~ Wind + Temp, data = airquality))
airquality = airquality[rows, ]


library(missRanger)
airqualityImp<- missRanger(airquality)

fit = lm(Ozone ~ Wind + Temp, data = airqualityImp)
summary(fit)


# run 20 imputations
airqualityMImp <- replicate(20, missRanger(airquality), simplify = FALSE)


# fit 20 models
models <- lapply(airqualityMImp, function(x) lm(Ozone ~ Wind + Temp, x))

# use mice package to compute corrected p-values
require(mice)
summary(pooled_fit <- pool(models)) 


# Another option to deal with this:

library(mitools)
data(smi)
models<-with(smi, glm(drinkreg~wave*sex,family=binomial()))
summary(MIcombine(models))




library(lavaan)

1) laavan -> everything is multivariate normal, i.e. all relationships follow a linear regression

2) anything else -> piecewiseSEM

library(piecewiseSEM)

library(ggdag)
library(ggplot2)

theme_set(theme_dag())

dag <- dagify(rich ~ distance + elev + abiotic + age + hetero + firesev + cover,
              firesev ~ elev + age + cover,
              cover ~ age + elev + abiotic ,
              exposure = "cover",
              outcome = "rich"
)



library(piecewiseSEM)
mod = psem(
  lm(rich ~ distance + elev + abiotic + age + hetero + firesev + cover, data = keeley),
  lm(firesev ~ elev + age + cover, data = keeley), 
  lm(cover ~ age + elev + hetero + abiotic, data = keeley)
)

summary(mod)
plot(mod)




library(brms)
m2 = brms::brm(SiblingNegotiation ~ FoodTreatment * SexParent
               + (1|Nest) + offset(log(BroodSize)), 
               data = Owls , 
               prior = prior(normal(0,5), class = b),
               family = negbinomial)
summary(m2)


plot(m2, ask = FALSE)



