#########################################################################################
#
# Demostration MA685
#
# last.modifed JM 26-01-2016
#
#########################################################################################

#install.packages('ISLR')
require(ISLR)

# Chaper 3 Lab: linear model and its extensions

#### advertising data set (available online) ###############################################################
adv <- read.table('Advertising.csv', sep=',', header=TRUE)
summary(adv)

#### basic association plots
par(mfrow=c(1,3))
plot(Sales~TV, data=adv)
abline(coef(lm(Sales~TV, data=adv)), col=2, lwd=2)
plot(Sales~Radio, data=adv)
abline(coef(lm(Sales~Radio, data=adv)), col=2, lwd=2)
plot(Sales~Newspaper, data=adv)
abline(coef(lm(Sales~Newspaper, data=adv)), col=2, lwd=2)

#### multiple regression (p. 19f)
m1 <- lm(Sales ~ TV + Radio + Newspaper, data=adv)
summary(m1)

# coefficient estimates
coef(m1)
# confidence interval
confint(m1)
# prediction interval
predict(m1, data.frame(TV=500, Radio=1000, Newspaper=100), interval='prediction')

# Multiple R-squared:  0.8972,	Adjusted R-squared:  0.8956 (P. 21)
summary(m1)$adj.r.sq
# F-statistic: 570.3 on 3 and 196 DF,  p-value: < 2.2e-16 (slide 22)
summary(m1)$fstatistic
# -> at least one predictor is useful

#### model diagnostics: model structure ok?
plot(m1) 

# correlation of error terms
plot(residuals(m1))
abline(0,0,col=2)

# check correlations
cor(adv[,-1])
# VIF -> no colinearity
library(car)
vif(m1)

# extension of LiMo: nonlinearity -> interaction effects (p. 36f)
m2 <- lm(Sales ~ TV * Radio , data=adv)
summary(m2)
# -> interactions are important! H_A: beta_3 != 0
# Multiple R-squared:  0.9678,	Adjusted R-squared:  0.9673

# model diagnostics: model structure ok?
par(mfrow=c(2,2))
plot(m2) # nonlinear model structure

# wrong model specification -> consider quadratic term
m3 <- lm(Sales ~ TV*Radio + I(TV^2), data=adv)
summary(m3)
plot(m3) # -> high leverage point 131

# remove high leverage point -> remove measurement error?
m4 <- lm(Sales ~ TV * Radio + I(TV^2), data=adv[-131,])
summary(m4)
plot(m4)  

# nonparametric 'smooth' addditive regression 
require(mgcv)
m5 <- gam(Sales ~ s(TV) + s(Radio) + s(TV,Radio), data=adv[-131,])
summary(m5)
plot(m5)

# model diagnostics
gam.check(m5)



