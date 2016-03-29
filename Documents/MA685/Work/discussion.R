library(SemiPar)
data(age.income)
attach(age.income)
par(mfrow=c(1,1),mar=c(4.5,4.5,1,1),oma=c(0,0,0,0))
plot(age,log.income)
hist(age)
alims <- range(age)
age.grid <- seq(from=alims[1],to=alims[2])
simple <- lm(log.income~age,list(age=age.grid))


# polymnomial
fit.p <- lm(log.income ~ poly(age,4), data = age.income)
summary(fit.p)

preds <- predict(fit.p, newdata=list(age=age.grid),se=TRUE)
se.bands <- cbind(preds$fit + 2*preds$se.fit, 
                  preds$fit - 2*preds$se.fit)

plot(age,log.income,xlim=alims,cex=.5,col="darkgrey")
title("Smoothing Spline Comparison")
fit.d <- smooth(spline(age,log.income,df=16))
lines(fit.d, col="red", lwd=2)

library(splines)
fit.s1 <- lm(log.income~bs(age,knots=c(30,40,50)),data=age.income)

fit.cv <- smooth.spline(age,log.income,cv = TRUE)
 