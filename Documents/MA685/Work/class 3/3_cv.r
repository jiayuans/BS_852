#########################################################################################
#
# Demostration MA685
# Chaper 5 Lab: Cross-Validation and the Bootstrap
#
# last.modifed JM 26-01-2016
#
#########################################################################################
library(ISLR)

##### The Validation Set Approach ############################
attach(Auto)

dim(Auto)
## split training and test data
set.seed(1)
train <- sample(392,196)
str(train)
# linear model with subset=train option
lm.fit <- lm(mpg~horsepower,data=Auto,subset=train)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit) # non-linear relationship 

# quadratic model
lm.fit2 <- lm(mpg~poly(horsepower,2),data=Auto,subset=train)
summary(lm.fit2)
plot(lm.fit2) # looks better

# cubic model or higher polynomials?
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
plot(lm.fit3) # looks also better
summary(lm.fit3) # cubic term not significant

### or compare the test MSE 

# use prediction on test data
plot(mpg[-train] ~ predict(lm.fit, Auto)[-train])

# compute mean squared error = test error 
mean((mpg - predict(lm.fit, Auto))[-train]^2)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
# -> cubic model is best

### resampling/different data results in somewhat different estimates
set.seed(2)
train=sample(392,196)

lm.fit <- lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2 <- lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3 <- lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# -> quadratic model is best
# -> validation set approach results highly variable test error estimations

#### Leave-One-Out Cross-Validation ###################################

# automatic cv implemented in boot:cv.glm
library(boot)

# use glm function for automated LOOCV
glm.fit <- glm(mpg~horsepower,data=Auto)
coef(glm.fit)
lm.fit <- lm(mpg~horsepower,data=Auto)
coef(lm.fit)

# perform LOOCV
glm.fit <- glm(mpg~horsepower, data=Auto)
cv.err <- cv.glm(Auto,glm.fit)

cv.err$delta
# ?cv.glm
#  delta: A vector of length two.  The first component is the raw
#          cross-validation estimate of prediction error.  The second
#          component is the adjusted cross-validation estimate.  The
#          adjustment is designed to compensate for the bias introduced
#          by not using leave-one-out cross-validation.

# compare MSE for polynomials on Auto data
cv.error <- integer(5)
for (i in 1:5){
   glm.fit <- glm(mpg ~ poly(horsepower,i), data=Auto)
   cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error
plot(cv.error, type='b')

### special case for LS or polynomial regression

# cv for one model
glm.fit <- glm(mpg ~ horsepower, data=Auto)
system.time(
    cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
)
# single model fit
system.time(
    lm.fit <- lm(mpg ~ horsepower, data=Auto)
)

# compute leverage statistics
n <- dim(Auto)[1]
plot(hatvalues(lm.fit))
plot(residuals(lm.fit) ~ (1-hatvalues(lm.fit)))
sum((residuals(lm.fit)/(1-hatvalues(lm.fit)))^2)/n

system.time(sum((residuals(lm.fit)/(1-hatvalues(lm.fit)))^2)/n)

##### k-Fold Cross-Validation #####################################

set.seed(17) # 1 2 17
set.seed(1) 
set.seed(2) 
cv.error.10=rep(0,10)
for (i in 1:10){
    glm.fit <- glm(mpg ~ poly(horsepower, i),data=Auto)
    cv.error.10[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10

# less variable + computationally faster
plot(cv.error.10, type='b')
lines(cv.error.10, type='b', col=2)
lines(cv.error.10, type='b', col=3)

#### The Bootstrap #################################################

# discussion section
alpha.fn=function(data,index){
 X=data$X[index]
 Y=data$Y[index]
 return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
 }
alpha.fn(Portfolio,1:100)
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
boot(Portfolio,alpha.fn,R=1000)

### Estimating the Accuracy of a Linear Regression Model
require(boot)

# define bootstrap estimation function
boot.fn <- function(data, index){
    m1 <- lm(mpg ~ horsepower, data=data, subset=index)
    return(coef(m1))
}

# compute linear model for complete data
boot.fn(Auto,1:392)
coef(lm(mpg ~ horsepower, data=Auto)) # -> works fine

# create bootstrap estimates
set.seed(1)
boot.fn(Auto, sample(392,392,replace=TRUE))
boot.fn(Auto, sample(392,392,replace=TRUE))

# compute standard errors of bootstrap estimates
boot(data=Auto, statistic=boot.fn, R=1000)

# compare to the results of the linear model
m1 <- lm(mpg~horsepower,data=Auto)
summary(m1)$coef
# why do se differ?
par(mfrow=c(2,2))
plot(m1)
# -> model assumptions questionable

# compute bootstrap for linear model with quadratic effect
boot.fn=function(data,index){
    m2 <- lm(mpg~horsepower+I(horsepower^2),data=data,subset=index)
    coefficients(m2)
}

set.seed(1)
boot(Auto,boot.fn,1000)

# compare to the results of the corresponding linear model
m2 <- lm(mpg~horsepower+I(horsepower^2),data=Auto)
summary(m2)$coef
plot(m2)

