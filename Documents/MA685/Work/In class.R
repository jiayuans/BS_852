library(ISLR)
attach(Auto)
dim(Auto)

set.seed(1)
train <- sample(392,196)
str(train)
 
lm.fit =lm(mpg ~ horsepower, data=Auto, subset=train)
summary(lm.fit)
lm.fit2=lm(mpg ~ poly(horsepower,2) ,data=Auto ,subset =train )
summary(lm.fit2)
lm.fit3=lm(mpg ~ poly(horsepower,3) ,data=Auto ,subset =train )
summary(lm.fit3)

mean((mpg -predict (lm.fit ,Auto))[-train ]^2)
mean((mpg -predict (lm.fit2 ,Auto))[-train ]^2)
mean((mpg -predict (lm.fit3 ,Auto))[-train ]^2)

set.seed(2)
train <- sample(392,196)
lm.fit =lm(mpg ~ horsepower ,subset =train)
mean((mpg -predict (lm.fit ,Auto))[-train ]^2)
lm.fit2=lm(mpg ~ poly(horsepower,2) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit2 ,Auto))[-train ]^2)
lm.fit3=lm(mpg ~ poly(horsepower,3) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit3 ,Auto))[-train ]^2)

lm.fit =lm(mpg ~ horsepower ,data=Auto)
coef(lm.fit)

glm.fit=glm(mpg ~ horsepower,data=Auto)
coef(glm.fit)

library (boot)
glm.fit=glm(mpg ~ horsepower ,data=Auto)
cv.err =cv.glm(Auto,glm.fit)
cv.err$delta

cv.error=rep (0,5)
for (i in 1:5){
  glm.fit=glm(mpg ~ poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm (Auto ,glm.fit)$delta [1]
}
cv.error
