#########################################################################################
#
# Demostration MA685
# Chapter 4 Lab: Logistic Regression, LDA, QDA, and KNN
#
# last.modifed JM 02-09-2016
#
#########################################################################################
library(ISLR)
attach(Smarket)

### Examine the Stock Market Data (in ISLR pkg)
?Smarket

dim(Smarket) # 1250 days, 9 variables
names(Smarket)
#     ‘Year’ The year that the observation was recorded
#     ‘Lag1’ Percentage return for previous day
#     ...
#     ‘Lag5’ Percentage return for 5 days previous
#     ‘Volume’ Volume of shares traded on the previous day (number of daily shares traded in billions)
#     ‘Today’ Percentage return for today
#      ‘Direction’ A factor with levels ‘Down’ and ‘Up’ indicating whether the market had a positive or negative return on a given day
summary(Smarket)

# correlation matrix
pairs(Smarket)
cor(Smarket[,-9]) # Direction is qualitative
# -> little correlation between Todays return and lag variables
image(cor(Smarket[,-9]))
# -> substantial correlation between Year and volume:
plot(Volume, type='l')
plot(Volume, type='p', col=Direction)

### Linear Model
num_direction <- as.numeric(Direction)-1
lm.fit <- lm(num_direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Smarket)
summary(lm.fit)

# check model fit
plot(num_direction ~ Volume)
abline(coef(lm.fit)[c(1,7)], col=2, )
summary(predict(lm.fit, type='response')) # prediction range [0.4, 0.65]

### Logistic Regression (GLM with family='binomial')
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Smarket, family=binomial)
summary(glm.fit)
# -> smallest p-value for Lag1, 
# -> Lag1 negative coefficient: positive return yesterday, then the  market less likely to go up today?
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]

# predict prob that the market will go up
glm.probs <- predict(glm.fit,type="response")
glm.probs[1:10]

### assess classification quality

# convert pred prob into class labels
glm.pred <- rep("Down",1250)
glm.pred[glm.probs > .5] <- "Up"

# confusion matrix 
table(glm.pred, Direction)
# diagonal elements indicate correct prediction
(507+145)/1250
# or use mean()
mean(glm.pred == Direction)
# -> training error rate: 1-0.522 = 0.478 (overly optimistic)

### Prediction error on held out data for ts classification problems

# subset data into training (years in [2001, 2004]) and held out data [2005]
train <- (Year < 2005) # training (years in [2001, 2004]) 
sum(train)
Smarket.2005 <- Smarket[!train,] # held out data in [2005]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

# fit model on training data - use subset = train
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data=Smarket, family=binomial, subset=train)
summary(glm.fit)

# predicted probs for test set + assign class labels
glm.probs <- predict(glm.fit, Smarket.2005,type="response")
glm.pred <- rep("Down",252)
glm.pred[glm.probs>.5] <- "Up"

# classification quality
table(glm.pred,Direction.2005)
# correctly classified
mean(glm.pred==Direction.2005)
# test set error
mean(glm.pred!=Direction.2005) # 0.52 > 0.5 -> worse than random guessing

### model selection: only variables Lag1 and Lag2 that have highest predictive power
glm.fit <- glm(Direction ~ Lag1 + Lag2, data=Smarket, family=binomial, subset=train)
glm.probs <- predict(glm.fit, Smarket.2005, type="response")
glm.pred <- rep("Down",252)
glm.pred[glm.probs>.5] <- "Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005) # -> 56% correctly predicted

106/(106+76)
predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")

### more than two classes

require(ordinal) # cumulative link models
vignette(package='ordinal')
vignette('clm_intro',package='ordinal') # -> very good tutorial

require(glmnet)
vignette(package='glmnet')
vignette('glmnet_beta', package='glmnet')
data(MultinomialExample)

### Linear Discriminant Analysis
require(MASS) # MASS:lda()
?lda #     lda(formula, data, ..., subset, na.action)

# define training data again
require(ISLR)
attach(Smarket)

train <- (Year < 2005) # training (years in [2001, 2004])
sum(train)
Smarket.2005 <- Smarket[!train,] # held out data in [2005]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

# fit lda
lda.fit <- lda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
# check estimates: prop., means, delta coef
lda.fit

# prediction
lda.pred <- predict(lda.fit, Smarket.2005)
str(lda.pred)

# check prection quality
table(lda.pred$class,Direction.2005)
mean(lda.pred$class==Direction.2005) # log.model: 1- test error = precision = 56%

# recreate class predictions using posterior + 50% threshold
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
# apply 50% threshold
lda.pred$posterior[1:20,1]
lda.pred$class[1:20]

# posterior prob > 0.9
sum(lda.pred$posterior[,1]>.9)

summary(lda.pred$posterior)

### visulize results using klaR:partimat
require(klaR) # install.packages('klaR')
partimat(Direction ~ Lag1+Lag2, data=Smarket, subset=train, method="lda") 

### Quadratic Discriminant Analysis
# MASS:qda()

# fit model
qda.fit <- qda(Direction ~ Lag1+Lag2, data=Smarket, subset=train)
qda.fit # no coef for discriminant function since QDA involves quadratic terms

# make predictions
qda.class <- predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005) # ~60% accuracy

# visulize results using klaR:partimat
partimat(Direction ~ Lag1+Lag2, data=Smarket, subset=train,method="qda") 
partimat(Direction ~ Lag1+Lag2, data=Smarket, subset=train,method="naiveBayes") 

#### K-Nearest Neighbors
library(class) # class:knn()

### 4 imputs for knn:
?knn
#   train: matrix or data frame of training set cases.
train.X=cbind(Lag1,Lag2)[train,]
#   test: matrix or data frame of test set cases. A vector will be interpreted as a row vector for a single case.
test.X=cbind(Lag1,Lag2)[!train,]
#   cl: factor of true classifications of training set
train.Direction=Direction[train]
#   k: number of neighbours considered.

# compute knn, k=1
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
# confusion matrix
table(knn.pred, Direction.2005)
(83+43)/252 # bad: flip coin

# compute knn, k=3
knn.pred <- knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005) #better 54%

#### summarize results:

#logistic regression: 56%
mean(glm.pred==Direction.2005) 
#lda: 56%
mean(lda.pred$class==Direction.2005) # log.model: test error 56%
#qda: 60%
mean(qda.class==Direction.2005) 
#knn: 54%
mean(knn.pred==Direction.2005) #better 54%

# --> QDA does best for Stock data


### KNN CV
require(class)

# iris data: 50 flowers  from each of 3 species of iris; 
# measurements in centimeters of the variables sepal length and  width and petal length and width
plot(iris, col=iris[,5])

train <- seq(2,150,by=2)
length(train)
iris_train <- iris[train,]
iris_test <- iris[-train,]

# knn, k=3
knn3 <- knn(iris_train[,-5], iris_test[,-5], cl=iris_train[,5], k = 3, prob=TRUE)
summary(knn3)

str(knn3)
table(knn3, iris_test[,5])
mean(knn3 == iris_test[,5])

knn7 <- knn(iris_train[,-5], iris_test[,-5], cl=iris_train[,5], k = 7, prob=TRUE)
mean(knn7 == iris_test[,5])

knn31 <- knn(iris_train[,-5], iris_test[,-5], cl=iris_train[,5], k = 31, prob=TRUE)
mean(knn31 == iris_test[,5])

# knn LOOCV
K <- 30
cv.err <- integer(K)
for(k in 1:K){
    # compute knn.cv (LOOCV)
    knnk <- knn.cv(iris_train[,-5], cl=iris_train[,5], k = k, prob=TRUE)
   # average accuracy
    cv.err[k] <-  1- mean(knnk == iris_test[,5])
}
plot(cv.err, type='b')
lines(cv.err, type='b')

# LOOCV by hand: 
K <- 30
cv.err <- integer(K)
for(k in 1:K){
    acc <- integer(nrow(iris_train))
    for(i in 1:nrow(iris_train)){
        # partition data
        iris.test1 <-  iris_train[i,]
        iris.train1 <- iris_train[-i,]
        # run knn
        knnk <- knn(train = iris.train1[,-5], test = iris.test1[,-5], cl=iris.train1[,5], k=k)
        # compute accuracy
        acc[i] <- mean(knnk == iris.test1[,5])
    }
   # average cv error
    cv.err[k] <- 1-mean(acc)
}
plot(cv.err, type='b')
