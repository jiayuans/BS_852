x=seq(1,10)
y=x
f=outer(x,y,function (x,y)cos(y)/(1+x^2))
contour (x,y,f)
contour (x,y,f,nlevels =45, add=T)
fa=(f-t(f))/2
contour (x,y,fa,nlevels =15)

image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa ,theta =30)
persp(x,y,fa ,theta =30, phi =20)
persp(x,y,fa ,theta =30, phi =70)
persp(x,y,fa ,theta =30, phi =40)


library (ISLR)
dim(Hitters)
Hitters =na.omit(Hitters)
sum(is.na(Hitters))
library (leaps)
regfit.full=regsubsets (Salary~.,data=Hitters ,nvmax =19)
reg.summary =summary (regfit.full)
names(reg.summary )
reg.summary$rsq
par(mfrow =c(1,1))
plot(reg.summary$rss,xlab=" Number of Variables ",ylab=" RSS",
     type="l")
plot(reg.summary$adjr2,xlab =" Number of Variables ",
     ylab=" Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col ="red",cex =2, pch =20)
plot(regfit.full,scale ="bic")
coef(regfit.full,7)
regfit.fwd=regsubsets (Salary~.,data=Hitters ,nvmax =1,
                        method ="forward")
summary(regfit.fwd )
regfit.bwd=regsubsets (Salary~.,data=Hitters ,nvmax =19,
                          method ="backward")
summary (regfit.bwd )


set.seed (1)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train)

regfit.best=regsubsets(Salary~.,data=Hitters [train,],
                         nvmax =19)
test.mat=model.matrix(Salary~.,data=Hitters[test,])
val.errors =rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*% coefi
  val.errors[i]= mean((Hitters$Salary[test]-pred)^2)
}

x=model.matrix (Salary~.,Hitters)[,-1]
y=Hitters$Salary
