 ### install package aod

 #### READ DATA, EXTRACT COMPLETE RECORDS -- FEMALES WITH CHD=0 OR CHD>4 
   framdat2 <- read.table("framdat2.txt",header=T, na.strings=c("."))
   names(framdat2)

  work.data <-   subset(framdat2[ , c(1,2,5,7)], 
   is.na(framdat2[,1])==F & is.na(framdat2[,2])==F & is.na(framdat2[,5])==F & 
   is.na(framdat2[,7])==F & (framdat2$SEX == 2) & 
                              (framdat2$CHD==0 | framdat2$CHD > 4))
 
     chd_sw = work.data$CHD >= 4  ## 1 is event, 0 is no event
       gli4 = work.data$GLI       
       sex <- work.data$SEX
       age <- work.data$AGE

   ##### CRUDE ANALYSIS
     mod.crude <- glm( chd_sw ~ gli4, family=binomial)
  estimates.crude <- summary(mod.crude)

     estimates.crude
     estimates.crude$coefficients
     OR.crude = exp(estimates.crude$coefficients[ ,1])
     LL.crude = exp(estimates.crude$coefficients[ ,1]-1.96*estimates.crude$coefficients[ ,2])
     UL.crude = exp(estimates.crude$coefficients[ ,1]+1.96*estimates.crude$coefficients[ ,2])
     cbind(OR.crude, LL.crude, UL.crude)

   library(aod)
  confint.default(mod.crude)
  exp(cbind(OR = coef(mod.crude), confint.default(mod.crude)))  
  wald.test(b = coef(mod.crude), Sigma = vcov(mod.crude), Terms = 2)

  #### ADJUSTED ANALYSIS
    mod.age <- glm( chd_sw ~ age, family=binomial)
     summary(mod.age)

    mod.age.adjusted <- glm( chd_sw ~ age+gli4, family=binomial)
    anova(mod.age, mod.age.adjusted)
     estimates.age.adjusted <- summary(mod.age.adjusted)
     estimates.age.adjusted
     estimates.age.adjusted$coefficients

     OR.adj =exp(estimates.age.adjusted$coefficients[ ,1])
     LL.adj = exp(estimates.age.adjusted$coefficients[ ,1]-1.96*estimates.age.adjusted$coefficients[ ,2])
     UL.adj = exp(estimates.age.adjusted$coefficients[ ,1]+1.96*estimates.age.adjusted$coefficients[ ,2])
      cbind(OR.adj, LL.adj, UL.adj)

  confint.default(mod.age.adjusted)
  exp(cbind(OR = coef(mod.age.adjusted), confint.default(mod.age.adjusted)))  
  wald.test(b = coef(mod.age.adjusted), Sigma = vcov(mod.age.adjusted), Terms = 3)

 ### question: is there confounding?
       OR.crude / OR.adj 
 ### question: is there a significant association after we adjust for confounding?
     estimates.age.adjusted
     print( "Adjusted OR and 95% CI")
      OR.adj 
 ### 
  log.odds.CHD <- log( mod.age.adjusted$fitted.values/(1-mod.age.adjusted$fitted.values))
   plot(age,log.odds.CHD,xlab="Age", ylab="log-odds-CHD")
   points(age[gli4==1],log.odds.CHD[gli4==1],col=2)

  odds.CHD <-  mod.age.adjusted$fitted.values/(1-mod.age.adjusted$fitted.values)
   plot(age,odds.CHD,xlab="Age", ylab="odds-CHD")
   points(age[gli4==1],odds.CHD[gli4==1],col=2)

     library(verification)
   roc.data <- cbind(chd_sw,  mod.age.adjusted$fitted.values, mod.crude$fitted.values )
  roc.area( roc.data[,1], roc.data[,2])
  roc.area( roc.data[,1], roc.data[,3])
par(mfrow=c(1,2))
  roc.plot( roc.data[,1], roc.data[,2],cex.lab=1.8,cex.axis=1.8,cex=1.8, main="Adjusted Model")
 roc.plot( roc.data[,1], roc.data[,3],cex.lab=1.8,cex.axis=1.8,cex=1.8, main="Crude Model")



