
### test for trend:

prop.trend.test( x=c(27,23,16,19,12), 
                 n=c(144,225,135,180,148), 
                 score=c(1,2,3,4,5))

### effect of linear transfromation:

prop.trend.test( x=c(27,23,16,19,12), 
                 n=c(144,225,135,180,148), 
                 score=c(1,2,3,4,5)*10)

### non-linear trend:

prop.trend.test( x=c(27,23,16,19,12), 
                 n=c(144,225,135,180,148), 
                 score=c(1,2,4,8,16))