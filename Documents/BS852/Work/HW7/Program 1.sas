proc lifetest data= Lungca plots = (s,ls,lls);
time X9*Y(0);
strata X1;
run;