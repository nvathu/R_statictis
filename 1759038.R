A=1+7+5+9+0+3+8
B=3+8
C=1759038

#1
#a
pnorm(A+2,mean=A,sd=B)
#0.5721373
#b
pnorm(A+5,mean=A,sd=B)-pnorm(A,mean=A,sd=B)
#0.1752819
#c
1-pnorm(A+5,mean=A,sd=B)
#0.3247181
#d
qnorm(B/100,mean=A,sd=B)
#19.50819

#2
#a
CI_sigma_known = function(mean, sigma, n, alpha){
  CV = qnorm(1 - alpha/2)
  lower = mean - CV*sigma/sqrt(n)
  upper = mean + CV*sigma/sqrt(n)
  CI = c(lower, upper)
  return(CI)
}
#b
CI_sigma_known(A,B,40,0.05)
#(29.59113, 36.40887)
#c
CI_sigma_unknown = function(center, stddev, n, alpha){
  t = qt(1-alpha/2, n-1)
  lb = center - t*stddev/sqrt(n)
  ub = center + t*stddev/sqrt(n)
  return(c(lb, ub ))
}
#d
set.seed(C)
xmau=rnorm(50,mean=A,sd=B)
#e
mu=mean(xmau)
st=sd(xmau)
CI_sigma_unknown(mu,st,50,0.01)
#(28.25531, 35.94223)
#3
#a
CI_proportion = function(p, n, alpha){
  s=sqrt((p*(1-p))/n)
  t = qt(1-alpha/2, n-1)
  lb = p - t*s/sqrt(n)
  ub = p + t*s/sqrt(n)
  return(c(lb, ub))
}
#b
CI_proportion(B/100,100*A,0.05)
#(0.1098141, 0.1101859)






