source('sourceDir.R')
sourceDir('R')
library(tidyverse)

#  generate -----
r = 100;p = 20;n = 40
m = 10  # num of important response
rho=0.2
zseed = 1
betaValue = 1.5
SigmaX=matrix(rho,p,p)
diag(SigmaX)=1
betaA=matrix(sample(c(-1,1)*betaValue,size=m*p, replace = TRUE), nrow = m,ncol = p)
beta=matrix(0,r,p)
beta[1:m,]=betaA
sigma=1
X = matrix(rnorm(n*p),n)%*%chol(SigmaX)
Y = X %*% t(beta) + sqrt(sigma)*matrix(rnorm(n*r),n,r)

result=YKnock_filter(X, Y)
print(result$selected)
