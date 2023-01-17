#' Importance statistics based on a GLM
#'
#' Computes the signed maximum statistic
#'   \deqn{W_j = \max(Z_j, \tilde{Z}_j) \cdot \mathrm{sgn}(Z_j - \tilde{Z}_j),}
#' where \eqn{Z_j} and \eqn{\tilde{Z}_j} are the maximum values of
#' \eqn{\lambda} at which the jth variable and its knockoff, respectively,
#' enter the generalized linear model.
#'
#' @param X n-by-p matrix of qualitative predictors.
#' @param Y n-by-r matrix of original responses.
#' @param Yk n-by-r matrix of knockoff responses.
#' @param nlambda The number of lambda values - default is 100.
#' @param standardize Logical flag for Y variable standardization. Default is \code{standardize=TRUE}.
#'
#' @return A r-by-2 matrix of statistics \eqn{Z}.
#'
#' @details This function uses the \code{glmnet} package to fit a generalized linear model
#' via penalized maximum likelihood.
#'
#' The statistics \eqn{W_j} are constructed by taking the difference
#' between the coefficient of the j-th responses and its knockoff.
#''
#'
#' The optional \code{nlambda} parameter can be used to control the granularity of the
#' grid of \eqn{\lambda}'s. The default value of \code{nlambda} is \code{100}.
#'
#' @rdname stat.modelY_classification_coef
#' @export

stat_modelY_classification_coef <- function(X, Y,Yk,nlambda=100,standardize=TRUE) {
  # Randomly swap columns of X and Xk
  #swap = rbinom(ncol(X),1,0.5)
  #swap.M = matrix(swap,nrow=nrow(X),ncol=length(swap),byrow=TRUE)
  #X.swap  = X * (1-swap.M) + Xk * swap.M
  #Xk.swap = X * swap.M + Xk * (1-swap.M)
  t1<-proc.time()
  M = cbind(Y,Yk)
  if(standardize) M = scale(M)
  N = as.matrix(X)
  n = nrow(N); p = ncol(N)
  r = ncol(M)/2
  orig = 1:r
  glmnet.fit <- glmnet::glmnet(M,N,nlambda = nlambda,family='multinomial',intercept=TRUE,
                               type.measure = "class", standardize=F,standardize.response=F, parallel=FALSE, nfolds = 5)

  coeff <- glmnet.fit$beta
  #betas = matrix(0,2*r,length(coeff))
  first_nonzero <- function(x) match(T, abs(x) > 0) # NA if all(x==0)

  Zs=matrix(0,2*r,length(coeff))

  for(i in 1:length(coeff)){
    beta<-as.matrix(coeff[[i]])
    indices <- apply(beta, 1, first_nonzero)
    names(indices) <- NULL
    Zs[,i] = ifelse(is.na(indices), 0,  glmnet.fit$lambda[indices] * n)
  }
  Z = apply(Zs,1,mean)

  # W = abs(Z[orig]) - abs(Z[orig+r])
  # plot(W)
  # print((proc.time()-t1)[3])
  return(Z)
  #
}


