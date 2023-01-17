#' GLM statistics for knockoff
#'
#' Computes the signed maximum statistic
#'   \deqn{W_j = \max(Z_j, \tilde{Z}_j) \cdot \mathrm{sgn}(Z_j - \tilde{Z}_j),}
#' where \eqn{Z_j} and \eqn{\tilde{Z}_j} are the maximum values of
#' \eqn{\lambda} at which the jth variable and its knockoff, respectively,
#' enter the generalized linear model.
#'
#' @param X n-by-p matrix of quantitative predictors.
#' @param Y n-by-r matrix of original responses.
#' @param Yk n-by-r matrix of knockoff responses.
#' @param generate_lambda whether to use the method in the YKnock paper to generate lambda sequence. If not, it will use the method of glmnet package to generate lambda.
#' @param nlambda The number of lambda values - default is 100.
#' @param standardize Logical flag for Y variable standardization. Default is \code{standardize=TRUE}.
#'
#' @return A r-by-2 matrix of statistics \eqn{Z}.
#'
#' @details This function uses \code{glmnet} to compute the regularization path
#' on a fine grid of \eqn{\lambda}'s.
#'
#'
#' @rdname stat_modelY_maxlam
#' @export
stat_modelY_maxlam <- function(X, Y, Yk,generate_lambda=TRUE,nlambda=100,standardize=TRUE) {
  # Randomly swap columns of X and Xk
  #swap = rbinom(ncol(X),1,0.5)
  #swap.M = matrix(swap,nrow=nrow(X),ncol=length(swap),byrow=TRUE)
  #X.swap  = X * (1-swap.M) + Xk * swap.M
  #Xk.swap = X * swap.M + Xk * (1-swap.M)
  t1<-proc.time()
  M = scale(cbind(Y,Yk))
  if(standardize)
    N = scale(X)
  else
    N = X
  n = nrow(N); p = ncol(N)
  r = ncol(M)/2
  orig = 1:r
  #parallel=FALSE

  # Compute statistics
  if (generate_lambda) {
    # Unless a lambda sequence is provided by the user, generate it
    #print('generate lambda')
    lambda_max = max(abs(t(M) %*% N)) / n
    lambda_min = lambda_max / 2e3
    k = (0:(nlambda-1)) / nlambda
    lambda = lambda_max * (lambda_min/lambda_max)^k
  }
  else {
    lambda = NULL
  }
  glmnet.fit <- glmnet::glmnet(M,N,lambda=NULL,family='mgaussian',intercept=TRUE,
                               standardize=F,standardize.response=F)
  coeff <- glmnet.fit$beta
  beta1 <- as.matrix(coeff[[1]])  # coefficent for X1   2*r by nlambda
  beta2 <- as.matrix(coeff[[2]])

  first_nonzero <- function(x) match(T, abs(x) > 0) # NA if all(x==0)
  indices <- apply(beta1, 1, first_nonzero)
  names(indices) <- NULL
  Z = ifelse(is.na(indices), 0,  glmnet.fit$lambda[indices] * n)
  # indices <- apply(beta2, 1, first_nonzero)
  # names(indices) <- NULL
  # Z2 = ifelse(is.na(indices), 0,  glmnet.fit$lambda[indices] * n)

  #W = abs(Z[orig]) - abs(Z[orig+r])
  #plot(W)
  #print((proc.time()-t1)[3])
  return(Z)
  #return(list(W=W,chi=chi))
  #
}
