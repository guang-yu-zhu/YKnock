#' Importance statistics based on a GLM with cross-validation
#'
#' Fits a generalized linear model via penalized maximum likelihood and cross-validation.
#' Then, compute the difference statistic
#'   \deqn{W_j = |Z_j| - |\tilde{Z}_j|}
#' where \eqn{Z_j} and \eqn{\tilde{Z}_j} are the coefficient estimates for the
#' jth variable and its knockoff, respectively. The value of the regularization
#' parameter \eqn{\lambda} is selected by cross-validation and computed with \code{glmnet}.
#'
#' @param X n-by-p matrix of quantitative predictors.
#' @param Y n-by-r matrix of original responses.
#' @param Yk n-by-r matrix of knockoff responses.
#' @param generate_lambda whether to use the method in the YKnock paper to generate lambda sequence. If not, it will use the method of glmnet package to generate lambda.
#' @param nlambda The number of lambda values - default is 100.
#' @param nfolds  number of folds - default is 5.
#'
#' @details This function uses the \code{glmnet} package to fit a generalized linear model
#' via penalized maximum likelihood.
#'
#' The statistics \eqn{W_j} are constructed by taking the difference
#' between the coefficient of the j-th responses and its knockoff.
#'
#' By default, the value of the regularization parameter is chosen by 5-fold cross-validation.
#'
#'
#' The optional \code{nlambda} parameter can be used to control the granularity of the
#' grid of \eqn{\lambda}'s. The default value of \code{nlambda} is \code{100}.
#'
#' @rdname stat.modelY_coef
#' @export
stat_modelY_coef <- function(X, Y, Yk,generate_lambda=TRUE,nlambda=100, nfolds = 5) {
  # Standardize variables
  M = scale(cbind(Y,Yk))
  N = X
  n = nrow(N); p = ncol(N)
  r = ncol(M)/2
  orig = 1:r
  #parallel=FALSE

  # Compute statistics
  if (generate_lambda) {
    # Unless a lambda sequence is provided by the user, generate it
    # print('generate lambda')
    lambda_max = max(abs(t(M) %*% N)) / n
    lambda_min = lambda_max / 2e3
    k = (0:(nlambda-1)) / nlambda
    lambda = lambda_max * (lambda_min/lambda_max)^k
  }
  else {
    lambda = NULL
  }
  cv.glmnet.fit <- glmnet::cv.glmnet(M,N,lambda=NULL,family='mgaussian',intercept=TRUE,
                                     standardize=F,standardize.response=F, parallel=FALSE, nfolds = nfolds)

  coeff <- coef(cv.glmnet.fit, s = "lambda.min") # list of p (2r+1) vectors
  betas = matrix(0,2*r,p)
  for(i in 1:length(coeff)){
    betas[,i]<-as.matrix(coeff[[i]])[-1]
  }
  Z<-apply(betas,1,function(ss){sum(ss^2)})
  #W = pmax(Z[orig], Z[orig + r])
  #chi = sign(Z[orig] - Z[orig + p])#*(1-2*swap)
  #W = abs(Z[orig]) - abs(Z[orig+r])
  return(Z)
}
