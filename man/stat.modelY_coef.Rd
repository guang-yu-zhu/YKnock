% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_modelY_coef.R
\name{stat_modelY_coef}
\alias{stat_modelY_coef}
\title{Importance statistics based on a GLM with cross-validation}
\usage{
stat_modelY_coef(X, Y, Yk, generate_lambda = TRUE, nlambda = 100, nfolds = 5)
}
\arguments{
\item{X}{n-by-p matrix of quantitative predictors.}

\item{Y}{n-by-r matrix of original responses.}

\item{Yk}{n-by-r matrix of knockoff responses.}

\item{generate_lambda}{whether to use the method in the YKnock paper to generate lambda sequence. If not, it will use the method of glmnet package to generate lambda.}

\item{nlambda}{The number of lambda values - default is 100.}

\item{nfolds}{number of folds - default is 5.}
}
\description{
Fits a generalized linear model via penalized maximum likelihood and cross-validation.
Then, compute the difference statistic
  \deqn{W_j = |Z_j| - |\tilde{Z}_j|}
where \eqn{Z_j} and \eqn{\tilde{Z}_j} are the coefficient estimates for the
jth variable and its knockoff, respectively. The value of the regularization
parameter \eqn{\lambda} is selected by cross-validation and computed with \code{glmnet}.
}
\details{
This function uses the \code{glmnet} package to fit a generalized linear model
via penalized maximum likelihood.

The statistics \eqn{W_j} are constructed by taking the difference
between the coefficient of the j-th responses and its knockoff.

By default, the value of the regularization parameter is chosen by 5-fold cross-validation.


The optional \code{nlambda} parameter can be used to control the granularity of the
grid of \eqn{\lambda}'s. The default value of \code{nlambda} is \code{100}.
}
