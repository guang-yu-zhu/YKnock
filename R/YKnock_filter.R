#' @docType package
#' @name YKnock
#' @import stats methods
NULL

#' The YKncok Filter
#'
#' This function runs the Knockoffs procedure from start to finish, selecting variables
#' relevant for predicting the outcome of interest.
#'
#' This function creates the knockoffs, computes the importance statistics,
#' and selects variables.
#' It is the main entry point for the knockoff package.
#'
#' @param X n-by-p matrix or data frame of predictors.
#' @param Y n-by-r matrix or data frame of responses.
#' @param knockoffs method used to construct knockoffs for the \eqn{X} variables.
#' It must be a function taking a n-by-p matrix as input and returning a n-by-p matrix of knockoff variables.
#' By default, approximate model-X Gaussian knockoffs are used.
#' @param statistic statistics used to assess variable importance. By default,
#' a lasso statistic with cross-validation is used. See the Details section for more information.
#' @param fdr target false discovery rate (default: 0.1).
#' @param offset either 0 or 1 (default: 1). This is the offset used to compute the rejection threshold on the
#' statistics. The value 1 yields a slightly more conservative procedure ("knockoffs+") that
#' controls the false discovery rate (FDR) according to the usual definition,
#' while an offset of 0 controls a modified FDR.
#'
#' @return An object of class "knockoff.result". This object is a list
#'  containing at least the following components:
#'  \item{Y}{matrix of original responses}
#'  \item{Yk}{matrix of knockoff responses}
#'  \item{statistic}{computed test statistics}
#'  \item{threshold}{computed selection threshold}
#'  \item{selected}{named vector of selected variables}
#'
#' @details
#'
#' The parameter \code{knockoffs} controls how knockoff variables are created.
#' By default, the model-Y scenario is assumed and a multivariate normal distribution
#' is fitted to the original variables \eqn{Y}.
#'
#'
#' The default importance statistic is \link{stat_modelY_coef}.
#'
#' It is possible to provide custom functions for the knockoff constructions
#' or the importance statistics. Some examples can be found in the vignette.
#'
#' @references
#'   Identification of Significant Gene Expression Changes in Multiple Perturbation Experiments using Knockoffs
#' Tingting Zhao, Guangyu Zhu, Patrick Flaherty
#' bioRxiv 2021.10.18.464822;
#'   \href{https://www.biorxiv.org/content/10.1101/2021.10.18.464822v1}{https://www.biorxiv.org/content/10.1101/2021.10.18.464822v1}
#'
#'
#' @examples
#' r = 100;p = 20;n = 40
#' m = 10  # num of important response
#' rho=0.2
#' betaValue = 1.5
#' SigmaX=matrix(rho,p,p)
#' diag(SigmaX)=1
#' betaA=matrix(sample(c(-1,1)*betaValue,size=m*p, replace = TRUE), nrow = m,ncol = p)
#' beta=matrix(0,r,p)
#' beta[1:m,]=betaA
#' sigma=1
#' X = matrix(rnorm(n*p),n)%*%chol(SigmaX)
#' Y = X %*% t(beta) + sqrt(sigma)*matrix(rnorm(n*r),n,r)
#' result=YKnock_filter(X, Y)
#' print(result$selected)
#'
#' # Basic usage with default arguments
#' result = YKnock_filter (X, Y)
#' print(result$selected)
#'
#'
#' @export
YKnock_filter <- function(X, Y,
                          knockoffs=create_YKnock,
                          statistic=stat_modelY_coef,
                          fdr=0.10,
                          offset=1
) {
  # Validate input types.
  if (is.data.frame(X)) {
    X.names = names(X)
    X = as.matrix(X, rownames.force = F)
  } else if (is.matrix(X)) {
    X.names = colnames(X)
  } else {
    stop('Input X must be a numeric matrix or data frame')
  }
  if (!is.numeric(X)) stop('Input X must be a numeric matrix or data frame')

  if (is.data.frame(Y)) {
    Y.names = names(Y)
    Y = as.matrix(Y, rownames.force = F)
  } else if (is.matrix(Y)) {
    Y.names = colnames(Y)
  } else {
    stop('Input Y must be a numeric matrix or data frame')
  }

  if(offset!=1 && offset!=0) {
    stop('Input offset must be either 0 or 1')
  }

  if (!is.function(knockoffs)) stop('Input knockoffs must be a function')
  if (!is.function(statistic)) stop('Input statistic must be a function')

  # Validate input dimensions
  n = nrow(X); p = ncol(X); r= ncol(Y)

  # Create knockoff variables
  Yk = knockoffs(Y)

  # Compute statistics
  W = statistic(X, Y, Yk)

  # Run the knockoff filter
  t = knockoff.threshold(W, fdr=fdr, offset=offset)
  selected = sort(which(W >= t))
  if (!is.null(X.names))
    names(selected) = Y.names[selected]

  # Package up the results.
  structure(list(call = match.call(),
                 X = X,
                 Y = Y,
                 Y = Yk,
                 statistic = W,
                 threshold = t,
                 selected = selected),
            class = 'YKnock')
}
