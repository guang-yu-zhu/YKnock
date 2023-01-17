#' Second-order Gaussian knockoffs
#'
#' This function samples second-order multivariate Gaussian knockoff variables.
#' First, a multivariate Gaussian distribution is fitted to the observations of Y.
#' Then, Gaussian knockoffs are generated according to the estimated model.
#'
#' @param Y n-by-p matrix of original responses.
#' @param method either "equi", "sdp" or "asdp" (default: "asdp").
#' This determines the method that will be used to minimize the correlation between the original variables and the knockoffs.
#' @param shrink whether to shrink the estimated covariance matrix (default: F).
#' @return A n-by-r matrix of knockoff variables.
#'
#' @family create
#'
#' @details
#' If the argument \code{shrink} is set to T, a James-Stein-type shrinkage estimator for
#' the covariance matrix is used instead of the traditional maximum-likelihood estimate. This option
#' requires the package \code{corpcor}. See \code{\link[corpcor]{cov.shrink}} for more details.
#'
#' Even if the argument \code{shrink} is set to F, in the case that the estimated covariance
#' matrix is not positive-definite, this function will apply some shrinkage.
#'
#' @references
#'   Candes et al., Panning for Gold: Model-free Knockoffs for High-dimensional Controlled Variable Selection,
#'   arXiv:1610.02351 (2016).
#'   \href{https://web.stanford.edu/group/candes/knockoffs/index.html}{https://web.stanford.edu/group/candes/knockoffs/index.html}
#'
#'
#' @keywords internal
create_YKnock <- function(Y, method=c("asdp","equi","sdp"), shrink=F) {
  method = match.arg(method)

  # Estimate the mean vectorand covariance matrix
  mu = colMeans(Y)

  # Estimate the covariance matrix
  if(!shrink) {
    Sigma = cov(Y)
    # Verify that the covariance matrix is positive-definite
    if(!is_posdef(Sigma)) {
      shrink=TRUE
    }
  }
  if(shrink) {
    if (!requireNamespace('corpcor', quietly=T))
      stop('corpcor is not installed', call.=F)
    Sigma = tryCatch({suppressWarnings(matrix(as.numeric(corpcor::cov.shrink(Y,verbose=F)), nrow=ncol(Y)))},
                     warning = function(w){}, error = function(e) {
                       stop("SVD failed in the shrinkage estimation of the covariance matrix. Try upgrading R to version >= 3.3.0")
                     }, finally = {})
  }

  # Sample the Gaussian knockoffs
  # Do not use ASDP unless p>500
  if ((nrow(Sigma)<=500) && method=="asdp") {
    method="sdp"
  }


  diag_s = diag(switch(method,
                       'equi' = create.solve_equi(Sigma),
                       'sdp'  = create.solve_sdp(Sigma),
                       'asdp' = create.solve_asdp(Sigma)))

  if (is.null(dim(diag_s))) {
    diag_s = diag(diag_s,length(diag_s))
  }

  # If diag_s is zero, we can only generate trivial knockoffs.
  if(all(diag_s==0)) {
    warning("The conditional knockoff covariance matrix is not positive definite. Knockoffs will have no power.")
    return(Y)
  }

  SigmaInv_s = solve(Sigma,diag_s)
  mu_k = Y - sweep(Y,2,mu,"-") %*% SigmaInv_s
  Sigma_k = 2*diag_s - diag_s %*% SigmaInv_s
  Y_k = mu_k + matrix(rnorm(ncol(Y)*nrow(Y)),nrow(Y)) %*% chol(Sigma_k)
  return(Y_k)
}
