#' Threshold for the knockoff filter
#'
#' Computes the threshold for the knockoff filter.
#'
#' @param W the test statistics
#' @param fdr target false discovery rate (default: 0.1)
#' @param offset either 0 or 1 (default: 1). The offset used to compute the rejection threshold on the
#' statistics. The value 1 yields a slightly more conservative procedure ("knockoffs+") that
#' controls the FDR according to the usual definition, while an offset of 0 controls a modified FDR.
#' @return The threshold for variable selection.
#'
#' @export
knockoff.threshold <- function(W, fdr=0.10, offset=1) {
  if(offset!=1 && offset!=0) {
    stop('Input offset must be either 0 or 1')
  }
  ts = sort(c(0, abs(W)))
  ratio = sapply(ts, function(t)
    (offset + sum(W <= -t)) / max(1, sum(W >= t)))
  ok = which(ratio <= fdr)
  ifelse(length(ok) > 0, ts[ok[1]], Inf)
}

