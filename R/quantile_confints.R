#' Quantile confidence interval based on interpolating the order
#' statistic as suggested in Nyblom (1991)
#'
#' @param x vector of observations
#' @param q quantile of interest
#' @param conf_level A conf_level * 100% confidence interval is
#'   computed
#' @param x_is_sorted Boolean (Default: FALSE) to safe sorting x, if
#'   it is already sorted. This is merely for speed reasons in
#'   situations where it is more efficient to only sort x once.
#' @details The interpolation procedure suggested by Nyblom (1992),
#'   which extends work by Hettmansperger and Sheather (1986) for the
#'   median is applied to the order statistic.
#' @references Nyblom J, Note in interpolated order statistics,
#'   Statistics and Probability Letters 14, p. 129-131.
#' @importFrom stats dbinom pbinom qbinom
#' @return A vector of length two containing the lower and upper limit of the confidence interval
#' @export

quantile_confint_nyblom <- function(x, q, conf_level=0.95, x_is_sorted=FALSE) {
  ##Define variables needed later
  if (!x_is_sorted) { x <-sort(x) }
  n <- length(x)
  alpha <- 1-conf_level

  ##Find d
  (d <- n - qbinom(1-alpha/2, size=n, prob=(1-q)))
  ##Find e
  xq <- qbinom(alpha/2, size=n, prob=(1-q))
  subtract_one <- (xq > 0) & (pbinom(xq, size=n, prob=(1-q)) > alpha/2)
  (e <- n - xq + subtract_one)

  d <- max(d,1)
  e <- min(e,n)

  ##Check paper
  lambda <- function(r, beta, q) {
    pi_r <- dbinom(r-1, prob=q, size=n)
    pi_rp1 <- dbinom(r, prob=q, size=n)
    (1 + (r*(1-q)*(pi_rp1 - beta))/( (n-r)*q*(beta - pi_r)) )^(-1)
  }
  ci_limit <- function(r,beta) {
    lambda <- lambda(r=r, beta=beta, q=q)
    (1-lambda)*x[r] + lambda*x[pmin(r+1,n)]  #can't go beyond n
  }

  return(c(ci_limit(d, beta=alpha/2),   ci_limit(e, beta=1-alpha/2)))
}
