#' Quantile confidence interval based on interpolating the order
#' statistic as suggested in Nyblom (1991)
#'
#' @param x vector of observations
#' @param p quantile of interest, \eqn{0 \leq p \leq 1}{0 <= p <= 1}
#' @param conf.level A conf.level * 100% confidence interval is
#'   computed
#' @param x_is_sorted Boolean (Default: FALSE) to safe sorting x, if
#'   it is already sorted. This is merely for speed reasons in
#'   situations where it is more efficient to only sort x once.
#' @param interpolate Boolean (Default: TRUE) stating whether to
#'   interpolate the order statistics. If no interpolation is selected
#'   then this is just the standard exact procedure based on the order
#'   statistics. Note: This procedure is conservative (i.e. coverage
#'   is usualler larger than the nominal conf.level and hence the
#'   interval is actually in general too large).
#' @details The interpolation procedure suggested by Nyblom (1992),
#'   which extends work by Hettmansperger and Sheather (1986) for the
#'   median is applied to the order statistic.
#' @examples
#' set.seed(123)
#' x <- rnorm(25)
#' quantile_confint_nyblom(x=x, p=0.8, conf.level=0.95, interpolate=TRUE)
#' @references Nyblom J, Note in interpolated order statistics,
#'   Statistics and Probability Letters 14, p. 129-131.
#' @importFrom stats dbinom pbinom qbinom
#' @return A vector of length two containing the lower and upper limit
#'   of the confidence interval
#' @export

quantile_confint_nyblom <- function(x, p, conf.level=0.95, x_is_sorted=FALSE, interpolate=TRUE) {
  ##Define variables needed later
  if (!x_is_sorted) { x <-sort(x) }
  n <- length(x)
  alpha <- 1-conf.level

  ##Find d
  (d <- n - qbinom(1-alpha/2, size=n, prob=(1-p)))
  ##Find e
  x_p <- qbinom(alpha/2, size=n, prob=(1-p))
  subtract_one <- (x_p > 0) & (pbinom(x_p, size=n, prob=(1-p)) > alpha/2)
  (e <- n - x_p + subtract_one)

  d <- max(d,1)
  e <- min(e,n)

  ##If no interpolatation is to be done.
  if (!interpolate) return(x[c(d,e)])

  ##See Nyblom paper for the formula
  lambda <- function(r, beta, p) {
    pi_r <- dbinom(r-1, prob=p, size=n)
    pi_rp1 <- dbinom(r, prob=p, size=n)
    (1 + (r*(1-p)*(pi_rp1 - beta))/( (n-r)*p*(beta - pi_r)) )^(-1)
  }
  ci_limit <- function(r,beta) {
    lambda <- lambda(r=r, beta=beta, p=p)
    (1-lambda)*x[r] + lambda*x[pmin(r+1,n)]  #can't go beyond n
  }
  ##Return the Nyblom interval
  return(c(ci_limit(d, beta=alpha/2),   ci_limit(e, beta=1-alpha/2)))
}



#' Confidence interval method for the median by the method of
#' Hettmansperger & #' Sheather (1991)
#'
#' @param x vector of observations
#' @param conf.level A conf.level * 100% confidence interval is
#'   computed
#' @param x_is_sorted Boolean (Default: FALSE) to safe sorting x, if
#'   it is already sorted. This is merely for speed reasons in
#'   situations where it is more efficient to only sort x once.
#' @param interpolate Boolean (Default: TRUE) stating whether to
#'   interpolate the order statistics. If no interpolation is selected
#'   then this is just the standard exact procedure based on the order
#'   statistics. Note: This procedure is conservative (i.e. coverage
#'   is usualler larger than the nominal conf.level and hence the
#'   interval is actually in general too large).
#' @details The interpolation procedure suggested by Hettmansperger
#'   and Sheather (1986) for the median is applied to the order
#'   statistic.
#' @references Hettmansperger TP and Sheather SJ (1986), Confidence
#'   intervals based on interpolated order statistics, Statistics and
#'   Probability Letters, 4, p. 75-79.
#' @importFrom stats dbinom pbinom qbinom
#' @return A vector of length two containing the lower and upper limit
#'   of the confidence interval
#' @export
#'
confint_median_hs <- function(x, conf.level=0.95, x_is_sorted=FALSE, interpolate=TRUE) {
  if (!x_is_sorted) { x <- sort(x) }
  n <- length(x)
  d <- 1

  ##Check that we can actually meet the desired confidence at the widest possible interval
  conf_level_d <- 1-2*pbinom((d)-1, size=n, prob=0.5)
  if (conf_level_d < conf.level) {
    warning("Proper coverage level can not be attained. Using widest possible interval!")
    ##Stop with an artificially enlarged interval to capture the entire span
    return(cbind(x[c(1,n)],x[c(1,n)]))
  }

  ##Loop until we find the first d s.t. c(x[d],x[n-d+1]) has a coverage lower than conf.level
  repeat {
    conf_level_dp1 <- 1-2*pbinom((d+1)-1, size=n, prob=0.5)

    if ((conf_level_dp1 < conf.level) | (d > floor(n/2))) break

    d <- d+1
    conf_level_d <- conf_level_dp1
  }

  ##Exact interval, no interpolation (if requested)
  if (!interpolate) return( c(x[d],x[n-d+1]))

  ##Improved interval using the Hettmansperger & Sheather (1986)
  ##interpolation method of the order statistics
  I <- (conf_level_d - conf.level) / (conf_level_d - conf_level_dp1)
  lambda <- (n-d) * I / (d + (n-2*d)*I)
  ci_improved <- c((1-lambda)*x[d]     +  lambda*x[d+1],
                   (1-lambda)*x[n-d+1] +  lambda*x[n-d])

  #Done, return Hettmansperger and Sheather interval
  return(ci_improved)
}

#' Confidence interval method for a given quantile based on the basic
#' bootstrap and using the percentile method.
#'
#' @param x vector of observations
#' @param p quantile of interest
#' @param conf.level A conf.level * 100% confidence interval is
#'   computed
#' @param R number of replications to use in the bootstrap (Default: 999)
#' @details Basic bootstrap with the confidence interval computed based on the percentile method.
#' @importFrom stats quantile
#' @return A vector of length two containing the lower and upper limit
#'   of the confidence interval
#' @export
#'
quantile_confint_boot <- function(x, p, conf.level=0.95, R=999) {
  b <- boot::boot(x, statistic=function(data, idx) quantile(x[idx],prob=q,type=3), R=R)
  boot::boot.ci(b, conf=conf.level, type="perc")$percent[4:5]
}
