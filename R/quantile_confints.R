#' Confidence interval method for a given quantile based on the basic
#' bootstrap and using the percentile method.
#'
#' @param x vector of observations
#' @param p quantile of interest
#' @param conf.level A conf.level * 100\% confidence interval is
#'   computed
#' @param R number of replications to use in the bootstrap (Default: 999)
#' @param type Type of empirical quantile estimation procedure, @seealso the \code{\link{quantile}} function.
#' @details Basic bootstrap with the confidence interval computed based on the percentile method.
#' @importFrom stats quantile
#' @return A vector of length two containing the lower and upper limit
#'   of the two-sided confidence interval.
#' @examples
#' set.seed(123)
#' x <- rnorm(25)
#' quantile_confint_boot(x=x, p=0.8, conf.level=0.95, R=999)
#' @export
#'
quantile_confint_boot <- function(x, p, conf.level=0.95, R=999, type = 7) {
  b <- boot::boot(x, statistic=function(data, idx) quantile(x[idx],prob=p,type=type), R=R)
  boot::boot.ci(b, conf=conf.level, type="perc")$percent[4:5]
}

#' Two-sided confidence interval method for the median by the method of
#' Hettmansperger & Sheather (1991)
#'
#' @param x vector of observations
#' @param conf.level A conf.level * 100\% confidence interval is
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
#' @examples
#' set.seed(123)
#' x <- rnorm(25)
#' median_confint_hs(x=x, conf.level=0.95, interpolate=TRUE)
#' @export
#'
median_confint_hs <- function(x, conf.level=0.95, x_is_sorted=FALSE, interpolate=TRUE) {
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

#' Two-sided quantile confidence interval based on interpolating the order
#' statistic as suggested in Nyblom (1991)
#'
#' @param x vector of observations
#' @param p quantile of interest, \eqn{0 \leq p \leq 1}{0 <= p <= 1}
#' @param conf.level A conf.level * 100\% confidence interval is
#'   computed
#' @param x_is_sorted Boolean (Default: FALSE) indicating if \code{x}
#'   is already sorted and, hence, it is not necessary to sort it again.
#'   This is merely for speed reasons in situations where it is more efficient
#'   to only sort x once.
#' @param interpolate Boolean (Default: TRUE) stating whether to
#'   interpolate the order statistics. If no interpolation is selected
#'   then this is just the standard exact procedure based on the order
#'   statistics. Note: This procedure is conservative (i.e. coverage
#'   is usualler larger than the nominal conf.level and hence the
#'   interval is actually in general too large).
#' @param fix_interval Boolean (Default: TRUE) For the case with no
#'   interpolation, try to extend interval upwards if coverage is too
#'   little.
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
#'
quantile_confint_nyblom <- function(x, p, conf.level=0.95, x_is_sorted=FALSE, interpolate=TRUE,fix_interval=TRUE) {
  if (!x_is_sorted) { x <- sort(x) }
  n <- length(x)
  alpha <- 1-conf.level

  ##Alternative implementation, which is probably faster
  d <- qbinom( alpha/2, prob=p, size=n)
  d <- d + (isTRUE(all.equal(pbinom(d, prob=p, size=n),alpha/2)))
  e <- qbinom(1-alpha/2, size=n, prob=p) + 1

  ##Safeguard the output
  d <- max(d,1)
  e <- min(e,n)

  ##Stop here if no interpolation wanted.
  ##If no interpolatation is to be done.
  if (!interpolate) {
    ##This part is borrowed from the EnvStats:::ci.qnpar procedure,
    ##which is called as part of EnvStats::eqnpar (empirical quantile
    ##non-parametric) If there is too little coverage for the
    ##interval then increase length by one (if possible)
    if (fix_interval & (p != 0) & (p != 1)) {
      if ((pbinom(e, n, p) - pbinom(d - 1, n, p)) <= 1 - alpha) {
        e <- min(n, e + 1)
      }
    }
    return(x[c(d,e)])
  }


  ##See Nyblom (1992) paper for the formula
  lambda <- function(r, beta, p) {
    pi_r   <- pbinom(r-1, prob=p, size=n)
    pi_rp1 <- pbinom(r, prob=p, size=n)

    (1 + (r*(1-p)*(pi_rp1 - beta))/( (n-r)*p*(beta - pi_r)) )^(-1)
  }
  ci_limit <- function(r,beta) {
    lambda <- lambda(r=r, beta=beta, p=p)
    (1-lambda) * x[r] + lambda * x[pmin(r+1,n)]  #can't go beyond n
  }

  ##Return the Nyblom interval (or at least my interpretation of it)
  return(c(ci_limit(d, beta=alpha/2),   ci_limit(e-1, beta=1-alpha/2)))
}

#' Standard exact two-sided quantile confidence interval based on the
#' binomial distribution
#'
#' @param x vector of observations
#' @param p quantile of interest, \eqn{0 \leq p \leq 1}{0 <= p <= 1}
#' @param conf.level A \code{conf.level} * 100\% confidence interval is
#'   computed
#' @param x_is_sorted Boolean (Default: FALSE) to safe sorting x, if
#'   it is already sorted. This is merely for speed reasons in
#'   situations where it is more efficient to only sort x once.
#' @param fix_interval Boolean (Default: TRUE) For the case with no
#'   interpolation, try to extend interval upwards if coverage is too
#'   little.
#' @details This function is a pure call-through to the Nyblom function
#'   with \code{interpolate=FALSE}.
#' @examples
#' set.seed(123)
#' x <- rnorm(25)
#' quantile_confint_exact(x=x, p=0.8, conf.level=0.95)
#' @references Nyblom J, Note in interpolated order statistics,
#'   Statistics and Probability Letters 14, p. 129-131.
#' @importFrom stats dbinom pbinom qbinom
#' @return A vector of length two containing the lower and upper limit
#'   of the confidence interval
#' @export
#'
quantile_confint_exact <- function(x, p, conf.level=0.95, x_is_sorted=FALSE, fix_interval=TRUE) {
  quantile_confint_nyblom(x=x, p=p, conf.level=conf.level, x_is_sorted=x_is_sorted, fix_interval=fix_interval, interpolate=FALSE)
}
