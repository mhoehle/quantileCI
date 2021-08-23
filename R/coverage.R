#' Computing the coverage of different confidence interval methods for
#' quantiles by Monte Carlo integration.
#'
#' @param qci_fun Function which given n, p and conf.level
#'   computed a set of different confidence intervals. Should return a
#'   matrix of dimension 2 x (no. of methods) which contains the lower
#'   and upper bound of each confidence interval method.
#' @param n Size of the sample to generate in the simulation
#' @param rfunc Function for generating the samples
#' @param qfunc Quantile function for computing the true quantile
#' @param p The quantile of interest 0 <= p <= 1
#' @param conf.level conf.level * 100\% two-sided confidence intervals
#'   are computed
#' @param \dots Additional arguments passed to \code{rfunc} and \code{qfunc}
#' @return A vector of Booleans of length (no. methods) stating if
#'   each method contains the true value or not
#' @importFrom stats qnorm rnorm
#' @examples
#' ##Function to compute different methods on same x.
#' quantile_confints <- function(x, p, conf.level, x_is_sorted=FALSE) {
#'   if (!x_is_sorted) { x <- sort(x)}
#'
#'   ##Compute the various confidence intervals as above
#'   res <- data.frame(
#'     nyblom_exact=quantileCI::quantile_confint_nyblom(x=x, p=p, conf.level=conf.level,
#'                                                  x_is_sorted=TRUE, interpolate=FALSE),
#'     nyblom_interp=quantileCI::quantile_confint_nyblom(x=x, p=p, conf.level=conf.level,
#'                                                  x_is_sorted=TRUE, interpolate=TRUE),
#'     boot=quantileCI::quantile_confint_boot(x, p=p, conf.level=conf.level, R=999)
#'   )
#'   if (p == 0.5) {
#'     res$hs_interp = quantileCI::median_confint_hs(x=x, conf.level=conf.level,
#'                                                  x_is_sorted=TRUE, interpolate=TRUE)
#'   }
#'   return(res)
#' }
#'
#' ## One run of the simulation function
#' quantileCI::qci_coverage_one_sim(qci_fun=quantile_confints, n=100,p=0.5,conf.level=0.95)
#'
#' ## Several runs, calculate row means to get coverage by sampling
#' res <- sapply(1L:10L, function(i) {
#'   quantileCI::qci_coverage_one_sim(qci_fun=quantile_confints, n=100,p=0.5,conf.level=0.95)
#' })
#' res
#' @export
#'
qci_coverage_one_sim <- function(qci_fun, n, rfunc=rnorm, qfunc=qnorm, p=0.5, conf.level=0.95, ...) {
  ##Draw the sample and check the truth
  x <- rfunc(n,...)
  truth <- qfunc(p,...)

  ##Compute confidence intervals for all methods
  cis <- qci_fun(x=x, p=p, conf.level=conf.level)

  ##Check if intervals cover the true value
  covers <- (cis[1,] <= truth) & (cis[2,] >= truth)
  return(as.data.frame(covers))
}
