% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quantile_confints.R
\name{quantile_confint_boot}
\alias{quantile_confint_boot}
\title{Confidence interval method for a given quantile based on the basic
bootstrap and using the percentile method.}
\usage{
quantile_confint_boot(x, p, conf.level = 0.95, R = 999, type = 7)
}
\arguments{
\item{x}{vector of observations}

\item{p}{quantile of interest}

\item{conf.level}{A conf.level * 100\% confidence interval is
computed}

\item{R}{number of replications to use in the bootstrap (Default: 999)}

\item{type}{Type of empirical quantile estimation procedure, @seealso the \code{\link{quantile}} function.}
}
\value{
A vector of length two containing the lower and upper limit
  of the two-sided confidence interval.
}
\description{
Confidence interval method for a given quantile based on the basic
bootstrap and using the percentile method.
}
\details{
Basic bootstrap with the confidence interval computed based on the percentile method.
}
\examples{
set.seed(123)
x <- rnorm(25)
quantile_confint_boot(x=x, p=0.8, conf.level=0.95, R=999)
}
