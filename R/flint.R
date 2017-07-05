#' Water Monitoring Sample from Flint, Michigan, 2015
#'
#'@description The data correspond to the original January as well as the revised June version of the 2015 "Lead and Copper Report and Consumer Notice of Lead Result" report.
#' Data are taken from the sources stated below, not the original report. Note that a lead level of zero
#' means below the detection limit.
#'
#' @name flint
#' @docType data
#' @format A data frame with 71 rows and 2 variables:
#' \describe{
#'   \item{lead}{The measured lead concentration in the tap water in parts per billion (ppb = mg/L) as now also made explicit using the \code{units} package.}
#'   \item{exclude}{Logical indicating if the observations was removed by the authorities or not. In total two observations were removed as explained in the references.}
#' }
#' @references Langkjær-Bain, R. (2017), The murky tale of Flint's deceptive water data. Significance, 14: 16–21. \url{http://onlinelibrary.wiley.com/doi/10.1111/j.1740-9713.2017.01016.x/full}.
#'
#'Quantiles and the Flint water crisis (2017), Wicklin, R, \url{http://blogs.sas.com/content/iml/2017/05/17/quantiles-flint-water-crisis.html}
#'
#' Youtube video by Michigan Radio explaining the computation of the quantile \url{https://www.youtube.com/watch?v=9pql00zr700}
#'
#' @usage data(flint)
#' @import units
#' @keywords data
NULL

# flint <- read.csv(system.file("extdata", "flint.csv", package = "quantileCI"))[,-1]
# flint$lead <- units::set_units(flint$lead, ppb)
# save(flint, file=file.path("data", "flint.RData"))
