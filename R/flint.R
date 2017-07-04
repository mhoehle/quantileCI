#' Water Monitoring Sample from Flint, Michigan
#'
#' @name flint
#' @docType data
#' @format A data frame with 71 rows and 2 variables:
#' \describe{
#'   \item{lead}{The measured lead concentration in the tap water (in ppb = mg/L)}
#'   \item{exclude}{Logical indicating if the observations was removed by the authorities or not. In total two observations were removed as explained in the references.}
#' }
#' @references Langkjær-Bain, R. (2017), The murky tale of Flint's deceptive water data. Significance, 14: 16–21. \url{http://onlinelibrary.wiley.com/doi/10.1111/j.1740-9713.2017.01016.x/full}.
#'
#' Youtube video by Michigan Radio explaining the computation of the quantile \url{https://www.youtube.com/watch?v=9pql00zr700}
#' @usage data(flint)
#' @keywords data
NULL

# flint <- read.csv(system.file("extdata", "flint.csv", package = "quantileCI"))[,-1]
# saveRDS(flint, file=file.path("data", "flint.rds"))
# save(flint, file=file.path("data", "flint.RData"))
