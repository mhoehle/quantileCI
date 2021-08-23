##https://stat.ethz.ch/pipermail/r-help/2007-February/124958.html
##with simple out of bound fix by hoehle
quantile_confint_so <-function(x, p,conf.level=0.95, x_is_sorted=FALSE){
# x is the sample, p (0<p<1) the quantile, P the confidence level
  if (!x_is_sorted) {x<-sort(x)}
  alpha <- 1-conf.level
  n<-length(x)
  s <- min(which(pbinom((0:n),n,p) >= 1-(alpha)/2))
  r <- max(which(pbinom((0:n),n,p) <= (alpha)/2))
  c(x[pmax(r,1)],x[pmin(s,n)]) ##hoehle: add out of index protection using pmax/pmin
# x[r] is the lower limit, x[s] the upper limit, of the CI
}

set.seed(123)
x <- rnorm(10)

for (p in seq(0.1,1,length=10)) {
  test_that("Comparing summing up probs with qbinom quantile",
            expect_equal(quantile_confint_so(x=x, p=p, conf.level=0.95),
                         quantile_confint_nyblom(x=x, p=p, conf.level=0.95, interpolate=FALSE,fix_interval=FALSE)))
}

## set.seed(123)
## for (n in c(10,25,100)) {
##   for (p in seq(0,1,length=10)) {
##     for (alpha in c(0.05, 0.1)) {
##       test_that("Comparing the two Nyblom (1992) methods",
##                 expect_equal(quantile_confint_nyblom(x=x, p=p, conf.level=1-alpha, interpolate=TRUE),
##                              quantile_confint_nyblom2(x=x, p=p, conf.level=1-alpha, interpolate=TRUE)))
##     }
##   }
## }

##Check procedures for the median
set.seed(123)
for (i in 1:10) {
  x <- rnorm(rpois(1,10*i))
  test_that("Equality of Nyblom procedure to Hettmansperger & Sheather procedure for median",
            expect_equal(
              quantile_confint_nyblom(x=x, p=0.5, conf.level=0.95, interpolate=TRUE),
              median_confint_hs(x=x, conf.level=0.95, interpolate=TRUE)))
}

