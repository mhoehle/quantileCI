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

for (p in seq(0,1,length=10)) {
  test_that("Manual summing up probs matching qbinom quantile",
            expect_equal(quantile_confint_so(x=x, p=p, conf.level=0.95),
                         quantile_confint_nyblom(x=x, p=p, conf.level=0.95, interpolate=FALSE)))
}

quantile_confint_nyblom(x=x, p=p, conf.level=0.95, interpolate=TRUE)

