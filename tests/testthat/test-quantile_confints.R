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

x <- rnorm(11)
quantile_confint_nyblom(x=x, p=0.5, conf.level=1-alpha, interpolate=TRUE)
median_confint_hs(x=x, conf.level=1-alpha, interpolate=TRUE)

#Sandbox test zone
if (FALSE) {
  EnvStats::eqnpar(x=x, p=p, ci=TRUE)
  quantile_confint_hmm(x=x, p=p, conf.level=0.95)

  ##Check prob P(x[2] <= xi_p <= x[5])
  mean(replicate(1e5, {
    xi_p <- qnorm(0.05)
    x <- sort(rnorm(10))
    (x[2] <= xi_p) & (xi_p <= x[5] )
  }))
  pbinom(5-1, size=10, prob=0.05) - pbinom(2-1, size=10, prob=0.05)

  `%<=%` <- function(x,y) {
    (x<y) | sapply(seq_len(length(x)), function(i) isTRUE(all.equal(x[i],y[i])))
  }
  `%>=%` <- function(x,y) {
    (x>y) | sapply(seq_len(length(x)), function(i) isTRUE(all.equal(x[i],y[i])))
  }

  n <- length(x)
  r <- 1:n
  p <- 0.5
  alpha <- 1

  c(5,2) %<=% c(3,1)
  c(5,2) %>=% c(7,1)

  idx <- which((1 - pbinom(r-1, prob=p, size=n)) %>=% (1 - alpha/2))
  idx
  idx <- which( pbinom(r-1, prob=p, size=n) %<=% (alpha/2))
  idx
  max(idx)
  d <- qbinom( alpha/2, prob=p, size=n)
  d <- d + (isTRUE(all.equal(pbinom(d, prob=p, size=n),alpha/2)))
  d

  idx <- which( pbinom(r-1, prob=p, size=n) >= 1-alpha/2)
  e <- min(idx)

  1 - (1-pbinom(e-1,size=n, prob=p))

  c(e,  qbinom(1-alpha/2, size=n, prob=p) + 1)

  pbinom(e-1, size=n, prob=p) - pbinom(d-1, size=n, prob=p)


}
