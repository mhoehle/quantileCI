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

  ######################################################################
  ## Compare with Table 1 of Nyblom (1992)
  ######################################################################
  set.seed(123); n<- 11; x <- sort(rnorm(n)) ; p <- 0.25 ; conf.level=0.9
  (ci <- quantile_confint_nyblom(x, p=p, conf.level=conf.level, interpolate=FALSE))
  (idx <- pmatch(ci, x))

  #Tail probabilities
  (p_tail_16 <- c(pbinom(1 - 1, prob=0.25, size=n),   1-pbinom(6-1, prob=0.25, size=n)))
  1-sum(p_tail_16)

  (p_tail_25 <- c(pbinom(2 - 1, prob=0.25, size=n),   1-pbinom(5-1, prob=0.25, size=n)))
  1-sum(p_tail_25)

  #Compute lower limit
  lambda <- 0.1496291638
  q_truth <- qnorm(p)
  (1-lambda) * x[1] + lambda*x[2]

  quantile_confint_nyblom(x, p=p, conf.level=0.9)

  ##Selection of methods to use
  quantile_confints <- function(x, p, conf.level, x_is_sorted=FALSE, ...) {
    if (!x_is_sorted) { x <- sort(x)}

    ##Compute the various confidence intervals as above
    res <- data.frame(
                      EnvStats_asymp=as.numeric(EnvStats::eqnpar(x=x, p=p, ci=TRUE, ci.method="normal.approx",approx.conf.level=conf.level)$interval$limits),
                      nyblom_interp=quantileCI::quantile_confint_nyblom(x=x, p=p, conf.level=conf.level,x_is_sorted=TRUE,interpolate=TRUE)
                      )
    if (p == 0.5) {
      res$hs_interp = quantileCI::median_confint_hs(x=x,  conf.level=conf.level,x_is_sorted=TRUE,interpolate=TRUE)
    }
    return(res)
  }

  simulate.coverage_qci <- function(n=n,p=p,conf.level=0.9, nSim=1e4, ...) {
    ##Windows users: change to below lapply function or use snow.
    lapplyFun <- function(x, mc.cores=NA, ...) lapply(x, ...) #parallel::mclapply
    ##lapplyFun <- parallel::mclapply

    sims <- dplyr::bind_rows(
      lapplyFun(1L:nSim, function(i) {
        quantileCI::qci_coverage_one_sim(qci_fun=quantile_confints, n=n,p=p,conf.level=conf.level,...)
      }, mc.cores = parallel::detectCores() - 1)
    ) %>% summarise_each(funs(mean))
    return(sims)
  }

  simulate.coverage_qci(n=n, p=p, conf.level=0.9, nSim=25e3)
  simulate.coverage_qci(n=25, p=0.8, conf.level=0.95, nSim=25e3)

  simulate.coverage_qci(n=101, p=0.9, rfunc=rt, qfunc=qt, conf.level=0.95, df=1,nSim=1e5)


  ##Numbers from the blog post
  set.seed(as.integer(charToRaw("R")))
  n <- 25 ; x <- sort(rnorm(n)) ; p <- 0.8 ; alpha <- 0.1
  ##Numbers from Nyblom
  set.seed(123); n<- 11; x <- sort(rnorm(n)) ; p <- 0.25 ; alpha <- 0.1
  x_p <- qnorm(p)
  hat_x_p <- quantile(x, prob=p, type=1)
  ci1 <- quantileCI::quantile_confint_nyblom(x, p=p, interpolate=FALSE, conf.level=1-alpha)
  ci2 <- quantileCI::quantile_confint_nyblom(x, p=p, interpolate=TRUE, conf.level=1-alpha)

  idx <- pmatch(ci1, x)

  ##Compute coverage for smoothed interval by Monte Carlo sampling (this might be slow)
  ##Try to do this using numerical integration instead.
  d <- 17 ; e <- 24
  lambda1 <- 0.09313647
  lambda2 <- 0.4259059
  ##See Nyblom (1992) paper for the formula
  lambda <- function(r, beta, p) {
    pi_r   <- pbinom(r-1, prob=p, size=n)
    pi_rp1 <- pbinom(r, prob=p, size=n)

    (1 + (r*(1-p)*(pi_rp1 - beta))/( (n-r)*p*(beta - pi_r)) )^(-1)
  }
  d <- idx[1]
  e <- idx[2]
  lambda1 <- lambda(r=d, beta=alpha/2, p=p)
  lambda2 <- lambda(r=e-1, beta=1-alpha/2, p=p)


  tails  <- replicate(1e6, {
    y <- sort(rnorm(n))
    ##These numbers are computed by the Nyblom procedure. Computed manually
    ##from the functions defined inside the quantile_confint_nyblom function.
    c((1-lambda1)*y[d] + lambda1*y[d+1] > x_p, (1-lambda2)*y[e-1] + lambda2*y[e] <= x_p)

  })
  dim(tails)
  rowMeans(tails)
  cov2 <- 1 - sum(rowMeans(tails))
  cov2

  ######################################################################
  ##Compute coverage numerically! NOT WORKING ATM!
  ######################################################################

  ##Tail probs of the weighted sum, i.e. compute
  ##  P( (1-\lambda) x_{(r)} + lambda * x_{(r+1)} > xi_p)
  p_wsum <- function(xi_p, r,lambda, F, f) {
    ##Function giving the PMF of z = (1-\lambda) x_{(r)} + lambda * x_{(r+1)}
    f_z <- function(z) {
      f_to_int <- function(xr,z) {
        xrp1 <- (z - (1-lambda)*xr)/lambda
        ##consistency check: (1-lambda)*xr + (lambda)*xrp1
        ##See https://en.wikipedia.org/wiki/Order_statistic for density
        (xrp1 > xr) * exp(lfactorial(n) - lfactorial(r-1) - lfactorial(n - r -1)) * F(xr)^(r-1) * (1-F(xrp1))^(n-r-1)*f(xr)*f(xrp1)
        ##(xrp1 > xr) * exp(lfactorial(n) - lfactorial(r-1) - lfactorial(n - r -1) + (r-1)*log(F(xr)) + (n-r-1)*log((1-F(xrp1))) + log(f(xr)) + log(f(xrp1)))
      }
      one <- function(z) {
        integrate( f_to_int, lower=-Inf, upper=z, z=z)$value
      }
      sapply(z, one)
    }
    #Calculate P(z >= x_p) by numerical integration
    integrate(f_z, lower=x_p,upper=10)$value
  }
  #Get the two tail probabilities for the procedure
  p_wsum( x_p, r=d, lambda=lambda1,   F=pnorm, f=dnorm)
  p_wsum( x_p, r=e-1, lambda=lambda2, F=pnorm, f=dnorm)
}
