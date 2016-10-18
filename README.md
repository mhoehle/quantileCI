# quantileCI

Compute confidence intervals for quantiles in the one-sample
situation. The R package can be installed directly from github as
follows:

`
devtools::install_github("hoehleatsu/quantileCI")
`

Here is a minimal working example on how to use the package once it is
installed:

`
x <- rnorm(25)
#Compute 25% quantile
quantile(x, prob=0.25, type=3)
#Compute confidence interval for it (two methods)
quantileCI::quantile_confint_nyblom(x=x, p=0.25, conf.level=0.95)
quantileCI::quantile_confint_nyblom(x=x, p=0.25, conf.level=0.95, interpolate=FALSE)
`
