# quantileCI

Compute confidence intervals for quantiles in the one-sample
situation. 

## Installing the package

The R package can be installed directly from github as
follows:

`
devtools::install_github("hoehleatsu/quantileCI")
`

If you have problems installing from sources: package binaries are also available from my [r-universe](https://hoehleatsu.r-universe.dev) and can be installed using:

`
options(repos = c(
    hoehleatsu = 'https://hoehleatsu.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('quantileCI')
`

## Examples
Here is a minimal working example on how to use the package once it is installed:

```
    require(quantileCI)
    x <- rnorm(25)
    #Compute 25% quantile as inverse of ECDF (i.e. type=1)
    quantile(x, prob=0.25, type=1)
    #Compute confidence interval for it (two methods)
    quantileCI::quantile_confint_exact(x=x, p=0.25, conf.level=0.95)
    quantileCI::quantile_confint_nyblom(x=x, p=0.25, conf.level=0.95)
```

...and here is the package applied to some actual data, i.e. monitoring data from the [Flint Water System](https://staff.math.su.se/hoehle/blog/2017/06/18/quantiles.html). Altogether, we want a 90% CI such that the CI consists of two one-sided tests each done at the 5% significance level.

```
    require(quantileCI)
    data(flint)
    #Compute type=5 quantile as in https://www.youtube.com/watch?v=9pql00zr700
    quantile(flint$lead, probs=0.9, type=5)
    ##Compute type=1
    quantile(flint$lead, probs=0.9, type=1)
    ##Compute corresponding non-parametric 90% CI
    quantileCI::quantile_confint_exact(flint$lead, p=0.9, conf.level=0.9)
```

A more comprehensive treatment of the package is available as part of the blog post [Better confidence intervals for quantiles](https://staff.math.su.se/hoehle/blog/2016/10/23/quantileCI.html).
