library(dplyr)
library(magrittr)
library(ggplot2)

##Data taken from Quantiles and the Flint water crisis - The DO Loop.html
##Used 0 for "not detectable" */
##Lead Concentration (ppb)"
flint <- data.frame(lead=c(0,0,0,0,0,0,0,0,0,0,0,0,0,
                  1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,
                  3,3,3,3,3,3,3,3,3,3,3,4,4,
                  5,5,5,5,5,5,5,5,6,6,6,6,7,7,7,
                  8,8,9,10,10,11,13,18,20,21,22,29,43,43,104)) %>%
  mutate(exclude = (lead == 20 | lead == 104)) %>% as.tbl

write.csv(flint, file="flint.csv")
##Synthetic data set, where 90% quantile is below 15 (depends on definition)
if (FALSE) {
  flint <- data.frame(lead=c(1,2,3,4,5,6,7,8,9,20)) %>%
    mutate(exclude = FALSE) %>% as.tbl
}

table(flint$lead)
nrow(flint)

ecdf(flint$lead)(c(17,18))
ecdf(flint$lead[!flint$exclude])(c(12,13))
quantile(flint$lead, probs=0.9, type=1)
quantile(flint$lead[!flint$exclude], probs=0.9, type=1)

##Sample according e-CFR §141.80   General requirements.
idx <- 0.9*nrow(flint)
sort(flint %$% lead )[idx]
sort(flint %$% lead)[c(63,64)]
ceiling(idx)

##This is what we should do
quantile(flint$lead, probs=0.9, type=1)
##And this is what everybody would understand
mean(flint$lead > 15)


##Compute 90% quantile using the 9 different methods
c(sapply(1:9, function(i) {
  structure(flint %>% summarise(q90=quantile(lead, prob=0.9,type=i)) %>% .[[1]],names=paste0("y90%_",i))
}),structure(Hmisc::hdquantile(flint$lead, probs=0.9, se=FALSE),names="y90%_hd"))

##Same but now with the outlier
sapply(1:9, function(i) {
  structure(flint %>% filter(!exclude) %>% summarise(q90=quantile(lead, prob=0.9,type=i)) %>% .[[1]], names=paste0("y90%_",i))
})

##Check min-max (this is the same for all methods. But this
##doesn't match with the description in the paper by Hyndman & Yan?
sapply(1:9, function(i) {
  structure(flint %$% lead %>% quantile(probs=c(0,1),type=i),names=paste0("y",c(0,100),"%_",i))
})

##Illustrate the different quantile methods on a grid.
flint %>% arrange(lead)

prob <- seq(0,1,length.out=nrow(flint))

df <- data.frame(p=sort(unique(c(seq(0,1,length=1000),seq(0.8,1,length=2000),seq(0.89,0.91,length=1000)))))
df %<>% rowwise %>% do({
  data.frame(p=.$p, y_p=c(sapply(1:9, function(i) quantile(flint$lead,probs=.$p,type=i)),Hmisc::hdquantile(flint$lead, probs=.$p)),q_method=c(as.character(1:9),"hd"))
})

##Illustrate different outputs
ggplot(df, aes(x=p, y=y_p, color=q_method)) + geom_line() + scale_x_continuous(labels=scales::percent) + coord_cartesian(xlim=c(0.5,1)) + geom_point(data=data.frame(p=prob,y_p=flint$lead,q_method=NA),aes(x=p, y=y_p)) +  geom_point(data=data.frame(p=(1:nrow(flint))/nrow(flint),y_p=flint$lead,q_method=NA),aes(x=p, y=y_p),color="pink") + ylab(expression(y[p])) + geom_hline(yintercept=15,lty=2,col="indianred2")

ggplot(df, aes(x=p, y=y_p, color=q_method)) + geom_line() + scale_x_continuous(labels=scales::percent) + coord_cartesian(xlim=c(0.85,0.95),ylim=c(0,30)) + geom_point(data=data.frame(p=prob,y_p=flint$lead,q_method=NA),aes(x=p, y=y_p)) +  geom_point(data=data.frame(p=(1:nrow(flint))/nrow(flint),y_p=flint$lead,q_method=NA),aes(x=p, y=y_p),color="pink") + ylab(expression(y[p])) + geom_hline(yintercept=15,lty=2,col="indianred2")

ggplot(df, aes(x=p, y=y_p, color=q_method)) + geom_line() + scale_x_continuous(labels=scales::percent) + coord_cartesian(xlim=c(0.88,0.92),ylim=c(12,22)) + geom_point(data=data.frame(p=prob,y_p=flint$lead,q_method=NA),aes(x=p, y=y_p)) + geom_point(data=data.frame(p=(1:nrow(flint))/nrow(flint),y_p=flint$lead,q_method=NA),aes(x=p, y=y_p),color="pink") + ylab(expression(y[p])) + geom_hline(yintercept=15,lty=2,col="indianred2")

##Subset containing method 1 and 6
ggplot(df %>% filter(q_method %in% c(1,6)), aes(x=p, y=y_p, color=q_method)) + geom_line() + scale_x_continuous(labels=scales::percent) + coord_cartesian(xlim=c(0.88,1),ylim=c(12,max(flint$lead))) + geom_point(data=data.frame(p=prob,y_p=flint$lead,q_method=NA),aes(x=p, y=y_p)) + geom_point(data=data.frame(p=(1:nrow(flint))/nrow(flint),y_p=flint$lead,q_method=NA),aes(x=p, y=y_p),color="pink") + geom_point(data=data.frame(p=(1:nrow(flint))/(nrow(flint)+1),y_p=flint$lead,q_method=NA),aes(x=p, y=y_p),color="steelblue") + ylab(expression(y[p])) + geom_hline(yintercept=15,lty=2,col="indianred2")

##Illustrate type=1 with segments
flint %<>% mutate(cdf=(1:nrow(flint))/nrow(flint))
flint_ecdf <- rbind(data.frame(lead=-Inf, exclude=FALSE,cdf=0), flint,
                    data.frame(lead=Inf, exclude=FALSE,cdf=1))
ggplot(flint_ecdf, aes(x=lead,y=cdf)) + geom_step(direction="hv")

ggplot(flint, aes(x=cdf,y=lead)) + geom_step(direction="hv") + coord_cartesian(ylim=range(flint$lead))

ggplot(flint, aes(x=lead)) + stat_ecdf(geom="step")
#

plot(ecdf(flint$lead))
######################################################################
##Sample size calculation
######################################################################
alpha <- 0.05
beta <- 0.05 #0.05
p0 <- 0.1 #0.9 #0.1 ##0.683 #0.1
p1 <- seq(0.15,0.5,length=100) #0.95 #0.15 ##0.818#0.15

z_alpha <- qnorm(1-alpha)
z_beta <- qnorm(1-beta)
n_star <- ceiling((z_alpha + z_beta)^2 / (2 * (asin(sqrt(p1)) - asin(sqrt(p0))))^2)

##This is the line for finding the difference if a statistical test
##is used.
df <- data.frame(p1=p1, n_star=n_star)
ggplot(df, aes(x=p1, y=n_star)) + geom_line()


p_emprate_above_threshold <- function(n, p0, p1) {
  first <- floor(p0*n + 1)
  1 - pbinom(first-1, size=n, prob=p1)
}

##detect change with prob 1-beta
n_detect_with_prob <- function(p0, p1, p_detect=0.95) {
  smallest <- 1 ; largest <- 1000
  repeat {
    n_grid <- smallest:largest
    dist <- abs(p_emprate_above_threshold(n=n_grid, p0=p0, p1=p1) - p_detect)
    if (which.min(dist) < 0.9*length(dist)) {
      return(n_grid[which.min(dist)])
    } else {
      smallest <- floor(0.9*largest) ; largest <- largest*2
    }
  }
}

n_detect_with_prob(p0=p0, p1=0.15, p_detect=0.95)
n_detect_with_prob(p0=p0, p1=0.15, p_detect=0.95)

p_flint <- mean(flint$lead > 15)
n_detect_with_prob(p0=p0, p1=p_flint, p_detect=0.95)
n_detect_with_prob(p0=p0, p1=p_flint, p_detect=0.90)

n_detect_with_prob(p0=p0, p1=0.17, p_detect=0.95)
p_emprate_above_threshold(n=71,p0=0.1,p1=0.17)


p_emprate_above_threshold(n=10, p0=0.1, p1=0.15)
p_emprate_above_threshold(n=60, p0=0.1, p1=0.15)
p_emprate_above_threshold(n=100, p0=0.1, p1=0.15)

df %<>% rowwise %>% mutate(n=100, p_above_threshold = p_emprate_above_threshold(n=n, p0=0.1, p1=p1))

ggplot(df, aes(x=p1, y=p_above_threshold)) + geom_line() + ylab("Probability to detect") + xlab(expression(p[1])) + scale_y_continuous(labels=scales::percent) + ggtitle(substitute(n==x, list(x=df$n[1])))

######################################################################
## Precision
######################################################################

##by gard
alpha <- 0.05
p <- 0.9
xi_true <- qnorm(0.9)
z <- qnorm(alpha/2,lower.tail=FALSE)
d <- 0.1

x <- rnorm(40)
xi_hat <- quantile(x, prob=p)
(n <- ceiling( (z^2 * p * (1-p)) / ( d*xi_true * dnorm(xi_true))^2))
(n <- ceiling( (z^2 * p * (1-p)) / ( d*xi_hat * dnorm(xi_hat))^2))

ks <- density(x)
plot(ks)
f <- approxfun(x=ks$x, y=ks$y)
lines(ks$x, f(ks$x), col="blue")
lines(x_grid <- seq(min(x),max(x),length=1000),dnorm(x_grid))
lines(rep(xi_true,2),c(0,1e99),lty=2)
f(xi_hat) / dnorm(xi_true)

##Appears to be quite variable.
(n <- ceiling( (z^2 * p * (1-p)) / ( d*xi_hat * f(xi_hat))^2))

##Check adequacy of the sample size
y_p <- replicate(1e5,quantile(rnorm(n=n), prob=p, type=1))
mean(abs(y_p - xi_true) / xi_true > d)


##Acceptable is +/- 2 on the quantile, which is expected to be somewhere
##around 10-20 (20 is worst)
d <- 2/15
alpha <- 0.1 ; z <- qnorm(alpha/2,lower.tail=FALSE)

ks <- density(flint$lead)
f <- approxfun(x=ks$x, y=ks$y)
xi_hat <- quantile(flint$lead, prob=p, type=1)
(n <- ceiling( (z^2 * p * (1-p)) / ( d*xi_hat * f(xi_hat))^2))

##Alternative kernel density estimate
library(ks)
kde <- ks::kde(x=flint$lead)
names(kde)
f2 <- approxfun(kde$eval.point, kde$estimate)
f2(xi_hat)
f(xi_hat)

######################################################################
## Confidence intervals
######################################################################

##Compare CIs
quantileCI::quantile_confint_nyblom(x=flint %$% lead,p=0.9,conf.level=0.95)
MKmisc::quantileCI(flint %$% lead, p=0.9,conf.level=0.95)
jmuOutlier::quantileCI(flint %$% lead, probs=0.9, conf.level=0.95)

##Compare with excluded.
quantileCI::quantile_confint_nyblom(x=flint %>% filter(!exclude) %$% lead,p=0.9)
jmuOutlier::quantileCI(flint %>% filter(!exclude) %$% lead, probs=0.9)
MKmisc::quantileCI(flint %>% filter(!exclude) %$% lead, p=0.9)

