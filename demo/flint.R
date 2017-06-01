library(dplyr)
library(magrittr)

##Data taken from Quantiles and the Flint water crisis - The DO Loop.html
##Used 0 for "not detectable" */
##Lead Concentration (ppb)"
flint <- data.frame(lead=c(0,0,0,0,0,0,0,0,0,0,0,0,0,
                  1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,
                  3,3,3,3,3,3,3,3,3,3,3,4,4,
                  5,5,5,5,5,5,5,5,6,6,6,6,7,7,7,
                  8,8,9,10,10,11,13,18,20,21,22,29,43,43,104)) %>%
  mutate(exclude = (lead == 20 | lead == 104)) %>% as.tbl

nrow(flint)

##Show different quantiles
sapply(1:9, function(i) {
  flint %>% summarise(q90=quantile(lead, prob=0.9,type=i)) %>% .[[1]]
})

##Remove outliers
sapply(1:9, function(i) {
  flint %>% filter(!exclude) %>% summarise(q90=quantile(lead, prob=0.9,type=i)) %>% .[[1]]
})

require(quantileCI)
quantileCI::quantile_confint_nyblom(x=flint %$% lead,p=0.9)
MKmisc::quantileCI(flint %$% lead, p=0.9)
jmuOutlier::quantileCI(flint %$% lead, probs=0.9)

quantileCI::quantile_confint_nyblom(x=flint %>% filter(!exclude) %$% lead,p=0.9)
jmuOutlier::quantileCI(flint %>% filter(!exclude) %$% lead, probs=0.9)
MKmisc::quantileCI(flint %>% filter(!exclude) %$% lead, p=0.9)
