
## Starter code from Mike
n <- 1e5
prs <- rnorm(n)

library(mvtnorm)
corr_dat <- rmvnorm(n, sigma=cbind(c(1,.5), c(.5,1)))

sdoh_risk <- corr_dat[,1] #Sdoh... would be positively correlated with risk of condition, but negatively correlated with likihood of treatment/diagnosis... 
ancestry <- corr_dat[,2]

smoothScatter(sdoh_risk, ancestry)

# some functions for later
logistic <- function(z) exp(z)/(1 + exp(z))
cut_fun <- function(x, n) {
  cut(x, quantile(x, 0:n/n),
      include.lowest = TRUE, labels=1:n)
}
pos <- function(x) ifelse(x < 0, 0, x) # making it so that when we take the log for prob, it's between 0-1
plot(logistic, xlim=c(-2,2), lwd=3, col="dodgerblue")

# some packages for later
library(dplyr)
library(ggplot2)
library(tibble)

# make cases
# can play with -4 to make baseline odds match that of bpd
# if prevalnce of bpd is 10% do exp(-4) where I replace -4 with diff numbers until I reach 10%
# NOT PREVALANCE - odds
prob <- logistic(-4 + .5 * sdoh_risk + pos(.5 * prs) + sdoh_risk * pos(.5 * prs)) # prob of having the disorder #add interaction of sdoh and prs -4 makes logodds e to the -4
#eun line 34 1,000 times, count how many times the interaction term was significant, that's the power
case <- rbinom(n, size=1, prob=prob) # Using prob to define true cases
fit <- glm(case ~ prs, family=binomial)
dat <- tibble(prs, sdoh_risk, ancestry, case)

dat |>
  mutate(prs_bin = cut_fun(prs, 10),
         sdoh_risk_bin = cut_fun(sdoh_risk, 4)) |>
  group_by(prs_bin, sdoh_risk_bin) |>
  summarize(prevalence = mean(case), n = n()) |>
  ggplot(aes(prs_bin, prevalence, color=sdoh_risk_bin, group=sdoh_risk_bin)) +
  geom_point() +
  geom_line() +
  ggtitle("no interaction, just shift")

write.csv(dat, "data/simulated_prs/mike_examples/03_29_24.csv", row.names = F)