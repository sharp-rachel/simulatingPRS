#1: variables being tested
# beta, replicates, group size, alpha/threshold
betas <- 0:30/10
nreps <- 1000
#BPD sample size - 27304; 1033 with BPD
n <- 27304
alpha <- .05
prs <- rnorm(n)

# SDOH risk under normal dist
sdoh_risk <- rmvnorm(n, sigma=c(1,.5))

logistic <- function(z) exp(z)/(1 + exp(z))
pos <- function(x) ifelse(x < 0, 0, x) # making it so that when we take the log for prob, it's between 0-1

#Need to find odds of BPD - cases/controls - in my cohort?
# exp(-3.25) = 0.0387 - calculated odds of BPD in my cohort = 0.0393
sim <- function(n,b, sdoh_risk, prs) rbinom(n, 1, logistic(-3.25 + .5 * sdoh_risk + pos(.5 * prs) + b * sdoh_risk * prs))

power <- sapply(betas, function(b) {
  sig <- replicate(nreps, { # repeat for each replicate
    y <- sim(n, b, sdoh_risk, prs) # apply the given beta at a set group size to the sim
    # make group labels to match the phenos
    fit <- glm(y ~ prs + sdoh_risk + prs:sdoh_risk, family=binomial) # test them in a lm
    coef <- summary(fit)$coefficients # get the coeff
    coef["prs:sdoh_risk","Pr(>|z|)"] < alpha # see if they're sig
  })
  mean(sig) # pull the average sig (true/false)
})


plot(betas, power, type="b")


