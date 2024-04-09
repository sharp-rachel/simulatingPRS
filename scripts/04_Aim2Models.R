#1: variables being tested
# beta, replicates, group size, alpha/threshold
betas <- 0:30/10
nreps <- 20
n <- 500
alpha <- .05
prs <- rnorm(n)

# Correlated vars
corr_dat <- rmvnorm(n, sigma=cbind(c(1,.5), c(.5,1)))
sdoh_risk <- corr_dat[,1]
ancestry <- corr_dat[,2]

logistic <- function(z) exp(z)/(1 + exp(z))
pos <- function(x) ifelse(x < 0, 0, x) # making it so that when we take the log for prob, it's between 0-1

#2: function for sim phenos from a distribution
sim <- function(n,b) rnorm(n, rep(c(0, 0+b), each=n/2))

sim <- function(n,b, sdoh_risk, prs) rbinom(n, 1, logistic(-4 + .5 * sdoh_risk + pos(.5 * prs) + b * sdoh_risk * prs))

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


