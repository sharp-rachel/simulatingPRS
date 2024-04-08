# Load libs
library(tidyverse)
library(mvtnorm)

# Setting up the PRS and correlated variables
n <- 1e5
prs <- rnorm(n)
corr_dat <- rmvnorm(n, sigma = cbind(c(1, .5), c(.5, 1)))
sdoh_risk <- corr_dat[, 1]
ancestry <- corr_dat[, 2]

# Define logistic function
logistic <- function(z) exp(z) / (1 + exp(z))

# Making it so that when we take the log for prob, it's between 0-1
pos <- function(x) ifelse(x < 0, 0, x)

# Define sim function for power analysis
sim <- function(n, b) rnorm(n, rep(c(0, 0 + b), each = n/2))

# Define simulate_and_fit function for logistic regression model
simulate_and_fit <- function(prs, sdoh_risk, beta_interaction) {
  prob <- logistic(-4 + .5 * sdoh_risk + pos(.5 * prs) + beta_interaction * sdoh_risk * prs)
  case <- rbinom(length(prs), size = 1, prob = prob)
  fit <- glm(case ~ prs + sdoh_risk + prs:sdoh_risk, family = binomial)
  return(summary(fit)$coefficients)
}

# Set up parameters
beta_values <- seq(0, 3, by = 0.1)
nreps <- 10000
n <- 10
alpha <- 0.05

# Perform power analysis
power <- sapply(beta_values, function(b) {
  sig <- replicate(nreps, {
    coef <- simulate_and_fit(prs, sdoh_risk, b)
    
    # Check if coefficient for prs:sdoh_risk is significant
    coef["prs:sdoh_risk", "Pr(>|z|)"] < alpha
  })
  return(mean(sig))
})

# Plot power against effect size
plot(beta_values, power, type = "b", xlab = "Effect Size (Beta)", ylab = "Power", main = "Power Analysis")