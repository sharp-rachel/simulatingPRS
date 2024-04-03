# Load libs
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tibble)
library(mvtnorm)

# Setting up the PRS
n <- 1e5
prs <- rnorm(n)

# Correlated vars
corr_dat <- rmvnorm(n, sigma=cbind(c(1,.5), c(.5,1)))
sdoh_risk <- corr_dat[,1]
ancestry <- corr_dat[,2]

#smoothScatter(sdoh_risk, ancestry)

# some functions for later
logistic <- function(z) exp(z)/(1 + exp(z))

cut_fun <- function(x, n) {
  cut(x, quantile(x, 0:n/n),
      include.lowest = TRUE, labels=1:n)
}

pos <- function(x) ifelse(x < 0, 0, x) # making it so that when we take the log for prob, it's between 0-1
#plot(logistic, xlim=c(-2,2), lwd=3, col="dodgerblue")

# fxn to simulate model fit with different betas
simulate_and_fit <- function(beta_interaction) {
  prob <- logistic(-4 + .5 * sdoh_risk + pos(.5 * prs) + beta_interaction * sdoh_risk * prs)
  case <- rbinom(n, size=1, prob=prob)
  fit <- glm(case ~ prs + sdoh_risk + prs:sdoh_risk, family=binomial)
  return(fit)
}

# Loop through different beta values for the interaction term
beta_values <- seq(-1, 1, by = 0.1)
significant_interaction_count <- numeric(length(beta_values))

for (i in seq_along(beta_values)) {
  fit <- simulate_and_fit(beta_values[i])
  # Check if the interaction term is significant (p-value < 0.05)
  if (summary(fit)$coefficients['prs:sdoh_risk', 'Pr(>|z|)'] < 0.05) {
    significant_interaction_count[i] <- 1
  }
}

# Compute sensitivity (percentage of significant interaction terms)
sensitivity <- mean(significant_interaction_count)
