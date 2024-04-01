# Script from Brian/Rachel at steelstring

# Load libs
library(tidyverse)

# Load example data from mike

prs.dat <- read.csv("data/simulated_prs/mike_examples/03_29_24.csv")


# Try glm with simple model

# Define functions for case probabilities
model1 <- glm(data = prs.dat, case ~ prs, family = binomial)
summary(model1)

# Make it additive w/ PRS and ancestry
model2 <- glm(data = prs.dat, case ~ prs + ancestry, family = binomial)
summary(model2)

# Make it additive with ancestry and sdoh
model3 <- glm(data = prs.dat, case ~ prs + ancestry + sdoh_risk, family = binomial)
summary(model3)

# Make it interaction with ancestry and sdoh
model4 <- glm(data = prs.dat, case ~ prs + ancestry + sdoh_risk + ancestry:sdoh_risk, family = binomial)
summary(model4)
