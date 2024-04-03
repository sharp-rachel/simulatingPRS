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
model4 <- glm(data = prs.dat, case ~ prs + ancestry + sdoh_risk + prs:sdoh_risk, family = binomial)
summary(model4)

#Was the interaction term significant - repated = senstivity 
# in how many sims was the coefficient sig?
# we should have X power in our sample size to detect x effect size

##Iterate through sample size, effect size, correlation
##Step-wise thresholding effects?


###
#outside loop where I iterate through effect sizes -> sapply([0.01, 0.02, 0.03])
#inisde loop
# sig <- replicate(10,000{
#  rbinom
#  glm()
#  logical -> pval < 0.05
#})
#mean(sig)