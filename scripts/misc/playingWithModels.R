###
corrplot::corrplot(cor(dat))

#### END OF MIKES CODE ####

###
#prs ~ N(0,1)
#case ~ binomial(f(prs)) - genetics only model
#case ~ binomial(log(prs + SDOH))
#case ~ binomial(log(prs + SDOH + interaction))

# Simulate prs ~ N(0,1)
n <- 1e5
prs <- rnorm(n)

# Define functions for case probabilities
f1 <- function(x) binomial(x) # genetics only model
f2 <- function(x, sdoh) binomial(log(x + sdoh)) # model with log(prs + SDOH)
f3 <- function(x, sdoh, interaction) binomial(log(x + sdoh + interaction)) # model with log(prs + SDOH + interaction)

# Simulate SDOH and Ancestry
library(mvtnorm)
corr_dat <- rmvnorm(n, sigma=cbind(c(1,.5), c(.5,1)))
sdoh_risk <- corr_dat[,1]
ancestry <- corr_dat[,2]

# Make cases for each model
# Do I want to make cases for each model... or use an assumed underlying case distribution?
case1 <- rbinom(n, size=1, prob=f1(prs))
case2 <- rbinom(n, size=1, prob=f2(prs, sdoh_risk))
case3 <- rbinom(n, size=1, prob=f3(prs, sdoh_risk, prs * sdoh_risk))

# Fit logistic regression models
model1 <- glm(case1 ~ prs, family = binomial)
model2 <- glm(case2 ~ prs + sdoh_risk, family = binomial)
model3 <- glm(case3 ~ prs + sdoh_risk + prs * sdoh_risk, family = binomial)

# Evaluate model performance metrics
summary(model1)
summary(model2)
summary(model3)

# Calculate AIC and BIC
AIC_model1 <- AIC(model1)
BIC_model1 <- BIC(model1)
AIC_model2 <- AIC(model2)
BIC_model2 <- BIC(model2)
AIC_model3 <- AIC(model3)
BIC_model3 <- BIC(model3)

# Calculate accuracy, sensitivity, and specificity for each model
accuracy_model1 <- mean((predict(model1, type = "response") > 0.5) == case1)
accuracy_model2 <- mean((predict(model2, type = "response") > 0.5) == case2)
accuracy_model3 <- mean((predict(model3, type = "response") > 0.5) == case3)

# Sensitivity: True Positive Rate
sensitivity_model1 <- sum(predict(model1, type = "response")[case1 == 1] > 0.5) / sum(case1 == 1)
sensitivity_model2 <- sum(predict(model2, type = "response")[case2 == 1] > 0.5) / sum(case2 == 1)
sensitivity_model3 <- sum(predict(model3, type = "response")[case3 == 1] > 0.5) / sum(case3 == 1)

# Specificity: True Negative Rate
specificity_model1 <- sum(predict(model1, type = "response")[case1 == 0] <= 0.5) / sum(case1 == 0)
specificity_model2 <- sum(predict(model2, type = "response")[case2 == 0] <= 0.5) / sum(case2 == 0)
specificity_model3 <- sum(predict(model3, type = "response")[case3 == 0] <= 0.5) / sum(case3 == 0)

# Print results
cat("Model 1 - AIC:", AIC_model1, "BIC:", BIC_model1, "Accuracy:", accuracy_model1, "Sensitivity:", sensitivity_model1, "Specificity:", specificity_model1, "\n")
cat("Model 2 - AIC:", AIC_model2, "BIC:", BIC_model2, "Accuracy:", accuracy_model2, "Sensitivity:", sensitivity_model2, "Specificity:", specificity_model2, "\n")
cat("Model 3 - AIC:", AIC_model3, "BIC:", BIC_model3, "Accuracy:", accuracy_model3, "Sensitivity:", sensitivity_model3, "Specificity:", specificity_model3, "\n")