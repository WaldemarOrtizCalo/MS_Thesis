#   [Library]                                                                 ####
library(nimble)
library(tidyverse)
library(tidybayes)

#   [Data]                                                                 ####
set.seed(1)
p <- 15    # number of explanatory variables
n <- 100   # number of observations
X <- matrix(rnorm(p*n), nrow = n, ncol = p) # explanatory variables
true_betas <- c(c(0.1, 0.2, 0.3, 0.4, 0.5), rep(0, p-5)) # coefficients
sigma <- 1
y <- rnorm(n, X %*% true_betas, sigma)

#   [Model]                                                                 ####
code <- nimbleCode({
  beta0 ~ dnorm(0, sd = 100)
  beta[1:p] ~ dmnorm(zeros[1:p], omega[1:p, 1:p])
  sigma ~ dunif(0, 100)  # prior for variance components based on Gelman (2006)
  for(i in 1:n) {
    y[i] ~ dnorm(beta0 + (beta[1:p] %*% x[i, 1:p])[1,1], sd = sigma)
  }
})

constants <- list(n = n, p = p, x = X, zeros = rep(0, p), omega = 0.0001 * diag(p))
data <- list(y = y)
inits <- list(beta0 = mean(y), beta = rep(0, p), sigma = 0.5)
model <- nimbleModel(code, constants = constants, data = data, inits = inits)

mcmc.out <- nimbleMCMC(model, thin = 10,
                       niter = 20000, nchains = 3, setSeed = TRUE)



mcmc.out %>%
  as_tibble() %>%
  ggplot() +
  geom_histogram(aes(x = chain1[,"beta[1]"]), color = "white") +
  labs(x = "survival probability")

MCMCtrace(object = mcmc.out,
          pdf = FALSE, # no export to PDF
          ind = TRUE, # separate density lines per chain
          params = "all")
