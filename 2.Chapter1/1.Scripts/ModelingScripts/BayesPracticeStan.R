
library(rstan)
library(shinystan)
library(tidyverse)
library(mcmcplots)
#   [Linear Model]                                                                 ####
# Data 
set.seed(123)
n <- 16
a <- 40  #intercept
b <- -1.5  #slope
sigma2 <- 25  #residual variance (sd=5)
x <- 1:n  #values of the year covariate
eps <- rnorm(n, mean = 0, sd = sqrt(sigma2))  #residuals
y <- a + b * x + eps  #response variable
# OR
y <- (model.matrix(~x) %*% c(a, b)) + eps
data <- data.frame(y, x)  #dataset


data <- within(data, {cx <- as.numeric(scale(x, scale = FALSE))})
  
# Model 
model <- stan_model("2.Chapter1/1.Scripts/StanModels/LMPractice.stan")

Xmat <- model.matrix(~x, data = data)

data.list <- with(data, list(Y = y, X = Xmat, nX = ncol(Xmat), n = nrow(data)))

data.list

inits <- rep(list(list(beta0 = mean(data$y), beta1 = diff(tapply(data$y,
                                                                 data$x, mean)), sigma = sd(data$y))), 2)

params <- c("beta","beta0", "cbeta0", "sigma")

nChains = 2
burnInSteps = 1000
thinSteps = 1
numSavedSteps = 3000  #across all chains
nIter = ceiling(burnInSteps + (numSavedSteps * thinSteps)/nChains)
nIter

data.rstan <- stan(data = data.list, file = "2.Chapter1/1.Scripts/StanModels/LMPractice.stan", 
                   chains = nChains, iter = nIter, warmup = burnInSteps,
                   thin = thinSteps, save_dso = TRUE)


launch_shinystan(data.rstan)

s = as.array(data.rstan)
mcmc <- do.call(mcmc.list, plyr:::alply(s[, , -(length(s[1, 1, ]))], 2, as.mcmc))
denplot(mcmc, parms = c("beta0","beta","cbeta0","sigma"))

#   [Discrete Choice]                                                                 ####
#      [data]                                                              ####

# Create softmax and Gumbel random number generator
set.seed(574)
softmax <- function(x) exp(x)/sum(exp(x))
rgumbel <- function(N, mu= 0, beta = 1) mu - beta * log(-log(runif(N))) 
# Number of decision-makers
I <- 1000
# Number of attributes
P <- 10
# Preferences
beta_true <- rnorm(10)
# Number of choices per decisionmaker (between 5-10)
decisionmakers <- tibble(i = 1:I, 
                             choices = sample(5:10, I, replace = T))

# Function that takes x (which contains a number of choices)
# and returns some randomly conceived choice attributes as well
# as a binary indicating which row was chosen

make_choices <- function(x) {
  X <- matrix(sample(0:1, P*x$choices, replace = T), x$choices, P)
  X <- rbind(0, X) # Add outside good
  u <- as.numeric(X %*% beta_true)
  choice <- rmultinom(1, 1, softmax(u))
  data.frame(choice = choice,
             X = X)
}

# The fake data
decisionmakers_full <- decisionmakers  %>%
  group_by(i) %>% 
  do(make_choices(.)) %>% ungroup

# Get the indexes of the rows that correspond to each individual's
# first and last entries

indexes <- decisionmakers_full %>%
  mutate(row = 1:n()) %>% 
  group_by(i) %>% 
  summarise(start = first(row),
            end = last(row))

X <- as.matrix(decisionmakers_full %>% dplyr::select(X.1:X.10))
#      [priors]                                                              ####
library(MCMCpack) # contains dirichlet density function

# Write out the negative log likelihood of a matrix of shares (each row a simplex)
dirichlet_n_log_lik <- function(alpha, share_matrix) {
  if(length(alpha) != ncol(share_matrix)) stop("alpha is not the same
                                               length as the number of choices")
  out <- NULL
  for(i in 1:nrow(share_matrix)) {
    out[i] <- log(ddirichlet(t(share_matrix[i,]), alpha))
  }
  -sum(out)
}

# Implement Dirichlet regression
dirichlet_reg <- function(share_matrix) {
  # This finds values of par to minimize the negative log likelihood function
  out <- optim(par = rep(1, ncol(share_matrix)), 
               fn = dirichlet_n_log_lik, 
               share_matrix = share_matrix,
               method = "L-BFGS-B", lower = 0.001)
  out
}

# Get individual 12's X
XX <- X[decisionmakers_full$i==12,]

N_rep <- 100
beta_scale_grid <- c(0.1, 0.2, 0.5, 0.75, 1, 1.2, 1.5, 2)
loss <- rep(NA, 8)

count <- 0
for(sig in beta_scale_grid) {
  count <- count + 1
  out <- matrix(NA, N_rep, nrow(XX))
  
  for(i in 1:N_rep) {
    beta <- rnorm(ncol(XX), 0, sig)
    out[i,] <- t(softmax(XX %*% beta))
  }
  
  reg <- dirichlet_reg(out)
  
  loss[count] <- sum((1 - reg$par)^2)
}


#      [model]                                                              ####
library(rstan)

compiled_model <- stan_model("2.Chapter1/1.Scripts/StanModels/DCPractice.stan",
                             model_name = "discrete_choice_practice")

data_list <- list(I = I, 
                  N = nrow(X), 
                  P = P, 
                  start_index = indexes$start,
                  end_index = indexes$end,
                  X = X,
                  choice = decisionmakers_full$choice
)

fitted_model <- sampling(compiled_model, data= data_list, 
                         cores = 4, iter = 1000)

shinystan::launch_shinystan(fitted_model)
library(bayesplot)
dat <- as.data.frame(fitted_model, pars = "beta")
bayesplot::mcmc_intervals(dat) + 
  geom_point(aes(x= beta_true, y = names(dat)), colour = "red", size = 2)

