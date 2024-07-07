# Load necessary libraries
library(survival)
library(AER)

# Define the likelihood function for the Tobit model
logLikTobit <- function(params, y, X, left=NULL, right=NULL) {
  beta <- params[1:(length(params)-1)]
  sigma <- params[length(params)]
  
  XB <- X %*% beta
  uncensored <- rep(TRUE, length(y))
  
  ll <- numeric(length(y))
  
  if (!is.null(left)) {
    ll[y <= left] <- log(pnorm((left - XB[y <= left]) / sigma))
    uncensored[y <= left] <- FALSE
  }
  
  if (!is.null(right)) {
    ll[y >= right] <- log(1 - pnorm((right - XB[y >= right]) / sigma))
    uncensored[y >= right] <- FALSE
  }
  
  ll[uncensored] <- dnorm((y[uncensored] - XB[uncensored]) / sigma, log = TRUE) - log(sigma)
  
  return(-sum(ll))
}

# Define the function to fit the Tobit model
fitTobit <- function(y, X, left=NULL, right=NULL) {
  X <- as.matrix(X)  # Ensure X is a matrix
  start_params <- c(rep(0, ncol(X)), 1)
  fit <- optim(start_params, logLikTobit, y = y, X = X, left = left, right = right, method = "BFGS", hessian = TRUE)
  
  return(fit)
}

# Example usage with simulated data
set.seed(123)

# Simulate data
n <- 100
X <- cbind(1, rnorm(n), rnorm(n))
beta <- c(1, 2, -1)
sigma <- 1
y <- X %*% beta + rnorm(n) * sigma

# Define left censoring point
left_censoring <- 0
y[y < left_censoring] <- left_censoring

# Fit the Tobit model
fit <- fitTobit(y, X, left = left_censoring)

# Extract results
coefficients <- fit$par[1:(length(fit$par)-1)]
sigma <- fit$par[length(fit$par)]
logLik <- -fit$value

cat("Coefficients:", coefficients, "\n")
cat("Sigma:", sigma, "\n")
cat("Log-Likelihood:", logLik, "\n")
