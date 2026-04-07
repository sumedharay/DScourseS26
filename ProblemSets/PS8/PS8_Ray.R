# ---- Problem Set 8 ---- #
# Sumedha Ray # 

library(nloptr)
library(modelsummary)

## Generating the data ##
set.seed(100)

N <- 100000
K <- 10

X <- cbind(1, matrix(rnorm(N * (K - 1)), nrow = N, ncol = K - 1))
eps <- rnorm(N, mean = 0, sd = 0.5)
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
Y <- X %*% beta + eps

## Closed Form OLS ##
beta_ols_cf <- solve(t(X) %*% X) %*% t(X) %*% Y
print(beta_ols_cf)

## Gradient Descent OLS ##
learning_rate <- 0.0000003
beta_gd <- rep(0, K)

for (i in 1:10000) {
  gradient <- -t(X) %*% (Y - X %*% beta_gd)
  beta_gd <- beta_gd - learning_rate * gradient
}
print(beta_gd)

## nloptr L-BFGS and Nelder-Mead OLS ##
ols_obj <- function(beta, Y, X) {
  return(sum((Y - X %*% beta)^2))
}

ols_grad <- function(beta, Y, X) {
  return(as.vector(-2 * t(X) %*% (Y - X %*% beta)))
}

opts_lbfgs <- list("algorithm" = "NLOPT_LD_LBFGS", "xtol_rel" = 1e-8)
result_lbfgs <- nloptr(x0 = rep(0, K),
                       eval_f = ols_obj,
                       eval_grad_f = ols_grad,
                       opts = opts_lbfgs,
                       Y = Y, X = X)
print(result_lbfgs$solution)

opts_nm <- list("algorithm" = "NLOPT_LN_NELDERMEAD", 
                "xtol_rel" = 1e-8,
                "maxeval" = 1000000)
result_nm <- nloptr(x0 = rep(0, K),
                    eval_f = ols_obj,
                    opts = opts_nm,
                    Y = Y, X = X)
print(result_nm$solution)

## MLE with L-BFGS ##
mle_obj <- function(theta, Y, X) {
  beta <- theta[1:(length(theta) - 1)]
  sig  <- theta[length(theta)]
  n    <- nrow(X)
  loglik <- -n/2 * log(2*pi) - n*log(sig) - sum((Y - X %*% beta)^2) / (2*sig^2)
  return(-loglik)
}

mle_grad <- function(theta, Y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta) - 1)]
  sig  <- theta[length(theta)]
  grad[1:(length(theta) - 1)] <- -t(X) %*% (Y - X %*% beta) / (sig^2)
  grad[length(theta)] <- dim(X)[1]/sig - crossprod(Y - X %*% beta) / (sig^3)
  return(grad)
}

opts_mle <- list("algorithm" = "NLOPT_LD_LBFGS", "xtol_rel" = 1e-8)
result_mle <- nloptr(x0 = c(result_lbfgs$solution, 1),
                     eval_f = mle_obj,
                     eval_grad_f = mle_grad,
                     lb = c(rep(-Inf, K), 0.001),
                     opts = opts_mle,
                     Y = Y, X = X)
print(result_mle$solution)

model <- lm(Y ~ X - 1)
modelsummary(model, 
             output = "PS8_Ray_table.tex",
             title = "OLS Estimates via lm()")
