rm(list = ls())

library(mvtnorm)
library(matrixcalc)
library(Matrix)

source("panel_probit_code.R")

J <- 3  # number alternatives
K <- 3  # number coefficients
N <- 1000 # number deciders
T <- 1 # number choice occasions


beta <- c(-1, 1, 3) # mean of mixing distribution
# Omega <- oeli::sample_covariance_matrix(dim = K, df = 5) # covariance of mixing distribution
Omega <- matrix(0, K, K)
# Omega <- matrix(c(1, 0.5, 0.2, 0.5, 1, 0.3, 0.2, 0.3, 1), 3, 3)


mix <- any(Omega != 0)

Sigma_diff <- oeli::sample_covariance_matrix(dim = (J - 1), df = 5) # covariance of error-term
Sigma_diff <- Sigma_diff / Sigma_diff[1, 1]
oeli::check_covariance_matrix(Sigma_diff)
Sigma <- cbind(0, rbind(0, Sigma_diff))


Sigma <- matrix(c(0, 0, 0, 0, 1, 0.5, 0, 0.5, 1), 3, 3)



data <- simulate_data(N, T, J, K, beta, Omega, Sigma + 1)


x <- pars_2_x(beta, Omega, Sigma, mix)

x_init <- x #+ rnorm(length(x), sd = 0.1)
nlm_out <- nlm(nll, x_init, data = data, J = J, K = K, mix = mix, CML = F, print.level = 2, iterlim = 1000)

estimation <- x_2_pars(nlm_out$estimate, J, K, mix)

true_pars <- x_2_pars(x, J, K, mix)


result <- list("estimation" = estimation, "true" = true_pars)

