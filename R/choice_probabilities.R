
rm(list=ls())
J <- 3
P <- 3
N <- 1000
T <- 1

beta <- rnorm(P)
Omega <- matrix(0, P, P)
Sigma <- oeli::sample_covariance_matrix(J)

true_pars <- list("beta" = beta, "Sigma" = Sigma)

Sigma <- rbind(0, cbind(0, oeli::diff_cov(Sigma, ref = 1)))

scale <- sqrt(Sigma[2, 2])
beta <- beta / scale
Sigma <- Sigma / scale^2

data <- list("X" = list(), "y" = list())

preferences <- list()
for (n in seq_len(N)) {
  preferences[[n]] <- oeli::rmvnorm(mean = beta, Sigma = Omega)
}

for (n in seq_len(N)) {
  X_nt <- matrix(rnorm(J * P, sd = 2), nrow = J, ncol = P)
  V_nt <- X_nt %*% preferences[[n]]
  eps_nt <- oeli::rmvnorm(n = 1, mean = 0, Sigma = Sigma + 1)
  U_nt <- V_nt + eps_nt
  y_nt <- which.max(U_nt)
  data[["X"]][[n]] <- X_nt
  data[["y"]][[n]] <- y_nt
}

theta_true <- c(beta, oeli::cov_to_chol(oeli::diff_cov(Sigma))[-1])
ind_Sigma <- P + (1:(J * (J - 1) / 2 - 1))



mnp_probs <- function(y, X, J, beta, Sigma) {
  sapply(1:length(y), function(n) {
    D_n <- oeli::delta(ref = y[n], dim = J)
    mvtnorm::pmvnorm(
      upper = as.numeric(D_n %*% (-X[[n]] %*% beta)),
      sigma = as.matrix(D_n %*% Sigma %*% t(D_n))
    )
  })
}

mnp_panel_probs <- function(y, X, J, beta, Sigma, Omega, CML = FALSE) {

}

nll <- function(theta, data) {

  beta <- theta[1:P]
  Sigma <- oeli::undiff_cov(oeli::chol_to_cov(c(1, theta[ind_Sigma])))
  J <- nrow(Sigma)
  ll <- 0
  J <- nrow(Sigma)

  ### calculate MNP probabilities
  probs <- mnp_probs(
    y = unlist(data[["y"]]),
    X = data[["X"]],
    J = J,
    beta = beta,
    Sigma = Sigma
  )

  ### avoid numerical issues
  probs <- sapply(probs, max, 1e-6)

  ### return negative log-likelihood
  -sum(log(probs))
}

out <- nlm(nll, theta_true, data, print.level = 2, iterlim = 1000)

round(cbind(theta_true, out$estimate), 1)
cbind(
  Sigma,
  oeli::undiff_cov(oeli::chol_to_cov(c(1, out$estimate[ind_Sigma])))
)


cbind(
  true_pars$beta,
  out$estimate[1:3] * scale
)

cbind(
  rbind(0, cbind(0, oeli::diff_cov(true_pars$Sigma, ref = 1))),
  oeli::undiff_cov(oeli::chol_to_cov(c(1, out$estimate[ind_Sigma]))) * scale^2
)







choice_probabilities <- function(

    parameters,
    data

    # TODO
    #choice_parameters,
    #choice_data,
    #choice_formula,
    #choice_alternatives,
    #cml = FALSE
) {

  N <- attr(data, "N")
  J <- attr(data, "J")

  beta <- parameters$beta
  Omega <- parameters$Omega
  Sigma <- parameters$Sigma

  probs <- data.frame(
    "choice_identifier" = 1:N,
    "probs" = NA
  )

  # TODO
  n <- 1

  for (n in seq_len(N)) {

    y_nt_vecs <- data[["y"]][[n]]
    T_n <- length(y_nt_vecs)



    delta_nt_mats <- lapply(seq_along(y_nt_vecs), function(t) {
      oeli::delta(ref = y_nt_vecs[[t]], dim = J)
    })


    D_n <- as.matrix(Matrix::bdiag(delta_nt_mats))

    X_nt_mats <- data[["X"]][[n]]
    X_n <- do.call(rbind, X_nt_mats)

    V_nt_vecs <- lapply(X_nt_mats, function(x) x %*% beta)

    V_n <- do.call(rbind, V_nt_vecs)

    Gamma_n <- X_n %*% Omega %*% t(X_n) + diag(T_n) %x% Sigma



    upper <- as.numeric(D_n %*% (-V_n))
    sigma <- as.matrix(D_n %*% Gamma_n %*% t(D_n))

    if (CML <- FALSE) {
      chunks <- oeli::chunk_vector(1:20, 2, type = 2)
      prob <- 1
      for (chunk in chunks) {
        l_n <- prob * mvtnorm::pmvnorm(
          upper = upper[chunk],
          sigma = sigma[chunk, chunk, drop = FALSE]
        )
      }
    } else {
      probs[n , 2] <- as.numeric(mvtnorm::pmvnorm(upper = upper, sigma = sigma))
    }


  }

  return(probs)

}


choice_likelihood_tmp <- function(
  x, data
) {

  mix <- FALSE
  K <- attr(data, "K")
  J <- attr(data, "J")


  pars <- x_2_pars(x, J, K, mix)

  choice_probs <- choice_probabilities(
    pars, data
  )

  ll <- sum(log(choice_probs$probs))

  return(-ll)

}
