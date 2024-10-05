


data_sim_tmp <- function(
    parameters
  ) {
  # TODO
  J <- 3  # number alternatives
  K <- 3  # number coefficients
  N <- 1000 # number deciders
  T <- 1 # number choice occasions

  beta <- parameters$beta
  Omega <- parameters$Omega
  Sigma <- parameters$Sigma
  mix <- parameters$mix

  data <- list("X" = list(), "y" = list())
  for (n in seq_len(N)) {
    X_nt_mats <- lapply(1:T, function(t) matrix(rnorm(J * K, sd = 3), nrow = J, ncol = K))
    b_n <- beta
    V_nt_vecs <- lapply(X_nt_mats, function(x) x %*% b_n)
    eps_nt_vecs <- lapply(1:T, function(t) matrix(oeli::rmvnorm(mean = rep(0, J), Sigma = Sigma)))
    U_nt_vecs <- mapply(function(V, e) V + e, V_nt_vecs, eps_nt_vecs, SIMPLIFY = FALSE)
    y_nt_vecs <- lapply(U_nt_vecs, which.max)
    data[["X"]][[n]] <- X_nt_mats
    data[["y"]][[n]] <- y_nt_vecs
  }
  structure(
    data,
    "N" = N,
    "J" = J,
    "K" = K
  )
}

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
