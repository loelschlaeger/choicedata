
comp_b_n <- function(beta, Omega) {
  mix <- any(Omega != 0)
  if (mix) {
    matrix(oeli::rmvnorm(mean = beta, Sigma = Omega))
  } else {
    beta
  }
}

comp_X_nt <- function(J, K) {
  matrix(rnorm(J * K, sd = 3), nrow = J, ncol = K)
}

comp_X_nt_mats <- function(J, K, T) {
  lapply(1:T, function(t) comp_X_nt(J, K))
}

comp_X_n <- function(X_nt_mats) {
  do.call(rbind, X_nt_mats)
}

comp_V_nt <- function(X_nt, beta, Omega) {
  X_nt %*% comp_b_n(beta, Omega)
}

comp_V_nt_vecs <- function(X_nt_mats, beta, Omega) {
  b_n <- comp_b_n(beta, Omega)
  lapply(X_nt_mats, function(x) x %*% b_n)
}

comp_V_n <- function(V_nt_vecs) {
  do.call(rbind, V_nt_vecs)
}

comp_eps_nt <- function(Sigma) {
  matrix(oeli::rmvnorm(mean = rep(0, nrow(Sigma)), Sigma = Sigma))
}

comp_eps_nt_vecs <- function(Sigma, T) {
  lapply(1:T, function(t) comp_eps_nt(Sigma))
}

comp_eps_n <- function(eps_nt_vecs) {
  do.call(rbind, eps_nt_vecs)
}

comp_U_nt <- function(V_nt, eps_nt) {
  V_nt + eps_nt
}

comp_U_nt_vecs <- function(V_nt_vecs, eps_nt_vecs) {
  mapply(comp_U_nt, V_nt_vecs, eps_nt_vecs, SIMPLIFY = FALSE)
}

comp_U_n <- function(U_nt_vecs) {
  do.call(rbind, U_nt_vecs)
}

comp_y_nt <- function(U_nt) {
  which.max(U_nt)
}

comp_y_nt_vecs <- function(U_nt_vecs) {
  lapply(U_nt_vecs, which.max)
}

comp_y_n <- function(y_nt_vecs) {
  do.call(rbind, y_nt_vecs)
}

simulate_data <- function(N, T, J, K, beta, Omega, Sigma) {
  data <- list("X" = list(), "y" = list())
  for (n in seq_len(N)) {
    X_nt_mats <- comp_X_nt_mats(J, K, T)
    V_nt_vecs <- comp_V_nt_vecs(X_nt_mats, beta, Omega)
    eps_nt_vecs <- comp_eps_nt_vecs(Sigma, T)
    U_nt_vecs <- comp_U_nt_vecs(V_nt_vecs, eps_nt_vecs)
    y_nt_vecs <- comp_y_nt_vecs(U_nt_vecs)
    data[["X"]][[n]] <- X_nt_mats
    data[["y"]][[n]] <- y_nt_vecs
  }
  return(data)
}

comp_delta_nt_mats <- function(y_nt_vecs, J) {
  lapply(seq_along(y_nt_vecs), function(t) oeli::delta(ref = y_nt_vecs[[t]], dim = J))
}

comp_D_n <- function(delta_nt_mats) {
  Matrix::bdiag(delta_nt_mats)
}

comp_Gamma_n <- function(X_n, Omega, Sigma, T) {
  X_n %*% Omega %*% t(X_n) + diag(T) %x% Sigma
}

comp_l_n <- function(D_n, V_n, Gamma_n, CML = FALSE) {
  upper <- as.numeric(D_n %*% (-V_n))
  sigma <- as.matrix(D_n %*% Gamma_n %*% t(D_n))
  if (CML) {
    chunks <- oeli::chunk_vector(1:20, 2, type = 2)
    prob <- 1
    for (chunk in chunks) {
      prob <- prob * mvtnorm::pmvnorm(
        upper = upper[chunk],
        sigma = sigma[chunk, chunk, drop = FALSE]
      )
    }
    return(prob)
  } else {
    prob <- mvtnorm::pmvnorm(upper = upper, sigma = sigma)
    return(prob)
  }
}

comp_ll <- function(data, beta, Omega, Sigma, CML = FALSE) {
  ll <- 0
  N <- length(data[["X"]])
  J <- nrow(Sigma)
  for (n in seq_len(N)) {
    y_nt_vecs <- data[["y"]][[n]]
    T <- length(y_nt_vecs)
    delta_nt_mats <- comp_delta_nt_mats(y_nt_vecs, J)
    D_n <- comp_D_n(delta_nt_mats)
    X_nt_mats <- data[["X"]][[n]]
    X_n <- comp_X_n(X_nt_mats)
    V_nt_vecs <- lapply(X_nt_mats, function(x) x %*% beta)
    V_n <- comp_V_n(V_nt_vecs)
    Gamma_n <- comp_Gamma_n(X_n, Omega, Sigma, T)
    ll <- ll + log(comp_l_n(D_n, V_n, Gamma_n, CML))
  }
  as.numeric(ll)
}

x_2_pars <- function(x, J, K, mix) {
  at <- if (mix) cumsum(c(K + 1, J * (J + 1) / 2)) else K + 1
  pars <- oeli::split_vector_at(x, at)
  names(pars) <- if (mix) c("beta", "Omega", "Sigma") else c("beta", "Sigma")
  if (mix) {
    pars[["Omega"]] <- oeli::chol_to_cov(pars[["Omega"]])
  } else {
    pars[["Omega"]] <- matrix(0, K, K)
  }
  pars[["Sigma"]] <- oeli::undiff_cov(oeli::chol_to_cov(c(1, pars[["Sigma"]])))
  return(pars)
}

pars_2_x <- function(beta, Omega, Sigma, mix) {
  o <- if(mix) oeli::cov_to_chol(Omega) else numeric()
  l <- oeli::cov_to_chol(Sigma[-1, -1, drop = FALSE])[-1]
  c(beta, o, l)
}


nll <- function(x, data, J, K, mix, CML = FALSE) {
  pars <- x_2_pars(x, J, K, mix)
  -comp_ll(data, pars[["beta"]], pars[["Omega"]], pars[["Sigma"]], CML)
}

