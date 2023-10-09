#' Define choice covariates
#'
#' @description
#' These functions create and validate an object of class
#' \code{\link{probit_covariates}}, which contains the model covariate,
#' see the details.
#'
#' \code{\link{sample_probit_covariates}} samples covariates.
#'
#' \code{\link{as.list.probit_covariates}} and
#' \code{\link{as.data.frame.probit_covariates}} transform the covariates in
#' \code{list} or \code{data.frame} form, respectively.
#'
#' \code{\link{covariate_names}} and \code{\link{covariate_numbers}} provide the
#' names and number of covariates for a given model specification.
#'
#' @param probit_data
#' An \code{\link{probit_data}} object.
#' @param probit_formula
#' An \code{\link{probit_formula}} object.
#' @param probit_alternatives
#' An \code{\link{probit_alternatives}} object.
#'
#' @section Covariate matrices:
#' A covariate matrix contains the choice covariates of a decider at some choice
#' occasion. It is of dimension \code{J} x \code{P}, where \code{J} is the
#' number of alternatives and \code{P} the number of effects. See
#' \code{\link{compute_P}} to compute the number \code{P}.
#'
#' By default, covariates are sampled from independent normal distributions, but
#' customization is possible:
#' - the levels per covariate can be defined via \code{covariate_levels},
#' - covariates with constant value over occasions can be defined via
#'   \code{occasion_constant},
#' - the means for each covariate can be defined via \code{covariate_mean},
#' - the standard deviation for each covariate can be defined via
#'   \code{covariate_sd},
#' - the correlation between covariates can be defined via
#'   \code{covariate_correlation}.
#'
#' @return
#' An \code{\link{probit_covariates}} object. It is a \code{list} of
#' \code{list}s of \code{matrix} elements.
#'
#' More precise: Let the return value be \code{out}, then
#' - \code{out} contains the covariate matrices of all deciders at all choice
#'   occasions,
#' - \code{out[[n]]} is a \code{list} of the covariate matrices of decider
#'   \code{n} at all of their choice occasions,
#' - \code{out[[n]][[t]]} is the covariate \code{matrix} of decider \code{n} at
#'   their \code{t}-th choice occasion.
#'
#' @keywords object

probit_covariates <- function(
  probit_data = NULL, probit_formula, probit_alternatives
) {

  ### transform 'probit_data' to 'probit_covariates'
  # TODO

  ### validate 'probit_covariates'
  # TODO
  # validate_probit_covariates()
}

#' @rdname probit_covariates
#' @param x
#' An \code{\link{probit_covariates}} object.

is.probit_covariates <- function(x) {
  inherits(x, "probit_covariates")
}

#' @rdname probit_covariates
#' @inheritParams probit_formula
#' @inheritParams expand_Tp
#' @inheritParams probit_alternatives
#' @inheritParams probit_data
#' @param covariate_levels
#' Defines the number of levels for the covariates. Two formats are allowed:
#' - Can be a single, positive \code{numeric} which defines the number of levels
#'   for all covariates. By default, \code{covariate_levels = Inf}.
#' - Can be a named \code{numeric} \code{vector} for specific mean values for
#'   specific covariates. Names must correspond to the output of
#'   \code{\link{covariate_names}}. Means for missing covariates are set to
#'   \code{0}.
#' @param occasion_constant
#' A \code{character} \code{vector} of covariate names
#' (see \code{\link{covariate_names}})
#' that are supposed to be constant across choice occasions.
#' @param covariate_mean
#' Defines the mean for the covariates. Two formats are allowed:
#' - Can be a single \code{numeric} which defines a mean for all covariates.
#'   By default, \code{covariate_mean = 0}.
#' - Can be a named \code{numeric} \code{vector} for specific mean values for
#'   specific covariates. Names must correspond to the output of
#'   \code{\link{covariate_names}}. Means for missing covariates are set to
#'   \code{0}.
#' @param covariate_sd
#' Defines the standard deviation for the covariates. Two formats are allowed:
#' - Can be a single, positive \code{numeric} which defines a standard deviation
#'   for all covariates. By default, \code{covariate_sd = 0}.
#' - Can be a named \code{numeric} \code{vector} for specific standard
#'   deviations for specific covariates. Names must correspond to the output of
#'   \code{\link{covariate_names}}. Standard deviations for missing covariates
#'   are set to \code{1}.
#' @param covariate_correlation
#' Defines the correlation across covariates. Two formats are allowed:
#' - Can be a single \code{numeric} between \code{-1} and \code{1} which defines
#'   the correlation across all covariates.
#'   By default, \code{covariate_correlation = 0}.
#' - Can be a correlation \code{matrix} of dimension \code{n}, where \code{n}
#'   must equal the output of \code{\link{covariate_number}}. The
#'   \code{(i,j)}-th element defines the correlation between covariate \code{i}
#'   and covariate \code{j} in the output of \code{\link{covariate_names}}.
#' @export

sample_probit_covariates <- function(
  probit_formula, N, Tp = 1, probit_alternatives, seed = NULL,
  covariate_levels = Inf, occasion_constant = character(),
  covariate_mean = 0, covariate_sd = 1, covariate_correlation = 0,
  delimiter = "_"
) {

  ### input checks
  Tp <- expand_Tp(N = N, Tp = Tp)
  effects <- probit_effects(
    probit_formula = probit_formula,
    probit_alternatives = probit_alternatives
  )

  ### checks for covariate specifications
  covariate_names <- covariate_names(
    probit_formula, probit_alternatives, delimiter = delimiter
  )
  covariate_number <- covariate_number(probit_formula, probit_alternatives)
  checkmate::assert_numeric(
    covariate_levels, finite = FALSE, lower = 1, any.missing = FALSE
  )
  covariate_levels <- round(covariate_levels, digits = 0)
  if (checkmate::test_named(covariate_levels, type = "strict")) {
    checkmate::assert_subset(names(covariate_levels), covariate_names)
    covariate_levels_input <- covariate_levels
    covariate_levels <- rep(Inf, length.out = covariate_number)
    names(covariate_levels) <- covariate_names
    covariate_levels[names(covariate_levels_input)] <- covariate_levels_input
  } else {
    checkmate::assert_number(covariate_levels, lower = 1, finite = FALSE)
    covariate_levels <- rep(covariate_levels, length.out = covariate_number)
    names(covariate_levels) <- covariate_names
  }
  checkmate::assert_subset(
    occasion_constant, covariate_names, empty.ok = TRUE
  )
  checkmate::assert_numeric(covariate_mean, finite = TRUE, any.missing = FALSE)
  if (checkmate::test_named(covariate_mean, type = "strict")) {
    checkmate::assert_subset(names(covariate_mean), covariate_names)
    covariate_mean_input <- covariate_mean
    covariate_mean <- rep(0, length.out = covariate_number)
    names(covariate_mean) <- covariate_names
    covariate_mean[names(covariate_mean_input)] <- covariate_mean_input
  } else {
    checkmate::assert_number(covariate_mean, finite = TRUE)
    covariate_mean <- rep(covariate_mean, length.out = covariate_number)
    names(covariate_mean) <- covariate_names
  }
  checkmate::assert_numeric(
    covariate_sd, finite = TRUE, lower = 0, any.missing = FALSE
  )
  if (checkmate::test_named(covariate_sd, type = "strict")) {
    checkmate::assert_subset(names(covariate_sd), covariate_names)
    covariate_sd_input <- covariate_sd
    covariate_sd <- rep(1, length.out = covariate_number)
    names(covariate_sd) <- covariate_names
    covariate_sd[names(covariate_sd_input)] <- covariate_sd_input
  } else {
    checkmate::assert_number(covariate_sd, lower = 0, finite = TRUE)
    covariate_sd <- rep(covariate_sd, length.out = covariate_number)
    names(covariate_sd) <- covariate_names
  }
  if (checkmate::test_number(covariate_correlation, lower = -1, upper = 1)) {
    covariate_correlation_input <- covariate_correlation
    covariate_correlation <- diag(covariate_number)
    covariate_correlation[row(covariate_correlation) != col(covariate_correlation)] <- covariate_correlation_input
  }
  oeli::assert_correlation_matrix(covariate_correlation, dim = covariate_number)
  covariate_Sigma <- diag(covariate_sd) %*% covariate_correlation %*% diag(covariate_sd)
  oeli::assert_covariance_matrix(covariate_Sigma, dim = covariate_number)

  ### draw covariate values
  if (!is.null(seed)) {
    set.seed(seed)
  }
  covariates <- as.data.frame(
    MASS::mvrnorm(n = sum(Tp), mu = covariate_mean, Sigma = covariate_Sigma)
  )
  id <- rep(1:N, times = Tp)
  idc <- unlist(sapply(Tp, seq.int, simplify = FALSE))
  covariates <- cbind("id" = id, "idc" = idc, covariates)
  for (cov in covariate_names) {

    ### set covariate levels
    if (covariate_levels[cov] < sum(Tp)) {
      brks <- stats::quantile(
        covariates[[cov]], seq(0, 1, length.out = covariate_levels[cov] + 1)
      )
      ints <- findInterval(covariates[[cov]], brks, all.inside = TRUE)
      covariates[cov] <- (brks[ints] + brks[ints + 1]) / 2
    }

    ### set occasion constant covariates
    if (cov %in% occasion_constant) {
      covariates[cov] <- covariates[1, cov]
    }
  }

  ### validate data.frame format
  validate_probit_covariates(
    covariates = covariates, N = N, Tp = Tp, probit_formula = probit_formula,
    probit_alternatives = probit_alternatives
  )

  ### transform to list format
  covariates <- as.list(probit_covariates)
}

#' @rdname probit_covariates

validate_probit_covariates <- function(
    covariates, N, Tp = 1, probit_formula, probit_alternatives
) {

  ### input checks
  checkmate::assert_list(x)
  effects <- overview_effects(
    probit_formula = probit_formula,
    probit_alternatives = probit_alternatives
  )

  ### validate 'probit_covariates'
  if (checkmate::test_list(covariates)) {
    # TODO
  } else if (checkmate::test_data_frame(covariates)) {
    # TODO
  } else {
    # TODO
    stop()
  }

  ### return validated 'probit_covariates' object
  structure(x, class = c("probit_covariates", "list"))
}

#' @rdname probit_covariates
#' @exportS3Method

as.list.probit_covariates <- function() {

  x <- lapply(1:N, function(n) {
    lapply(1:Tp[n], function(t) {

      ### build general matrix
      X_nt <- matrix(0, nrow = J, ncol = nrow(effects))
      rownames(X_nt) <- probit_alternatives$alternatives
      colnames(X_nt) <- effects$name

      ### check for custom covariates
      for (e in 1:nrow(effects)) {
        if (effects[e, "covariate"] %in% names(custom_sampler)) {
          X_nt[, e] <- custom_sampler[[effects[e, "covariate"]]](n, t)
        }
      }

      ### check for alternative-constant covariates
      for (e in 1:nrow(effects)) {
        if (!effects[e, "as_covariate"]) {
          k <- which(effects[, "covariate"] == effects[e, "covariate"])[1]
          X_nt[, e] <- X_nt[1, k]
        }
      }

      ### check for alternative-specific effects
      for (e in 1:nrow(effects)) {
        j <- which(probit_alternatives$alternatives == effects[e, "alternative"])
        if (effects[e, "as_effect"]) {
          X_nt[-j, e] <- 0
        }
        if (startsWith(effects[e, "name"], "ASC_")) {
          X_nt[j, e] <- 1
        }
      }

      ### return covariate matrix for decider n at choice occasion t
      return(X_nt)
    })
  })


}

#' @rdname probit_covariates
#' @exportS3Method

as.data.frame.probit_covariates <- function() {
  # TODO
}

#' @rdname probit_covariates

covariate_names <- function(
  probit_formula, probit_alternatives, delimiter = "_"
) {
  effects <- probit_effects(
    probit_formula = probit_formula, probit_alternatives = probit_alternatives,
    delimiter = delimiter
  )
  covariate_names <- character()
  for (e in seq_len(nrow(effects))) {
    cov <- effects[e, "covariate"]
    ### quick and dirty: ignore ASCs
    if (!is.na(cov)) {
      if (effects[e, "as_covariate"]) {
        covariate_names <- c(
          covariate_names,
          paste(cov, probit_alternatives$alternatives, sep = delimiter)
        )
      } else {
        covariate_names <- c(covariate_names, cov)
      }
    }
  }
  ### quick and dirty: drop doubles
  unique(covariate_names)
}

#' @rdname probit_covariates

covariate_number <- function(probit_formula, probit_alternatives) {
  length(
    covariate_names(
      probit_formula = probit_formula, probit_alternatives = probit_alternatives
    )
  )
}

#' Expand \code{Tp}
#'
#' @description
#' This function expands the number of choice occasions \code{Tp} to a
#' \code{vector} of length \code{N}.
#'
#' @param N
#' A positive \code{integer}, the number of deciders.
#' @param Tp
#' A positive \code{integer}, the number of choice occasions per decider.
#' Can also be a \code{vector} of length \code{N} for a variable number of
#' choice occasions per decider.
#' By default, \code{Tp = 1}.
#'
#' @return
#' An \code{integer} \code{vector} of length \code{N}.

expand_Tp <- function(N, Tp = 1) {
  if (missing(N)) {
    stop("Please specify the number 'N' of deciders.")
  }
  checkmate::assert_int(N, lower = 1)
  checkmate::assert_numeric(Tp)
  if (length(Tp) == 1) {
    Tp <- rep(Tp, N)
  }
  checkmate::assert_integerish(Tp, lower = 1, len = N, any.missing = FALSE)
  as.integer(Tp)
}







