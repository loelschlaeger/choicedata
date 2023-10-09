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
  formula, N, J, Tp = 1, alternatives = LETTERS[1:J], base = alternatives[1],
  re = NULL, ordered = FALSE, seed = NULL,
  covariate_levels = Inf, occasion_constant = character(),
  covariate_mean = 0, covariate_sd = 1, covariate_correlation = 0
) {

  ### input checks
  Tp <- expand_Tp(N = N, Tp = Tp)
  probit_formula <- probit_formula(
    formula = formula, re = re, ordered = ordered
  )
  probit_alternatives <- probit_alternatives(
    J = J, alternatives = alternatives, base = base, ordered = ordered
  )
  effects <- overview_effects(
    probit_formula = probit_formula,
    probit_alternatives = probit_alternatives
  )
  covariate_numer <- covariate_number(probit_formula, probit_alternatives)
  if (length(covariate_levels) == 1) {
    checkmate::assert_int(covariate_levels, lower = 1)
    covariate_levels <- structure(

    )
  }
  checkmate::assert_integerish(
    covariate_levels, lower = 1, any.missing = FALSE, names = ...
  )

  covariate_Sigma <- diag(covariate_sd) %*% covariate_correlation %*% diag(covariate_sd)


  ### draw covariate values
  if (!is.null(seed)) {
    set.seed(seed)
  }
  covariates <- as.data.frame(
    MASS::mvrnorm(n = sum(Tp), mu = covariate_mean, Sigma = covariate_Sigma)
  )

  ### set covariate levels

  ### set occasion constant covariates

  ### validate data.frame format

  ### transform to list format

  ### validate 'probit_covariates'
  validate_probit_covariates(
    x = x, formula = formula, N = N, J = J, Tp = Tp, alternatives = alternatives,
    base = base, re = re, ordered = ordered
  )
}

#' @rdname probit_covariates

validate_probit_covariates <- function(
    x = list(), formula, N, J, Tp = 1, alternatives = LETTERS[1:J],
    base = alternatives[1], re = NULL, ordered = FALSE
) {

  ### input checks
  checkmate::assert_list(x)

  ### construct objects
  probit_formula <- probit_formula(
    formula = formula, re = re, ordered = ordered
  )
  probit_alternatives <- probit_alternatives(
    J = J, alternatives = alternatives, base = base, ordered = ordered
  )
  effects <- overview_effects(
    probit_formula = probit_formula,
    probit_alternatives = probit_alternatives
  )

  ### validate 'probit_covariates'
  # TODO

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







