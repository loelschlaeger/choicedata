#' Define choice covariates
#'
#' @description
#' These functions create and validate an object of class
#' \code{\link{choice_covariates}}, which defines the model covariates,
#' see the details.
#'
#' \code{\link{sample_choice_covariates}} samples covariates.
#'
#' \code{\link{as.list.choice_covariates}} and
#' \code{\link{as.data.frame.choice_covariates}} transform the covariates in
#' \code{list} or \code{data.frame} form, respectively.
#'
#' In \code{list} form, it is a \code{list} of \code{list}s of \code{matrix}
#' elements. More precise: Let the return value be \code{out}, then
#' - \code{out} contains the covariate matrices of all deciders at all choice
#'   occasions,
#' - \code{out[[n]]} is a \code{list} of the covariate matrices of decider
#'   \code{n} at all of their choice occasions,
#' - \code{out[[n]][[t]]} is the covariate \code{matrix} of decider \code{n} at
#'   their \code{t}-th choice occasion.
#'
#' \code{\link{covariate_names}} and \code{\link{covariate_number}} provide the
#' names and number of covariates for a given model specification.
#'
#' @param choice_data
#' A \code{\link{choice_data}} object.
#' @param choice_formula
#' A \code{\link{choice_formula}} object.
#' @param choice_alternatives
#' A \code{\link{choice_alternatives}} object.
#' @inheritParams choice_effects
#' @param seed
#' An \code{integer}, passed to \code{set.seed()} to make the sampling of
#' covariates reproducible.
#' By default, \code{seed = NULL}, i.e., no seed is set.
#' @param ...
#' Currently not used.
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
#' A \code{data.frame}.
#'
#' @keywords object

choice_covariates <- function(choice_data) {

  ### transform 'choice_data' to 'choice_covariates'
  # TODO

  ### validate 'choice_covariates'
  # validate_choice_covariates()
}

#' @rdname choice_covariates
#' @param x
#' An \code{\link{choice_covariates}} object.

is.choice_covariates <- function(x) {
  inherits(x, "choice_covariates")
}

#' @rdname choice_covariates
#' @inheritParams choice_formula
#' @inheritParams expand_Tp
#' @inheritParams choice_alternatives
#' @inheritParams choice_data
#' @param covariate_levels
#' Defines the number of levels for the covariates. Two formats are allowed:
#' - Can be a single, positive \code{numeric} which defines the number of levels
#'   for all covariates. By default, \code{covariate_levels = Inf}.
#' - Can be a named \code{numeric} \code{vector} for specific number of levels
#'   for specific covariates. Names must correspond to the output of
#'   \code{\link{covariate_names}}. Number of levels for missing covariates are
#'   set to \code{Inf}.
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
#' @param empirical
#' If \code{TRUE}, \code{covariate_mean}, \code{covariate_sd}, and
#' \code{covariate_correlation} specify the empirical mean, standard
#' deviation and correlation. If \code{FALSE} (default), they specify the
#' values for the population.
#' @export

sample_choice_covariates <- function(
  choice_formula, N, Tp = 1, choice_alternatives, seed = NULL,
  covariate_levels = Inf,
  occasion_constant = character(),
  covariate_mean = 0,
  covariate_sd = 1,
  covariate_correlation = 0,
  empirical = FALSE,
  delimiter = "_",
  column_decider = "id",
  column_occasion = "idc"
) {

  ### input checks
  Tp <- expand_Tp(N = N, Tp = Tp)
  effects <- choice_effects(
    choice_formula = choice_formula,
    choice_alternatives = choice_alternatives
  )
  checkmate::assert_flag(empirical)
  checkmate::assert_string(column_decider)
  checkmate::assert_string(column_occasion)
  stopifnot(column_decider != column_occasion)
  covariate_levels <- check_covariate_levels(
    covariate_levels = covariate_levels, choice_formula = choice_formula,
    choice_alternatives = choice_alternatives, delimiter = delimiter
  )
  occasion_constant <- check_occasion_constant(
    occasion_constant = occasion_constant, choice_formula = choice_formula,
    choice_alternatives = choice_alternatives, delimiter = delimiter
  )
  covariate_mean <- check_covariate_mean(
    covariate_mean = covariate_mean, choice_formula = choice_formula,
    choice_alternatives = choice_alternatives, delimiter = delimiter
  )
  covariate_sd <- check_covariate_sd(
    covariate_sd = covariate_sd, choice_formula = choice_formula,
    choice_alternatives = choice_alternatives, delimiter = delimiter
  )
  covariate_correlation <- check_covariate_correlation(
    covariate_correlation = covariate_correlation, choice_formula = choice_formula,
    choice_alternatives = choice_alternatives, delimiter = delimiter
  )
  covariate_names <- covariate_names(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )

  ### draw covariate values
  if (!is.null(seed)) {
    set.seed(seed)
  }
  covariates <- as.data.frame(
    matrix(
      stats::rnorm(sum(Tp) * length(covariate_mean)),
      ncol = length(covariate_mean)
    )
  )
  covariates <- cbind(
    rep(1:N, times = Tp),                          # decider ids
    unlist(sapply(Tp, seq.int, simplify = FALSE)), # choice occasion ids
    covariates
  )
  colnames(covariates) <- c(column_decider, column_occasion, covariate_names)

  ### enforce mean, standard deviation and correlation
  Sigma <- outer(covariate_sd, covariate_sd) * covariate_correlation
  eigen_Sigma <- eigen(Sigma, symmetric = TRUE)
  reg <- as.matrix(covariates[, -(1:2), drop = FALSE])
  if (empirical) {
    reg <- scale(reg, center = TRUE, scale = FALSE)
    reg <- reg %*% svd(reg)$v
    reg <- scale(reg, center = FALSE, scale = TRUE)
  }
  reg <- covariate_mean +
    eigen_Sigma$vectors %*% diag(sqrt(pmax(eigen_Sigma$values, 0))) %*% t(reg)
  covariates[, -(1:2)] <- t(reg)

  ### enforce levels and occasion-constant covariates
  modified_cov <- character()
  for (cov in covariate_names) {

    ### set occasion-constant covariates
    if (cov %in% occasion_constant) {
      for (n in 1:N) {
        if (Tp[n] > 1) {
          covariates[covariates$id == n, cov] <- covariates[covariates$id == n, cov][1]
        }
      }
      if (any(Tp > 1)) {
        modified_cov <- c(modified_cov, cov)
      }
    }

    ### set covariate levels
    if (is.finite(covariate_levels[cov])) {
      if (length(unique(covariates[[cov]])) < covariate_levels[cov]) {
        warning(
          "column '", cov, "' can only have ", length(unique(covariates[[cov]])),
          " levels", call. = FALSE
        )
      } else {
        brks <- stats::quantile(
          covariates[[cov]], seq(0, 1, length.out = covariate_levels[cov] + 1)
        )
        ints <- findInterval(covariates[[cov]], brks, all.inside = TRUE)
        covariates[cov] <- (brks[ints] + brks[ints + 1]) / 2
        modified_cov <- c(modified_cov, cov)
      }
    }
  }
  if (length(modified_cov) > 0) {
    warning(
      ifelse(empirical, "empirical", ""),
      " means, standard deviations, and correlations cannot be enforced for covariates\n",
      paste(unique(modified_cov), collapse = ", "),
      call. = FALSE
    )
  }

  ### validate object
  validate_choice_covariates(
    covariates, N = N, Tp = Tp, choice_formula = choice_formula,
    choice_alternatives = choice_alternatives, delimiter = delimiter,
    column_decider = column_decider, column_occasion = column_occasion
  )

}

#' @keywords internal

check_covariate_levels <- function(
    covariate_levels, choice_formula, choice_alternatives, delimiter
  ) {
  checkmate::assert_numeric(
    covariate_levels, finite = FALSE, lower = 1, any.missing = FALSE
  )
  covariate_names <- covariate_names(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )
  covariate_number <- length(covariate_names)
  covariate_levels <- round(covariate_levels, digits = 0)
  if (checkmate::test_named(covariate_levels, type = "strict")) {
    covariate_levels <- covariate_spec_sugar(
      covariate_spec = covariate_levels, choice_formula = choice_formula,
      choice_alternatives = choice_alternatives, delimiter = delimiter,
      named_vector = TRUE
    )
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
  return(covariate_levels)
}

#' @keywords internal

check_occasion_constant <- function(
    occasion_constant, choice_formula, choice_alternatives, delimiter
  ) {
  covariate_names <- covariate_names(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )
  occasion_constant <- covariate_spec_sugar(
    covariate_spec = occasion_constant, choice_formula = choice_formula,
    choice_alternatives = choice_alternatives, delimiter = delimiter,
    named_vector = FALSE
  )
  checkmate::assert_subset(
    occasion_constant, covariate_names, empty.ok = TRUE
  )
  return(occasion_constant)
}

#' @keywords internal

check_covariate_mean <- function(
    covariate_mean, choice_formula, choice_alternatives, delimiter
  ) {
  checkmate::assert_numeric(covariate_mean, finite = TRUE, any.missing = FALSE)
  covariate_names <- covariate_names(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )
  covariate_number <- length(covariate_names)
  if (checkmate::test_named(covariate_mean, type = "strict")) {
    covariate_mean <- covariate_spec_sugar(
      covariate_spec = covariate_mean, choice_formula = choice_formula,
      choice_alternatives = choice_alternatives, delimiter = delimiter,
      named_vector = TRUE
    )
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
  return(covariate_mean)
}

#' @keywords internal

check_covariate_sd <- function(
    covariate_sd, choice_formula, choice_alternatives, delimiter
  ) {
  checkmate::assert_numeric(
    covariate_sd, finite = TRUE, lower = 0, any.missing = FALSE
  )
  covariate_names <- covariate_names(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )
  covariate_number <- length(covariate_names)
  if (checkmate::test_named(covariate_sd, type = "strict")) {
    covariate_sd <- covariate_spec_sugar(
      covariate_spec = covariate_sd, choice_formula = choice_formula,
      choice_alternatives = choice_alternatives, delimiter = delimiter,
      named_vector = TRUE
    )
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
  return(covariate_sd)
}

#' @keywords internal

check_covariate_correlation <- function(
    covariate_correlation, choice_formula, choice_alternatives, delimiter
  ) {
  covariate_names <- covariate_names(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )
  covariate_number <- length(covariate_names)
  if (checkmate::test_number(covariate_correlation)) {
    if (covariate_correlation < -1 || covariate_correlation > 1) {
      stop("correlation must be between -1 and 1", call. = FALSE)
    }
    covariate_correlation_input <- covariate_correlation
    covariate_correlation <- diag(covariate_number)
    covariate_correlation[row(covariate_correlation) != col(covariate_correlation)] <- covariate_correlation_input
  }
  oeli::assert_correlation_matrix(covariate_correlation, dim = covariate_number)
  return(covariate_correlation)
}

#' @keywords internal

covariate_spec_sugar <- function(
    covariate_spec, choice_formula, choice_alternatives, delimiter, named_vector
  ) {

  ### syntactic sugar for selecting alternative-specific covariate
  checkmate::assert_flag(named_vector)
  if (named_vector) {
    checkmate::assert_vector(covariate_spec, any.missing = FALSE, names = "strict")
  } else {
    checkmate::assert_character(covariate_spec, any.missing = FALSE, unique = TRUE)
  }
  covariate_spec_input <- covariate_spec
  choice_effects <- choice_effects(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )
  covariate_names <- covariate_names(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )

  ### quick and dirty check if alternative-specific covariate has been specified
  ### without alternative label
  for (i in seq_along(covariate_spec)) {

    cov_name_i <- ifelse(named_vector, names(covariate_spec)[i], covariate_spec[i])

    if (!cov_name_i %in% covariate_names) {
      if (cov_name_i %in% choice_formula$var_types[c(1, 3)]) {

        ### extend covariate name by all alternative labels
        cov_name_i_extended <- paste(
          cov_name_i, choice_alternatives$alternatives, sep = delimiter
        )

        if (named_vector) {

          ### remove redundancies
          cov_name_i_extended <- setdiff(cov_name_i_extended, names(covariate_spec))

          ### add covariate name with alternative labels
          covariate_spec_input[cov_name_i_extended] <- covariate_spec_input[cov_name_i]

          ### remove bad covariate specification without alternative label
          covariate_spec_input <- covariate_spec_input[names(covariate_spec_input) != cov_name_i]

        } else {

          ### remove redundancies
          cov_name_i_extended <- setdiff(cov_name_i_extended, covariate_spec)

          covariate_spec_input <- c(covariate_spec_input, cov_name_i_extended)
          covariate_spec_input <- covariate_spec_input[covariate_spec_input != cov_name_i]

        }
      }
    }
  }

  if (!named_vector) {
    covariate_spec_input <- unique(covariate_spec_input)
  }

  return(covariate_spec_input)
}

#' @keywords internal

effect_is_ASC <- function(effect_name, delimiter) {
  checkmate::assert_string(effect_name)
  checkmate::assert_string(delimiter, n.chars = 1)
  startsWith(effect_name, paste0("ASC", delimiter))
}

#' @rdname choice_covariates

validate_choice_covariates <- function(
  x, N, Tp = 1, choice_formula, choice_alternatives, delimiter,
  column_decider, column_occasion
) {

  ### input checks
  Tp <- expand_Tp(N = N, Tp = Tp)
  effects <- choice_effects(
    choice_formula = choice_formula,
    choice_alternatives = choice_alternatives
  )
  covariate_names <- covariate_names(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )

  ### validate 'choice_covariates'
  if (checkmate::test_class(x, "choice_covariates")) {
    return(x)
  } else if (checkmate::test_list(x)) {

    # TODO: add validation cases for list
    class(x) <- c("choice_covariates", "list")


  } else if (checkmate::test_data_frame(x)) {

    # TODO: add validationcases for data.frame
    class(x) <- c("choice_covariates", "data.frame")

  } else {
    stop("Input 'x' must be a 'list' or a 'data.frame'.")
  }

  ### add attributes
  structure(
    x,
    "Tp" = Tp,
    "choice_formula" = choice_formula,
    "choice_alternatives" = choice_alternatives,
    "delimiter" = delimiter,
    "column_decider" = column_decider,
    "column_occasion" = column_occasion
  )
}

#' @rdname choice_covariates
#' @exportS3Method

as.list.choice_covariates <- function(x, ...) {

  ### input checks
  checkmate::assert_data_frame(x)
  checkmate::assert_class(x, "choice_covariates")
  Tp <- attr(x, "Tp")
  N <- length(Tp)
  choice_formula <- attr(x, "choice_formula")
  choice_alternatives <- attr(x, "choice_alternatives")
  J <- choice_alternatives$J
  delimiter <- attr(x, "delimiter")
  effects <- choice_effects(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )

  ### transform to list
  covariates_matrix <- lapply(1:N, function(n) {
    lapply(1:Tp[n], function(t) {

      ### build covariate matrix for decider n at choice occasion t
      covariates_nt <- x[x[["id"]] == n & x[["idc"]] == t, ]
      X_nt <- matrix(0, nrow = J, ncol = nrow(effects))
      rownames(X_nt) <- choice_alternatives$alternatives
      colnames(X_nt) <- effects$name
      for (e in seq_len(nrow(effects))) {

        ### check for alternative-specific effects
        if (effects[e, "as_effect"]) {

          ### check for alternative-constant covariates
          if (effects[e, "as_covariate"]) {

            X_nt[effects[e, "alternative"], effects[e, "name"]] <-
              covariates_nt[[effects[e, "name"]]]

          } else {

            ### check for ASCs
            X_nt[effects[e, "alternative"], effects[e, "name"]] <-
            if (effect_is_ASC(effects[e, "name"], delimiter)) {
              1
            } else {
              covariates_nt[[effects[e, "covariate"]]]
            }

          }

        } else {
          for (alt in choice_alternatives$alternatives) {
            X_nt[alt, effects[e, "name"]] <-
              covariates_nt[[paste(effects[e, "name"], alt, sep = delimiter)]]
          }
        }

      }
      return(X_nt)
    })
  })

  ### validate
  validate_choice_covariates(
    covariates_matrix, N = N, Tp = Tp, choice_formula = choice_formula,
    choice_alternatives = choice_alternatives, delimiter = delimiter,
    column_decider = attr(x, "column_decider"),
    column_occasion = attr(x, "column_occasion")
  )
}

#' @rdname choice_covariates
#' @param row.names,optional
#' Currently not used.
#' @exportS3Method

as.data.frame.choice_covariates <- function(x, row.names, optional, ...) {

  ### input checks
  checkmate::assert_list(x)
  checkmate::assert_class(x, "choice_covariates")
  Tp <- attr(x, "Tp")
  N <- length(Tp)
  choice_formula <- attr(x, "choice_formula")
  choice_alternatives <- attr(x, "choice_alternatives")
  J <- choice_alternatives$J
  delimiter <- attr(x, "delimiter")
  effects <- choice_effects(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )
  column_decider <- attr(x, "column_decider")
  column_occasion <- attr(x, "column_occasion")

  ### create structure of data.frame
  covariate_names <- covariate_names(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )
  covariate_number <- covariate_number(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives
  )
  covariates_df <- data.frame(
    matrix(NA_real_, nrow = sum(Tp), ncol = covariate_number)
  )
  id <- rep(1:N, times = Tp)
  idc <- unlist(sapply(Tp, seq.int, simplify = FALSE))
  covariates_df <- cbind(id, idc, covariates_df)
  colnames(covariates_df) <- c(column_decider, column_occasion, covariate_names)

  ### enter covariates into data.frame
  for (n in 1:N) {
    for (t in 1:Tp[n]) {
      cov_row <- which(
        covariates_df[[column_decider]] == n & covariates_df[[column_occasion]] == t
      )
      X_nt <- x[[n]][[t]]
      for (e in seq_len(nrow(effects))) {
        if (effects[e, "as_covariate"]) {
          if (effects[e, "as_effect"]) {
            covariates_df[cov_row, effects[e, "name"]] <-
              X_nt[effects[e, "alternative"], effects[e, "name"]]
          } else {
            cov_names <- paste(
              effects[e, "covariate"], choice_alternatives$alternatives,
              sep = delimiter
            )
            covariates_df[cov_row, cov_names] <- X_nt[, effects[e, "covariate"]]
          }

        } else {
          if (!effect_is_ASC(effects[e, "name"], delimiter)) {
            covariates_df[cov_row, effects[e, "covariate"]] <-
              X_nt[effects[e, "alternative"], effects[e, "name"]]
          }
        }
      }
    }
  }

  ### validate
  validate_choice_covariates(
    covariates_df, N = N, Tp = Tp, choice_formula = choice_formula,
    choice_alternatives = choice_alternatives, delimiter = delimiter,
    column_decider = column_decider, column_occasion = column_occasion
  )
}

#' @rdname choice_covariates

covariate_names <- function(
  choice_formula, choice_alternatives, delimiter = "_"
) {
  effects <- choice_effects(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
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
          paste(cov, choice_alternatives$alternatives, sep = delimiter)
        )
      } else {
        covariate_names <- c(covariate_names, cov)
      }
    }
  }
  ### quick and dirty: drop doubles
  unique(covariate_names)
}

#' @rdname choice_covariates

covariate_number <- function(choice_formula, choice_alternatives) {
  length(
    covariate_names(
      choice_formula = choice_formula, choice_alternatives = choice_alternatives
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
#'
#' @keywords internal

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





