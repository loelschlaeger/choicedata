#' Define choice covariates
#'
#' @description
#' The \code{choice_covariates} object defines the choice model covariates.
#'
#' - \code{generate_choice_covariates} samples covariates
#' - \code{as.list.choice_covariates} transforms the covariates to a \code{list}
#'   of model design matrices, see details
#' - \code{as.data.frame} transforms the covariates to a \code{data.frame}
#' - \code{covariate_names} provides the covariate names for a given model
#'   specification
#'
#' @param covariates
#' TODO
#'
#' @return
#' A \code{data.frame}.
#'
#' @section Covariate matrices:
#' A covariate matrix contains the choice covariates of a decider at some choice
#' occasion. It is of dimension \code{J} x \code{P}, where \code{J} is the
#' number of alternatives and \code{P} the number of effects. See
#' \code{\link{compute_P}} to compute the number \code{P}.
#'
#' @examples
#' # TODO
#'
#' @export

choice_covariates <- function(covariates) {

  ### transform 'choice_data' to 'choice_covariates'
  # TODO

  ### validate 'choice_covariates'
  # validate_choice_covariates()
}

#' @noRd

is.choice_covariates <- function(
    x, error = FALSE, var_name = oeli::variable_name(x)
) {
  check_not_missing(x, var_name = var_name)
  check <- inherits(x, "choice_covariates")
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort(
      "Input {.var {var_name}} must be an object of class
      {.cls choice_covariates}",
      call = NULL
    )
  } else {
    isTRUE(check)
  }
}

#' @rdname choice_covariates
#' @export

generate_choice_covariates <- function(
  choice_effects,
  choice_identifiers,
  labels = covariate_names(choice_effects),
  marginals = list(),
  correlation = diag(length(labels))
) {

  ### input checks


  ### draw covariates

}

#' @rdname choice_covariates
#' @exportS3Method

as.list.choice_covariates <- function(x, ...) {

  ### input checks
  is.choice_covariates(x, error = TRUE)
  if (checkmate::test_list(x)) {
    return(x)
  }
  Tp <- attr(x, "Tp")
  N <- length(Tp)
  column_decider <- attr(x, "column_decider")
  column_occasion <- attr(x, "column_occasion")
  choice_formula <- attr(x, "choice_formula")
  choice_alternatives <- attr(x, "choice_alternatives")
  J <- attr(choice_alternatives, "J")
  delimiter <- attr(x, "delimiter")
  effects <- choice_effects(
    choice_formula = choice_formula,
    choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )

  ### transform to list
  covariates_matrix <- lapply(1:N, function(n) {
    lapply(1:Tp[n], function(t) {

      ### build covariate matrix for decider n at choice occasion t
      covariates_nt <- x[x[[column_decider]] == n & x[[column_occasion]] == t, ]
      X_nt <- matrix(0, nrow = J, ncol = nrow(effects))
      rownames(X_nt) <- as.character(choice_alternatives)
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
          for (alt in as.character(choice_alternatives)) {
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
    covariates_matrix,
    N = N,
    Tp = Tp,
    choice_formula = choice_formula,
    choice_alternatives = choice_alternatives,
    delimiter = delimiter,
    column_decider = column_decider,
    column_occasion = column_occasion
  )
}

#' @rdname choice_covariates
#' @param row.names,optional
#' Currently not used.
#' @exportS3Method

as.data.frame.choice_covariates <- function(x, row.names, optional, ...) {

  ### input checks
  is.choice_covariates(x, error = TRUE)
  if (checkmate::test_data_frame(x)) {
    return(x)
  }
  Tp <- attr(x, "Tp")
  N <- length(Tp)
  choice_formula <- attr(x, "choice_formula")
  choice_alternatives <- attr(x, "choice_alternatives")
  J <- attr(choice_alternatives, "J")
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
              effects[e, "covariate"], as.character(choice_alternatives),
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
}

#' @rdname choice_covariates

covariate_names <- function(choice_effects) {
  covariate_names <- character()
  for (e in seq_len(nrow(effects))) {
    cov <- effects[e, "covariate"]
    ### quick and dirty: ignore ASCs
    if (!is.na(cov)) {
      if (effects[e, "as_covariate"]) {
        covariate_names <- c(
          covariate_names,
          paste(cov, as.character(choice_alternatives), sep = delimiter)
        )
      } else {
        covariate_names <- c(covariate_names, cov)
      }
    }
  }
  ### quick and dirty: drop doubles
  unique(covariate_names)
}





