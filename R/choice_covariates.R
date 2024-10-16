#' Define choice covariates
#'
#' @description
#' The \code{choice_covariates} object defines the choice model covariates.
#'
#' - \code{generate_choice_covariates()} samples covariates
#' - \code{design_matrices()} transforms the covariates to a \code{list}
#'   of model design matrices, see details
#' - \code{as.data.frame.design_matrices()} transforms the design matrices
#'   (back) to a \code{data.frame}
#' - \code{covariate_names()} provides the covariate names for a given model
#'   specification
#'
#' @inheritParams choice_identifiers
#'
#' @param column_covariates \[`character()`\]\cr
#' Names of columns in \code{data_frame} that store covariate values.
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

choice_covariates <- function(
    data_frame,
    column_decider = "deciderID",
    column_occasion = "occasionID",
    as_cross_section = FALSE,
    column_covariates = setdiff(
      colnames(data_frame), c(column_decider, column_occasion)
    )
  ) {

  ### input checks
  check_not_missing(data_frame)
  oeli::input_check_response(
     check = checkmate::check_names(column_covariates, type = "strict"),
     var_name = "column_covariates"
  )
  oeli::input_check_response(
    check = checkmate::check_data_frame(data_frame),
    var_name = "data_frame"
  )
  oeli::input_check_response(
    check = checkmate::check_names(
      colnames(data_frame),
      must.include = c(column_decider, column_occasion, column_covariates)
    ),
    var_name = "data_frame"
  )
  identifiers <- choice_identifiers(
    data_frame = data_frame[, c(column_decider, column_occasion)],
    column_decider = column_decider,
    column_occasion = column_occasion,
    as_cross_section = as_cross_section
  )

  ### build covariates
  covariates <- data_frame[, column_covariates]
  structure(
    cbind(identifiers, covariates),
    class = c("choice_covariates", "data.frame"),
    column_decider = attr(identifiers, "column_decider"),
    column_occasion = attr(identifiers, "column_occasion"),
    as_cross_section = attr(identifiers, "as_cross_section"),
    column_covariates = column_covariates
  )

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
  choice_effects = NULL,
  choice_identifiers = generate_choice_identifiers(N = 100),
  labels = covariate_names(choice_effects),
  n = nrow(choice_identifiers),
  marginals = list(),
  correlation = diag(length(labels)),
  # TODO: occasion_constant = ...
  verbose = FALSE
) {

  ### input checks
  is.choice_identifiers(choice_identifiers, error = TRUE)
  oeli::input_check_response(
    check = checkmate::check_names(labels, type = "strict"),
    var_name = "labels"
  )
  oeli::input_check_response(
    check = checkmate::check_count(n),
    var_name = "n"
  )
  oeli::input_check_response(
    check = if (n == nrow(choice_identifiers)) {
      TRUE
    } else {
      "Must be equal to the total number of identifiers"
    },
    var_name = "n"
  )

  ### build covariates
  covariates <- oeli::correlated_regressors(
    labels = labels,
    n = n,
    marginals = marginals,
    correlation = correlation,
    verbose = verbose
  )
  choice_covariates(
    data_frame = cbind(choice_identifiers, covariates),
    column_decider = attr(choice_identifiers, "column_decider"),
    column_occasion = attr(choice_identifiers, "column_occasion"),
    as_cross_section = attr(choice_identifiers, "as_cross_section"),
    column_covariates = labels
  )

}

#' @rdname choice_covariates

covariate_names <- function(choice_effects) {

  ### input checks
  is.choice_effects(choice_effects, error = TRUE)
  choice_formula <- attr(choice_effects, "choice_formula")
  var_types <- choice_formula$var_types
  choice_alternatives <- attr(choice_effects, "choice_alternatives")
  delimiter <- attr(choice_effects, "delimiter")

  ### build covariate names
  covariate_names <- character()
  for (cov in unlist(var_types[c(1, 3)])) {
    covariate_names <- c(
      covariate_names,
      paste(cov, as.character(choice_alternatives), sep = delimiter)
    )
  }
  for (cov in var_types[[2]]) {
    covariate_names <- c(covariate_names, cov)
  }
  return(covariate_names)

}

#' @rdname choice_covariates
#'
#' @param new_format \[`character(1)`\]\cr
#' - `"df_wide"`
#' - `"df_long"`
#' - `"design_matrices"`
#'
#' @exportS3Method

change_format.choice_covariates <- function(
  x,
  new_format = NULL,
  choice_effects
) {

  ### input checks


  ### change format of choice covariates


  check_not_missing(new_format)
  check_not_missing(choice_effects)

  cat("changed format to design matrices")

  return(x)
}

#' @noRd

df_wide_to_df_long <- function() {

}

#' @noRd

df_long_to_df_wide <- function() {

}

#' @noRd

df_to_design_matrices <- function(choice_covariates, choice_effects) {

  ### input checks
  is.choice_covariates(choice_covariates, error = TRUE)

  ### already in list format?
  if (checkmate::test_class(
    choice_covariates, c("choice_covariats", "design_matrices")
  )) {
    return(choice_covariates)
  }

  ### extract information
  column_decider <- attr(choice_covariates, "column_decider")
  column_occasion <- attr(choice_covariates, "column_occasion")
  choice_identifiers <- choice_identifiers(
    data_frame = choice_covariates,
    column_decider = column_decider,
    column_occasion = column_occasion,
    as_cross_section = attr(choice_covariates, "as_cross_section")
  )
  Tp <- read_Tp(choice_identifiers)
  N <- length(Tp)
  choice_formula <- attr(choice_effects, "choice_formula")
  choice_alternatives <- attr(choice_effects, "choice_alternatives")
  J <- attr(choice_alternatives, "J")
  delimiter <- attr(choice_effects, "delimiter")

  ### transform to list
  design_matrices <- lapply(seq_len(N), function(n) {
    lapply(seq_len(Tp[n]), function(t) {

      ### build covariate matrix for decider n at choice occasion t
      id_nt <- choice_covariates[[column_decider]] == n & choice_covariates[[column_occasion]] == t
      covariates_nt <- choice_covariates[id_nt, ]
      X_nt <- matrix(0, nrow = J, ncol = nrow(choice_effects))
      rownames(X_nt) <- as.character(choice_alternatives)
      colnames(X_nt) <- choice_effects$effect_name

      for (e in seq_len(nrow(choice_effects))) {
        e_name <- choice_effects[e, "effect_name"]
        e_covariate <- choice_effects[e, "covariate"]
        e_alternative <- choice_effects[e, "alternative"]
        e_as_covariate <- choice_effects[e, "as_covariate"]
        e_as_effect <- choice_effects[e, "as_effect"]
        e_is_ASC <- is.na(e_covariate)

        ### check for alternative-specific effects
        if (e_as_effect) {

          ### check for alternative-constant covariates
          if (e_as_covariate) {
            X_nt[e_alternative, e_name] <- covariates_nt[[e_name]]
          } else {

            ### check for ASCs
            X_nt[e_alternative, e_name] <-
              if (e_is_ASC) 1 else covariates_nt[[e_covariate]]
          }

        } else {

          for (alt in as.character(choice_alternatives)) {
            X_nt[alt, e_name] <-
              covariates_nt[[paste(e_name, alt, sep = delimiter)]]
          }
        }

      }
      return(X_nt)
    })
  })

  ### return
  structure(
    design_matrices,
    class = c("choice_covariates", "design_matrices", "list"),
    "choice_effects" = choice_effects,
    "choice_identifiers" = choice_identifiers
  )

}

#' @noRd

design_matrices_to_df <- function(choice_covariates, choice_effects) {

  ### input checks
  is.choice_covariates(x, error = TRUE)

  ### already in data.frame format?
  if (checkmate::test_data_frame(x)) {
    return(x)
  }

  ### extract information
  choice_identifiers <- attr(x, "choice_identifiers")
  column_decider <- attr(choice_identifiers, "column_decider")
  column_occasion <- attr(choice_identifiers, "column_occasion")
  as_cross_section <- attr(choice_identifiers, "as_cross_section")
  choice_effects <- attr(x, "choice_effects")
  choice_alternatives <- attr(choice_effects, "choice_alternatives")
  delimiter <- attr(choice_effects, "delimiter")
  Tp <- read_Tp(choice_identifiers)
  N <- length(Tp)

  ### create structure of data.frame
  covariate_names <- covariate_names(choice_effects)
  covariate_number <- length(covariate_names)
  covariates_df <- data.frame(
    matrix(NA_real_, nrow = sum(Tp), ncol = covariate_number)
  )
  covariates_df <- cbind(choice_identifiers, covariates_df)
  colnames(covariates_df) <- c(column_decider, column_occasion, covariate_names)

  ### enter covariates into data.frame
  for (n in seq_len(N)) {
    for (t in seq_len(Tp[n])) {
      cov_row <- which(
        covariates_df[[column_decider]] == n & covariates_df[[column_occasion]] == t
      )
      X_nt <- x[[n]][[t]]
      for (e in seq_len(nrow(choice_effects))) {

        e_name <- choice_effects[e, "effect_name"]
        e_covariate <- choice_effects[e, "covariate"]
        e_alternative <- choice_effects[e, "alternative"]
        e_as_covariate <- choice_effects[e, "as_covariate"]
        e_as_effect <- choice_effects[e, "as_effect"]
        e_is_ASC <- is.na(e_covariate)

        if (e_as_covariate) {
          if (e_as_effect) {
            covariates_df[cov_row, e_name] <- X_nt[e_alternative, e_name]
          } else {
            cov_names <- paste(
              e_covariate, as.character(choice_alternatives),
              sep = delimiter
            )
            covariates_df[cov_row, cov_names] <- X_nt[, e_covariate]
          }

        } else {
          if (!e_is_ASC) {
            covariates_df[cov_row, e_covariate] <- X_nt[e_alternative, e_name]
          }
        }
      }
    }
  }

  ### return
  choice_covariates(
    data_frame = covariates_df,
    column_decider = column_decider,
    column_occasion = column_occasion,
    as_cross_section = as_cross_section
  )

}





