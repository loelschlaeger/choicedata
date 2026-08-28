#' Define choice covariates
#'
#' @description
#' The `choice_covariates` object defines the choice model covariates.
#'
#' - `generate_choice_covariates()` samples covariates.
#' - `covariate_names()` gives the covariate names for given `choice_effects`.
#' - `design_matrices()` builds design matrices.
#'
#' @param data_frame \[`data.frame`\]\cr
#' Contains the choice covariates.
#'
#' @param format \[`character(1)`\]\cr
#' Format of `data_frame`. Use `"wide"` when covariates for all alternatives are
#' stored in a single row per occasion and `"long"` when each alternative forms
#' a separate row.
#'
#' @param column_decider \[`character(1)`\]\cr
#' Column name with decider identifiers.
#'
#' @param column_occasion \[`character(1)` | `NULL`\]\cr
#' Column name with occasion identifiers. Set to `NULL` for cross-sectional
#' data.
#'
#' @param column_alternative \[`character(1)` | `NULL`\]\cr
#' Column name with alternative identifiers when `format = "long"`.
#'
#' @param column_ac_covariates \[`character()` | `NULL`\]\cr
#' Column names with alternative-constant covariates.
#'
#' @param column_as_covariates \[`character()` | `NULL`\]\cr
#' Column names with alternative-specific covariates.
#'
#' @param delimiter \[`character(1)`\]\cr
#' Delimiter separating alternative identifiers from covariate names in wide
#' format.
#'
#' @inheritParams choice_identifiers
#'
#' @return
#' `choice_covariates()` and `generate_choice_covariates()` return a
#' `choice_covariates` tibble. `covariate_names()` returns a character vector.
#' `design_matrices()` returns one numeric design matrix per choice occasion in
#' a list; its `Tp` attribute records the panel lengths.
#'
#' @section Design matrices:
#' A covariate design matrix contains the choice covariates of a decider at a
#' choice occasion. It is of dimension \code{J} x \code{P}, where \code{J} is
#' the number of choice alternatives and \code{P} the number of effects.
#'
#' @export
#'
#' @keywords data
#'
#' @examples
#' ### sample covariates from choice effects
#' choice_effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ price | income | comfort,
#'     error_term = "probit",
#'     random_effects = c(
#'       "price" = "cn",
#'       "income" = "cn"
#'     )
#'   ),
#'   choice_alternatives = choice_alternatives(J = 3)
#' )
#' (choice_covariates <- generate_choice_covariates(
#'   choice_effects = choice_effects,
#'   choice_identifiers = generate_choice_identifiers(N = 3, Tp = 2)
#' ))

choice_covariates <- function(
  data_frame,
  format = "wide",
  column_decider = "deciderID",
  column_occasion = NULL,
  column_alternative = NULL,
  column_ac_covariates = NULL,
  column_as_covariates = NULL,
  delimiter = "_",
  cross_section = is.null(column_occasion)
) {

  ### input checks
  check_not_missing(data_frame)
  check_format(format)
  check_column_decider(column_decider, null.ok = FALSE)
  check_column_occasion(column_occasion, column_decider, null.ok = TRUE)
  check_column_alternative(column_alternative, null.ok = format == "wide")
  if (format == "wide") column_alternative <- NULL
  check_column_covariates(
    column_ac_covariates, null.ok = TRUE, var_name = "column_ac_covariates"
  )
  check_column_covariates(
    column_as_covariates, null.ok = TRUE, var_name = "column_as_covariates"
  )
  check_delimiter(delimiter)
  check_cross_section(cross_section)
  ac_as_covariates <- check_as_covariates(
    data_frame,
    format = format,
    column_choice = NULL,
    column_decider = column_decider,
    column_occasion = column_occasion,
    column_alternative = column_alternative,
    column_ac_covariates = column_ac_covariates,
    column_as_covariates = column_as_covariates,
    delimiter = delimiter
  )
  column_ac_covariates <- ac_as_covariates$column_ac_covariates
  column_as_covariates <- ac_as_covariates$column_as_covariates
  column_as_covariates_wide <- ac_as_covariates$column_as_covariates_wide
  required_columns = c(
    column_decider, column_occasion, column_alternative, column_ac_covariates,
    if (format == "long") column_as_covariates else column_as_covariates_wide
  )
  data_frame <- check_data_frame(
    data_frame,
    forbidden_columns = if (format == "wide") column_alternative,
    required_columns = required_columns
  )

  ### transform to wide format
  alternatives <- NULL
  if (identical(format, "long") && !is.null(column_alternative)) {
    alternatives <- data_frame[[column_alternative]]
    alternatives <- alternatives[!is.na(alternatives)]
    alternatives <- unique(as.character(alternatives))
  }

  data_frame_wide <- if (format == "long") {
    long_to_wide(
      data_frame = data_frame,
      column_as_covariates = column_as_covariates,
      column_choice = NULL,
      column_alternative = column_alternative,
      column_decider = column_decider,
      column_occasion = column_occasion,
      alternatives = alternatives,
      delimiter = delimiter
    )
  } else {
    data_frame
  }

  ### extract choice identifiers
  identifier_data <- if (identical(format, "long")) {
    data_frame[c(column_decider, column_occasion)]
  } else {
    data_frame_wide[c(column_decider, column_occasion)]
  }
  choice_identifiers <- choice_identifiers(
    data_frame = identifier_data,
    format = format,
    column_decider = column_decider,
    column_occasion = column_occasion,
    cross_section = cross_section
  )
  column_decider <- attr(choice_identifiers, "column_decider")
  column_occasion <- attr(choice_identifiers, "column_occasion")

  ### build 'choice_covariates' object
  choice_covariates <- if (format == "long") {
    columns <- c(
      column_decider, column_occasion, column_alternative,
      column_ac_covariates, column_as_covariates
    )
    columns <- unique(columns[!is.na(columns)])
    tibble::as_tibble(data_frame[, columns, drop = FALSE])
  } else {
    choice_covariates <- cbind(
      choice_identifiers,
      data_frame[c(column_ac_covariates, column_as_covariates_wide)]
    )
  }
  structure(
    choice_covariates,
    class = tibble_class("choice_covariates", class(data_frame)),
    format = format,
    column_decider = column_decider,
    column_occasion = column_occasion,
    column_alternative = column_alternative,
    column_ac_covariates = column_ac_covariates,
    column_as_covariates = column_as_covariates,
    delimiter = delimiter,
    cross_section = attr(choice_identifiers, "cross_section")
  )
}

#' @noRd

is.choice_covariates <- function(
    x, error = FALSE, var_name = oeli::variable_name(x)
) {
  check_choice_object(
    x = x,
    class_name = "choice_covariates",
    error = error,
    var_name = var_name
  )
}

#' @rdname choice_covariates
#'
#' @inheritParams oeli::correlated_regressors
#'
#' @param choice_effects \[`choice_effects` | `NULL`\]\cr
#' A \code{\link{choice_effects}} object.
#'
#' @param choice_identifiers \[`choice_identifiers`\]\cr
#' A \code{\link{choice_identifiers}} object.
#'
#' @export

generate_choice_covariates <- function(
  choice_effects = NULL,
  choice_identifiers = generate_choice_identifiers(N = 100),
  labels = covariate_names(choice_effects),
  n = nrow(choice_identifiers),
  marginals = list(),
  correlation = diag(length(labels)),
  verbose = FALSE,
  delimiter = "_"
) {

  ### input checks
  is.choice_identifiers(choice_identifiers, error = TRUE)
  if (!is.null(choice_effects)) is.choice_effects(choice_effects)

  covariates <- if (length(labels) == 0L) {
    data.frame(row.names = seq_len(n))
  } else {
    oeli::correlated_regressors(
      labels = labels,
      n = n,
      marginals = marginals,
      correlation = correlation,
      verbose = verbose
    )
  }
  choice_covariates(
    data_frame = cbind(choice_identifiers, covariates),
    format = "wide",
    column_decider = attr(choice_identifiers, "column_decider"),
    column_occasion = attr(choice_identifiers, "column_occasion"),
    column_alternative = NULL,
    column_ac_covariates = NULL,
    column_as_covariates = NULL,
    delimiter = delimiter,
    cross_section = attr(choice_identifiers, "cross_section")
  )
}

#' @rdname choice_covariates
#' @export

covariate_names <- function(choice_effects) {

  ### input checks
  is.choice_effects(choice_effects, error = TRUE)
  choice_formula <- attr(choice_effects, "choice_formula")
  covariate_types <- choice_formula$covariate_types
  choice_alternatives <- attr(choice_effects, "choice_alternatives")
  delimiter <- attr(choice_effects, "delimiter")

  ### build covariate names
  covariate_names <- character()
  for (cov in unlist(covariate_types[c(1, 3)])) {
    covariate_names <- c(
      covariate_names,
      paste(cov, as.character(choice_alternatives), sep = delimiter)
    )
  }
  for (cov in covariate_types[[2]]) {
    covariate_names <- c(covariate_names, cov)
  }
  return(covariate_names)

}

#' @noRd

drop_intercept <- function(form, df, r) {
  mm <- oeli::try_silent(
    stats::model.matrix(form, data = df, rhs = r, lhs = 0)
  )
  oeli::input_check_response(
    check = if (inherits(mm, "fail")) as.character(mm) else TRUE,
    var_name = "formula"
  )
  if (is.null(mm) || NCOL(mm) == 0L) {
    return(NULL)
  }
  keep <- colnames(mm) != "(Intercept)"
  if (!any(keep)) {
    mm[, 0, drop = FALSE]
  } else {
    mm[, keep, drop = FALSE]
  }
}

#' @rdname choice_covariates
#'
#' @param x
#' A \code{\link{choice_data}} or \code{\link{choice_covariates}} object.
#'
#' @export

design_matrices <- function(
    x,
    choice_effects,
    choice_identifiers = extract_choice_identifiers(x)
) {

  ### input checks
  check_not_missing(choice_effects)
  is.choice_effects(choice_effects, error = TRUE)
  is.choice_identifiers(choice_identifiers, error = TRUE)
  check_choice_class_union(
    x = x,
    class_names = c("choice_data", "choice_covariates"),
    var_name = "x"
  )

  prep <- prepare_choice_long_data(x, choice_effects, choice_identifiers)
  stored_formula <- attr(choice_effects, "choice_formula")
  choice_formula <- resolve_choice_formula(
    stored_formula,
    x,
    choice_alternatives = attr(choice_effects, "choice_alternatives")
  )
  oeli::input_check_response(
    check = if (identical(
      stored_formula$covariate_types,
      choice_formula$covariate_types
    )) TRUE else paste(
      "Does not match `x`; recreate `choice_effects` with `choice_data = x`"
    ),
    var_name = "choice_effects"
  )
  form <- choice_formula$formula
  P <- nrow(choice_effects)

  design_list <- vector("list", length = nrow(prep$ids_df))
  availability <- vector("list", length = nrow(prep$ids_df))
  model_matrices <- lapply(seq_len(3L), function(r) {
    drop_intercept(form, prep$x_long, r)
  })

  for (k in seq_len(nrow(prep$ids_df))) {
    df_nt <- subset_choice_occasion(prep, k)
    available <- attr(df_nt, "availability")
    ordered_type <- identical(prep$choice_type, "ordered")
    row_index <- attr(df_nt, "row_index")
    if (ordered_type) row_index <- row_index[1L]
    subset_matrix <- function(mm) {
      if (is.null(mm)) NULL else mm[row_index, , drop = FALSE]
    }
    mm1 <- subset_matrix(model_matrices[[1L]])
    mm2 <- subset_matrix(model_matrices[[2L]])
    mm3 <- subset_matrix(model_matrices[[3L]])

    X_nt <- matrix(0, nrow = if (ordered_type) 1L else prep$J, ncol = P)
    if (!ordered_type) {
      rownames(X_nt) <- prep$alts
    }
    colnames(X_nt) <- choice_effects$effect_name

    for (e in seq_len(P)) {
      e_name <- choice_effects$effect_name[e]
      e_cov <- choice_effects$covariate[e]
      e_alt <- choice_effects$alternative[e]
      e_as_cov <- choice_effects$as_covariate[e]
      e_as_eff <- choice_effects$as_effect[e]
      e_is_ASC <- is.na(e_cov)

      if (!e_as_eff) {
        if (!is.null(mm1) && !is.na(e_cov) && e_cov %in% colnames(mm1)) {
          vals <- mm1[, e_cov, drop = TRUE]
          if (ordered_type) {
            X_nt[1, e_name] <- vals[1]
          } else {
            names(vals) <- as.character(df_nt[["alternative"]])
            X_nt[names(vals), e_name] <- vals
          }
        }
      } else if (e_as_eff && !e_as_cov) {
        if (ordered_type) {
          cli::cli_abort(
            "Ordered choice models cannot include alternative-specific
            effects.",
            call = NULL
          )
        }
        j <- match(e_alt, as.character(df_nt[["alternative"]]))
        if (isTRUE(e_is_ASC) && !is.na(j)) {
          X_nt[e_alt, e_name] <- 1
        } else if (!is.na(j) && !is.null(mm2) &&
            e_cov %in% colnames(mm2)) {
          X_nt[e_alt, e_name] <- mm2[j, e_cov]
        }
      } else {
        if (ordered_type) {
          cli::cli_abort(
            "Ordered choice models cannot include alternative-specific
            effects.",
            call = NULL
          )
        }
        if (!is.null(mm3) && e_cov %in% colnames(mm3)) {
          j <- match(e_alt, as.character(df_nt[["alternative"]]))
          if (!is.na(j)) {
            X_nt[e_alt, e_name] <- mm3[j, e_cov]
          }
        }
      }
    }

    oeli::input_check_response(
      check = if (all(is.finite(X_nt))) TRUE else "Must contain finite values",
      var_name = "design matrix"
    )
    design_list[[k]] <- X_nt
    availability[[k]] <- available
  }

  structure(
    design_list,
    class = c("choice_design_matrices", "list"),
    Tp = prep$Tp,
    alternatives = prep$alts,
    availability = availability,
    choice_type = prep$choice_type
  )
}
