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
#' `choice_covariates` tibble with the identifier and covariate columns. The
#' column roles are stored in the attributes `format`, `column_decider`,
#' `column_occasion`, `column_alternative`, `column_ac_covariates`,
#' `column_as_covariates`, `delimiter`, and `cross_section`, analogous to
#' \code{\link{choice_data}}.
#'
#' `covariate_names()` returns a `character` vector.
#'
#' `design_matrices()` returns a `list` of class `choice_design_matrices` with
#' one numeric design matrix per choice occasion, see the section below. The
#' attributes `Tp` (the number of choice occasions per decider), `alternatives`,
#' `availability` (the indices of the available alternatives per occasion), and
#' `choice_type` describe the structure.
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
  labels = if (is.null(choice_effects)) {
    character()
  } else {
    covariate_names(choice_effects)
  },
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

  ### build covariate names from the variables underlying each term
  covariate_names <- character()
  for (cov in formula_term_variables(unlist(covariate_types[c(1, 3)]))) {
    covariate_names <- c(
      covariate_names,
      paste(cov, as.character(choice_alternatives), sep = delimiter)
    )
  }
  for (cov in formula_term_variables(covariate_types[[2]])) {
    covariate_names <- c(covariate_names, cov)
  }
  unique(covariate_names)

}

#' @noRd

formula_term_variables <- function(terms) {
  variables <- lapply(terms, function(term) {
    parsed <- tryCatch(str2lang(term), error = function(e) NULL)
    if (is.null(parsed)) term else all.vars(parsed)
  })
  variables <- unique(unlist(variables, use.names = FALSE))
  setdiff(variables, ".")
}

#' @noRd

drop_intercept <- function(form, df, r) {
  mm <- oeli::try_silent(
    stats::model.matrix(
      form, data = df, rhs = r, lhs = 0, na.action = stats::na.pass
    )
  )
  oeli::input_check_response(
    check = if (inherits(mm, "fail")) as.character(mm) else TRUE,
    var_name = "formula"
  )
  if (is.null(mm) || NCOL(mm) == 0L) {
    return(NULL)
  }
  if (nrow(mm) != nrow(df)) {
    cli::cli_abort(
      "Covariates in part {r} of {.var formula} could not be evaluated for
      every row of the data.",
      call = NULL
    )
  }
  keep <- colnames(mm) != "(Intercept)"
  mm <- if (!any(keep)) mm[, 0, drop = FALSE] else mm[, keep, drop = FALSE]
  colnames(mm) <- gsub("\\s+", "", colnames(mm))
  mm
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
  build_design_matrices(prep, x, choice_effects)
}

#' @noRd

build_design_matrices <- function(prep, x, choice_effects) {

  ### resolve formula on the long data and check that it matches the data
  stored_formula <- attr(choice_effects, "choice_formula")
  x_long <- structure(
    prep$x_long,
    class = unique(c(class(x)[1L], class(prep$x_long))),
    format = "long",
    column_alternative = "alternative",
    column_decider = prep$column_decider,
    column_occasion = prep$column_occasion,
    column_ac_covariates = attr(x, "column_ac_covariates")
  )
  choice_formula <- resolve_choice_formula(
    stored_formula,
    x_long,
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
  ordered_type <- identical(prep$choice_type, "ordered")
  n_occasions <- nrow(prep$ids_df)

  ### model matrices for the three formula parts
  model_matrices <- lapply(seq_len(3L), function(r) {
    drop_intercept(form, prep$x_long, r)
  })
  effect_name <- as.character(choice_effects$effect_name)
  effect_cov <- as.character(choice_effects$covariate)
  effect_alt <- as.character(choice_effects$alternative)
  effect_as_cov <- choice_effects$as_covariate
  effect_as_eff <- choice_effects$as_effect
  effect_is_ASC <- is.na(effect_cov)
  if (ordered_type && any(effect_as_eff)) {
    cli::cli_abort(
      "Ordered choice models cannot include alternative-specific effects.",
      call = NULL
    )
  }
  column_of <- function(mm) {
    if (is.null(mm)) rep(NA_integer_, P) else match(effect_cov, colnames(mm))
  }
  col1 <- column_of(model_matrices[[1L]])
  col2 <- column_of(model_matrices[[2L]])
  col3 <- column_of(model_matrices[[3L]])
  mm1 <- model_matrices[[1L]]
  mm2 <- model_matrices[[2L]]
  mm3 <- model_matrices[[3L]]
  template <- matrix(
    0, nrow = if (ordered_type) 1L else prep$J, ncol = P,
    dimnames = list(if (!ordered_type) prep$alts, effect_name)
  )

  ### build one design matrix per choice occasion
  design_list <- vector("list", length = n_occasions)
  availability <- vector("list", length = n_occasions)
  for (k in seq_len(n_occasions)) {
    occasion <- subset_choice_occasion(prep, k)
    row_index <- occasion$row_index
    occasion_alts <- occasion$alternatives
    X_nt <- template
    if (ordered_type) {
      first_row <- row_index[1L]
      for (e in seq_len(P)) {
        if (!is.na(col1[e])) X_nt[1L, e] <- mm1[first_row, col1[e]]
      }
    } else {
      for (e in seq_len(P)) {
        if (!effect_as_eff[e]) {
          if (!is.na(col1[e])) {
            X_nt[occasion_alts, e] <- mm1[row_index, col1[e]]
          }
        } else {
          j <- match(effect_alt[e], occasion_alts)
          if (is.na(j)) next
          if (!effect_as_cov[e]) {
            if (effect_is_ASC[e]) {
              X_nt[effect_alt[e], e] <- 1
            } else if (!is.na(col2[e])) {
              X_nt[effect_alt[e], e] <- mm2[row_index[j], col2[e]]
            }
          } else if (!is.na(col3[e])) {
            X_nt[effect_alt[e], e] <- mm3[row_index[j], col3[e]]
          }
        }
      }
    }
    oeli::input_check_response(
      check = if (all(is.finite(X_nt))) TRUE else "Must contain finite values",
      var_name = "design matrix"
    )
    design_list[[k]] <- X_nt
    availability[[k]] <- occasion$availability
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
