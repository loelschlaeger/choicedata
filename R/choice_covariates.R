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
#' Column names with alternative-specific covariates in `data_frame`.
#'
#' @param delimiter \[`character(1)`\]\cr
#' Delimiter separating alternative identifiers from covariate names in wide
#' format. May consist of one or more characters.
#'
#' @inheritParams choice_identifiers
#'
#' @return
#' A `tibble`.
#'
#' @section Design matrices:
#' A covariate design matrix contains the choice covariates of a decider at a
#' choice occasion. It is of dimension \code{J} x \code{P}, where \code{J} is
#' the number of choice alternatives and \code{P} the number of effects. See
#' \code{\link{compute_P}} to compute the number \code{P}.
#'
#' @export
#'
#' @keywords data
#'
#' @examples
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
#'
#' ids <- generate_choice_identifiers(N = 3, Tp = 2)
#'
#' choice_covariates <- generate_choice_covariates(
#'   choice_effects = choice_effects,
#'   choice_identifiers = ids
#' )

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
  choice_identifiers <- choice_identifiers(
    data_frame = data_frame_wide[c(column_decider, column_occasion)],
    column_decider = column_decider,
    column_occasion = column_occasion,
    cross_section = cross_section
  )
  column_decider <- attr(choice_identifiers, "column_decider")
  column_occasion <- attr(choice_identifiers, "column_occasion")

  ### build 'choice_covariates' object
  choice_covariates <- if (format == "long") {
    choice_covariates <- cbind(
      choice_identifiers,
      data_frame_wide[, c(column_ac_covariates, column_as_covariates_wide)]
    ) |> wide_to_long(
      column_choice = NULL,
      column_alternative = column_alternative,
      alternatives = alternatives,
      delimiter = delimiter
    )
    columns <- c(
      column_decider, column_occasion, column_alternative,
      column_ac_covariates, column_as_covariates
    )
    choice_covariates <- choice_covariates[, columns, drop = FALSE]
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
  validate_choice_object(
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
#' Optional \code{\link{choice_effects}} object used to align covariate labels.
#'
#' @param choice_identifiers \[`choice_identifiers`\]\cr
#' A \code{\link{choice_identifiers}} object describing the simulated panel.
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

  covariates <- oeli::correlated_regressors(
    labels = labels,
    n = n,
    marginals = marginals,
    correlation = correlation,
    verbose = verbose
  )
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
  if (inherits(mm, "fail") || is.null(mm) || NCOL(mm) == 0L) {
    return(NULL)
  }
  keep <- colnames(mm) != "(Intercept)"
  if (!any(keep)) {
    mm[, 0, drop = FALSE]
  } else {
    mm[, keep, drop = FALSE]
  }
}

#' @noRd

prepare_choice_long_data <- function(x, choice_effects, choice_identifiers) {

  format <- attr(x, "format")
  column_choice <- attr(x, "column_choice")
  column_decider <- attr(x, "column_decider")
  column_occasion <- attr(x, "column_occasion")
  column_alternative <- attr(x, "column_alternative")
  delimiter <- attr(x, "delimiter")
  choice_alternatives <- attr(choice_effects, "choice_alternatives")
  alts <- as.character(choice_alternatives)
  choice_type <- attr(x, "choice_type")
  if (is.null(choice_type)) {
    ordered_alternatives <- isTRUE(attr(choice_alternatives, "ordered"))
    choice_type <- if (ordered_alternatives) "ordered" else "discrete"
  }

  if (identical(format, "wide")) {
    base_alt <- attr(choice_alternatives, "base")
    tmp_choice <- column_choice
    if (is.null(tmp_choice) || !(tmp_choice %in% names(x))) {
      tmp_choice <- ".choicedata_dummy_choice"
      x[[tmp_choice]] <- base_alt
    }
    x_long <- wide_to_long(
      data_frame = x,
      column_choice = tmp_choice,
      column_alternative = "alternative",
      alternatives = alts,
      delimiter = delimiter,
      choice_type = choice_type
    )
    column_choice_long <- tmp_choice
    column_alternative_long <- "alternative"
  } else {
    x_long <- x
    column_alternative_long <- if (is.null(column_alternative)) {
      "alternative"
    } else {
      column_alternative
    }
    if (!column_alternative_long %in% names(x_long)) {
      cli::cli_abort(
        "Missing {.val {column_alternative_long}} column in {.var x}
        (long format expected).",
        call = NULL
      )
    }
    if (!identical(column_alternative_long, "alternative")) {
      x_long[["alternative"]] <- x_long[[column_alternative_long]]
    }
    column_choice_long <- column_choice
  }

  if (!column_decider %in% names(x_long)) {
    cli::cli_abort(
      "Missing {.val {column_decider}} column in {.var x}.",
      call = NULL
    )
  }
  if (!is.null(column_occasion) && !column_occasion %in% names(x_long)) {
    cli::cli_abort(
      "Missing {.val {column_occasion}} column in {.var x}.",
      call = NULL
    )
  }

  ids_df <- as.data.frame(choice_identifiers)
  cd_id <- attr(choice_identifiers, "column_decider")
  co_id <- attr(choice_identifiers, "column_occasion")
  co_vals <- if (!is.null(co_id)) ids_df[[co_id]] else rep(1L, nrow(ids_df))

  list(
    x_long = x_long,
    format = format,
    column_choice = column_choice_long,
    column_decider = column_decider,
    column_occasion = column_occasion,
    column_alternative = column_alternative_long,
    alts = alts,
    J = attr(choice_alternatives, "J"),
    effect_names = choice_effects$effect_name,
    Tp = read_Tp(choice_identifiers),
    ids_df = ids_df,
    cd_id = cd_id,
    co_id = co_id,
    co_vals = co_vals,
    choice_type = choice_type
  )
}

#' @noRd

subset_choice_observation <- function(prep, index) {
  dec_val <- prep$ids_df[[prep$cd_id]][index]
  occ_val <- prep$co_vals[index]
  if (is.null(prep$column_occasion)) {
    id_prep <- prep$x_long[[prep$column_decider]] == dec_val
    df_nt <- prep$x_long[id_prep, , drop = FALSE]
  } else {
    df_nt <- prep$x_long[
      prep$x_long[[prep$column_decider]] == dec_val &
        prep$x_long[[prep$column_occasion]] == occ_val,
      , drop = FALSE
    ]
  }
  ord <- match(prep$alts, as.character(df_nt[["alternative"]]))
  if (anyNA(ord)) {
    cli::cli_abort(
      "Missing rows for some alternatives in {.var x} at decider
      {.val {dec_val}} and occasion
      {.val {if (is.null(prep$column_occasion)) 1 else occ_val}}.",
      call = NULL
    )
  }
  df_nt[ord, , drop = FALSE]
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
  validate_choice_class_union(
    x = x,
    class_names = c("choice_data", "choice_covariates"),
    var_name = "x"
  )

  prep <- prepare_choice_long_data(x, choice_effects, choice_identifiers)
  choice_formula <- attr(choice_effects, "choice_formula")
  choice_formula <- resolve_choice_formula(choice_formula, x)
  form <- choice_formula$formula
  P <- nrow(choice_effects)

  design_list <- vector("list", length = nrow(prep$ids_df))

  for (k in seq_len(nrow(prep$ids_df))) {
    df_nt <- subset_choice_observation(prep, k)
    ordered_type <- identical(prep$choice_type, "ordered")
    df_for_mm <- if (ordered_type) df_nt[1, , drop = FALSE] else df_nt

    mm1 <- drop_intercept(form, df_for_mm, 1L)
    mm2 <- drop_intercept(form, df_for_mm, 2L)
    mm3 <- drop_intercept(form, df_for_mm, 3L)

    X_nt <- matrix(0, nrow = if (ordered_type) 1L else prep$J, ncol = P)
    if (!ordered_type) {
      rownames(X_nt) <- prep$alts
    }
    colnames(X_nt) <- prep$effect_names

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
        if (isTRUE(e_is_ASC)) {
          X_nt[e_alt, e_name] <- 1
        } else if (!is.null(mm2) && e_cov %in% colnames(mm2)) {
          j <- match(e_alt, prep$alts)
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
          j <- match(e_alt, prep$alts)
          X_nt[e_alt, e_name] <- mm3[j, e_cov]
        }
      }
    }

    design_list[[k]] <- X_nt
  }

  structure(
    design_list,
    class = c("choice_design_matrices", "list"),
    Tp = prep$Tp,
    alternatives = prep$alts
  )
}

#' @noRd

extract_choice_indices <- function(
    choice_data,
    choice_effects,
    choice_identifiers = extract_choice_identifiers(choice_data)
  ) {

  is.choice_data(choice_data, error = TRUE)
  is.choice_effects(choice_effects, error = TRUE)
  is.choice_identifiers(choice_identifiers, error = TRUE)

  prep <- prepare_choice_long_data(
    choice_data, choice_effects, choice_identifiers
  )
  column_choice <- prep$column_choice
  if (is.null(column_choice) || !column_choice %in% names(prep$x_long)) {
    cli::cli_abort(
      "Cannot extract choices because column {.val {column_choice}} is
      missing.",
      call = NULL
    )
  }

  choice_list <- vector("list", length = nrow(prep$ids_df))
  for (k in seq_len(nrow(prep$ids_df))) {
    df_nt <- subset_choice_observation(prep, k)
    values_raw <- df_nt[[column_choice]]
    if (is.factor(values_raw) && is.ordered(values_raw)) {
      values <- as.numeric(values_raw)
    } else if (is.factor(values_raw)) {
      values <- suppressWarnings(as.numeric(as.character(values_raw)))
    } else {
      values <- suppressWarnings(as.numeric(values_raw))
    }
    if (identical(prep$choice_type, "ranked")) {
      expected <- seq_len(prep$J)
      rank_values <- suppressWarnings(as.numeric(values_raw))
      rank_integers <- suppressWarnings(as.integer(rank_values))
      if (length(rank_integers) != prep$J || anyNA(rank_integers) ||
          !isTRUE(all.equal(rank_values, rank_integers)) ||
          !identical(sort(rank_integers), expected)) {
        cli::cli_abort(
          "Ranked choice data must contain a full ranking of all alternatives.",
          call = NULL
        )
      }
      order_idx <- order(rank_integers)
      ranking_alts <- df_nt[["alternative"]][order_idx]
      choice_list[[k]] <- match(ranking_alts, prep$alts)
    } else if (identical(prep$choice_type, "ordered")) {
      non_missing <- !is.na(values_raw)
      if (!any(non_missing)) {
        cli::cli_abort(
          "Ordered choice data must contain an observed category for each
          observation.",
          call = NULL
        )
      }
      if (length(unique(values_raw[non_missing])) != 1L) {
        cli::cli_abort(
          "Ordered choice data must report a single category per observation.",
          call = NULL
        )
      }
      selected <- values_raw[which(non_missing)][1]
      idx <- NA_integer_
      if (is.factor(values_raw)) {
        idx <- as.integer(selected)
      } else if (is.numeric(selected)) {
        idx <- as.integer(selected)
      } else {
        idx <- match(as.character(selected), prep$alts)
      }
      if (is.na(idx) || idx < 1L || idx > prep$J) {
        cli::cli_abort(
          "Ordered choice categories must align with declared alternatives.",
          call = NULL
        )
      }
      choice_list[[k]] <- idx
    } else {
      chosen_idx <- which(values == 1)
      if (length(chosen_idx) != 1) {
        cli::cli_abort(
          "Choice data must contain exactly one chosen alternative per
          observation.",
          call = NULL
        )
      }
      chosen_alt <- df_nt[["alternative"]][chosen_idx]
      choice_list[[k]] <- match(chosen_alt, prep$alts)
    }
  }

  structure(choice_list, Tp = prep$Tp)
}

