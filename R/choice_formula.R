#' Define choice model formula
#'
#' @description
#' The `choice_formula` object defines the choice model equation.
#'
#' @param formula \[`formula`\]\cr
#' A symbolic description of the choice model, see details.
#'
#' @param error_term \[`character(1)`\]\cr
#' Defines the model's error term. Current options are:
#'
#' - `"probit"` (default): errors are multivariate normally distributed
#' - `"logit"`: errors follow a type-I extreme value distribution
#'
#' @param random_effects \[`character()`\]\cr
#' Named vector defining random effects, see details.
#'
#' @return
#' An object of class `choice_formula`, which is a `list` of the elements:
#' \describe{
#'   \item{`formula`}{The model formula.}
#'   \item{`error_term`}{The name of the model's error term specification.}
#'   \item{`choice`}{The name of the response variable.}
#'   \item{`covariate_types`}{The (up to) three different types of covariates.}
#'   \item{`ASC`}{Does the model have ASCs?}
#'   \item{`random_effects`}{The names of covariates with random effects.}
#' }
#'
#' @section Specifying the model formula:
#' The structure of `formula` is `choice ~ A | B | C`, i.e., a standard
#' \code{\link[stats]{formula}} object but with three parts on the right-hand
#' side, separated by `|`, where
#' \itemize{
#'   \item `choice` is the name of the discrete response variable,
#'   \item `A` are names of \strong{alternative-specific covariates} with
#'   \strong{a coefficient that is constant across alternatives},
#'   \item `B` are names of \strong{covariates that are constant across
#'   alternatives},
#'   \item and `C` are names of \strong{alternative-specific covariates}
#'   with \strong{alternative-specific coefficients}.
#' }
#'
#' The following rules apply:
#' \enumerate{
#'   \item By default, intercepts (referred to as alternative-specific
#'   constants, ASCs) are added to the model. They can be removed by adding
#'   `+ 0` in the second part, e.g., `choice ~ A | B + 0 | C`. To not include
#'   any covariates of the second type but to estimate ASCs, add `1` in the
#'   second part, e.g., `choice ~ A | 1 | C`. The expression
#'   `choice ~ A | 0 | C` is interpreted as no covariates of the second type and
#'   no ASCs.
#'   \item To not include covariates of any type, add `0` in the respective
#'   part, e.g., `choice ~ 0 | B | C`.
#'   \item Some parts of the formula can be omitted when there is no ambiguity.
#'   For example, `choice ~ A` is equivalent to `choice ~ A | 1 | 0`.
#'   \item Multiple covariates in one part are separated by a `+` sign, e.g.,
#'   `choice ~ A1 + A2`.
#'   \item Arithmetic transformations of covariates in all three parts of the
#'   right-hand side are possible via the function `I()`, e.g.,
#'   `choice ~ I(A1^2 + A2 * 2)`. In this case, a random effect can be defined
#'   for the transformed covariate, e.g.,
#'   `random_effects = c("I(A1^2 + A2 * 2)" = "cn")`.
#' }
#'
#' @section Specifying random effects:
#' Specify random effects as `"<covariate>" = "<distribution>"`. Each covariate
#' must appear explicitly on the right-hand side of `formula`; use `"ASC"` for
#' alternative-specific constants.
#'
#' Available distributions are:
#'
#' - `"cn"`: correlated normal
#' - `"n"`: uncorrelated normal
#' - `"cln"`: positively signed correlated log-normal
#' - `"ln"`: positively signed uncorrelated log-normal
#' - `"cln-"`: negatively signed correlated log-normal
#' - `"ln-"`: negatively signed uncorrelated log-normal
#'
#' @export
#'
#' @keywords model
#'
#' @examples
#' ### specify a choice formula
#' choice_formula(
#'   formula = choice ~ I(A^2 + 1) | B | I(log(C)),
#'   error_term = "probit",
#'   random_effects = c("I(A^2+1)" = "cn", "B" = "cn")
#' )

choice_formula <- function(
  formula,
  error_term = "probit",
  random_effects = character()
) {

  ### input checks
  formula <- check_formula(formula)
  formula_env <- environment(formula)
  error_term <- check_error_term(error_term, choices = c("probit", "logit"))
  random_effects <- check_random_effects(
    random_effects,
    choices = c("cn", "n", "cln", "ln", "cln-", "ln-")
  )

  ### read formula
  formula <- Formula::as.Formula(formula)
  formula_lhs <- attr(formula, "lhs")
  formula_rhs <- attr(formula, "rhs")

  ### check LHS
  if (length(formula_lhs) != 1 || length(all.vars(formula_lhs[[1]])) != 1) {
    cli::cli_abort(
      "Input {.var formula} must have exactly one left-hand side",
      call = NULL
    )
  }
  if (length(as.character(formula_lhs[[1]])) != 1) {
    cli::cli_abort(
      "Transformation of the left-hand side of {.var formula} is not allowed",
      call = NULL
    )
  }
  choice <- as.character(formula_lhs[[1]])

  ### normalize and check RHS
  formula_rhs <- switch(
    length(formula_rhs),
    `1` = c(formula_rhs, list(1, 0)),
    `2` = c(formula_rhs, list(0)),
    `3` = formula_rhs,
    cli::cli_abort(
      "Input {.var formula} must not have more than two '|' separators",
      call = NULL
    )
  )
  formula_rhs_char <- paste(
    vapply(formula_rhs, deparse1, character(1)), collapse = " | "
  )
  formula <- Formula::as.Formula(sprintf("%s ~ %s", choice, formula_rhs_char))
  environment(formula) <- formula_env
  rhs_terms <- lapply(seq_len(3L), function(r) {
    rhs_formula <- stats::as.formula(
      call("~", formula_rhs[[r]]), env = formula_env
    )
    stats::terms(rhs_formula, allowDotAsName = TRUE)
  })
  covariate_types <- lapply(rhs_terms, function(x) {
    gsub("\\s+", "", attr(x, "term.labels"))
  })
  ASC <- identical(attr(rhs_terms[[2L]], "intercept"), 1L)
  if ("ASC" %in% unlist(covariate_types)) {
    cli::cli_abort(
      "Covariate name {.val ASC} in {.var formula} is not allowed",
      call = NULL
    )
  }

  ### check random_effects
  available_effects <- unlist(covariate_types, use.names = FALSE)
  available_keys <- canonical_formula_term(available_effects)
  random_effect_names <- gsub("\\s+", "", names(random_effects))
  for (i in seq_along(random_effect_names)) {
    random_effect <- random_effect_names[i]
    if (identical(random_effect, ".")) {
      cli::cli_abort(
        "Input {.var random_effects} cannot use '.'; specify the covariates
        explicitly in {.var formula}.",
        call = NULL
      )
    }
    if (!identical(random_effect, "ASC")) {
      matched <- match(canonical_formula_term(random_effect), available_keys)
      if (!is.na(matched)) random_effect_names[i] <- available_effects[matched]
    }
    if (!random_effect_names[i] %in% c(available_effects, if (ASC) "ASC")) {
      cli::cli_abort(
        "Input {.var random_effects} contains {.val {random_effect}}, but it is
        not on the right-hand side of {.var formula}",
        call = NULL
      )
    }
  }
  if (anyDuplicated(random_effect_names)) {
    cli::cli_abort(
      "Input {.var random_effects} contains duplicate formula terms.",
      call = NULL
    )
  }
  names(random_effects) <- random_effect_names

  ### build object
  structure(
    list(
      formula = formula,
      error_term = error_term,
      choice = choice,
      covariate_types = covariate_types,
      ASC = ASC,
      random_effects = random_effects
    ),
    class = c("choice_formula", "list")
  )
}

#' @noRd

canonical_formula_term <- function(term) {
  vapply(term, function(x) {
    term_info <- tryCatch(
      stats::terms(
        stats::as.formula(paste("~", x)),
        allowDotAsName = TRUE
      ),
      error = function(error) NULL
    )
    if (is.null(term_info)) return(gsub("\\s+", "", x))
    labels <- attr(term_info, "term.labels")
    orders <- attr(term_info, "order")
    if (length(labels) == 1L && length(orders) == 1L && orders > 1L) {
      factors <- attr(term_info, "factors")
      components <- rownames(factors)[factors[, 1L] > 0L]
      return(paste(sort(gsub("\\s+", "", components)), collapse = ":"))
    }
    if (length(labels) == 1L) gsub("\\s+", "", labels) else {
      gsub("\\s+", "", x)
    }
  }, character(1), USE.NAMES = FALSE)
}

#' @noRd

is.choice_formula <- function(
  x,
  error = FALSE,
  var_name = oeli::variable_name(x)
) {
  check_choice_object(
    x = x,
    class_name = "choice_formula",
    error = error,
    var_name = var_name
  )
}

#' @rdname choice_formula
#'
#' @param x \[`choice_formula`\]\cr
#' A \code{\link{choice_formula}} object.
#'
#' @param ...
#' Currently not used.
#'
#' @exportS3Method

print.choice_formula <- function(x, ...) {
  is.choice_formula(x, error = TRUE)
  cli::cli_h3("Choice formula")
  ul <- cli::cli_ul()
  cli::cli_li(deparse1(x$formula))
  cli::cli_li(paste("error term:", x$error_term))
  if (length(x$random_effects) > 0) {
    cli::cli_li("random effects:")
    ul2 <- cli::cli_ul()
    cli::cli_li(paste0(names(x$random_effects), ": ", x$random_effects))
    cli::cli_end(ul2)
  }
  cli::cli_end(ul)
  invisible(x)
}

#' @noRd

resolve_choice_formula <- function(
    choice_formula, x, choice_alternatives = NULL
  ) {

  ### input checks
  is.choice_formula(choice_formula, error = TRUE)
  check_choice_class_union(
    x,
    c("choice_data", "choice_covariates"),
    var_name = "x"
  )
  if (!is.null(choice_alternatives)) {
    is.choice_alternatives(
      choice_alternatives, error = TRUE, var_name = "choice_alternatives"
    )
  }
  form <- oeli::quiet(choice_formula$formula)
  format <- attr(x, "format")
  check_format(format)
  if (!Formula::is.Formula(form)) {
    cli::cli_abort(
      "The stored model formula must inherit from {.cls Formula}.",
      call = NULL
    )
  }

  ### ensure long representation and an 'alternative' column when needed
  alternative_specific_terms <- c(
    choice_formula$covariate_types[[1]],
    choice_formula$covariate_types[[3]]
  )
  needs_long_data <- length(alternative_specific_terms) > 0L
  if (identical(format, "wide") && needs_long_data) {

    ### use declared alternatives, or infer them from relevant columns
    delimiter <- attr(x, "delimiter")
    if (is.null(delimiter)) delimiter <- "_"
    choice_type <- attr(x, "choice_type")
    if (is.null(choice_type)) choice_type <- "unordered"
    alts <- if (is.null(choice_alternatives)) {
      excluded_columns <- c(
        attr(x, "column_decider"),
        attr(x, "column_occasion"),
        attr(x, "column_ac_covariates")
      )
      inference_columns <- setdiff(names(x), excluded_columns)
      guess_alternatives_wide(
        data_frame = x[, inference_columns, drop = FALSE],
        column_choice = attr(x, "column_choice"),
        delimiter = delimiter,
        allow_missing_columns = attr(x, "column_choice")
      )
    } else {
      as.character(choice_alternatives)
    }
    if (length(alts) == 0L) {
      cli::cli_abort(
        "Could not infer alternatives from column names.", call = NULL
      )
    }

    ### temporary choice column if missing
    tmp_choice <- attr(x, "column_choice")
    if (is.null(tmp_choice)) {
      tmp_choice <- ".choicedata_dummy_choice"
      x[[tmp_choice]] <- alts[1L]
    } else if (
      !(tmp_choice %in% names(x)) && !identical(choice_type, "ranked")
    ) {
      tmp_choice <- ".choicedata_dummy_choice"
      x[[tmp_choice]] <- alts[1L]
    }

    x <- wide_to_long(
      data_frame = x,
      column_choice = tmp_choice,
      column_alternative = "alternative",
      column_ac_covariates = attr(x, "column_ac_covariates"),
      alternatives = alts,
      delimiter = delimiter,
      choice_type = choice_type
    )
  } else if (identical(format, "long")) {
    alt_col <- attr(x, "column_alternative")
    if (is.null(alt_col)) alt_col <- "alternative"
    if (!identical(alt_col, "alternative")) {
      x[["alternative"]] <- x[[alt_col]]
    }
  }

  ### resolve covariate types
  covariate_types <- lapply(seq_len(3L), function(r) {
    mm <- oeli::try_silent(
      stats::model.matrix(form, data = x, lhs = 0, rhs = r)
    )
    oeli::input_check_response(
      check = if (inherits(mm, "fail")) as.character(mm) else TRUE,
      var_name = "formula"
    )
    if (inherits(mm, "fail")) {
      character()
    } else {
      if (is.null(mm) || ncol(mm) == 0L) return(character())
      assignments <- attr(mm, "assign")
      keep <- if (is.null(assignments) ||
          length(assignments) != ncol(mm)) {
        colnames(mm) != "(Intercept)"
      } else {
        assignments != 0L
      }
      colnames(mm)[keep]
    }
  })
  choice_formula$covariate_types <- covariate_types

  ### resolve random effects to actual column names (if any)
  re <- choice_formula$random_effects
  if (length(re) > 0) {
    term_map <- list()
    all_cols <- character(0)
    for (r in seq_len(3L)) {
      mm <- oeli::try_silent(
        stats::model.matrix(form, data = x, lhs = 0, rhs = r)
      )
      if (inherits(mm, "fail")) next
      if (is.null(mm) || ncol(mm) == 0L) next
      asg <- attr(mm, "assign")
      if (is.null(asg) || length(asg) != ncol(mm)) {
        keep <- colnames(mm) != "(Intercept)"
        asg <- seq_len(ncol(mm))[keep]
      } else {
        keep <- asg != 0L
        asg <- asg[keep]
      }
      cols <- colnames(mm)[keep]
      if (length(cols) == 0L) next
      labs <- stats::terms(form, rhs = r, data = x) |> attr("term.labels")
      all_cols <- c(all_cols, cols)
      if (length(labs)) {
        for (i in seq_along(labs)) {
          key <- canonical_formula_term(labs[i])
          term_map[[key]] <- unique(c(term_map[[key]], cols[asg == i]))
        }
      }
    }
    all_cols <- unique(all_cols)
    new_names <- character(0); new_vals <- character(0)
    keys <- names(re); keys <- c(keys[keys == "."], keys[keys != "."])
    for (k in keys) {
      dist <- unname(re[[k]])
      cols_k <- if (identical(k, "ASC")) {
        "ASC"
      } else if (identical(k, ".")) {
        all_cols
      } else {
        kk <- canonical_formula_term(k)
        cols_k <- term_map[[kk]]
        if (is.null(cols_k)) intersect(k, all_cols) else cols_k
      }
      if (length(cols_k)) {
        new_names <- c(new_names, cols_k)
        new_vals  <- c(new_vals, rep(dist, length(cols_k)))
      }
    }
    if (length(new_names)) {
      keep <- !duplicated(new_names, fromLast = TRUE)
      choice_formula$random_effects <- stats::setNames(
        new_vals[keep], new_names[keep]
      )
    } else {
      choice_formula$random_effects <- character(0)
    }
  }

  choice_formula
}
