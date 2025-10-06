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
#' Defines the random effects in the model. The expected format of elements in
#' `random_effects` is `"<covariate>" = "<distribution>"`, where
#' `"<covariate>"` is the name of a variable on the `formula` right-hand side.
#' Every random effect must reference an explicit covariate (or `"ASC"` for
#' alternative-specific constants) that appears in the supplied model formula.
#'
#' Current options for `"<distribution>"` are:
#'
#' - `"cn"`: correlated (with other `"cn"` random effects) normal distribution
#'
#' To have random effects for the ASCs, use `"ASC"` for `"<covariate>"`.
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
#' @export
#'
#' @keywords model
#'
#' @examples
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
  error_term <- check_error_term(error_term, choices = c("probit", "logit"))
  random_effects <- check_random_effects(random_effects, choices = c("cn"))

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

  ### check RHS
  rhs_char <- switch(
    length(formula_rhs),
    `1` = paste(as.character(formula_rhs[1]), "| 1 | 0"),
    `2` = paste(
      as.character(formula_rhs[1]), "|",  as.character(formula_rhs[2]), "| 0"
    ),
    `3` = as.character(formula)[3],
    cli::cli_abort(
      "Input {.var formula} must not have more than two '|' separators",
      call = NULL
    )
  )
  rhs_char <- strsplit(rhs_char, split = "|", fixed = TRUE)[[1]]
  rhs_char <- gsub("\\s+", "",  rhs_char) # remove whitespaces
  split <- "\\+(?![^()]*\\))" # avoid splitting expressions enclosed in I(...)
  covariate_types <- strsplit(rhs_char, split, perl = TRUE) |> lapply(trimws)
  ASC <- ifelse(0 %in% covariate_types[[2]], FALSE, TRUE)
  if ("ASC" %in% unlist(covariate_types)) {
    cli::cli_abort(
      "Covariate name {.val ASC} in {.var formula} is not allowed",
      call = NULL
    )
  }

  ### rebuild formula based on covariate_types
  formula_rhs_char <- paste(
    sapply(covariate_types, paste, collapse = "+"), collapse = "|"
  )
  formula <- Formula::as.Formula(sprintf("%s ~ %s", choice, formula_rhs_char))
  covariate_types <- lapply(covariate_types, function(x) x[!x %in% 0:1])

  ### check random_effects
  names(random_effects) <- gsub("\\s+", "", names(random_effects)) # remove ws
  for (random_effect in names(random_effects)) {
    if (identical(random_effect, ".")) {
      cli::cli_abort(
        "Input {.var random_effects} cannot use '.'; specify the covariates
        explicitly in {.var formula}.",
        call = NULL
      )
    }
    if (!random_effect %in% c(unlist(covariate_types), if(ASC) "ASC")) {
      cli::cli_abort(
        "Input {.var random_effects} contains {.val {random_effect}}, but it is
        not on the right-hand side of {.var formula}",
        call = NULL
      )
    }
  }

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

is.choice_formula <- function(
  x,
  error = FALSE,
  var_name = oeli::variable_name(x)
) {
  validate_choice_object(
    x = x,
    class_name = "choice_formula",
    error = error,
    var_name = var_name
  )
}

#' @rdname choice_formula
#'
#' @param x \[`choice_formula`\]\cr
#' A `choice_formula` object.
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

resolve_choice_formula <- function(choice_formula, x) {

  ### input checks
  stopifnot(
    is.choice_formula(choice_formula, error = FALSE),
    is.choice_data(x, error = FALSE) || is.choice_covariates(x, error = FALSE)
  )
  form <- oeli::quiet(choice_formula$formula)
  format <- attr(x, "format")
  check_format(format)
  stopifnot(Formula::is.Formula(form))

  ### ensure long representation & an 'alternative' column
    if (identical(format, "wide")) {

      ### infer alternatives from column names if needed
      delimiter <- attr(x, "delimiter")
      if (is.null(delimiter)) delimiter <- "_"
      choice_type <- attr(x, "choice_type")
      if (is.null(choice_type)) choice_type <- "discrete"
      esc <- function(s) gsub("([][{}()+*^$|\\.?*\\\\])", "\\\\\\1", s)
      alts <- character()
      base_names <- character()
      if (identical(choice_type, "ordered")) {
        column_choice <- attr(x, "column_choice")
        if (!is.null(column_choice) && column_choice %in% names(x)) {
          vals <- x[[column_choice]]
          if (is.factor(vals)) {
            alts <- levels(vals)
          } else {
            alts <- unique(stats::na.omit(as.character(vals)))
          }
        }
      }
      if (length(alts) == 0L) {
        splits <- lapply(
          names(x),
          function(col) {
            matches <- gregexpr(delimiter, col, fixed = TRUE)[[1]]
            if (length(matches) == 1L && matches[1] == -1L) {
              return(NULL)
            }
            last <- matches[length(matches)]
            start <- last + nchar(delimiter)
            if (start > nchar(col)) {
              return(NULL)
            }
            list(
              base = if (last > 1) substr(col, 1, last - 1L) else "",
              alternative = substring(col, start)
            )
          }
        )
        splits <- Filter(Negate(is.null), splits)
        if (length(splits)) {
          base_names <- unique(vapply(splits, `[[`, character(1), "base"))
          base_names <- base_names[nzchar(base_names)]
          alts <- unique(vapply(splits, `[[`, character(1), "alternative"))
          alts <- alts[nzchar(alts)]
        }
      }
      if (length(alts) == 0L) {
        cli::cli_abort(
          "Could not infer alternatives from column names.", call = NULL
        )
      }

      ### temporary choice column if missing
      tmp_choice <- attr(x, "column_choice")
      if (is.null(tmp_choice)) {
        if (identical(choice_type, "ranked") && length(base_names)) {
          tmp_choice <- base_names[1L]
        } else {
          tmp_choice <- ".choicedata_dummy_choice"
          x[[tmp_choice]] <- alts[1L]
        }
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
        alternatives = alts,
        delimiter = delimiter,
        choice_type = choice_type
      )
  } else {
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
    fail <- inherits(mm, "fail") || is.null(mm) || ncol(mm) < 2L
    if (isTRUE(fail)) character() else colnames(mm)[-1L]
  })
  choice_formula$covariate_types <- covariate_types

  ### resolve random effects to actual column names (if any)
  re <- choice_formula$random_effects
  if (length(re) > 0) {
    squash <- function(s) gsub("\\s+", "", s)
    term_map <- list()
    all_cols <- character(0)
    for (r in seq_len(3L)) {
      mm <- oeli::try_silent(
        stats::model.matrix(form, data = x, lhs = 0, rhs = r)
      )
      if (inherits(mm, "fail") || is.null(mm) || ncol(mm) < 2L) next
      cols <- colnames(mm)[-1L]
      asg  <- attr(mm, "assign")[-1L]
      labs <- stats::terms(form, rhs = r, data = x) |> attr("term.labels")
      all_cols <- c(all_cols, cols)
      if (length(labs)) {
        for (i in seq_along(labs)) {
          key <- squash(labs[i])
          term_map[[key]] <- unique(c(term_map[[key]], cols[asg == i]))
        }
      }
    }
    all_cols <- unique(all_cols)
    new_names <- character(0); new_vals <- character(0)
    keys <- names(re); keys <- c(keys[keys == "."], keys[keys != "."])
    for (k in keys) {
      dist <- unname(re[[k]])
      cols_k <- if (identical(k, ".")) {
        all_cols
      } else {
        kk <- squash(k)
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

