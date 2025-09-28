#' Define choice identifiers
#'
#' @description
#' The `choice_identifiers` object defines identifiers for the deciders and
#' choice occasions.
#'
#' - `generate_choice_identifiers()` generates identifiers.
#' - `extract_choice_identifiers()` extracts choice identifiers.
#'
#' @param data_frame \[`data.frame`\]\cr
#' Contains the choice identifiers.
#'
#' @param format \[`character(1)`\]\cr
#' Either `"wide"` or `"long"`.
#'
#' In the long case, unique combinations of `column_decider` and
#' `column_occasion` (if present) are used.
#'
#' @param column_decider \[`character(1)`\]\cr
#' The name of the identifier column for deciders.
#'
#' @param column_occasion \[`character(1)` | `NULL`\]\cr
#' The name of the identifier column for choice occasions (panel data).
#' Can be `NULL` for the cross-sectional case.
#'
#' @param cross_section \[`logical(1)`\]\cr
#' Treat choice data as cross-sectional?
#'
#' @return
#' An object of class `choice_identifiers`, which is a `tibble` with columns:
#'
#' 1. `column_decider` contains the decider identifiers,
#' 2. `column_occasion` contains the choice occasion identifiers (only if
#'    `column_occasion` is not `NULL` and `cross_section = FALSE`).
#'
#' @export
#'
#' @keywords data
#'
#' @examples
#' ### panel case
#' generate_choice_identifiers(N = 2, Tp = 2)
#'
#' ### cross-sectional case
#' generate_choice_identifiers(N = 5, column_occasion = NULL)
#'
#' ### read choice identifiers
#' choice_identifiers(
#'   data_frame = travel_mode_choice,
#'   format = "long",
#'   column_decider = "individual",
#'   column_occasion = NULL,
#'   cross_section = TRUE
#' )

choice_identifiers <- function(
  data_frame,
  format = "wide",
  column_decider = "deciderID",
  column_occasion = "occasionID",
  cross_section = FALSE
) {

  ### input checks
  check_format(format)
  check_column_occasion(
    column_occasion, column_decider = column_decider, null.ok = cross_section
  )
  check_data_frame(
    data_frame, required_columns = c(column_decider, column_occasion)
  )
  check_cross_section(cross_section)

  ### collapse duplicates first if data are in long format
  ids_df <- if (identical(format, "long")) {
    unique(data_frame[c(column_decider, column_occasion)])
  } else {
    data_frame[c(column_decider, column_occasion)]
  }

  ### identifier checks
  decider_identifiers <- as.character(ids_df[[column_decider]])
  if (anyNA(decider_identifiers)) {
    cli::cli_abort(
      "Column {.val {column_decider}} of {.var data_frame} must not have NAs",
      call = NULL
    )
  }

  if (is.null(column_occasion)) {
    ### ensure uniqueness after collapsing
    if (anyDuplicated(decider_identifiers)) {
      cli::cli_abort(
        "Column {.val {column_decider}} of {.var data_frame} must not have
        duplicated values if there are no identifiers for the choice occasions",
        call = NULL
      )
    }
    occasion_identifiers <- rep("1", length(decider_identifiers))
    column_occasion <- "occasionID"
  } else {
    occasion_identifiers <- as.character(ids_df[[column_occasion]])
    if (anyNA(occasion_identifiers)) {
      cli::cli_abort(
        "Column {.val {column_occasion}} of {.var data_frame} must not have
        NAs",
        call = NULL
      )
    }
    for (decider_identifier in unique(decider_identifiers)) {
      ids <- which(decider_identifiers == decider_identifier)
      if (anyDuplicated(occasion_identifiers[ids])) {
        cli::cli_abort(
          "Column {.val {column_occasion}} of {.var data_frame} must have unique
          values for any decider, but decider {.val {decider_identifier}} has
          duplicates",
          call = NULL
        )
      }
    }
  }

  ### build identifiers
  if (cross_section) {
    ### transform to cross-section
    decider_identifiers <- decider_identifiers_to_cross_section(
      decider_identifiers = decider_identifiers,
      occasion_identifiers = occasion_identifiers, delimiter = "."
    )
    structure(
      data.frame(decider_identifiers),
      "names" = c(column_decider),
      "class" = tibble_class("choice_identifiers", class(data_frame)),
      column_decider = column_decider,
      cross_section = TRUE
    )
  } else {
    structure(
      data.frame(decider_identifiers, occasion_identifiers),
      "names" = c(column_decider, column_occasion),
      "class" = tibble_class("choice_identifiers", class(data_frame)),
      column_decider = column_decider,
      column_occasion = column_occasion,
      cross_section = FALSE
    )
  }
}

#' @noRd

is.choice_identifiers <- function(
  x,
  error = FALSE,
  var_name = oeli::variable_name(x)
) {
  validate_choice_object(
    x = x,
    class_name = "choice_identifiers",
    error = error,
    var_name = var_name
  )
}

#' @rdname choice_identifiers
#' @inheritParams expand_Tp
#' @export

generate_choice_identifiers <- function(
    N = length(Tp),
    Tp = 1,
    column_decider = "deciderID",
    column_occasion = "occasionID"
  ) {

  ### input checks
  Tp <- expand_Tp(N = N, Tp = Tp)
  column_occasion <- check_column_occasion(
    column_occasion, column_decider = column_decider, null.ok = TRUE
  )
  cross_section <- is.null(column_occasion)
  if (is.null(column_occasion)) {
    column_occasion <- "occasionID"
  }

  ### generate identifiers
  identifiers <- structure(
    data.frame(
      rep(1:N, times = Tp),                         # decider identifiers
      unlist(sapply(Tp, seq.int, simplify = FALSE)) # occasion identifiers
    ),
    "names" = c(column_decider, column_occasion)
  )
  choice_identifiers(
    data_frame = identifiers,
    column_decider = column_decider,
    column_occasion = column_occasion,
    cross_section = cross_section
  )

}

#' Unique decider identifiers in cross-sectional case
#'
#' @description
#' This helper function makes unique decider identifiers for a given combination
#' of decider and occasion identifiers.
#'
#' @param decider_identifiers,occasion_identifiers \[`atomic()`\]\cr
#' An `atomic` `vector` of identifiers.
#'
#' @return
#' An `atomic` `vector` of unique identifiers.
#'
#' @keywords internal

decider_identifiers_to_cross_section <- function(
  decider_identifiers, occasion_identifiers, delimiter = "."
) {

  ### input checks
  oeli::input_check_response(
    check = checkmate::check_atomic_vector(decider_identifiers),
    var_name = "decider_identifiers"
  )
  oeli::input_check_response(
    check = checkmate::check_atomic_vector(
      occasion_identifiers, len = length(decider_identifiers)
    ),
    var_name = "occasion_identifiers"
  )
  delimiter <- check_delimiter(delimiter)

  ### transform to cross-section
  if (anyDuplicated(decider_identifiers)) {
    decider_identifiers <- paste(
      decider_identifiers, occasion_identifiers, sep = delimiter
    )
  }
  decider_identifiers

}

#' Expand \code{Tp}
#'
#' @description
#' This helper function expands the number of choice occasions \code{Tp} to a
#' \code{vector} of length \code{N}.
#'
#' @param N \[`integer(1)`\]\cr
#' The number of deciders.
#'
#' @param Tp \[`integer(1)` | `integer(N)`\]\cr
#' The number of choice occasions per decider.
#'
#' Can also be of length \code{N} for a variable number of choice occasions per
#' decider.
#'
#' @return
#' An \code{integer} \code{vector} of length \code{N}.
#'
#' @keywords internal

expand_Tp <- function(N = length(Tp), Tp = 1) {
  check_not_missing(N)
  N <- check_N(N)
  Tp <- check_Tp(Tp, N = N)
  checkmate::assert_int(N, lower = 1)
  checkmate::assert_numeric(Tp)
  if (length(Tp) == 1) {
    Tp <- rep(Tp, N)
  }
  checkmate::assert_integerish(Tp, lower = 1, len = N, any.missing = FALSE)
  as.integer(Tp)
}

#' Read \code{Tp}
#'
#' @description
#' This helper function reads the number of choice occasions \code{Tp} from a
#' \code{\link{choice_identifiers}} object.
#'
#' @param choice_identifiers \[`choice_identifiers`\]\cr
#' The \code{\link{choice_identifiers}} object that defines the choice
#' occasions.
#'
#' @return
#' An \code{integer} \code{vector} of length \code{N}, where \code{N} is the
#' number of deciders.
#'
#' @keywords internal

read_Tp <- function(choice_identifiers) {

  ### input checks
  is.choice_identifiers(choice_identifiers, error = TRUE)
  column_decider <- attr(choice_identifiers, "column_decider")
  column_occasion <- attr(choice_identifiers, "column_occasion")

  ### read Tp
  if (is.null(column_occasion)) {
    rep(1L, times = nrow(choice_identifiers))
  } else {
    as.integer(table(choice_identifiers[, column_decider]))
  }
}

#' Get identifier position
#'
#' @description
#' This helper function gets a position based on a decider and a choice
#' occasion number.
#'
#' @inheritParams expand_Tp
#'
#' @param decider_number \[`integer(1)`\]\cr
#' A decider number, which is a number between 1 and `N`.
#'
#' @param occasion_number \[`integer(1)`\]\cr
#' An occasion number of decider `n`, which is a number between 1 and `Tp[n]`.
#'
#' @return
#' An \code{integer} position.
#'
#' @keywords internal

get_position_from_identifier <- function(
  N = length(Tp), Tp = 1, decider_number, occasion_number
) {
  Tp <- expand_Tp(N = N, Tp = Tp)
  checkmate::assert_integerish(
    decider_number, lower = 1, upper = N
  )
  checkmate::assert_integerish(
    occasion_number, lower = 1, upper = Tp[decider_number]
  )
  as.integer(c(0, cumsum(Tp))[decider_number] + occasion_number)
}

#' Get decider identifiers
#'
#' @description
#' This helper function extracts the (unique) decider identifiers from a
#' \code{\link{choice_identifiers}} object.
#'
#' @param choice_identifiers \[`choice_identifiers`\]\cr
#' A \code{\link{choice_identifiers}} object.
#'
#' @return
#' An \code{atomic} \code{vector} of decider identifiers.
#'
#' @keywords internal

get_decider_identifiers <- function(choice_identifiers) {
  is.choice_identifiers(choice_identifiers, error = TRUE)
  column_decider <- attr(choice_identifiers, "column_decider")
  unique(choice_identifiers[[column_decider]])
}

#' @rdname choice_identifiers
#'
#' @param x
#' An object of class
#'
#' * \code{\link{choice_data}},
#' * \code{\link{choice_covariates}}, containing the identifiers and covariate
#'   values for each decider, occasion, and alternative,
#'
#' @export

extract_choice_identifiers <- function(
  x,
  format = attr(x, "format"),
  column_decider = attr(x, "column_decider"),
  column_occasion = attr(x, "column_occasion"),
  cross_section = attr(x, "cross_section")
) {
  validate_choice_class_union(
    x = x,
    class_names = c("choice_data", "choice_covariates"),
    var_name = "x"
  )
  choice_identifiers(
    data_frame = x,
    format = if (!is.null(format)) format else "wide",
    column_decider = column_decider,
    column_occasion = column_occasion,
    cross_section = cross_section

  )
}
