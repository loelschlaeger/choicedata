#' Define identifiers
#'
#' @description
#' These functions define an object of class \code{\link{choice_identifiers}},
#' which defines identifiers for the deciders and the choice occasions.
#'
#' \code{\link{generate_choice_identifiers}} generates choice identifiers.
#'
#' \code{\link{read_choice_identifiers}} reads choice identifiers from a
#' \code{data.frame}.
#'
#' @param data
#' A \code{data.frame}.
#' @param column_decider
#' A \code{character}, the name of the \code{data.frame} column with identifier
#' for the deciders. The default is \code{"deciderID"}.
#' @param column_occasion
#' A \code{character}, different from \code{column_decider}, the name of the
#' \code{data.frame} column with identifier for the choice occasions. These
#' identifiers must be unique for a given decider.
#' @param x
#' TODO
#' @param error
#' TODO
#' @param data
#' TODO
#'
#' It can also be \code{NULL} (default), in which case data is treated as
#' cross-sectional. The decider ids must be unique in this case. A column for
#' the (trivial) choice occasion identifiers is generated anyways with the
#' default name \code{"occasionID"}.
#' @param as_cs
#' Either \code{TRUE} to treat the data as cross-sectional (i.e., dropping the
#' information of multiple choices by the same decider), or \code{FALSE}
#' (default), else.
#'
#' @return
#' A \code{\link{choice_identifiers}} object. It is a \code{data.frame} with
#' two columns, one named \code{column_decider} and containing the decider ids,
#' and the other one either named \code{column_occasion} or \code{"occasionID"} by
#' default, containing the choice occasion ids.

choice_identifiers <- function(
  data = data.frame(), column_decider = "deciderID",
  column_occasion = NULL, as_cs = FALSE
) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(column_decider)
  checkmate::assert_string(column_occasion, null.ok = TRUE)
  if (identical(column_decider, column_occasion)) {
    cli::cli_abort(
      "Names for {.var column_decider} and {.var column_occasion} must be
      different."
    )
  }
  checkmate::assert_flag(as_cs)
  if (!column_decider %in% colnames(data)) {
    cli::cli_abort(
      "Column {.val {column_decider}} not found in {.var data}."
    )
  }
  decider_ids <- as.character(data[[column_decider]])
  if (anyNA(decider_ids)) {
    cli::cli_abort(
      "Column {.val {column_decider}} of {.var data} must not have NAs."
    )
  }
  if (is.null(column_occasion)) {
    if (anyDuplicated(decider_ids)) {
      cli::cli_abort(
        "Column {.val {column_decider}} of {.var data} must not have
        duplicated values if there are no identifiers for the choice occasions."
      )
    }
    occasion_ids <- rep("1", length(decider_ids))
    column_occasion <- "occasionID"
    if (column_decider == "occasionID") {
      cli::cli_abort(
        "{.var column_decider} must not equal {.val occasionID}."
      )
    }
  } else if (!column_occasion %in% colnames(data)) {
    cli::cli_abort(
      "Column {.val {column_occasion}} not found in {.var data}."
    )
  } else {
    occasion_ids <- as.character(data[[column_occasion]])
    if (anyNA(occasion_ids)) {
      cli::cli_abort(
        "Column {.val {column_occasion}} of {.var data} must not have NAs."
      )
    }
    for (decider_id in unique(decider_ids)) {
      if (anyDuplicated(occasion_ids[which(decider_ids == decider_id)])) {
        cli::cli_abort(
          "Column {.val {column_occasion}} of {.var data} must have unique
          values for a given decider, but decider {.val decider_id} has
          duplicates for their occasion id."
        )
      }
    }
  }
  if (as_cs) {
    if (anyDuplicated(decider_ids)) {
      decider_ids <- paste(decider_ids, occasion_ids, sep = ".")
    }
    occasion_ids <- rep("1", length(decider_ids))
  }
  structure(
    data.frame(as.factor(decider_ids), as.factor(occasion_ids)),
    "names" = c(column_decider, column_occasion),
    "class" = c("choice_identifiers", "data.frame")
  )
}

#' @rdname choice_identifiers
#' @export

is.choice_identifiers <- function(x, error = TRUE) {
  check_not_missing(x)
  check <- inherits(x, "choice_identifiers")
  if (isTRUE(error) && !isTRUE(check)) {
    var_name <- oeli::variable_name(x)
    cli::cli_abort(
      "Input {.var {var_name}} must be an object of class {.cls choice_identifiers}",
      call = NULL
    )
  } else {
    isTRUE(check)
  }
}

#' @rdname choice_identifiers
#' @inheritParams expand_Tp

generate_choice_identifiers <- function(
  N, Tp = 1, column_decider = "deciderID", column_occasion = NULL
) {
  Tp <- expand_Tp(N = N, Tp = Tp)
  checkmate::assert_string(column_decider)
  checkmate::assert_string(column_occasion, null.ok = TRUE)
  if (is.null(column_occasion)) {
    column_occasion <- "occasionID"
    if (column_decider == "occasionID") {
      cli::cli_abort(
        "{.var column_decider} must not equal {.val occasionID}."
      )
    }
  }
  if (column_decider == column_occasion) {
    cli::cli_abort(
      "Names for {.var column_decider} and {.var column_occasion} must be
      different."
    )
  }
  data <- structure(
    data.frame(
      rep(1:N, times = Tp),                          # decider ids
      unlist(sapply(Tp, seq.int, simplify = FALSE))  # choice occasion ids
    ),
    "names" = c(column_decider, column_occasion)
  )
  choice_identifiers(
    data = data,
    column_decider = column_decider,
    column_occasion = column_occasion,
    as_cs = FALSE
  )
}

#' @rdname choice_identifiers

read_choice_identifiers <- function(
  data = data.frame(), column_decider = "deciderID", column_occasion = NULL,
  as_cs = FALSE
) {
  choice_identifiers(
    data = data, column_decider = column_decider,
    column_occasion = column_occasion, as_cs = as_cs
  )
}

#' Expand \code{Tp}
#'
#' @description
#' This helper function expands the number of choice occasions \code{Tp} to a
#' \code{vector} of length \code{N}.
#'
#' @param N (`integer(1)`)\cr
#' The number of deciders.
#'
#' @param Tp (`integer()`)\cr
#' The number of choice occasions per decider.
#'
#' Can also be a \code{vector} of length \code{N} for a variable number of
#' choice occasions per decider.
#'
#' @return
#' An \code{integer} \code{vector} of length \code{N}.
#'
#' @keywords internal

expand_Tp <- function(N, Tp = 1) {
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
