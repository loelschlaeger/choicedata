#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rdpack reprompt
#' @importFrom checkmate assert_flag
#' @importFrom checkmate check_string
#' @importFrom checkmate test_formula
#' @importFrom cli cat_line
#' @importFrom cli cli_abort
#' @importFrom cli cli_h3
#' @importFrom cli style_italic
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom oeli print_matrix
#' @importFrom rlang :=
#' @importFrom rlang sym
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_wider
#' @importFrom utils head
## usethis namespace: end
NULL

#' Change format of an object
#'
#' @description
#' This generic provides methods for changing the format of various objects
#' inside the `{choicedata}` package.
#'
#' @param x \[any\]\cr
#' Any `{choicedata}` object.
#'
#' @param new_format \[`character(1)`\]\cr
#' The requested format. Available values depend on the concrete method.
#'
#' @param ...
#' Additional arguments to be passed to the corresponding methods.
#'
#' @return
#' The transformed object.
#'
#' @export

change_format <- function(x, new_format, ...) {
  UseMethod("change_format")
}

