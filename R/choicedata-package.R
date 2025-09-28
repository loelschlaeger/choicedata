#' @keywords internal

"_PACKAGE"

## usethis namespace: start
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
#' @importFrom ggplot2 autoplot
#' @importFrom Matrix bdiag
#' @importFrom mvtnorm GenzBretz
#' @importFrom mvtnorm pmvnorm
#' @importFrom oeli print_matrix
#' @importFrom optimizeR ParameterSpaces
#' @importFrom patchwork wrap_plots
#' @importFrom Rdpack reprompt
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom rlang sym
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_wider
#' @importFrom utils head
#' @importFrom utils tail
## usethis namespace: end
NULL

#' @keywords internal

tibble_class <- function(choicedata_class, add_classes = NULL) {
  c(choicedata_class, "tbl_df", "tbl", add_classes) |> unique()
}
