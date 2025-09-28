#' Stated Preferences for Train Traveling
#'
#' @description
#' Data set of 2929 stated choices by 235 Dutch individuals deciding between
#' two hypothetical train trip options \code{"A"} and \code{"B"} based on the
#' price, the travel time, the number of rail-to-rail transfers (changes), and
#' the level of comfort.
#'
#' The data were obtained in 1987 by Hague Consulting Group for the National
#' Dutch Railways. Prices were recorded in Dutch guilder and in this data set
#' transformed to Euro at an exchange rate of 2.20371 guilders = 1 Euro.
#'
#' @format
#' A \code{tibble} with 2929 rows and 11 columns:
#' \describe{
#'   \item{deciderID \[`integer`\]}{The identifier for the decider.}
#'   \item{occasionID \[`integer`\]}{The identifier for the choice occasion.}
#'   \item{choice \[`character`\]}{The chosen alternative, either \code{"A"} or
#'   \code{"B"}.}
#'   \item{price_A \[`numeric`\]}{The price for alternative \code{"A"} in Euro.}
#'   \item{time_A \[`numeric`\]}{The travel time for alternative \code{"A"} in
#'   hours.}
#'   \item{change_A \[`integer`\]}{The number of changes for alternative
#'   \code{"A"}.}
#'   \item{comfort_A \[`factor`\]}{The comfort level for alternative \code{"A"},
#'   where `0` is the best comfort and `2` the worst.}
#'   \item{price_B \[`numeric`\]}{The price for alternative \code{"B"} in Euro.}
#'   \item{time_B \[`numeric`\]}{The travel time for alternative \code{"B"} in
#'   hours.}
#'   \item{change_B \[`integer`\]}{The number of changes for alternative
#'   \code{"B"}.}
#'   \item{comfort_B \[`factor`\]}{The comfort level for alternative \code{"B"},
#'   where `0` is the best comfort and `2` the worst.}
#' }
#'
#' @references
#' \insertRef{BenAkiva1993}{choicedata}
#'
#' @keywords dataset

"train_choice"

#' Revealed Preferences for Travel Mode Choice
#'
#' @description
#' Data set of revealed choices by 210 travelers between Sydney and Melbourne
#' who report their choice between the four travel modes plane, train, bus, or
#' car. The data were collected as part of a 1987 intercity mode choice study.
#'
#' @format
#' A \code{tibble} with 840 rows and 8 columns:
#' \describe{
#'   \item{individual \[`integer`\]}{The identifier for the decider.}
#'   \item{mode \[`character`\]}{The travel mode.}
#'   \item{choice \[`integer`\]}{Whether the mode was chosen.}
#'   \item{wait \[`integer`\]}{The terminal waiting time, 0 for car.}
#'   \item{cost \[`integer`\]}{The travel cost in dollars.}
#'   \item{travel \[`integer`\]}{The travel time in minutes.}
#'   \item{income \[`integer`\]}{The household income in thousand dollars.}
#'   \item{size \[`integer`\]}{The traveling group size.}
#' }
#'
#' @references
#' \insertRef{BenAkiva1993}{choicedata}
#'
#' @keywords dataset

"travel_mode_choice"
