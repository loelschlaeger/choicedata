#' Stated Preferences for Car Choice
#'
#' @description
#' Data set of 4654 individuals from California deciding between six virtual
#' car options.
#'
#' @format
#' A \code{data.frame} with 4654 rows and 70 columns:
#' \describe{
#'   \item{choice \[`factor(1)`\]}{choosen alternative out of six options}
#'   \item{college \[`logical(1)`\]}{individual has college education}
#'   \item{hsg2 \[`logical(1)`\]}{household size greater than two}
#'   \item{coml5 \[`logical(1)`\]}{commute less than five miles a day}
#' }
#' For each option \eqn{z \in \{1,\dots,6\}}:
#' \describe{
#'   \item{type(z) \[`factor(1)`\]}{car type, either `regcar` (regular car),
#'   `sportuv` (sport utility vehicle), `sportcar`, `stwagon` (station wagon),
#'   `truck`, or `van`}
#'   \item{price(z) \[`numeric(1)`\]}{price of vehicle divided by the logarithm
#'   of income}
#'   \item{range(z) \[`numeric(1)`\]}{hundreds of miles vehicle can travel
#'   between refueling or recharging}
#'   \item{acc(z) \[`numeric(1)`\]}{acceleration, tens of seconds required to
#'   reach 30 mph from stop}
#'   \item{speed(z) \[`numeric(1)`\]}{highest attainable speed in hundreds of
#'   mph}
#'   \item{pollution(z) \[`numeric(1)`\]}{tailpipe emissions as fraction of
#'   those for new gas vehicle}
#'   \item{size(z) \[`numeric(1)`\]}{size of the vehicle, either `0` for a mini,
#'   `1` for a subcompact, `2` for a compact, or `3` for a mid-size or large
#'   vehicle}
#'   \item{space(z) \[`numeric(1)`\]}{fraction of luggage space in a comparable
#'   new gas vehicle}
#'   \item{cost(z) \[`numeric(1)`\]}{cost per travel mile in tens of cents
#'   (home recharging for electric vehicles, station refueling otherwise)}
#'   \item{station(z) \[`numeric(1)`\]}{fraction of stations that can refuel
#'   or recharge the vehicle}
#' }
#'
#' @references
#' The data set is originally contained in the `{mlogit}` package.
#'
#' \insertRef{mcfadden2000mixed}{choicedata}
#'
#' \insertRef{brownstone1996transactions}{choicedata}
#'
#' \insertRef{Croissant:2020}{choicedata}
#'
#' @keywords dataset

"car_choice"

#' Stated Preferences for Train Traveling
#'
#' @description
#' Data set of 2929 stated choices by 235 Dutch individuals deciding between
#' two virtual train trip options \code{"A"} and \code{"B"} based on the price,
#' the travel time, the number of rail-to-rail transfers (changes), and the
#' level of comfort.
#'
#' The data were obtained in 1987 by Hague Consulting Group for the National
#' Dutch Railways. Prices were recorded in Dutch guilder and in this data set
#' transformed to Euro at an exchange rate of 2.20371 guilders = 1 Euro.
#'
#' @format
#' A \code{data.frame} with 2929 rows and 11 columns:
#' \describe{
#'   \item{deciderID \[`integer(1)`\]}{identifier for the decider}
#'   \item{occasionID \[`integer(1)`\]}{identifier for the choice occasion}
#'   \item{choice \[`character(1)`\]}{chosen alternative (either \code{"A"} or \code{"B"})}
#'   \item{price_A \[`numeric(1)`\]}{price for alternative \code{"A"} in Euro}
#'   \item{time_A \[`numeric(1)`\]}{travel time for alternative \code{"A"} in hours}
#'   \item{change_A \[`integer(1)`\]}{number of changes for alternative \code{"A"}}
#'   \item{comfort_A \[`integer(1)`\]}{comfort level (in decreasing comfort order) for alternative \code{"A"}}
#'   \item{price_B \[`numeric(1)`\]}{price for alternative \code{"B"} in Euro}
#'   \item{time_B \[`numeric(1)`\]}{travel time for alternative \code{"B"} in hours}
#'   \item{change_B \[`integer(1)`\]}{number of changes for alternative \code{"B"}}
#'   \item{comfort_B \[`integer(1)`\]}{comfort level (in decreasing comfort order) for alternative \code{"B"}}
#' }
#'
#' @references
#' The data set is originally contained in the `{mlogit}` package.
#'
#' \insertRef{BenAkiva1993}{choicedata}
#'
#' \insertRef{Meijer2006}{choicedata}
#'
#' \insertRef{Croissant:2020}{choicedata}
#'
#' @keywords dataset

"train_choice"
