#' Stated Preferences for Train Traveling
#'
#' @description
#' Data set of 2929 stated choices by 235 Dutch individuals deciding between
#' two hypothetical train trip options \code{"A"} and \code{"B"} based on the
#' price, the travel time, the number of rail-to-rail transfers (changes), and
#' the level of comfort.
#'
#' The data were obtained in 1987 by Hague Consulting Group for the National
#' Dutch Railways. Prices were recorded in cents of Dutch guilders and were
#' converted to Euro at an exchange rate of 2.20371 guilders = 1 Euro.
#'
#' @format
#' A \code{tibble} with 2929 rows and 11 columns:
#' \describe{
#'   \item{deciderID \[`integer`\]}{The identifier for the decider.}
#'   \item{occasionID \[`integer`\]}{The choice occasion within a decider.}
#'   \item{choice \[`character`\]}{The chosen alternative, either \code{"A"} or
#'   \code{"B"}.}
#'   \item{price_A \[`numeric`\]}{The price for alternative \code{"A"} in
#'   Euro.}
#'   \item{time_A \[`numeric`\]}{The travel time for alternative \code{"A"} in
#'   hours.}
#'   \item{change_A \[`integer`\]}{The number of changes for alternative
#'   \code{"A"}.}
#'   \item{comfort_A \[`factor`\]}{The comfort level for alternative
#'   \code{"A"}, where `0` is the best comfort and `2` the worst.}
#'   \item{price_B \[`numeric`\]}{The price for alternative \code{"B"} in
#'   Euro.}
#'   \item{time_B \[`numeric`\]}{The travel time for alternative \code{"B"} in
#'   hours.}
#'   \item{change_B \[`integer`\]}{The number of changes for alternative
#'   \code{"B"}.}
#'   \item{comfort_B \[`factor`\]}{The comfort level for alternative
#'   \code{"B"}, where `0` is the best comfort and `2` the worst.}
#' }
#'
#' @source
#' Adapted from \code{Train} in the
#' \href{https://CRAN.R-project.org/package=mlogit}{\pkg{mlogit} package}.
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
#' Monetary values were converted to Euro at an exchange rate of 1.6196
#' Australian dollars = 1 Euro.
#'
#' This data can be used to model the choice between plane, train, bus, and car
#' based on waiting time, travel cost, travel time, household income, and
#' traveling group size.
#'
#' @format
#' A \code{tibble} with 840 rows and 8 columns:
#' \describe{
#'   \item{individual \[`integer`\]}{The identifier for the decider.}
#'   \item{mode \[`character`\]}{The travel mode.}
#'   \item{choice \[`integer`\]}{Whether the mode was chosen.}
#'   \item{wait \[`integer`\]}{The terminal waiting time in minutes, 0 for
#'   car.}
#'   \item{cost \[`numeric`\]}{The in-vehicle cost for all trip stages, in
#'   euros.}
#'   \item{travel \[`integer`\]}{The travel time in minutes.}
#'   \item{income \[`numeric`\]}{The household income in thousands of euros.}
#'   \item{size \[`integer`\]}{The traveling group size.}
#' }
#'
#' @source
#' Adapted from \code{TravelMode} in the
#' \href{https://CRAN.R-project.org/package=AER}{\pkg{AER} package}.
#'
#' @references
#' \insertRef{Greene2003}{choicedata}
#'
#' @keywords dataset

"travel_mode_choice"

#' Stated Preferences for Electricity Suppliers
#'
#' @description
#' Stated choices by 361 US households among four hypothetical electricity
#' suppliers. Each household completed 8 to 12 choice tasks.
#'
#' This data can be used to model the choice between electricity suppliers based
#' on price, contract length, whether the supplier is local or well-known, and
#' whether time-of-day or seasonal rates are offered.
#'
#' @format
#' A \code{tibble} with 4308 rows and 27 columns:
#' \describe{
#'   \item{household \[`integer`\]}{The household identifier.}
#'   \item{occasion \[`integer`\]}{The choice occasion within a household.}
#'   \item{choice \[`character`\]}{The chosen supplier, from \code{"1"} to
#'   \code{"4"}.}
#'   \item{price_1--price_4 \[`numeric`\]}{The fixed electricity price in
#'   US-dollar cents per kWh for each supplier.}
#'   \item{contract_1--contract_4 \[`numeric`\]}{The contract length in years
#'   for each supplier.}
#'   \item{local_1--local_4 \[`logical`\]}{Whether each supplier is local.}
#'   \item{known_1--known_4 \[`logical`\]}{Whether each supplier is
#'   well-known.}
#'   \item{timeOfDay_1--timeOfDay_4 \[`logical`\]}{Whether each supplier
#'   offers a time-of-day rate.}
#'   \item{seasonal_1--seasonal_4 \[`logical`\]}{Whether each supplier offers
#'   a seasonal rate.}
#' }
#'
#' @source
#' Adapted from \code{Electricity} in the
#' \href{https://CRAN.R-project.org/package=mlogit}{\pkg{mlogit} package}.
#' The study is described by Huber and Train (2001).
#'
#' @references
#' \insertRef{HuberTrain2001}{choicedata}
#'
#' @keywords dataset

"electricity_choice"

#' Recreational Fishing Mode Choice
#'
#' @description
#' Choices by 1182 US respondents among fishing options.
#'
#' This data can be used to model the choice between beach, pier, private-boat,
#' and charter-boat fishing based on trip cost, expected catch rate, and
#' monthly income.
#'
#' @format
#' A \code{tibble} with 1182 rows and 11 columns:
#' \describe{
#'   \item{respondent \[`integer`\]}{The respondent identifier.}
#'   \item{choice \[`character`\]}{The chosen fishing mode.}
#'   \item{price_beach, price_pier, price_boat, price_charter
#'   \[`numeric`\]}{The cost of a fishing trip for each mode, in US dollars.}
#'   \item{catch_beach, catch_pier, catch_boat, catch_charter
#'   \[`numeric`\]}{The expected catch rate for each fishing mode.}
#'   \item{income \[`numeric`\]}{The respondent's monthly income in US dollars.}
#' }
#'
#' @source
#' Adapted from \code{Fishing} in the
#' \href{https://CRAN.R-project.org/package=mlogit}{\pkg{mlogit} package}.
#' The original study is described by Herriges and Kling (1999).
#'
#' @references
#' \insertRef{HerrigesKling1999}{choicedata}
#'
#' @keywords dataset

"fishing_choice"

#' Travel Mode Choice in Canada
#'
#' @description
#' Travel mode choices for trips in the Montreal--Toronto corridor. Choice sets
#' vary from two to four available modes among air, bus, car, and train.
#' Monetary values were converted to Euro at an exchange rate
#' of 1.6151 Canadian dollars = 1 Euro.
#'
#' This data can be used to model the choice between the available air, bus,
#' car, and train modes based on distance, cost, in-vehicle time, out-of-vehicle
#' time, service frequency, household income, and the number of urban trip
#' endpoints.
#'
#' @format
#' A \code{tibble} with 15520 rows and 10 columns:
#' \describe{
#'   \item{case \[`integer`\]}{The choice occasion identifier.}
#'   \item{mode \[`character`\]}{The available travel mode.}
#'   \item{choice \[`integer`\]}{Whether the mode was chosen.}
#'   \item{distance \[`numeric`\]}{The trip distance in kilometers.}
#'   \item{cost \[`numeric`\]}{The monetary cost of the mode in euros.}
#'   \item{inVehicleTime \[`numeric`\]}{The time spent in the vehicle, in
#'   minutes.}
#'   \item{outVehicleTime \[`numeric`\]}{The time spent outside the vehicle,
#'   in minutes.}
#'   \item{frequency \[`numeric`\]}{The number of scheduled services; car has
#'   value 0.}
#'   \item{income \[`numeric`\]}{The household-income value in thousands of
#'   euros.}
#'   \item{urban \[`numeric`\]}{A trip-level count of how many of the trip's
#'   two endpoints (the origin and the destination) were classified as large
#'   cities in the original study: \code{0} means that
#'   neither endpoint is a large city, \code{1} means that exactly one endpoint
#'   is a large city, and \code{2} means that both endpoints are large cities.}
#' }
#'
#' @source
#' Adapted from \code{ModeCanada} in the
#' \href{https://CRAN.R-project.org/package=mlogit}{\pkg{mlogit} package}.
#' The source data were provided by Frank Koppelman and used by Bhat (1995).
#'
#' @references
#' \insertRef{Bhat1995}{choicedata}
#'
#' @keywords dataset

"mode_canada_choice"

#' Rankings of Gaming Platforms
#'
#' @description
#' Complete rankings of six gaming platforms by 91 Dutch
#' respondents. Rank 1 is most preferred and rank 6 is least preferred.
#'
#' This data can be used to model the choice between gaming platforms based on
#' ownership, the respondent's age, and weekly gaming hours.
#'
#' @format
#' A \code{tibble} with 91 rows and 15 columns:
#' \describe{
#'   \item{respondent \[`integer`\]}{The respondent identifier.}
#'   \item{rank_Xbox, rank_PlayStation, rank_PSPortable, rank_GameCube,
#'   rank_GameBoy, rank_PC \[`integer`\]}{The rank assigned to each platform.}
#'   \item{owned_Xbox, owned_PlayStation, owned_PSPortable, owned_GameCube,
#'   owned_GameBoy, owned_PC \[`logical`\]}{Whether the respondent owns each
#'   platform.}
#'   \item{age \[`integer`\]}{The respondent's age in years.}
#'   \item{hours \[`numeric`\]}{The hours spent gaming per week.}
#' }
#'
#' @source
#' Adapted from \code{Game} in the
#' \href{https://CRAN.R-project.org/package=mlogit}{\pkg{mlogit} package}.
#' The original data are from the data archive for Fok et al. (2012).
#'
#' @references
#' \insertRef{Fok2012}{choicedata}
#'
#' @keywords dataset

"gaming_rankings"

#' Ordered Smoking Responses of Students
#'
#' @description
#' Ordered, cross-sectional smoking responses from 237 Statistics I students
#' at the University of Adelaide. The response levels are never, occasional,
#' regular, and heavy. One student did not report a smoking level, which is
#' stored as a missing response.
#'
#' This data can be used to model the choice of smoking based on age and
#' exercise frequency.
#'
#' @format
#' A \code{tibble} with 237 rows and 4 columns:
#' \describe{
#'   \item{student \[`integer`\]}{The student identifier.}
#'   \item{choice \[`ordered`\]}{The smoking level.}
#'   \item{age \[`numeric`\]}{The student's age in years.}
#'   \item{exercise \[`factor`\]}{Whether the student exercises frequently,
#'   sometimes, or not at all.}
#' }
#'
#' @source
#' Adapted from \code{survey} in the
#' \href{https://CRAN.R-project.org/package=MASS}{\pkg{MASS} package}.
#' The original student survey is documented by Venables and Ripley (2002).
#'
#' @references
#' \insertRef{VenablesRipley2002}{choicedata}
#'
#' @keywords dataset

"student_smoking_choice"

#' Dairy-Farm Water Conservation Choices
#'
#' @description
#' Stated panel choices by 98 dairy farmers in Mejia, Ecuador. Each farmer
#' completed four choice tasks with two conservation plans and a status quo.
#' The data have 1176 rows in long format.
#'
#' This data can be used to model the choice between two water-conservation
#' plans and the status quo based on irrigation, manure and waste management,
#' training, cost-share payment, farm size, milk production, and cattle
#' density.
#'
#' @format
#' A \code{tibble} with 1176 rows and 12 columns:
#' \describe{
#'   \item{farmer \[`integer`\]}{The farmer identifier.}
#'   \item{occasion \[`integer`\]}{The choice occasion from 1 to 4.}
#'   \item{alternative \[`character`\]}{Plan 1, plan 2, or the status quo.}
#'   \item{choice \[`integer`\]}{Whether the alternative was chosen.}
#'   \item{irrigation \[`factor`\]}{No new system, micro-sprinklers, or solid
#'   rain.}
#'   \item{manure \[`factor`\]}{No new practice, composting, or dispersion.}
#'   \item{waste \[`factor`\]}{No new practice, a collection center, or a
#'   municipal container.}
#'   \item{training \[`logical`\]}{Whether training for water conflict
#'   resolution and cooperation is offered.}
#'   \item{payment \[`numeric`\]}{The cost-share payment in US dollars per
#'   hectare.}
#'   \item{farm_size \[`numeric`\]}{The farm size in hectares.}
#'   \item{milk_production \[`numeric`\]}{The daily milk production in
#'   liters.}
#'   \item{cattle_density \[`numeric`\]}{The number of cattle per hectare.}
#' }
#'
#' @source
#' Adapted from the Mendeley Data source \doi{10.17632/ncj6ws6hbj.1}. The source
#' data are licensed under
#' \href{https://creativecommons.org/licenses/by/4.0/}{CC BY 4.0}.
#'
#' @references
#' \insertRef{Ortiz2023}{choicedata}
#'
#' @keywords dataset

"water_conservation_choice"

#' Choices for a Proposed Wind-Power Project
#'
#' @description
#' Stated panel choices by 308 residents for a proposed wind-power project
#' near Setskog, Norway. Each respondent completed six choice tasks. The
#' alternatives are the status quo and two project plans. Compensation values
#' were converted to Euro at an exchange rate of 10.8770 Norwegian kroner = 1
#' Euro.
#'
#' This data can be used to model the choice between two wind-power project
#' plans and the status quo based on the number and height of turbines,
#' power-line placement, compensation, and collective psychological ownership.
#'
#' @format
#' A \code{tibble} with 1848 rows and 16 columns:
#' \describe{
#'   \item{respondent \[`integer`\]}{The respondent identifier.}
#'   \item{occasion \[`integer`\]}{The choice occasion from 1 to 6.}
#'   \item{choice \[`character`\]}{The chosen alternative from 1 to 3.}
#'   \item{turbines_1--turbines_3 \[`integer`\]}{The number of turbines for
#'   each alternative.}
#'   \item{height_1--height_3 \[`integer`\]}{The turbine height in meters for
#'   each alternative.}
#'   \item{powerline_1--powerline_3 \[`factor`\]}{The power-line route and
#'   placement: none, overhead or underground throughout, or mixed between
#'   forests and residential areas.}
#'   \item{compensation_1--compensation_3 \[`numeric`\]}{The annual reduction
#'   in municipal taxes offered as compensation, in euros.}
#'   \item{psychological_ownership \[`numeric`\]}{A respondent-specific,
#'   model-estimated latent score for *collective psychological ownership* of
#'   the natural area affected by the proposed wind farm. It was constructed
#'   from three seven-point Likert items asking whether the area is "ours" and
#'   belongs collectively to residents. The construct was normalized to mean
#'   zero and standard deviation one in the study: positive values indicate
#'   stronger and negative values weaker feelings of shared ownership relative
#'   to the sample average.}
#' }
#'
#' @source
#' Adapted from the Mendeley Data source \doi{10.17632/3pdx4p3s9g.1}. The source
#' data are licensed under
#' \href{https://creativecommons.org/licenses/by/4.0/}{CC BY 4.0}.
#'
#' @references
#' \insertRef{Dugstad2023}{choicedata}
#'
#' \insertRef{Dugstad2024}{choicedata}
#'
#' @keywords dataset

"wind_power_choice"

#' Berserk Choices in a Lichess Arena Tournament
#'
#' @description
#' Revealed binary Berserk choices made by 5,852 players in the Lichess Yearly
#' Rapid Arena held on April 16, 2026. The chess tournament used a 10-minute
#' clock and lasted eight hours. Going Berserk at the beginning of a game
#' reduced a player's clock to five minutes. In return, a win earned one extra
#' tournament point. A player is considered to have been on a streak when they
#' entered the game after at least two consecutive wins. While on a streak, a
#' win was worth four instead of two points and a draw two instead of one. A
#' Berserk win on a streak was therefore worth five points. A draw or loss ends
#' the streak.
#'
#' This data can be used to model the risky choice to go Berserk based on
#' playing color, player rating, rating difference to the opponent, remaining
#' tournament time, and risk to lose their winning streak.
#'
#' @format
#' A \code{tibble} with 37,416 rows and 8 columns:
#' \describe{
#'   \item{deciderID \[`character`\]}{The player's Lichess username.}
#'   \item{occasionID \[`integer`\]}{The player's games in chronological order,
#'   numbered from 1.}
#'   \item{berserk \[`logical`\]}{Whether the player chose to go Berserk.}
#'   \item{white \[`logical`\]}{Whether the player had the white pieces.}
#'   \item{rating \[`integer`\]}{The player's Rapid rating at the beginning of
#'   the game.}
#'   \item{ratingDifference \[`integer`\]}{The player's rating minus the
#'   opponent's rating.}
#'   \item{minutesRemaining \[`numeric`\]}{The scheduled tournament time
#'   remaining, in minutes, when the game was created.}
#'   \item{streak \[`logical`\]}{Whether the player was on a double-point win
#'   streak at the beginning of the game.}
#' }
#'
#' @source
#' Derived from the public game export, results, and tournament metadata for the
#' \href{https://lichess.org/tournament/0Guo5doZ}{Lichess Yearly Rapid Arena}
#' using the
#' \href{https://lichess.org/api#tag/Arena-tournaments}{Lichess Arena API}.
#' The streak and Berserk rules are described in the
#' \href{https://lichess.org/tournament/help?system=arena}{Arena tournament FAQ}.
#' Lichess database exports are released under the
#' \href{https://creativecommons.org/publicdomain/zero/1.0/}{CC0 license}.
#'
#' @keywords dataset

"lichess_berserk_choice"
