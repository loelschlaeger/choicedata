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
