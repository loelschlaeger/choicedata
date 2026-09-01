# Berserk Choices in a Lichess Arena Tournament

Revealed binary Berserk choices made by 5,852 players in the Lichess
Yearly Rapid Arena held on April 16, 2026. The chess tournament used a
10-minute clock and lasted eight hours. Going Berserk at the beginning
of a game reduced a player's clock to five minutes. In return, a win
earned one extra tournament point. A player is considered to have been
on a streak when they entered the game after at least two consecutive
wins. While on a streak, a win was worth four instead of two points and
a draw two instead of one. A Berserk win on a streak was therefore worth
five points. A draw or loss ends the streak.

This data can be used to model the risky choice to go Berserk based on
playing color, player rating, rating difference to the opponent,
remaining tournament time, and risk to lose their winning streak.

## Usage

``` r
lichess_berserk_choice
```

## Format

A `tibble` with 37,416 rows and 8 columns:

- deciderID \[`character`\]:

  The player's Lichess username.

- occasionID \[`integer`\]:

  The player's games in chronological order, numbered from 1.

- berserk \[`logical`\]:

  Whether the player chose to go Berserk.

- white \[`logical`\]:

  Whether the player had the white pieces.

- rating \[`integer`\]:

  The player's Rapid rating at the beginning of the game.

- ratingDifference \[`integer`\]:

  The player's rating minus the opponent's rating.

- minutesRemaining \[`numeric`\]:

  The scheduled tournament time remaining, in minutes, when the game was
  created.

- streak \[`logical`\]:

  Whether the player was on a double-point win streak at the beginning
  of the game.

## Source

Derived from the public game export, results, and tournament metadata
for the [Lichess Yearly Rapid
Arena](https://lichess.org/tournament/0Guo5doZ) using the [Lichess Arena
API](https://lichess.org/api#tag/Arena-tournaments). The streak and
Berserk rules are described in the [Arena tournament
FAQ](https://lichess.org/tournament/help?system=arena). Lichess database
exports are released under the [CC0
license](https://creativecommons.org/publicdomain/zero/1.0/).
