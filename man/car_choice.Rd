% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{car_choice}
\alias{car_choice}
\title{Stated Preferences for Car Choice}
\format{
A \code{data.frame} with 4654 rows and 70 columns:
\describe{
\item{choice [\code{factor(1)}]}{choosen alternative out of six options}
\item{college [\code{logical(1)}]}{individual has college education}
\item{hsg2 [\code{logical(1)}]}{household size greater than two}
\item{coml5 [\code{logical(1)}]}{commute less than five miles a day}
}
For each option \eqn{z \in \{1,\dots,6\}}:
\describe{
\item{type(z) [\code{factor(1)}]}{car type, either \code{regcar} (regular car),
\code{sportuv} (sport utility vehicle), \code{sportcar}, \code{stwagon} (station wagon),
\code{truck}, or \code{van}}
\item{price(z) [\code{numeric(1)}]}{price of vehicle divided by the logarithm
of income}
\item{range(z) [\code{numeric(1)}]}{hundreds of miles vehicle can travel
between refueling or recharging}
\item{acc(z) [\code{numeric(1)}]}{acceleration, tens of seconds required to
reach 30 mph from stop}
\item{speed(z) [\code{numeric(1)}]}{highest attainable speed in hundreds of
mph}
\item{pollution(z) [\code{numeric(1)}]}{tailpipe emissions as fraction of
those for new gas vehicle}
\item{size(z) [\code{numeric(1)}]}{size of the vehicle, either \code{0} for a mini,
\code{1} for a subcompact, \code{2} for a compact, or \code{3} for a mid-size or large
vehicle}
\item{space(z) [\code{numeric(1)}]}{fraction of luggage space in a comparable
new gas vehicle}
\item{cost(z) [\code{numeric(1)}]}{cost per travel mile in tens of cents
(home recharging for electric vehicles, station refueling otherwise)}
\item{station(z) [\code{numeric(1)}]}{fraction of stations that can refuel
or recharge the vehicle}
}
}
\usage{
car_choice
}
\description{
Data set of 4654 individuals from California deciding between six virtual
car options.
}
\references{
The data set is originally contained in the \code{{mlogit}} package.

\insertRef{mcfadden2000mixed}{choicedata}

\insertRef{brownstone1996transactions}{choicedata}

\insertRef{Croissant:2020}{choicedata}
}
\keyword{dataset}
