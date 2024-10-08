% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/choice_alternatives.R
\name{choice_alternatives}
\alias{choice_alternatives}
\alias{print.choice_alternatives}
\title{Define choice alternatives}
\usage{
choice_alternatives(
  J = 2,
  alternatives = LETTERS[1:J],
  base = alternatives[1],
  ordered = FALSE
)

\method{print}{choice_alternatives}(x, ...)
}
\arguments{
\item{J}{[\code{integer(1)}]\cr
The number of choice alternatives.

Must be at least two.}

\item{alternatives}{[\code{character(J)}]\cr
Labels for the choice alternatives.}

\item{base}{[\code{character(1)}]\cr
The name of the base alternative for covariates that are not
alternative-specific, see details.

\code{base} must be contained in \code{alternatives}.

By default, \code{base} is the first element of \code{alternatives}.}

\item{ordered}{[\code{logical(1)}]\cr
Are the choice alternatives ordered?}

\item{x}{[\code{choice_alternatives}]\cr
The \code{choice_alternatives} object to be printed.}

\item{...}{Currently not used.}
}
\value{
An object of class \code{choice_alternatives}, which is a \code{character}
vector of the choice alternatives that has the following attributes:
\describe{
\item{\code{J}}{The number of choice alternatives.}
\item{\code{base}}{The name of the base alternative.}
\item{\code{ordered}}{Are the choice alternatives ordered?}
}
}
\description{
The \code{choice_alternatives} object defines the set of choice alternatives.
}
\section{Base alternative}{

The full set of coefficients for covariates that are constant across
alternatives (including alternative-specific constants) is not identified.
To achieve identifiability, the coefficient of alternative \code{base}
is fixed to zero. The other coefficients then have to be interpreted with
respect to \code{base}. The base alternative is marked with a \code{*} when
printing a \code{choice_alternatives} object.
}

\section{Ordered choice alternatives}{

When the set of choice alternatives is ordered, the choice model has only a
single utility equation
\deqn{U_{nt} = X_{nt}' \tilde{\beta}_n + \epsilon_{nt},}
per decider \eqn{n} and choice occasion \eqn{t}, where
\eqn{\epsilon_{nt} \sim \text{MVN}_{1} (0,\Sigma)} in the probit model
and logistic in the logit model.

This utility can be interpreted as the level of association that \eqn{n} has
with the choice question. It falls into discrete categories, which in turn
are linked to the ordered alternatives \eqn{j=1,\dots,J}. Formally,
\deqn{y_{nt} = \sum_{j = 1,\dots,J} j \cdot I(\gamma_{j-1} < U_{nt} \leq
\gamma_{j}),}
where \eqn{\gamma_0 = -\infty} and \eqn{\gamma_J = +\infty}. This implies
that alternative \eqn{j} is chosen, if the utility falls into the interval
\eqn{(\gamma_{j-1}, \gamma_j]}.
Monotonicity of the thresholds \eqn{(\gamma_j)_{j=1,\dots,J-1}} is ensured
by estimating logarithmic increments \eqn{d_j} with
\eqn{\gamma_j = \sum_{i\leq j} \exp{(d_i)}}, \eqn{j=1,\dots,J-1}.
For level normalization, we fix \eqn{\gamma_1 = 0}.
}

\examples{
choice_alternatives(
  J = 3,
  alternatives = c("gas", "electricity", "oil"),
  base = "gas",
  ordered = FALSE
)

}
