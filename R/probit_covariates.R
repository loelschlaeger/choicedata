#' Define choice covariates
#'
#' @description
#' These functions create and validate an object of class
#' \code{\link{probit_covariates}}, which contains the model covariate matrices,
#' see the details.
#'
#' \code{\link{sample_probit_covariates}} samples covariates.
#'
#' @param probit_data
#' An \code{\link{probit_data}} object.
#' @param probit_formula
#' An \code{\link{probit_formula}} object.
#' @param probit_alternatives
#' An \code{\link{probit_alternatives}} object.
#'
#' @section Covariate matrices:
#' A covariate matrix contains the choice covariates of a decider at some choice
#' occasion. It is of dimension \code{J} x \code{P}, where \code{J} is the
#' number of alternatives and \code{P} the number of effects.
#'
#' By default, covariates are sampled according to the \code{function} specified
#' as the \code{sampler} argument. However, it is possible to define custom
#' sampling \code{function}s for each covariate.
#'
#' For example, consider the model formula
#' \code{formula <- choice ~ cost | age | time}, which includes two
#' alternative-specific covariates, \code{cost} and \code{time}, and one
#' alternative-constant covariate, \code{age}, see
#' \code{\link{probit_formula}}. For \code{cost} and \code{time}, we could
#' specify a \code{function} that returns a \code{numeric} \code{vector} of
#' length \code{J}, where entry \code{j} corresponds to the covariate for
#' alternative \code{alternatives[j]}. For \code{age}, we could specify a
#' \code{function} that returns a single \code{numeric}, the decider's age.
#' Each of these \code{function}s must have two arguments, \code{n} and
#' \code{t}, so that the \code{function} call returns the covariates for
#' decider \code{n} at choice occastion \code{t}.
#'
#' An example call to \code{sample_probit_covariates()} looks as follows:
#' \preformatted{
#' sample_probit_covariates(
#'   formula = choice ~ cost | age | time, N = 3, J = 3, T = 1:3,
#'   cost = function(n, t) {
#'     runif(J, 1:3, 2:4)
#'   },
#'   age = function(n, t) {
#'     set.seed(t)
#'     sample(30:80, 1)
#'   }
#' )
#' }
#' Note that
#' 1. the cost covariate is drawn from distinct uniform distributions,
#' 2. we use \code{set.seed(t)} in the sampler for \code{age} (so that the value
#'    does not vary over choice occasions),
#' 3. and that \code{time} will be sampled according to the \code{function}
#'    specified as the \code{sampler} argument.
#'
#' @return
#' An \code{\link{probit_covariates}} object. It is a \code{list} of
#' \code{list} of \code{matrix} elements.
#'
#' More precise: Let the return value be \code{out}, then
#' - \code{out} contains the covariate matrices of all deciders at all choice
#'   occasions,
#' - \code{out[[n]]} is a \code{list} of the covariate matrices of decider
#'   \code{n} at all of her choice occasions,
#' - \code{out[[n]][[t]]} is the covariate \code{matrix} of decider \code{n} at
#'   her \code{t}-th choice occasion.
#'
#' @keywords object

probit_covariates <- function(
    probit_data, probit_formula, probit_alternatives
) {

  ### transform 'probit_data' to 'probit_covariates'
  # TODO

  ### validate 'probit_covariates'
  # TODO
  # validate_probit_covariates()
}

#' @rdname probit_covariates
#' @param x
#' An \code{\link{probit_covariates}} object.

is.probit_covariates <- function(x) {
  inherits(x, "probit_covariates")
}

#' @rdname probit_covariates
#' @inheritParams probit_formula
#' @inheritParams expand_T
#' @inheritParams probit_alternatives
#' @inheritParams probit_data
#' @export

sample_probit_covariates <- function(
  formula, N, J, T = 1, alternatives = LETTERS[1:J], base = alternatives[1],
  re = NULL, ordered = FALSE, seed = NULL
) {

  ### input checks
  T <- expand_T(N = N, T = T)
  probit_formula <- probit_formula(
    formula = formula, re = re, ordered = ordered
  )
  probit_alternatives <- probit_alternatives(
    J = J, labels = alternatives, base = base, ordered = ordered
  )
  effects <- overview_effects(
    probit_formula = probit_formula,
    probit_alternatives = probit_alternatives
  )

  ### draw covariate values
  if (!is.null(seed)) {
    set.seed(seed)
  }
  x <- lapply(1:N, function(n) {
    lapply(1:T[n], function(t) {

      ### build general matrix
      X_nt <- matrix(
        data = replicate(J * nrow(effects), sampler(n, t)),
        nrow = J,
        ncol = nrow(effects)
      )
      rownames(X_nt) <- probit_alternatives$alternatives
      colnames(X_nt) <- effects$name

      ### check for custom covariates
      for (e in 1:nrow(effects)) {
        if (effects[e, "covariate"] %in% names(custom_sampler)) {
          X_nt[, e] <- custom_sampler[[effects[e, "covariate"]]](n, t)
        }
      }

      ### check for alternative-constant covariates
      for (e in 1:nrow(effects)) {
        if (!effects[e, "as_covariate"]) {
          k <- which(effects[, "covariate"] == effects[e, "covariate"])[1]
          X_nt[, e] <- X_nt[1, k]
        }
      }

      ### check for alternative-specific effects
      for (e in 1:nrow(effects)) {
        j <- which(probit_alternatives$alternatives == effects[e, "alternative"])
        if (effects[e, "as_effect"]) {
          X_nt[-j, e] <- 0
        }
        if (startsWith(effects[e, "name"], "ASC_")) {
          X_nt[j, e] <- 1
        }
      }

      ### return covariate matrix for decider n at choice occasion t
      return(X_nt)
    })
  })

  ### validate 'probit_covariates'
  validate_probit_covariates(
    x = x, formula = formula, N = N, J = J, T = T, alternatives = alternatives,
    base = base, re = re, ordered = ordered
  )
}

#' @rdname probit_covariates

validate_probit_covariates <- function(
    x = list(), formula, N, J, T = 1, alternatives = LETTERS[1:J],
    base = alternatives[1], re = NULL, ordered = FALSE
) {

  ### input checks
  checkmate::assert_list(x)

  ### construct objects
  probit_formula <- probit_formula(
    formula = formula, re = re, ordered = ordered
  )
  probit_alternatives <- probit_alternatives(
    J = J, alternatives = alternatives, base = base, ordered = ordered
  )
  effects <- overview_effects(
    probit_formula = probit_formula,
    probit_alternatives = probit_alternatives
  )

  ### validate 'probit_covariates'
  # TODO

  ### return validated 'probit_covariates' object
  structure(x, class = c("probit_covariates", "list"))
}

#' Expand \code{T}
#'
#' @description
#' This function expands the number of choice occasions \code{T} to a
#' \code{vector} of length \code{N}.
#'
#' @param N
#' A positive \code{integer}, the number of deciders.
#' @param T
#' A positive \code{integer}, the number of choice occasions per decider.
#' Can also be a \code{vector} of length \code{N} for a variable number of
#' choice occasions per decider.
#' By default, \code{T = 1}.
#'
#' @return
#' An \code{integer} \code{vector} of length \code{N}.

expand_T <- function(N, T = 1) {
  if (missing(N)) {
    stop("Please specify the number 'N' of deciders.")
  }
  checkmate::assert_int(N, lower = 1)
  checkmate::assert_numeric(T)
  if (length(T) == 1) {
    T <- rep(T, N)
  }
  checkmate::assert_integerish(T, lower = 1, len = N, any.missing = FALSE)
  as.integer(T)
}




set.seed(5)

N <- 100
P <- 3
mu <- numeric(P)
sd <- rep(1, P)
cor <- diag(P)
levels <- rep(Inf, P)


cov <- sweep(sweep(cor, 1, diag(sd), "*"), 2, diag(sd), "*")

df <- as.data.frame(mvrnorm(n = N, mu = mu, Sigma = cov))

brks <- quantile(df$V3, seq(0, 1, length.out = 10))
ints <- findInterval(df$V3, brks, all.inside = T)
df$V3 <- (brks[ints] + brks[ints + 1]) / 2


head(df)

cor(df)



