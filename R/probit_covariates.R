#' Define choice covariates
#'
#' @description
#' These functions create and validate an object of class
#' \code{\link{probit_covariates}}, which contains the covariate matrices,
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
#' @details
#' # Covariate matrices
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
#' @param sampler
#' A \code{function} that returns a single \code{numeric}, a random number.
#' It must have two arguments, \code{n} and \code{t}, so that the call
#' \code{sampler(n, t)} returns the covariate value for decider \code{n} at
#' choice occasion \code{t}.
#' By default, \code{sampler = function(n, t) rnorm(n = 1, mean = 0, sd = 9)}.
#' @param ...
#' Optionally custom sampling \code{function}s for specific covariates.,
#' see the details for an example.
#' Each \code{function} must
#' 1. be named according to a covariate,
#' 2. have two arguments, \code{n} and \code{t} (see the documentation for
#'    argument \code{sampler}),
#' 3. return either a single \code{numeric}, if the covariate is not
#'    alternative-specific, or a \code{numeric} \code{vector} of length
#'    \code{J}, if the covariate is alternative-specific.
#' @export
#' @importFrom stats rnorm

sample_probit_covariates <- function(
    formula, N, J, T = 1, alternatives = LETTERS[1:J], base = alternatives[1],
    re = NULL, ordered = FALSE, seed = NULL,
    sampler = function(n, t) rnorm(n = 1, mean = 0, sd = 9), ...
) {

  ### input checks
  T <- expand_T(N = N, T = T)
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

  ### check sampler functions
  check_sampler <- function(FUN, name) {

    ### check if 'name' corresponds to 'sampler' or a covariate
    if (!name %in% c(na.omit(unique(effects$covariate)), "sampler")) {
      probit_stop(
        glue::glue("Bad input '{name}'."),
        "I suspect you want to define a custom sampler.",
        glue::glue("But there is no covariate '{name}'.")
      )
    }

    ### check if sampler is a function
    if (!is.function(FUN)) {
      probit_stop(
        glue::glue("Sampler for '{name}' is not a function.")
      )
    }

    ### check if sampler has arguments 'n' and 't'
    args <- names(formals(FUN))
    if (!(length(args) == 2 && all(args == c("n", "t")))) {
      probit_stop(
        glue::glue("The custom sampler for '{name}' is misspecified."),
        glue::glue("It should have the two arguments 'n' and 't'."),
        "Please see the documentation."
      )
    }

    ### check if sampler returns a `numeric`
    n_try <- sample.int(N, size = 1)
    t_try <- sample.int(T[n_try], size = 1)
    sampler_try <- try(FUN(n = n_try, t = t_try), silent = TRUE)
    if (!is.vector(sampler_try) || !is.numeric(sampler_try)) {
      probit_stop(
        glue::glue("I tried to call `{name}(n = {n_try}, t = {t_try})`."),
        "The return value was not the expected `numeric` `vector`.",
        "Please check."
      )
    }

    ### check length of sampler output
    expected_length <- ifelse(name %in% c(probit_formula$vars[[2]], "sampler"), 1, J)
    if (length(sampler_try) != expected_length) {
      probit_stop(
        glue::glue("I tried to call `{name}(n = {n_try}, t = {t_try})`."),
        glue::glue("The return value was not of length {expected_length}."),
        "Please check."
      )
    }

  }
  custom_sampler <- list(...)
  if (length(custom_sampler) != sum(names(custom_sampler) != "", na.rm=TRUE)) {
    probit_stop(
      "I found unnamed input(s).",
      "I suspect you want to define a custom sampler.",
      "Please make sure it is named according to a covariate."
    )
  }
  mapply(check_sampler, c(list(sampler), custom_sampler),
         c("sampler", names(custom_sampler)))

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
  if (!is.list(x)) {
    probit_stop(
      "Input 'x' is not a `list`."
    )
  }

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
#'
#' @examples
#' \dontrun{
#' expand_T(N = 10, T = 2)
#' expand_T(N = 10, T = 1:10)
#' }
#'
#' @keywords internal

expand_T <- function(N, T = 1) {
  if (missing(N)) {
    probit_stop(
      "Please specify the input 'N'.",
      "It should be a positive `integer`, the number of deciders."
    )
  }
  if (!is_positive_integer(N)) {
    probit_stop(
      "Input 'N' is misspecified.",
      "It should be a positive `integer`, the number of deciders."
    )
  }
  if (!(is.numeric(T) && is.vector(T))) {
    probit_stop(
      "Input 'T' is misspecified.",
      "It should be a `numeric` (`vector`)."
    )
  }
  if (length(T) == 1) {
    T <- rep(T, N)
  }
  if (length(T) != N) {
    probit_stop(
      "Input 'T' is misspecified.",
      glue::glue("It should be a `vector` of length 'N = {N}'.")
    )
  }
  if (!all(sapply(T, is_positive_integer))) {
    probit_stop(
      "Input 'T' is misspecified.",
      "It should be a `vector` of `integer` only."
    )
  }
  as.integer(T)
}


as.list.probit_covariates <- function() {

}

as.data.frame.probit_covariates <- function() {

}




