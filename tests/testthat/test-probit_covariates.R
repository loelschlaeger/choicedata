test_that("probit_covariates can be simulated", {
  formula <- choice ~ cost | age | time
  N <- 3
  T <- 1:N
  J <- 3
  alternatives <- letters[1:J]
  base <- alternatives[1]
  re <- NULL
  ordered <- FALSE
  seed <- 1
  sampler <- function(n, t) rnorm(n = 1, mean = 0, sd = 9)
  x <- sample_probit_covariates(
    formula = formula, N = N, J = J, T = T, alternatives = alternatives,
    base = base, re = re, ordered = ordered, seed = seed, sampler = sampler,
    age = function(n, t) sample(30:80, 1)
  )
  expect_s3_class(x, "probit_covariates")
  expect_true(is.probit_covariates(x))
  expect_true(is.list(x))
  expect_length(x, N)
  for (n in 1:N) {
    expect_length(x[[n]], T[n])
  }
})

test_that("probit_covariates example in details works", {
  x <- sample_probit_covariates(
    formula = choice ~ cost | age | time, N = 3, J = 3, T = 1:3,
    cost = function(n, t) {
      runif(J, 1:3, 2:4)
    },
    age = function(n, t) {
      set.seed(t)
      sample(30:80, 1)
    }
  )
  expect_s3_class(x, "probit_covariates")
})

test_that("covariate names can be generated", {
  expect_equal(
    covariate_names(
      probit_formula(formula = choice ~ cost | age | time),
      probit_alternatives(J = 2)
    ),
    c("cost_A", "cost_B", "age", "time_A", "time_B")
  )
})

test_that("T can be expanded", {
  expect_error(
    expand_T(),
    "Please specify the number 'N' of deciders."
  )
  expect_error(
    expand_T(N = 3.5),
    "Assertion on 'N' failed: Must be of type 'single integerish value', not 'double'."
  )
  expect_error(
    expand_T(N = 10, T = "one"),
    "Assertion on 'T' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    expand_T(N = 10, T = 1:9),
    "Assertion on 'T' failed: Must have length 10, but has length 9."
  )
  expect_error(
    expand_T(N = 10, T = 1.5),
    "Assertion on 'T' failed: Must be of type 'integerish', but element 1 is not close to an integer."
  )
  expect_equal(
    expand_T(N = 10, T = 1),
    c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
  )
  expect_equal(
    expand_T(N = 10, T = 1:10),
    1:10
  )
})
