test_that("covariates can be sampled", {
  formula <- choice ~ cost | age | time
  N <- 3
  Tp <- 1:N
  J <- 3
  alternatives <- letters[1:J]
  re <- NULL
  seed <- 1
  covariate_levels <- Inf
  occasion_constant <- character()
  covariate_mean <- 0
  covariate_sd <- 1
  covariate_correlation <- 0
  delimiter <- "_"








  expect_s3_class(x, "probit_covariates")
  expect_true(is.probit_covariates(x))
  expect_true(is.list(x))
  expect_length(x, N)
  for (n in 1:N) {
    expect_length(x[[n]], Tp[n])
  }
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

test_that("covariate number can be computed", {
  expect_equal(
    covariate_number(
      probit_formula(choice ~ A | B | C),
      probit_alternatives(J = 3)
    ),
    7
  )
})

test_that("Tp can be expanded", {
  expect_error(
    expand_Tp(),
    "Please specify the number 'N' of deciders."
  )
  expect_error(
    expand_Tp(N = 3.5),
    "Assertion on 'N' failed: Must be of type 'single integerish value', not 'double'."
  )
  expect_error(
    expand_Tp(N = 10, Tp = "one"),
    "Assertion on 'Tp' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    expand_Tp(N = 10, Tp = 1:9),
    "Assertion on 'Tp' failed: Must have length 10, but has length 9."
  )
  expect_error(
    expand_Tp(N = 10, Tp = 1.5),
    "Assertion on 'Tp' failed: Must be of type 'integerish', but element 1 is not close to an integer."
  )
  expect_equal(
    expand_Tp(N = 10, Tp = 1),
    c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
  )
  expect_equal(
    expand_Tp(N = 10, Tp = 1:10),
    1:10
  )
})
