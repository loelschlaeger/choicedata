test_that("covariates can be sampled", {
  N <- 10
  Tp <- sample(1:N, size = N, replace = TRUE)
  x <- sample_probit_covariates(
    probit_formula = probit_formula(choice ~ cost | age | time),
    N = N,
    Tp = Tp,
    probit_alternatives = probit_alternatives(J = 3)
  )
  expect_s3_class(x, "probit_covariates")
  expect_true(is.probit_covariates(x))
  expect_true(is.list(x))
  expect_length(x, N)
  for (n in 1:N) {
    expect_length(x[[n]], Tp[n])
  }
})

test_that("customization for covariates works", {
  N <- 10
  Tp <- sample(1:N, size = N, replace = TRUE)
  x <- sample_probit_covariates(
    probit_formula = probit_formula(choice ~ cost | age | time),
    N = N,
    Tp = Tp,
    probit_alternatives = probit_alternatives(J = 3, alternatives = c("la", "le", "lu")),
    occasion_constant = c("cost"),
    covariate_mean = c("cost_la" = 100, "time" = 12, "time_la" = -1),
    covariate_sd = c("time" = 2),
    covariate_levels = c("age" = 10),
    seed = 1
  )
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
