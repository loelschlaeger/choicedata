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
  expect_true(is.data.frame(x))
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
  expect_true(is.data.frame(x))
})

test_that("check for covariate_levels works", {
  expect_equal(
    check_covariate_levels(
      covariate_levels = 2,
      probit_formula = probit_formula(formula = choice ~ A),
      probit_alternatives = probit_alternatives(J = 2),
      delimiter = "_"
    ),
    c(A_A = 2, A_B = 2)
  )
  expect_equal(
    check_covariate_levels(
      covariate_levels = c("A_A" = 3),
      probit_formula = probit_formula(formula = choice ~ A),
      probit_alternatives = probit_alternatives(J = 2),
      delimiter = "_"
    ),
    c(A_A = 3, A_B = Inf)
  )
})

test_that("check for occasion_constant works", {
  expect_equal(
    check_occasion_constant(
      occasion_constant = c("A", "B"),
      probit_formula = probit_formula(formula = choice ~ A | B),
      probit_alternatives = probit_alternatives(J = 2),
      delimiter = "_"
    ),
    c("B", "A_A", "A_B")
  )
})

test_that("check for covariate_mean works", {
  expect_equal(
    check_covariate_mean(
      covariate_mean = 2,
      probit_formula = probit_formula(formula = choice ~ A),
      probit_alternatives = probit_alternatives(J = 2),
      delimiter = "_"
    ),
    c(A_A = 2, A_B = 2)
  )
  expect_equal(
    check_covariate_mean(
      covariate_mean = c("A_A" = 3),
      probit_formula = probit_formula(formula = choice ~ A),
      probit_alternatives = probit_alternatives(J = 2),
      delimiter = "_"
    ),
    c(A_A = 3, A_B = 0)
  )
})

test_that("check for covariate_sd works", {
  expect_equal(
    check_covariate_sd(
      covariate_sd = 2,
      probit_formula = probit_formula(formula = choice ~ A),
      probit_alternatives = probit_alternatives(J = 2),
      delimiter = "_"
    ),
    c(A_A = 2, A_B = 2)
  )
  expect_equal(
    check_covariate_sd(
      covariate_sd = c("A_A" = 3),
      probit_formula = probit_formula(formula = choice ~ A),
      probit_alternatives = probit_alternatives(J = 2),
      delimiter = "_"
    ),
    c(A_A = 3, A_B = 1)
  )
})

test_that("check for covariate_correlation works", {
  expect_equal(
    check_covariate_correlation(
      covariate_correlation = diag(2),
      probit_formula = probit_formula(formula = choice ~ A),
      probit_alternatives = probit_alternatives(J = 2),
      delimiter = "_"
    ),
    diag(2)
  )
  expect_equal(
    check_covariate_correlation(
      covariate_correlation = 1,
      probit_formula = probit_formula(formula = choice ~ A),
      probit_alternatives = probit_alternatives(J = 2),
      delimiter = "_"
    ),
    matrix(1, 2, 2)
  )
})

test_that("covariate specification sugar works", {
  expect_equal(
    covariate_spec_sugar(
      covariate_spec = "A",
      probit_formula = probit_formula(formula = choice ~ A),
      probit_alternatives = probit_alternatives(J = 2),
      delimiter = "_",
      named_vector = FALSE
    ),
    c("A_A", "A_B")
  )
  expect_equal(
    covariate_spec_sugar(
      covariate_spec = c("A" = 3),
      probit_formula = probit_formula(formula = choice ~ A),
      probit_alternatives = probit_alternatives(J = 2),
      delimiter = "_",
      named_vector = TRUE
    ),
    c(A_A = 3, A_B = 3)
  )
})

test_that("effect can be checked whether it is an ASC", {
  expect_true(effect_is_ASC("ASC_test", delimiter = "_"))
  expect_false(effect_is_ASC("ASCtest", delimiter = "_"))
})

test_that("covariates can be validated", {

})

test_that("covariates can be transformed between data.frame and list format", {
  x <- sample_probit_covariates(
    probit_formula = probit_formula(choice ~ cost | age | time),
    N = 5,
    Tp = 1:5,
    probit_alternatives = probit_alternatives(J = 3)
  )
  x_list <- as.list(x)
  x_df <- as.data.frame(x_list)
  expect_identical(x, x_df)
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
