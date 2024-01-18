test_that("covariates can be sampled", {
  N <- 100
  Tp <- sample(1:N, size = N, replace = TRUE)
  x <- sample_choice_covariates(
    choice_formula = choice_formula(choice ~ cost | age | time),
    N = N,
    Tp = Tp,
    choice_alternatives = choice_alternatives(J = 3)
  )
  expect_s3_class(x, "choice_covariates")
  expect_true(is.choice_covariates(x))
  expect_true(is.data.frame(x))
})

test_that("customization for covariates works", {
  N <- 100
  Tp <- 10
  expect_warning(
    x <- sample_choice_covariates(
      choice_formula = choice_formula(choice ~ cost | age),
      N = N,
      Tp = Tp,
      choice_alternatives = choice_alternatives(J = 3, alternatives = c("la", "le", "lu")),
      occasion_constant = c("cost"),
      covariate_mean = c("cost_la" = 100),
      covariate_sd = c("age" = 2),
      covariate_levels = c("age" = 2),
      covariate_correlation = 0.8,
      empirical = TRUE
    ),
    "empirical means, standard deviations, and correlations cannot be enforced for covariates"
  )

  #round(cor(x[,-c(1:2)]), 2)
  #round(apply(x[,-c(1:2)], 2, mean), 2)
  #round(apply(x[,-c(1:2)], 2, sd), 2)

  expect_s3_class(x, "choice_covariates")
  expect_true(is.choice_covariates(x))
  expect_true(is.list(x))
  expect_true(is.data.frame(x))
})

test_that("check for covariate_levels works", {
  expect_equal(
    check_covariate_levels(
      covariate_levels = 2,
      choice_formula = choice_formula(formula = choice ~ A),
      choice_alternatives = choice_alternatives(J = 2),
      delimiter = "_"
    ),
    c(A_A = 2, A_B = 2)
  )
  expect_equal(
    check_covariate_levels(
      covariate_levels = c("A_A" = 3),
      choice_formula = choice_formula(formula = choice ~ A),
      choice_alternatives = choice_alternatives(J = 2),
      delimiter = "_"
    ),
    c(A_A = 3, A_B = Inf)
  )
})

test_that("check for occasion_constant works", {
  expect_equal(
    check_occasion_constant(
      occasion_constant = c("A", "B"),
      choice_formula = choice_formula(formula = choice ~ A | B),
      choice_alternatives = choice_alternatives(J = 2),
      delimiter = "_"
    ),
    c("B", "A_A", "A_B")
  )
})

test_that("check for covariate_mean works", {
  expect_equal(
    check_covariate_mean(
      covariate_mean = 2,
      choice_formula = choice_formula(formula = choice ~ A),
      choice_alternatives = choice_alternatives(J = 2),
      delimiter = "_"
    ),
    c(A_A = 2, A_B = 2)
  )
  expect_equal(
    check_covariate_mean(
      covariate_mean = c("A_A" = 3),
      choice_formula = choice_formula(formula = choice ~ A),
      choice_alternatives = choice_alternatives(J = 2),
      delimiter = "_"
    ),
    c(A_A = 3, A_B = 0)
  )
})

test_that("check for covariate_sd works", {
  expect_equal(
    check_covariate_sd(
      covariate_sd = 2,
      choice_formula = choice_formula(formula = choice ~ A),
      choice_alternatives = choice_alternatives(J = 2),
      delimiter = "_"
    ),
    c(A_A = 2, A_B = 2)
  )
  expect_equal(
    check_covariate_sd(
      covariate_sd = c("A_A" = 3),
      choice_formula = choice_formula(formula = choice ~ A),
      choice_alternatives = choice_alternatives(J = 2),
      delimiter = "_"
    ),
    c(A_A = 3, A_B = 1)
  )
})

test_that("check for covariate_correlation works", {
  expect_equal(
    check_covariate_correlation(
      covariate_correlation = diag(2),
      choice_formula = choice_formula(formula = choice ~ A),
      choice_alternatives = choice_alternatives(J = 2),
      delimiter = "_"
    ),
    diag(2)
  )
  expect_equal(
    check_covariate_correlation(
      covariate_correlation = 1,
      choice_formula = choice_formula(formula = choice ~ A),
      choice_alternatives = choice_alternatives(J = 2),
      delimiter = "_"
    ),
    matrix(1, 2, 2)
  )
})

test_that("covariate specification sugar works", {
  expect_equal(
    covariate_spec_sugar(
      covariate_spec = "A",
      choice_formula = choice_formula(formula = choice ~ A),
      choice_alternatives = choice_alternatives(J = 2),
      delimiter = "_",
      named_vector = FALSE
    ),
    c("A_A", "A_B")
  )
  expect_equal(
    covariate_spec_sugar(
      covariate_spec = c("A" = 3),
      choice_formula = choice_formula(formula = choice ~ A),
      choice_alternatives = choice_alternatives(J = 2),
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

test_that("covariates can be transformed between data.frame and list format", {
  x <- sample_choice_covariates(
    choice_formula = choice_formula(choice ~ cost | age | time),
    N = 5,
    Tp = 1:5,
    choice_alternatives = choice_alternatives(J = 3)
  )
  x_list <- as.list(x)
  x_df <- as.data.frame(x_list)
  expect_identical(x, x_df)
})

test_that("covariate names can be generated", {
  expect_equal(
    covariate_names(
      choice_formula(formula = choice ~ cost | age | time),
      choice_alternatives(J = 2)
    ),
    c("cost_A", "cost_B", "age", "time_A", "time_B")
  )
})

test_that("covariate number can be computed", {
  expect_equal(
    covariate_number(
      choice_formula(choice ~ A | B | C),
      choice_alternatives(J = 3)
    ),
    7
  )
})

