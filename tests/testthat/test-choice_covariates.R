test_that("choice_covariates can be defined", {

  ### long format
  expect_true(
    choice_covariates(
      data_frame = travel_mode_choice,
      format = "long",
      column_decider = "individual",
      column_occasion = NULL,
      column_alternative = "mode",
      column_ac_covariates = NULL,
      column_as_covariates = NULL,
      delimiter = "_",
      cross_section = TRUE
    ) |> is.choice_covariates()
  )

  ### wide format
  expect_true(
    choice_covariates(
      data_frame = train_choice,
      format = "wide",
      column_decider = "deciderID",
      column_occasion = "occasionID",
      column_alternative = NULL,
      column_ac_covariates = NULL,
      column_as_covariates = NULL,
      delimiter = "_",
      cross_section = FALSE
    ) |> is.choice_covariates()
  )
})

test_that("choice_covariates respects custom delimiters in long format", {
  custom_delimiter <- "-"
  long_covariates <- tibble::tibble(
    individual = rep(1:2, each = 2),
    mode = rep(c("bus", "car"), times = 2),
    cost = c(10, 12, 14, 16),
    wait = c(3, 2, 5, 4)
  )

  result <- choice_covariates(
    data_frame = long_covariates,
    format = "long",
    column_decider = "individual",
    column_occasion = NULL,
    column_alternative = "mode",
    column_ac_covariates = NULL,
    column_as_covariates = c("cost", "wait"),
    delimiter = custom_delimiter,
    cross_section = TRUE
  )

  expect_true(is.choice_covariates(result))
  expect_identical(attr(result, "delimiter"), custom_delimiter)
})

test_that("covariates can be generated", {

  ### using choice effects
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ price | income | comfort,
      error_term = "probit",
      random_effects = c(
        "price" = "cn",
        "income" = "cn"
       )
    ),
    choice_alternatives = choice_alternatives(J = 3)
  )
  x <- generate_choice_covariates(choice_effects = choice_effects)
  expect_s3_class(x, "choice_covariates")
  expect_true(is.choice_covariates(x))

  ### without choice effects
  x <- generate_choice_covariates(labels = c("cost", "age", "time"))
  expect_s3_class(x, "choice_covariates")
  expect_true(is.choice_covariates(x))

})

test_that("covariate names can be deduced from choice effects", {
  choice_effects <- choice_effects(
    choice_formula(choice ~ cost | age | time, error_term = "probit"),
    choice_alternatives(J = 3)
  )
  expect_equal(
    covariate_names(choice_effects),
    c("cost_A", "cost_B", "cost_C", "time_A", "time_B", "time_C", "age")
  )
})

test_that("design matrices can be build", {

  ### simulation case
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      choice ~ cost | age | time, error_term = "probit"
    ),
    choice_alternatives = choice_alternatives(J = 3)
  )
  choice_covariates <- generate_choice_covariates(
    choice_effects = choice_effects,
    choice_identifiers = generate_choice_identifiers(N = 5, Tp = 1:5)
  )
  design_matrices <- design_matrices(
    x = choice_covariates,
    choice_effects = choice_effects
  )

  ### empirical data case (wide)
  choice_data <- choice_data(
    data_frame = train_choice,
    format = "wide",
    column_choice = "choice",
    column_decider = "deciderID",
    column_occasion = "occasionID",
    column_alternative = NULL,
    column_ac_covariates = NULL,
    column_as_covariates = NULL,
    delimiter = "_",
    cross_section = FALSE
  )
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      choice ~ price + time + change + comfort | 0
    ),
    choice_alternatives = choice_alternatives(
      J = 2, alternatives = c("A", "B")
    )
  )
  design_matrices <- design_matrices(
    x = choice_data,
    choice_effects = choice_effects
  )
  choice_ids <- extract_choice_identifiers(choice_data)
  choice_indices <- extract_choice_indices(
    choice_data = choice_data,
    choice_effects = choice_effects,
    choice_identifiers = choice_ids
  )
  expect_equal(
    unlist(choice_indices),
    match(choice_data[[attr(choice_data, "column_choice")]],
      attr(choice_effects, "choice_alternatives"))
  )

  ### empirical data case (long)
  choice_data <- choice_data(
    data_frame = travel_mode_choice,
    format = "long",
    column_choice = "choice",
    column_decider = "individual",
    column_occasion = NULL,
    column_alternative = "mode",
    delimiter = "_",
    cross_section = TRUE
  )
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      choice ~ cost | income + size | wait + travel
    ),
    choice_alternatives = choice_alternatives(
      J = 4, alternatives = c("bus", "car", "plane", "train")
    )
  )
  design_matrices <- design_matrices(
    x = choice_data,
    choice_effects = choice_effects
  )
})

