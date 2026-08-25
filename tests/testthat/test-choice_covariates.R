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
  expect_length(design_matrices, 15)
  expect_true(all(vapply(
    design_matrices,
    function(x) identical(dim(x), c(3L, nrow(choice_effects))),
    logical(1)
  )))

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
    ),
    choice_data = choice_data
  )
  design_matrices <- design_matrices(
    x = choice_data,
    choice_effects = choice_effects
  )
  expect_identical(
    colnames(design_matrices[[1]]),
    choice_effects$effect_name
  )
  unresolved_effects <- choice_effects(
    choice_formula(choice ~ factor(comfort)),
    attr(choice_effects, "choice_alternatives")
  )
  expect_error(
    design_matrices(choice_data, unresolved_effects),
    "recreate `choice_effects`"
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
  expect_true(all(vapply(
    design_matrices,
    function(x) identical(dim(x), c(4L, nrow(choice_effects))),
    logical(1)
  )))

  ### individual choice sets keep global design rows
  choice_set_df <- data.frame(
    deciderID = c(1, 1, 1, 2, 2),
    alternative = c("A", "B", "C", "A", "C"),
    choice = c(0L, 1L, 0L, NA, NA),
    cost = c(1, 2, 3, 4, 5)
  )
  choice_set_data <- choice_data(
    data_frame = choice_set_df,
    format = "long",
    column_choice = "choice",
    column_decider = "deciderID",
    column_alternative = "alternative",
    column_as_covariates = "cost"
  )
  choice_set_effects <- choice_effects(
    choice_formula = choice_formula(
      choice ~ cost | 0 | 0,
      error_term = "logit"
    ),
    choice_alternatives = choice_alternatives(
      J = 3,
      alternatives = c("A", "B", "C")
    ),
    choice_data = choice_set_data
  )
  choice_set_design <- design_matrices(
    x = choice_set_data,
    choice_effects = choice_set_effects
  )
  expect_true(all(vapply(
    choice_set_design,
    function(x) identical(dim(x), c(3L, nrow(choice_set_effects))),
    logical(1)
  )))
  expect_identical(
    attr(choice_set_design, "availability"),
    list(1:3, c(1L, 3L))
  )
  expect_true(all(choice_set_design[[2]]["B", ] == 0))

  choice_set_indices <- extract_choice_indices(
    choice_data = choice_set_data,
    choice_effects = choice_set_effects
  )
  expect_identical(choice_set_indices[[1]], 2L)
  expect_identical(choice_set_indices[[2]], integer())
})
