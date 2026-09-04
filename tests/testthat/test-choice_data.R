test_that("choice_data can be defined", {

  ### long format (all columns)
  expect_true(
    choice_data(
      data_frame = travel_mode_choice,
      format = "long",
      column_choice = "choice",
      column_decider = "individual",
      column_occasion = NULL,
      column_alternative = "mode",
      delimiter = "_",
      cross_section = TRUE
    ) |> is.choice_data()
  )

  ### long format (selected columns)
  expect_true(
    choice_data(
      data_frame = travel_mode_choice,
      format = "long",
      column_choice = "choice",
      column_decider = "individual",
      column_occasion = NULL,
      column_alternative = "mode",
      column_ac_covariates = "income",
      column_as_covariates = "wait",
      delimiter = "_",
      cross_section = TRUE
    ) |> is.choice_data()
  )

  ### wide format
  expect_true(
    choice_data(
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
    ) |> is.choice_data()
  )

  ### wide format (selected columns)
  expect_true(
    choice_data(
      data_frame = train_choice,
      format = "wide",
      column_choice = "choice",
      column_decider = "deciderID",
      column_occasion = "occasionID",
      column_alternative = NULL,
      column_ac_covariates = NULL,
      column_as_covariates = "price",
      delimiter = "_",
      cross_section = FALSE
    ) |> is.choice_data()
  )

  ### wide format with a missing response
  missing_wide <- train_choice[1:4, ]
  missing_wide$choice[2] <- NA_character_
  missing_data <- choice_data(
    data_frame = missing_wide,
    format = "wide",
    column_choice = "choice",
    column_decider = "deciderID",
    column_occasion = "occasionID"
  )
  expect_s3_class(missing_data, "choice_data")
  expect_true(is.na(missing_data$choice[2]))

  ### long format with individual choice sets and a missing response
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
  expect_s3_class(choice_set_data, "choice_data")
  expect_equal(nrow(choice_set_data), nrow(choice_set_df))
  expect_true(all(is.na(choice_set_data$choice[4:5])))

  bad_choice_set <- choice_set_df
  bad_choice_set$cost[1] <- NA
  expect_error(
    choice_data(
      data_frame = bad_choice_set,
      format = "long",
      column_choice = "choice",
      column_decider = "deciderID",
      column_alternative = "alternative",
      column_as_covariates = "cost"
    ),
    "outside response columns"
  )

})

test_that("simulation of probit choice data works for wide covariates", {

  set.seed(1)

  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ price | income,
      error_term = "probit",
      random_effects = c("price" = "cn")
    ),
    choice_alternatives = choice_alternatives(J = 3)
  )

  choice_identifiers <- generate_choice_identifiers(
    N = 5,
    Tp = c(2, 1, 3, 1, 2)
  )

  choice_covariates <- generate_choice_covariates(
    choice_effects = choice_effects,
    choice_identifiers = choice_identifiers
  )

  choice_parameters <- generate_choice_parameters(
    choice_effects = choice_effects
  )

  choice_preferences <- generate_choice_preferences(
    choice_parameters = choice_parameters,
    choice_effects = choice_effects,
    choice_identifiers = choice_identifiers
  )

  set.seed(1)
  simulated_data <- generate_choice_data(
    choice_effects = choice_effects,
    choice_identifiers = choice_identifiers,
    choice_covariates = choice_covariates,
    choice_parameters = choice_parameters,
    choice_preferences = choice_preferences,
    column_choice = "choice"
  )
  set.seed(1)
  simulated_default <- generate_choice_data(choice_effects)
  set.seed(1)
  repeated_default <- generate_choice_data(choice_effects)

  expect_true(is.choice_data(simulated_data))
  expect_identical(simulated_default, repeated_default)
  expect_identical(attr(simulated_data, "format"), "wide")
  expect_equal(nrow(simulated_data), nrow(choice_covariates))
  checkmate::expect_subset(
    unique(simulated_data[[attr(simulated_data, "column_choice")]]),
    as.character(attr(choice_effects, "choice_alternatives"))
  )

  alternatives <- as.character(attr(choice_effects, "choice_alternatives"))
  long_df <- wide_to_long(
    as.data.frame(choice_covariates),
    column_choice = NULL,
    alternatives = alternatives
  )
  first <- long_df[1, c("deciderID", "occasionID")]
  remove <- long_df$deciderID == first$deciderID &
    long_df$occasionID == first$occasionID &
    long_df$alternative == alternatives[2]
  long_covariates <- choice_covariates(
    long_df[!remove, ],
    format = "long",
    column_decider = "deciderID",
    column_occasion = "occasionID",
    column_alternative = "alternative"
  )
  set.seed(1)
  long_data <- generate_choice_data(
    choice_effects = choice_effects,
    choice_identifiers = choice_identifiers,
    choice_covariates = long_covariates,
    choice_parameters = choice_parameters,
    choice_preferences = choice_preferences
  )
  observation <- interaction(
    long_data[c("deciderID", "occasionID")], drop = TRUE
  )
  expect_identical(attr(long_data, "format"), "long")
  expect_equal(nrow(long_data), nrow(long_covariates))
  expect_true(all(tapply(long_data$choice, observation, sum) == 1L))
})

test_that("generate_choice_data keeps covariate order", {

  set.seed(1)

  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ price | income,
      error_term = "probit",
      random_effects = c("price" = "cn")
    ),
    choice_alternatives = choice_alternatives(J = 3)
  )

  choice_identifiers <- generate_choice_identifiers(
    N = 4,
    Tp = c(2, 1, 2, 1)
  )

  choice_covariates <- generate_choice_covariates(
    choice_effects = choice_effects,
    choice_identifiers = choice_identifiers
  )

  reverse_rows <- rev(seq_len(nrow(choice_covariates)))
  shuffled_covariates <- choice_covariates[reverse_rows, ]

  choice_parameters <- generate_choice_parameters(
    choice_effects = choice_effects
  )

  choice_preferences <- generate_choice_preferences(
    choice_parameters = choice_parameters,
    choice_effects = choice_effects,
    choice_identifiers = choice_identifiers
  )

  simulated_data <- generate_choice_data(
    choice_effects = choice_effects,
    choice_identifiers = choice_identifiers,
    choice_covariates = shuffled_covariates,
    choice_parameters = choice_parameters,
    choice_preferences = choice_preferences,
    column_choice = "choice"
  )

  expect_s3_class(simulated_data, "tbl_df")

})

test_that("alternative names can be guessed from wide format", {

  ### with column_choice available
  expect_identical(
    guess_alternatives_wide(
      data_frame = train_choice,
      column_choice = "choice",
      delimiter = "_"
    ),
    c("A", "B")
  )

  ### without column_choice available
  expect_identical(
    guess_alternatives_wide(
      data_frame = train_choice,
      column_choice = NULL,
      delimiter = "_"
    ),
    c("A", "B")
  )

  wide_with_delimiter <- data.frame(
    travel_time_car = c(10, 12),
    travel_time_bus = c(15, 18),
    travel_cost_car = c(3, 4),
    travel_cost_bus = c(2, 2)
  )
  expect_identical(
    guess_alternatives_wide(
      data_frame = wide_with_delimiter,
      column_choice = NULL,
      delimiter = "_"
    ),
    c("bus", "car")
  )
})

test_that("data can be transformed between long and wide format", {

  expect_s3_class(
    long_to_wide(
      travel_mode_choice,
      column_alternative = "mode",
      column_decider = "individual"
    ),
    "tbl_df"
  )
  expect_s3_class(wide_to_long(train_choice), "tbl_df")

  ### from long format to wide format
  expect_identical(
    long_to_wide(
      data_frame = travel_mode_choice,
      column_as_covariates = character(), # ignore as covariates
      column_choice = "choice",
      column_alternative = "mode",
      column_decider = "individual",
      column_occasion = NULL
    ) |> colnames(),
    c("individual", "income", "size", "choice")
  )
  expect_identical(
    long_to_wide(
      data_frame = travel_mode_choice,
      column_alternative = "mode",
      column_decider = "individual"
    ) |> colnames(),
    c("individual", "income", "size", "wait_plane", "wait_train",
      "wait_bus", "wait_car", "cost_plane", "cost_train", "cost_bus",
      "cost_car", "travel_plane", "travel_train", "travel_bus", "travel_car",
      "choice")
  )

  ### from wide format to long format
  expect_identical(
    wide_to_long(
      data_frame = train_choice[, 1:3]
    ) |> colnames(),
    c("deciderID", "occasionID", "choice", "alternative")
  )
  expect_identical(
    wide_to_long(
      data_frame = train_choice
    ) |> colnames(),
    c("deciderID", "occasionID", "choice", "alternative", "price",
      "time", "change", "comfort")
  )

  ### from wide format to long format without alternatives
  expect_identical(
    wide_to_long(
      data_frame = train_choice[, -3], column_choice = NULL
    ) |> colnames(),
    c("deciderID", "occasionID", "alternative", "price",
      "time", "change", "comfort")
  )

})

test_that("alternative-specific covariates can be detected", {

  ### long format (trivial case)
  expect_identical(
    check_as_covariates(
      data_frame = travel_mode_choice[, c("individual", "mode")],
      format = "long",
      column_choice = NULL,
      column_decider = "individual",
      column_occasion = NULL,
      column_alternative = "mode"
    ),
    list(
      column_ac_covariates = character(0),
      column_as_covariates = character(0),
      column_as_covariates_wide = character(0)
    )
  )

  ### long format (all columns)
  expect_identical(
    check_as_covariates(
      data_frame = travel_mode_choice,
      format = "long",
      column_choice = "choice",
      column_decider = "individual",
      column_occasion = NULL,
      column_alternative = "mode"
    ),
    list(
      column_ac_covariates = c("income", "size"),
      column_as_covariates = c("wait", "cost", "travel"),
      column_as_covariates_wide = c(
        "wait_bus", "cost_bus", "travel_bus", "wait_car", "cost_car",
        "travel_car", "wait_plane", "cost_plane", "travel_plane", "wait_train",
        "cost_train", "travel_train"
      )
    )
  )

  ### long format (selected columns)
  expect_identical(
    check_as_covariates(
      data_frame = travel_mode_choice,
      format = "long",
      column_choice = "choice",
      column_decider = "individual",
      column_alternative = "mode",
      column_ac_covariates = "size",
      column_as_covariates = "wait"
    ),
    list(
      column_ac_covariates = "size",
      column_as_covariates = "wait",
      column_as_covariates_wide = c(
        "wait_bus", "wait_car", "wait_plane", "wait_train"
      )
    )
  )
  expect_error(
    check_as_covariates(
      data_frame = travel_mode_choice,
      format = "long",
      column_choice = "choice",
      column_decider = "individual",
      column_alternative = "mode",
      column_ac_covariates = "unknown"
    ),
    "Unknown"
  )
  expect_error(
    check_as_covariates(
      data_frame = travel_mode_choice,
      format = "long",
      column_choice = "choice",
      column_decider = "individual",
      column_alternative = "mode",
      column_as_covariates = "unknown"
    ),
    "Unknown"
  )
  expect_error(
    check_as_covariates(
      data_frame = travel_mode_choice,
      format = "long",
      column_choice = "choice",
      column_decider = "individual",
      column_alternative = "mode",
      column_ac_covariates = "wait"
    ),
    "Found varying"
  )
  expect_error(
    check_as_covariates(
      data_frame = travel_mode_choice,
      format = "long",
      column_choice = "choice",
      column_decider = "individual",
      column_alternative = "mode",
      column_as_covariates = "size"
    ),
    "Found constant"
  )

  ### wide format (trivial case)
  expect_identical(
    check_as_covariates(
      data_frame = train_choice[, c("deciderID", "occasionID")],
      format = "wide",
      column_choice = NULL,
      column_decider = "deciderID",
      column_occasion = "occasionID",
    ),
    list(
      column_ac_covariates = character(0),
      column_as_covariates = character(0),
      column_as_covariates_wide = character(0)
    )
  )

  ### wide format (all columns)
  expect_identical(
    check_as_covariates(
      data_frame = train_choice,
      format = "wide",
      column_choice = "choice",
      column_decider = "deciderID",
      column_occasion = "occasionID",
      delimiter = "_"
    ),
    list(
      column_ac_covariates = character(0),
      column_as_covariates = c(
        "change", "comfort", "price", "time"
      ),
      column_as_covariates_wide = c(
        "price_A", "time_A", "change_A", "comfort_A",
        "price_B", "time_B", "change_B", "comfort_B"
      )
    )
  )

  ### wide format (selected columns)
  expect_identical(
    check_as_covariates(
      data_frame = train_choice,
      format = "wide",
      column_choice = "choice",
      column_decider = "deciderID",
      column_occasion = "occasionID",
      column_ac_covariates = character(),
      column_as_covariates = "price",
      delimiter = "_"
    ),
    list(
      column_ac_covariates = character(0),
      column_as_covariates = "price",
      column_as_covariates_wide = c("price_A", "price_B")
    )
  )
  expect_error(
    check_as_covariates(
      data_frame = train_choice,
      format = "wide",
      column_choice = "choice",
      column_decider = "deciderID",
      column_occasion = "occasionID",
      column_ac_covariates = "unknown",
      delimiter = "_"
    ),
    "Unknown"
  )
  expect_error(
    check_as_covariates(
      data_frame = train_choice,
      format = "wide",
      column_choice = "choice",
      column_decider = "deciderID",
      column_occasion = "occasionID",
      column_as_covariates = "unknown",
      delimiter = "_"
    ),
    "missing"
  )
  expect_error(
    check_as_covariates(
      data_frame = train_choice,
      format = "wide",
      column_choice = "choice",
      column_decider = "deciderID",
      column_occasion = "occasionID",
      column_ac_covariates = "price",
      delimiter = "_"
    ),
    "Unknown"
  )
})

test_that("long_to_wide accepts a factor column of alternatives", {
  data_frame <- travel_mode_choice
  data_frame$mode <- factor(data_frame$mode)
  wide <- long_to_wide(
    data_frame = data_frame,
    column_alternative = "mode",
    column_decider = "individual"
  )
  expect_equal(nrow(wide), 210L)
  expect_true(all(c("wait_plane", "cost_car") %in% names(wide)))
})
