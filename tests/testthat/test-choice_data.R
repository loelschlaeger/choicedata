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

})

test_that("multi-character delimiters are supported", {

  df <- data.frame(
    deciderID = c(1, 1),
    occasionID = c(1, 2),
    choice = c("bus", "train"),
    cost__bus = c(10, 12),
    cost__train = c(9, 11),
    time__bus = c(5, 4),
    time__train = c(3, 2)
  )

  multi_wide <- choice_data(
    data_frame = df,
    format = "wide",
    column_choice = "choice",
    column_decider = "deciderID",
    column_occasion = "occasionID",
    delimiter = "__",
    cross_section = FALSE
  )

  expect_true(is.choice_data(multi_wide))
  expect_identical(attr(multi_wide, "delimiter"), "__")

  multi_long <- wide_to_long(
    data_frame = df,
    column_choice = "choice",
    column_alternative = "alternative",
    delimiter = "__",
    choice_type = "discrete"
  )

  expect_true(is.data.frame(multi_long))
  expect_identical(
    sort(unique(multi_long$alternative)),
    c("bus", "train")
  )
  expect_true(all(c("cost", "time") %in% names(multi_long)))
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

  simulated_data <- generate_choice_data(
    choice_effects = choice_effects,
    choice_identifiers = choice_identifiers,
    choice_covariates = choice_covariates,
    choice_parameters = choice_parameters,
    choice_preferences = choice_preferences,
    column_choice = "choice"
  )

  expect_true(is.choice_data(simulated_data))
  expect_identical(attr(simulated_data, "format"), "wide")
  expect_equal(nrow(simulated_data), nrow(choice_covariates))
  checkmate::expect_subset(
    unique(simulated_data[[attr(simulated_data, "column_choice")]]),
    as.character(attr(choice_effects, "choice_alternatives"))
  )
})

test_that("generate_choice_data joins responses and covariates without reordering", {

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

  shuffled_covariates <- choice_covariates[rev(seq_len(nrow(choice_covariates))), ]

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

test_that("generate_choice_data aborts when joins drop identifiers", {

  set.seed(1)

  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ price | time,
      error_term = "logit"
    ),
    choice_alternatives = choice_alternatives(J = 3)
  )

  choice_identifiers <- generate_choice_identifiers(N = 4, Tp = rep(1L, 4L))

  choice_covariates <- generate_choice_covariates(
    choice_effects = choice_effects,
    choice_identifiers = choice_identifiers
  )

  incomplete_covariates <- choice_covariates[-1, ]

  choice_parameters <- generate_choice_parameters(choice_effects = choice_effects)

  expect_error(
    generate_choice_data(
      choice_effects = choice_effects,
      choice_identifiers = choice_identifiers,
      choice_covariates = incomplete_covariates,
      choice_parameters = choice_parameters,
      column_choice = "choice"
    ),
    "Missing rows",
    fixed = TRUE
  )
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

test_that("ranked choice data can be indexed", {
  ranked_df <- data.frame(
    deciderID = rep(1:2, each = 3),
    alternative = rep(c("A", "B", "C"), times = 2),
    choice = c(1, 2, 3, 2, 1, 3),
    stringsAsFactors = FALSE
  )

  ch_data <- choice_data(
    data_frame = ranked_df,
    format = "long",
    column_choice = "choice",
    column_decider = "deciderID",
    column_alternative = "alternative",
    choice_type = "ranked"
  )
  effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ 0 | 0 | 0,
      error_term = "probit"
    ),
    choice_alternatives = choice_alternatives(
      J = 3,
      alternatives = c("A", "B", "C")
    )
  )

  indices <- extract_choice_indices(ch_data, effects)
  expect_equal(indices[[1]], c(1L, 2L, 3L))
  expect_equal(indices[[2]], c(2L, 1L, 3L))
})

test_that("ranked choice data round-trips between long and wide", {
  ranked_df <- data.frame(
    deciderID = rep(1:2, each = 3),
    alternative = rep(c("A", "B", "C"), times = 2),
    choice = c(1, 2, 3, 2, 1, 3),
    stringsAsFactors = FALSE
  )

  wide_ranked <- long_to_wide(
    data_frame = ranked_df,
    column_choice = "choice",
    column_alternative = "alternative",
    column_decider = "deciderID",
    choice_type = "ranked"
  )

  expect_setequal(
    grep("^choice_", names(wide_ranked), value = TRUE),
    paste0("choice_", c("A", "B", "C"))
  )
  expect_equal(wide_ranked$choice, c("A", "B"))

  long_ranked <- wide_to_long(
    data_frame = wide_ranked,
    column_choice = "choice",
    column_alternative = "alternative",
    alternatives = c("A", "B", "C"),
    choice_type = "ranked"
  )

  long_ranked <- long_ranked[order(long_ranked$deciderID, long_ranked$alternative), ]
  ranked_df <- ranked_df[order(ranked_df$deciderID, ranked_df$alternative), ]
  expect_equal(long_ranked$choice, ranked_df$choice)
  expect_equal(long_ranked$alternative, ranked_df$alternative)
  expect_equal(long_ranked$deciderID, ranked_df$deciderID)
})

test_that("ordered choice data preserves categories", {
  ordered_df <- data.frame(
    deciderID = 1:4,
    choice = factor(c("low", "medium", "high", "medium"), ordered = TRUE)
  )

  ch_data <- choice_data(
    data_frame = ordered_df,
    format = "wide",
    column_choice = "choice",
    column_decider = "deciderID",
    choice_type = "ordered"
  )
  effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ 0 | 0 | 0,
      error_term = "probit"
    ),
    choice_alternatives = choice_alternatives(
      J = 3,
      alternatives = c("high", "low", "medium"),
      ordered = TRUE
    )
  )

  indices <- extract_choice_indices(ch_data, effects)
  expect_equal(unlist(indices), c(2L, 3L, 1L, 3L))
})

test_that("is.choice_data validates inputs and reports variable names", {
  cd <- choice_data(
    data_frame = train_choice,
    format = "wide",
    column_choice = "choice",
    column_decider = "deciderID",
    column_occasion = "occasionID"
  )
  expect_true(is.choice_data(cd))
  expect_error(
    is.choice_data(train_choice, var_name = "train_choice"),
    "train_choice",
    fixed = TRUE
  )
  expect_false(is.choice_data(train_choice, error = FALSE))
})

test_that("choice_data respects custom delimiters in long format", {

  custom_delimiter <- "-"
  long_data <- data.frame(
    individual = rep(1:2, each = 2),
    mode = rep(c("car", "bus"), times = 2),
    choice = c(1, 0, 0, 1),
    income = rep(c(50, 60), each = 2),
    wait = c(5, 10, 3, 6),
    stringsAsFactors = FALSE
  )

  choice_obj <- choice_data(
    data_frame = long_data,
    format = "long",
    column_choice = "choice",
    column_decider = "individual",
    column_occasion = NULL,
    column_alternative = "mode",
    column_ac_covariates = "income",
    column_as_covariates = "wait",
    delimiter = custom_delimiter,
    cross_section = TRUE
  )

  expect_true(is.choice_data(choice_obj))

  round_trip_wide <- long_to_wide(
    data_frame = as.data.frame(choice_obj),
    column_ac_covariates = attr(choice_obj, "column_ac_covariates"),
    column_as_covariates = attr(choice_obj, "column_as_covariates"),
    column_choice = attr(choice_obj, "column_choice"),
    column_alternative = attr(choice_obj, "column_alternative"),
    column_decider = attr(choice_obj, "column_decider"),
    column_occasion = attr(choice_obj, "column_occasion"),
    delimiter = custom_delimiter
  )

  expected_wide_columns <- c(
    "individual",
    "income",
    paste0("wait", custom_delimiter, c("car", "bus")),
    "choice"
  )

  expect_setequal(names(round_trip_wide), expected_wide_columns)
  expect_setequal(attr(choice_obj, "column_as_covariates_wide"),
    paste0("wait", custom_delimiter, c("car", "bus"))
  )
})

test_that("ordered simulations propagate the choice type", {

  for (error_term in c("logit", "probit")) {
    set.seed(if (error_term == "logit") 1 else 2)

    choice_effects <- choice_effects(
      choice_formula = choice_formula(
        formula = choice ~ income | 0,
        error_term = error_term
      ),
      choice_alternatives = choice_alternatives(
        J = 3,
        alternatives = c("low", "medium", "high"),
        ordered = TRUE
      )
    )

    choice_identifiers <- generate_choice_identifiers(
      N = 4,
      Tp = rep(1, 4)
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

    simulated_data <- generate_choice_data(
      choice_effects = choice_effects,
      choice_identifiers = choice_identifiers,
      choice_covariates = choice_covariates,
      choice_parameters = choice_parameters,
      choice_preferences = choice_preferences,
      column_choice = "choice"
    )

    expect_true(is.choice_data(simulated_data), info = error_term)
    expect_identical(attr(simulated_data, "choice_type"), "ordered")
    checkmate::expect_subset(
      unique(simulated_data[[attr(simulated_data, "column_choice")]]),
      as.character(attr(choice_effects, "choice_alternatives"))
    )
  }
})

test_that("ranked simulations support logit and probit error terms", {

  for (error_term in c("logit", "probit")) {
    set.seed(if (error_term == "logit") 11 else 12)

    choice_effects <- choice_effects(
      choice_formula = choice_formula(
        formula = choice ~ price | time,
        error_term = error_term
      ),
      choice_alternatives = choice_alternatives(J = 4)
    )

    choice_identifiers <- generate_choice_identifiers(
      N = 3,
      Tp = rep(1L, 3L)
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

    simulated_data <- generate_choice_data(
      choice_effects = choice_effects,
      choice_identifiers = choice_identifiers,
      choice_covariates = choice_covariates,
      choice_parameters = choice_parameters,
      choice_preferences = choice_preferences,
      column_choice = "choice",
      choice_type = "ranked"
    )

    expect_true(is.choice_data(simulated_data), info = error_term)
    expect_identical(attr(simulated_data, "choice_type"), "ranked")

    alt_names <- as.character(attr(choice_effects, "choice_alternatives"))
    rank_cols <- paste0("choice_", alt_names)
    expect_true(all(rank_cols %in% names(simulated_data)), info = error_term)
    expect_true(all(vapply(rank_cols, function(col) {
      is.integer(simulated_data[[col]])
    }, logical(1))), info = error_term)

    ranking_matrix <- as.matrix(simulated_data[rank_cols])
    expect_true(all(ranking_matrix >= 1 &
      ranking_matrix <= length(alt_names)), info = error_term)

    inferred_top <- alt_names[max.col(-ranking_matrix, ties.method = "first")]
    expect_equal(simulated_data$choice, inferred_top, info = error_term)
  }
})

