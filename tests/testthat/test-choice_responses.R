test_that("simulation of probit choice responses works", {
  set.seed(1)
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ X | Y | Z, error_term = "probit"
    ),
    choice_alternatives = choice_alternatives(J = 3)
  )
  choice_covariates <- generate_choice_covariates(
    choice_effects = choice_effects,
    choice_identifiers = generate_choice_identifiers(
      Tp = sample.int(5, 100, replace = TRUE),
    )
  )
  alternatives <- as.character(attr(choice_effects, "choice_alternatives"))
  choice_covariates <- wide_to_long(
    as.data.frame(choice_covariates),
    column_choice = NULL,
    alternatives = alternatives
  )
  first <- choice_covariates[1, c("deciderID", "occasionID")]
  remove <- choice_covariates$deciderID == first$deciderID &
    choice_covariates$occasionID == first$occasionID &
    choice_covariates$alternative == alternatives[2]
  choice_covariates <- choice_covariates[!remove, ]
  choice_covariates <- choice_covariates(
    choice_covariates,
    format = "long",
    column_decider = "deciderID",
    column_occasion = "occasionID",
    column_alternative = "alternative"
  )
  choice_parameters <- generate_choice_parameters(
    choice_effects = choice_effects
  )
  choice_identifiers <- extract_choice_identifiers(choice_covariates)
  choice_preferences <- generate_choice_preferences(
    choice_parameters = choice_parameters,
    choice_effects = choice_effects,
    choice_identifiers = choice_identifiers
  )
  set.seed(1)
  choice_responses <- generate_choice_responses(
    choice_effects = choice_effects,
    choice_covariates = choice_covariates,
    choice_parameters = choice_parameters,
    choice_identifiers = choice_identifiers,
    choice_preferences = choice_preferences,
    column_choice = "choice"
  )
  design <- design_matrices(
    choice_covariates,
    choice_effects,
    choice_identifiers
  )
  preferences <- split_choice_preferences(
    choice_preferences,
    choice_identifiers
  )
  preferences <- rep(
    preferences,
    times = read_Tp(choice_identifiers)
  )
  availability <- attr(design, "availability")
  set.seed(1)
  expected <- vapply(seq_along(design), function(i) {
    available <- availability[[i]]
    mean <- as.vector(design[[i]][available, ] %*% preferences[[i]])
    Sigma <- choice_parameters$Sigma[available, available, drop = FALSE]
    utility <- oeli::rmvnorm(mean = mean, Sigma = Sigma)
    alternatives[available[which.max(utility)]]
  }, character(1))
  expect_s3_class(choice_responses, "choice_responses")
  expect_true(is.choice_responses(choice_responses))
  expect_identical(choice_responses$choice, expected)
  expect_equal(
    nrow(choice_responses),
    sum(read_Tp(choice_identifiers))
  )
  expect_equal(
    sort(unique(choice_responses[[attr(choice_responses, "column_choice")]])),
    sort(as.character(attr(choice_effects, "choice_alternatives")))
  )
})

test_that("simulation of logit choice responses works", {
  data(train_choice)

  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ price | time,
      error_term = "logit",
      random_effects = c("price" = "cn")
    ),
    choice_alternatives = choice_alternatives(J = 2, alternatives = c("A", "B"))
  )

  covariates <- choice_covariates(
    data_frame = train_choice,
    format = "wide",
    column_decider = "deciderID",
    column_occasion = "occasionID"
  )
  covariates <- covariates[
    order(covariates$occasionID, covariates$deciderID),
  ]

  params <- choice_parameters(
    beta = rep(0.1, nrow(choice_effects)),
    Omega = matrix(1)
  )

  ids <- extract_choice_identifiers(covariates)
  set.seed(1)
  preferences <- generate_choice_preferences(
    choice_effects,
    params,
    ids
  )
  set.seed(1)
  responses <- generate_choice_responses(
    choice_effects = choice_effects,
    choice_covariates = covariates,
    choice_parameters = params,
    choice_identifiers = ids,
    choice_preferences = preferences
  )
  design <- design_matrices(covariates, choice_effects, ids)
  preferences <- split_choice_preferences(preferences, ids)
  column_decider <- attr(ids, "column_decider")
  preference_index <- match(
    ids[[column_decider]],
    unique(ids[[column_decider]])
  )
  preferences <- preferences[preference_index]
  alternatives <- as.character(attr(choice_effects, "choice_alternatives"))
  set.seed(1)
  expected <- vapply(seq_along(design), function(i) {
    utility <- as.vector(design[[i]] %*% preferences[[i]])
    error <- -log(-log(stats::runif(length(utility))))
    alternatives[which.max(utility + error)]
  }, character(1))

  expect_s3_class(responses, "choice_responses")
  expect_identical(responses$choice, expected)
  expect_true(all(responses$choice %in% c("A", "B")))
})

test_that("choice response simulation requires Sigma", {
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      choice ~ cost | age | time, error_term = "probit"
    ),
    choice_alternatives = choice_alternatives(J = 3)
  )
  choice_covariates <- generate_choice_covariates(
    choice_effects = choice_effects,
    choice_identifiers = generate_choice_identifiers(N = 3, Tp = rep(1, 3))
  )
  expect_error(
    generate_choice_responses(
      choice_effects = choice_effects,
      choice_covariates = choice_covariates,
      choice_parameters = choice_parameters(
        beta = numeric(nrow(choice_effects))
      ),
      choice_identifiers = extract_choice_identifiers(choice_covariates)
    ),
    "required",
    fixed = TRUE
  )
})

test_that("generate_choice_responses can return ranked columns", {

  set.seed(1)

  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ price | time,
      error_term = "logit"
    ),
    choice_alternatives = choice_alternatives(J = 3)
  )

  ids <- generate_choice_identifiers(N = 3, Tp = rep(1L, 3L))
  covariates <- generate_choice_covariates(
    choice_effects = choice_effects,
    choice_identifiers = ids
  )
  params <- generate_choice_parameters(choice_effects = choice_effects)

  alt_names <- as.character(attr(choice_effects, "choice_alternatives"))
  covariates <- wide_to_long(
    as.data.frame(covariates),
    column_choice = NULL,
    alternatives = alt_names
  )
  keep <- with(covariates, !(
    (deciderID == "1" & alternative == "B") |
      (deciderID == "2" & alternative == "C") |
      (deciderID == "3" & alternative != "B")
  ))
  covariates <- choice_covariates(
    covariates[keep, ],
    format = "long",
    column_decider = "deciderID",
    column_occasion = "occasionID",
    column_alternative = "alternative"
  )

  ranked_responses <- generate_choice_responses(
    choice_effects = choice_effects,
    choice_covariates = covariates,
    choice_parameters = params,
    choice_identifiers = ids,
    column_choice = "choice",
    choice_type = "ranked"
  )

  expect_s3_class(ranked_responses, "choice_responses")
  expect_identical(attr(ranked_responses, "column_choice"), "choice")

  rank_cols <- paste0("choice_", alt_names)
  expect_true(all(rank_cols %in% names(ranked_responses)))

  ranking_matrix <- as.matrix(ranked_responses[rank_cols])

  available <- list(c("A", "C"), c("A", "B"), "B")
  for (i in seq_along(available)) {
    unavailable <- !alt_names %in% available[[i]]
    expect_true(all(is.na(ranking_matrix[i, unavailable])))
    expect_setequal(
      stats::na.omit(ranking_matrix[i, ]),
      seq_along(available[[i]])
    )
    expect_true(ranked_responses$choice[i] %in% available[[i]])
  }
  inferred_top <- alt_names[apply(ranking_matrix, 1, which.min)]
  expect_equal(ranked_responses$choice, inferred_top)
})
