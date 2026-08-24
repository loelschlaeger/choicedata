test_that("choice preferences can be generated", {
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ price | income | comfort,
      error_term = "probit",
      random_effects = c("price" = "cn", "income" = "cn")
    ),
    choice_alternatives = choice_alternatives(J = 3)
  )
  P <- compute_P(choice_effects)
  re_position <- which(!is.na(choice_effects$mixing))
  params <- choice_parameters(
    beta = seq_len(P),
    Omega = diag(length(re_position)),
    Sigma = diag(3)
  )
  ids <- generate_choice_identifiers(N = 4)
  expected <- matrix(params$beta, nrow = 4, ncol = P, byrow = TRUE)
  set.seed(1)
  expected[, re_position] <- oeli::rmvnorm(
    n = 4,
    mean = params$beta[re_position],
    Sigma = params$Omega
  )
  set.seed(1)
  choice_preferences <- generate_choice_preferences(
    choice_effects = choice_effects,
    choice_parameters = params,
    choice_identifiers = ids
  )
  expect_true(
    is.choice_preferences(choice_preferences)
  )
  expect_s3_class(choice_preferences, "tbl_df")
  expect_equal(
    unname(as.matrix(choice_preferences[-1])),
    expected
  )
  prefs_list <- split_choice_preferences(choice_preferences)
  expect_length(prefs_list, nrow(choice_preferences))
  expect_equal(
    prefs_list[[1]],
    stats::setNames(
      as.numeric(choice_preferences[1, -1, drop = TRUE]),
      colnames(choice_preferences)[-1]
    )
  )
})
