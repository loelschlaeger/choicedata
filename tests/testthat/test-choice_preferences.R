test_that("choice preferences can be generated", {
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ price | income | comfort,
      error_term = "probit",
      random_effects = c("price" = "cn", "income" = "cn")
    ),
    choice_alternatives = choice_alternatives(J = 3)
  )
  choice_preferences <- generate_choice_preferences(
    choice_effects = choice_effects
  )
  expect_true(
    is.choice_preferences(choice_preferences)
  )
  prefs_list <- choicedata:::split_choice_preferences(choice_preferences)
  expect_length(prefs_list, nrow(choice_preferences))
  expect_equal(
    prefs_list[[1]],
    stats::setNames(
      as.numeric(choice_preferences[1, -1, drop = TRUE]),
      colnames(choice_preferences)[-1]
    )
  )
})

