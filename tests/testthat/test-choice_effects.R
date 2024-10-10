test_that("effect overview can be created", {

  ### test 1
  choice_formula <- choice_formula(
    formula = choice ~ cov,
    error_term = "logit",
    random_effects = c("cov", "ASC+")
  )
  choice_alternatives <- choice_alternatives(
    J = 3,
    alternatives = c("C", "B", "A"),
    base = "B"
  )
  expect_equal(
    choice_effects(
      choice_formula = choice_formula,
      choice_alternatives = choice_alternatives
    ),
    structure(
      list(
        effect_name = c("cov", "ASC_A", "ASC_C"),
        generic_name = c("b_1", "b_2", "b_3"),
        covariate = c("cov", NA, NA),
        alternative = c(NA, "A", "C"),
        as_covariate = c(TRUE, FALSE, FALSE),
        as_effect = c(FALSE, TRUE, TRUE),
        lc_effect = c(FALSE, FALSE, FALSE),
        mixing = structure(
          c(1L, 2L, 2L),
          levels = c("normal", "log-normal"),
          class = c("ordered", "factor")
        )
      ),
      row.names = c(NA, 3L),
      class = c("choice_effects", "data.frame"),
      choice_formula = choice_formula,
      choice_alternatives = choice_alternatives,
      delimiter = "_"
    )
  )

  ### test 2
  choice_formula <- choice_formula(
    formula = choice ~ A | B + 0 | C,
    error_term = "probit",
    random_effects = character()
  )
  choice_alternatives <- choice_alternatives(
    J = 2,
    alternatives = c("A", "B")
  )
  expect_equal(
    choice_effects(
      choice_formula = choice_formula,
      choice_alternatives = choice_alternatives,
      delimiter = "*"
    ),
    structure(
      list(
        effect_name = c("A", "B*B", "C*A", "C*B"),
        generic_name = c("alpha_1", "alpha_2", "alpha_3", "alpha_4"),
        covariate = c("A", "B", "C", "C"),
        alternative = c(NA, "B", "A", "B"),
        as_covariate = c(TRUE, FALSE, TRUE, TRUE),
        as_effect = c(FALSE, TRUE, TRUE, TRUE),
        lc_effect = c(FALSE, FALSE, FALSE, FALSE),
        mixing = structure(
          c(NA_integer_, NA_integer_, NA_integer_, NA_integer_),
          levels = c("normal", "log-normal"),
          class = c("ordered", "factor")
        )
      ),
      row.names = c(NA, -4L),
      class = c("choice_effects", "data.frame"),
      choice_formula = choice_formula,
      choice_alternatives = choice_alternatives,
      delimiter = "*"
    )
  )

  ### test 3
  choice_formula <- choice_formula(
    formula = choice ~ 0 | A + B + C + 0,
    error_term = "logit",
    random_effects = "A+"
  )
  choice_alternatives <- choice_alternatives(
    J = 3, ordered = TRUE
  )
  expect_equal(
    choice_effects(
      choice_formula = choice_formula,
      choice_alternatives = choice_alternatives
    ),
    structure(
      list(
        effect_name = c("B", "C", "A"),
        generic_name = c("alpha_1", "alpha_2", "b_1"),
        covariate = c("B", "C", "A"),
        alternative = c(NA_character_, NA_character_, NA_character_),
        as_covariate = c(FALSE, FALSE, FALSE),
        as_effect = c(FALSE, FALSE, FALSE),
        lc_effect = c(FALSE, FALSE, FALSE),
        mixing = structure(
          c(NA_integer_, NA_integer_, 2L),
          levels = c("normal", "log-normal"),
          class = c("ordered", "factor")
        )
      ),
      row.names = c(NA, -3L),
      class = c("choice_effects", "data.frame"),
      choice_formula = choice_formula,
      choice_alternatives = choice_alternatives,
      delimiter = "_"
    )
  )

  ### test 4
  choice_formula <- choice_formula(
    formula = choice ~ 0 | A + B + C + 0,
    error_term = "logit",
    random_effects = "A+",
    latent_classes = c("A", "B", "C")
  )
  choice_alternatives <- choice_alternatives(
    J = 3, ordered = TRUE
  )
  expect_equal(
    choice_effects(
      choice_formula = choice_formula,
      choice_alternatives = choice_alternatives
    ),
    structure(
      list(
        effect_name = c("B", "C", "A"),
        generic_name = c("alpha_1", "alpha_2", "b_1"),
        covariate = c("B", "C", "A"),
        alternative = c(NA_character_, NA_character_, NA_character_),
        as_covariate = c(FALSE, FALSE, FALSE),
        as_effect = c(FALSE, FALSE, FALSE),
        lc_effect = c(TRUE, TRUE, TRUE),
        mixing = structure(
          c(NA_integer_, NA_integer_, 2L),
          levels = c("normal", "log-normal"),
          class = c("ordered", "factor")
        )
      ),
      row.names = c(NA, -3L),
      class = c("choice_effects", "data.frame"),
      choice_formula = choice_formula,
      choice_alternatives = choice_alternatives,
      delimiter = "_"
    )
  )
})

test_that("misspecified effects can be detected", {
  expect_error(
    choice_effects(),
    "Please specify the input `choice_formula`"
  )
  expect_error(
    choice_effects(choice_formula = choice ~ A),
    "Input `choice_formula` must be an object of class"
  )
  expect_error(
    choice_effects(
      choice_formula = choice_formula(formula = A ~ B, error_term = "probit")
    ),
    "Please specify the input `choice_alternatives`"
  )
  expect_error(
    choice_effects(
      choice_formula = choice_formula(formula = A ~ B, error_term = "logit"),
      choice_alternatives = 2
    ),
    "Input `choice_alternatives` must be an object of class"
  )
  expect_error(
    choice_effects(
      choice_formula = choice_formula(formula = A ~ B, error_term = "probit"),
      choice_alternatives = choice_alternatives(J = 3),
      delimiter = 1
    ),
    "Input `delimiter` is bad: Must be of type 'string', not 'double'"
  )
  expect_error(
    is.choice_effects(1),
    "must be an object of class"
  )
})

test_that("printing effects works", {
  expect_snapshot(
    choice_effects(
      choice_formula = choice_formula(
        formula = choice ~ price | income | comfort,
        error_term = "probit",
        random_effects = c("price+", "income")
      ),
      choice_alternatives = choice_alternatives(J = 3)
    )
  )
})

test_that("number of effects can be computed", {
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ A | B + 0 | C + D,
      error_term = "logit",
      random_effects = c("A", "D+")
    ),
    choice_alternatives = choice_alternatives(
      J = 3
    )
  )
  expect_equal(compute_P(choice_effects), 9)
  expect_equal(compute_P_f(choice_effects), 5)
  expect_equal(compute_P_r(choice_effects), 4)
})

