test_that("effect overview can be created", {
  expect_error(
    choice_effects(),
    "Please specify the input `choice_formula`"
  )
  expect_error(
    choice_effects(choice_formula = choice ~ A),
    "Input `choice_formula` must be an object of class"
  )
  expect_error(
    choice_effects(choice_formula = choice_formula(formula = A ~ B)),
    "Please specify the input `choice_alternatives`"
  )
  expect_error(
    choice_effects(
      choice_formula = choice_formula(formula = A ~ B),
      choice_alternatives = 2
    ),
    "Input `choice_alternatives` must be an object of class"
  )
  expect_error(
    choice_effects(
      choice_formula = choice_formula(formula = A ~ B),
      choice_alternatives = choice_alternatives(J = 3),
      delimiter = 1
    ),
    "Input `delimiter` is bad: Must be of type 'string', not 'double'"
  )
  expect_equal(
    choice_effects(
      choice_formula = choice_formula(
        formula = choice ~ cov,
        re = c("cov", "ASC+")
      ),
      choice_alternatives = choice_alternatives(
        J = 3,
        alternatives = c("C", "B", "A"),
        base = "B"
      )
    ),
    structure(
      list(
        name = c("cov", "ASC_A", "ASC_C"),
        covariate = c("cov", NA, NA),
        alternative = c(NA, "A", "C"),
        as_covariate = c(TRUE, FALSE, FALSE),
        as_effect = c(FALSE, TRUE, TRUE),
        mixing = structure(
          c(1L, 2L, 2L),
          levels = c("normal", "log-normal"),
          class = c("ordered", "factor")
        )
      ),
      row.names = c(NA, -3L),
      class = c("choice_effects", "data.frame")
    )
  )
  expect_equal(
    choice_effects(
      choice_formula = choice_formula(
        formula = choice ~ A | B + 0 | C,
        re = NULL
      ),
      choice_alternatives = choice_alternatives(
        J = 2,
        alternatives = c("A", "B")
      ),
      delimiter = "*"
    ),
    structure(
      list(
        name = c("A", "B*B", "C*A", "C*B"),
        covariate = c("A", "B", "C", "C"),
        alternative = c(NA, "B", "A", "B"),
        as_covariate = c(TRUE, FALSE, TRUE, TRUE),
        as_effect = c(FALSE, TRUE, TRUE, TRUE),
        mixing = structure(
          c(NA_integer_, NA_integer_, NA_integer_, NA_integer_),
          levels = c("normal", "log-normal"),
          class = c("ordered", "factor")
        )
      ),
      row.names = c(NA, -4L),
      class = c("choice_effects", "data.frame")
    )
  )
  expect_equal(
    choice_effects(
      choice_formula = choice_formula(
        formula = choice ~ 0 | A + B + C + 0,
        re = "A+"
      ),
      choice_alternatives = choice_alternatives(
        J = 3, ordered = TRUE
      )
    ),
    structure(
      list(
        name = c("B", "C", "A"),
        covariate = c("B", "C", "A"),
        alternative = c(NA_character_, NA_character_, NA_character_),
        as_covariate = c(FALSE, FALSE, FALSE),
        as_effect = c(FALSE, FALSE, FALSE),
        mixing = structure(
          c(NA_integer_, NA_integer_, 2L),
          levels = c("normal", "log-normal"),
          class = c("ordered", "factor")
        )
      ),
      row.names = c(NA, -3L),
      class = c("choice_effects", "data.frame")
    )
  )
  expect_error(
    is.choice_effects(1),
    "must be an object of class"
  )
  expect_snapshot(
    choice_effects(
      choice_formula = choice_formula(
        formula = choice ~ price | income | comfort,
        re = c("price+", "income")
      ),
      choice_alternatives = choice_alternatives(J = 3)
    )
  )
})

test_that("number of effects can be computed", {
  formula <- choice ~ A | B + 0 | C + D
  re <- c("A", "D+")
  J <- 3
  expect_equal(compute_P(formula, re, J), 9)
  expect_equal(compute_P_f(formula, re, J), 5)
  expect_equal(compute_P_r(formula, re, J), 4)
})

