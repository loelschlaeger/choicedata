test_that("effect overview can be created", {
  expect_error(
    choice_effects(),
    "Please specify the input 'choice_formula'."
  )
  expect_error(
    choice_effects(choice_formula = choice ~ A),
    "Assertion on 'choice_formula' failed: Must inherit from class 'choice_formula', but has class 'formula'."
  )
  expect_error(
    choice_effects(choice_formula = choice_formula(formula = A ~ B)),
    "Please specify the input 'choice_alternatives'."
  )
  expect_error(
    choice_effects(
      choice_formula = choice_formula(formula = A ~ B),
      choice_alternatives = 2
    ),
    "Assertion on 'choice_alternatives' failed: Must inherit from class 'choice_alternatives', but has class 'numeric'."
  )
  expect_error(
    choice_effects(
      choice_formula = choice_formula(formula = A ~ B),
      choice_alternatives = choice_alternatives(J = 3),
      delimiter = 1
    ),
    "Assertion on 'delimiter' failed: Must be of type 'string', not 'double'."
  )
  expect_equal(
    choice_effects(
      choice_formula = choice_formula(
        formula = choice ~ cov,
        re = c("cov+", "ASC+")
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
        random = c(TRUE, TRUE, TRUE),
        log_normal = c(TRUE, TRUE, TRUE)
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
        random = c(FALSE, FALSE, FALSE, FALSE),
        log_normal = c(FALSE, FALSE, FALSE, FALSE)
      ),
      row.names = c(NA, -4L),
      class = c("choice_effects", "data.frame")
    )
  )
  expect_equal(
    choice_effects(
      choice_formula = choice_formula(
        formula = choice ~ A + B + C,
        re = "A+", ordered = TRUE
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
        random = c(FALSE, FALSE, TRUE),
        log_normal = c(FALSE, FALSE, TRUE)
      ),
      row.names = c(NA, -3L),
      class = c("choice_effects", "data.frame")
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

