test_that("choice_formula can be specified and validated", {
  expect_choice_formula <- function(object, var_types, ASC, re_n, re_ln) {
    expect(inherits(object, "choice_formula"), "bad class")
    expect(is.choice_formula(object), "bad formula")
    expect(identical(object$var_types, var_types), "bad variable types")
    expect(identical(object$ASC, ASC), "bad ASC")
    expect(identical(object$re_n, re_n), "bad normal random effects")
    expect(identical(object$re_ln, re_ln), "bad log-normal random effects")
  }
  f1 <- choice ~ A
  f2 <- choice ~ A | B
  f3 <- choice ~ A | B | C
  f4 <- choice ~ A | B + 0 | C
  f5 <- choice ~ A | B + 0 | C + D
  f6 <- choice ~ 0 | 0 | C
  f7 <- choice ~ 0 | 1 | C
  f8 <- choice ~ A + B
  f9 <- choice ~ A + B + 1
  re1 <- NULL
  re2 <- "A"
  re3 <- "A+"
  re4 <- c("A+", "B")
  re5 <- c("B", "ASC")
  re6 <- c("B", "ASC+")
  re7 <- c("A", "A+")
  expect_error(
    choice_formula(),
    "Please specify the input 'formula'."
  )
  expect_error(
    choice_formula(formula = "not_a_formula", re = re1, ordered = FALSE),
    "Assertion on 'formula' failed: Must be a formula, not character."
  )
  expect_error(
    choice_formula(formula = f1, re = 1, ordered = FALSE),
    "Assertion on 're' failed: Must be of type 'character', not 'double'."
  )
  expect_error(
    choice_formula(formula = f1, re = re1, ordered = "not_a_boolean"),
    "Assertion on 'ordered' failed: Must be of type 'logical flag', not 'character'."
  )
  expect_error(
    choice_formula(formula = f1, re = "bad_covariate", ordered = FALSE),
    "'re' contains 'bad_covariate', but it's not on the right side of 'formula'."
  )
  expect_choice_formula(
    choice_formula(formula = f1, re = re1, ordered = FALSE),
    var_types = list("A", character(), character()), ASC = TRUE,
    re_n = character(), re_ln = character()
  )
  expect_choice_formula(
    choice_formula(formula = f2, re = re2, ordered = FALSE),
    var_types = list("A", "B", character()), ASC = TRUE,
    re_n = "A", re_ln = character()
  )
  expect_choice_formula(
    choice_formula(formula = f2, re = re3, ordered = FALSE),
    var_types = list("A", "B", character()), ASC = TRUE,
    re_n = character(), re_ln = "A"
  )
  expect_choice_formula(
    choice_formula(formula = f2, re = re4, ordered = FALSE),
    var_types = list("A", "B", character()), ASC = TRUE,
    re_n = "B", re_ln = "A"
  )
  expect_choice_formula(
    choice_formula(formula = f6, re = character(), ordered = FALSE),
    var_types = list(character(), character(), "C"), ASC = FALSE,
    re_n = character(), re_ln = character()
  )
  expect_choice_formula(
    choice_formula(formula = f7, re = character(), ordered = FALSE),
    var_types = list(character(), character(), "C"), ASC = TRUE,
    re_n = character(), re_ln = character()
  )
  expect_choice_formula(
    choice_formula(formula = f8, re = character(), ordered = FALSE),
    var_types = list(c("A", "B"), character(), character()), ASC = TRUE,
    re_n = character(), re_ln = character()
  )
  expect_choice_formula(
    choice_formula(formula = f9, re = re4, ordered = TRUE),
    var_types = list(character(), c("A", "B"), character()), ASC = FALSE,
    re_n = "B", re_ln = "A"
  )
  expect_error(
    choice_formula(formula = f3, re = character(), ordered = TRUE),
    "Vertical bars in 'formula' are not allowed in the ordered case."
  )
  expect_choice_formula(
    choice_formula(formula = f8, re = re5, ordered = FALSE),
    var_types = list(c("A", "B"), character(), character()), ASC = TRUE,
    re_n = c("B", "ASC"), re_ln = character()
  )
  expect_error(
    choice_formula(formula = f5, re = re5, ordered = FALSE),
    "'re' contains 'ASC', but it's not on the right side of 'formula'."
  )
  expect_choice_formula(
    choice_formula(formula = f8, re = re6, ordered = FALSE),
    var_types = list(c("A", "B"), character(), character()), ASC = TRUE,
    re_n = "B", re_ln = "ASC"
  )
  expect_error(
    choice_formula(formula = f2, re = re7, ordered = FALSE),
    "cannot include both"
  )
  expect_error(
    choice_formula(formula = ~ bad),
    "'formula' should be in the form '<choice> ~ <covariates>'."
  )
  expect_error(
    choice_formula(formula = A ~ B | C | D | too_much),
    "'formula' should have no more than 2 of '|' separators."
  )
  expect_error(
    choice_formula(formula = A ~ A),
    "Variable 'A' cannot occur on both sides of 'formula'."
  )
  expect_error(
    choice_formula(formula = A ~ B | B),
    "'formula' contains covariate 'B' multiple times."
  )
})

test_that("choice_formula can be printed", {
  expect_error(
    print.choice_formula(1),
    "Assertion on 'x' failed: Must inherit from class 'choice_formula', but has class 'numeric'."
  )
  expect_snapshot(
    choice_formula(formula = choice ~ A | B, re = NULL, ordered = FALSE)
  )
  expect_snapshot(
    choice_formula(formula = choice ~ A + B, re = c("A+", "B"), ordered = TRUE)
  )
})
