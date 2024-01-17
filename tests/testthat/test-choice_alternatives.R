test_that("choice_alternatives can be specified and validated", {
  expect_error(
    choice_alternatives(J = pi),
    "Assertion on 'J' failed: Must be of type 'single integerish value', not 'double'."
  )
  expect_error(
    choice_alternatives(J = 2, alternatives = diag(2)),
    "Assertion on 'alternatives' failed: Must be of type 'character', not 'matrix'."
  )
  expect_error(
    choice_alternatives(J = 3, alternatives = 1:3),
    "Assertion on 'alternatives' failed: Must be of type 'character', not 'integer'."
  )
  expect_error(
    choice_alternatives(J = 1),
    "Assertion on 'J' failed: Element 1 is not >= 2."
  )
  expect_error(
    choice_alternatives(J = 3, alternatives = c("1", "2")),
    "Assertion on 'alternatives' failed: Must have length 3, but has length 2."
  )
  expect_error(
    choice_alternatives(J = 2, alternatives = c("same", "same")),
    "Assertion on 'alternatives' failed: Contains duplicated values, position 2."
  )
  expect_error(
    choice_alternatives(J = 2, base = "C"),
    "Base alternative must be in alternative set."
  )
  expect_error(
    choice_alternatives(J = 2, alternatives = c("A", "B"), base = c("A", "B")),
    "Assertion on 'base' failed: Must have length 1."
  )
  expect_s3_class(
    choice_alternatives(J = 3, base = "C"),
    "choice_alternatives"
  )
  expect_true(
    is.choice_alternatives(choice_alternatives(J = 3, base = "C"))
  )
  expect_error(
    choice_alternatives(J = 3, ordered = "not_a_logical"),
    "Assertion on 'ordered' failed: Must be of type 'logical flag', not 'character'."
  )
  expect_s3_class(
    choice_alternatives(J = 3, ordered = TRUE),
    "choice_alternatives"
  )
  expect_true(
    is.choice_alternatives(choice_alternatives(J = 3, ordered = TRUE))
  )
  expect_error(
    choice_alternatives(J = 2, ordered = TRUE),
    "Assertion on 'J' failed: Element 1 is not >= 3."
  )
  expect_s3_class(
    choice_alternatives(J = 3, alternatives = c("la", "le", "lu")),
    "choice_alternatives"
  )
})

test_that("choice_alternatives can be printed", {
  expect_error(
    print.choice_alternatives(1),
    "Assertion on 'x' failed: Must inherit from class 'choice_alternatives', but has class 'numeric'."
  )
  expect_snapshot(
    choice_alternatives(J = 3, alternatives = c("la", "le", "lu"))
  )
  expect_snapshot(
    choice_alternatives(J = 4, ordered = TRUE)
  )
})
