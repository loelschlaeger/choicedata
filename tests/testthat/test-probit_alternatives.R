test_that("probit_alternatives can be specified and validated", {
  expect_error(
    probit_alternatives(J = pi),
    "Assertion on 'J' failed: Must be of type 'single integerish value', not 'double'."
  )
  expect_error(
    probit_alternatives(J = 2, labels = diag(2)),
    "Assertion on 'labels' failed: Must be of type 'character', not 'matrix'."
  )
  expect_error(
    probit_alternatives(J = 3, labels = 1:3),
    "Assertion on 'labels' failed: Must be of type 'character', not 'integer'."
  )
  expect_error(
    probit_alternatives(J = 1),
    "Assertion on 'J' failed: Element 1 is not >= 2."
  )
  expect_error(
    probit_alternatives(J = 3, labels = c("1", "2")),
    "Assertion on 'labels' failed: Must have length 3, but has length 2."
  )
  expect_error(
    probit_alternatives(J = 2, labels = c("same", "same")),
    "Assertion on 'labels' failed: Contains duplicated values, position 2."
  )
  expect_error(
    probit_alternatives(J = 2, base = "C"),
    "Base alternative must be in alternative set."
  )
  expect_error(
    probit_alternatives(J = 2, labels = c("A", "B"), base = c("A", "B")),
    "Assertion on 'base' failed: Must have length 1."
  )
  expect_s3_class(
    probit_alternatives(J = 3, base = "C"),
    "probit_alternatives"
  )
  expect_true(
    is.probit_alternatives(probit_alternatives(J = 3, base = "C"))
  )
  expect_error(
    probit_alternatives(J = 3, ordered = "not_a_logical"),
    "Assertion on 'ordered' failed: Must be of type 'logical flag', not 'character'."
  )
  expect_s3_class(
    probit_alternatives(J = 3, ordered = TRUE),
    "probit_alternatives"
  )
  expect_true(
    is.probit_alternatives(probit_alternatives(J = 3, ordered = TRUE))
  )
  expect_error(
    probit_alternatives(J = 2, ordered = TRUE),
    "Assertion on 'J' failed: Element 1 is not >= 3."
  )
  expect_s3_class(
    probit_alternatives(J = 3, labels = c("la", "le", "lu")),
    "probit_alternatives"
  )
})

test_that("probit_alternatives can be printed", {
  expect_error(
    print.probit_alternatives(1),
    "Assertion on 'x' failed: Must inherit from class 'probit_alternatives', but has class 'numeric'."
  )
  expect_snapshot(
    probit_alternatives(J = 3, labels = c("la", "le", "lu"))
  )
  expect_snapshot(
    probit_alternatives(J = 4, ordered = TRUE)
  )
})
