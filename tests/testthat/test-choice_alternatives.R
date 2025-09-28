test_that("choice_alternatives can be specified and validated", {
  expect_error(
    choice_alternatives(J = pi),
    "Input `J` is bad: Must be of type 'single integerish value', not 'double'"
  )
  expect_error(
    choice_alternatives(J = 2, alternatives = diag(2)),
    "Input `alternatives` is bad: Must be of type 'character', not 'matrix'"
  )
  expect_error(
    choice_alternatives(J = 3, alternatives = 1:3),
    "Input `alternatives` is bad: Must be of type 'character', not 'integer'"
  )
  expect_error(
    choice_alternatives(J = 1),
    "Input `J` is bad: Element 1 is not >= 2"
  )
  expect_error(
    choice_alternatives(J = 3, alternatives = c("1", "2")),
    "Input `alternatives` is bad: Must have length 3, but has length 2"
  )
  expect_error(
    choice_alternatives(J = 2, alternatives = c("same", "same")),
    "Input `alternatives` is bad: Contains duplicated values, position 2"
  )
  expect_error(
    choice_alternatives(J = 2, base = "C"),
    "Input `base` is bad: Must be element of set"
  )
  expect_error(
    choice_alternatives(J = 2, alternatives = c("A", "B"), base = c("A", "B")),
    "Input `base` is bad: Must be element of set"
  )
  expect_s3_class(
    choice_alternatives(J = 3, base = "C"),
    "choice_alternatives"
  )
  expect_true(
    is.choice_alternatives(choice_alternatives(J = 3, base = "C"))
  )
  expect_s3_class(
    choice_alternatives(J = 3, alternatives = c("la", "le", "lu")),
    "choice_alternatives"
  )
})

test_that("choice_alternatives can be printed", {
  expect_error(
    print.choice_alternatives(1),
    "Input `x` is bad"
  )
  expect_snapshot(
    choice_alternatives(J = 3, alternatives = c("la", "le", "lu"))
  )
})
