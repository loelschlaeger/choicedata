test_that("choice set (not ranked) can be created", {
  choice_set <- probit_choice_set(
    probit_alternatives = probit_alternatives(J = 3), ranked = FALSE
  )
  expect_s3_class(
    choice_set,
    "probit_choice_set"
  )
  expect_true(
    is.probit_choice_set(choice_set)
  )
  expect_snapshot(
    print(choice_set)
  )
})

test_that("choice set (ranked) can be created", {
  choice_set <- probit_choice_set(
    probit_alternatives = probit_alternatives(J = 3), ranked = TRUE
  )
  expect_s3_class(
    choice_set,
    "probit_choice_set"
  )
  expect_true(
    is.probit_choice_set(choice_set)
  )
  expect_snapshot(
    print(choice_set)
  )
})
