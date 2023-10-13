test_that("choice set can be created", {
  expect_s3_class(
    probit_choice_set(
      probit_alternatives = probit_alternatives(J = 3), ranked = FALSE
    ),
    "probit_choice_set"
  )
})
