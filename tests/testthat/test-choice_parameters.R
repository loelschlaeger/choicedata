test_that("input checks for probit parameter work", {
  expect_error(
    choice_parameters(C = 2),
    "Input `C` is bad: Must be equal to 1"
  )
  expect_error(
    choice_parameters(latent_classes = "both", C = 3.1),
    "Input `C` is bad: Must be of type 'single integerish value', not 'double'"
  )
  expect_error(
    choice_parameters(s = "not_a_numeric"),
    "Input `s` is bad: Must be of type 'numeric', not 'character'"
  )
  expect_error(
    choice_parameters(alpha = "not_a_numeric"),
    "Input `alpha` is bad: Must be of type 'numeric', not 'character'"
  )
  expect_error(
    choice_parameters(b = "not_a_numeric"),
    "Input `b` is bad: Must be of type 'numeric', not 'character'"
  )
  expect_error(
    choice_parameters(Omega = "not_a_numeric"),
    "Input `Omega` is bad: Must be of type 'numeric', not 'character'"
  )
  expect_error(
    choice_parameters(Sigma = "not_a_numeric"),
    "Input `Sigma` is bad: Must be of type 'numeric', not 'character'"
  )
  expect_error(
    choice_parameters(gamma = "not_a_numeric"),
    "Input `gamma` is bad: Must be of type 'numeric', not 'character'"
  )
})

test_that("probit parameter can be created", {
  x <- choice_parameters()
  expect_true(is.choice_parameters(x))
  expect_s3_class(x, "choice_parameters")
  choice_formula <- choice_formula(
    formula = choice ~ A | 0 + B, re = "B"
  )
  set.seed(1)
  x <- sample_choice_parameters(x, choice_formula = choice_formula, J = 3)
  expect_s3_class(x, "choice_parameters")
  expect_snapshot(print(x))
})

test_that("probit parameter can be validated", {
  expect_error(
    validate_choice_parameters(choice_parameters = choice_parameters()),
    "Please specify the input `choice_formula`"
  )
  expect_error(
    validate_choice_parameters(
      choice_parameters = choice_parameters(),
      choice_formula = choice_formula(formula = A ~ B)
    ),
    "Please specify the input `J`"
  )
  expect_error(
    validate_choice_parameters(
      choice_parameters = choice_parameters(
        latent_classes = "both", C = 3, s = 1:3
      ),
      choice_formula = choice_formula(formula = A ~ B),
      J = 3
    ),
    "Parameter `s` is bad: Element 2 is not <= 1"
  )
  expect_error(
    validate_choice_parameters(
      choice_parameters = choice_parameters(
        latent_classes = "both", C = 3, s = c(0.5, 0.1, 0.4)
      ),
      choice_formula = choice_formula(formula = A ~ B),
      J = 3
    ),
    "Parameter `s` is bad: Must be descending"
  )
  expect_s3_class(
    validate_choice_parameters(
      choice_parameters = choice_parameters(
        alpha = 1:3, Sigma = diag(3)
      ),
      choice_formula = choice_formula(formula = A ~ B),
      J = 3
    ),
    "choice_parameters"
  )
  expect_s3_class(
    validate_choice_parameters(
      choice_parameters = choice_parameters(
        latent_classes = "re", b = matrix(1:6, ncol = 2), C = 2,
      ),
      choice_formula = choice_formula(
        formula = choice ~ A + B + C, re = c("A", "B", "C"), ordered = TRUE
      ),
      J = 3,
      allow_missing = TRUE
    ),
    "choice_parameters"
  )
  expect_s3_class(
    validate_choice_parameters(
      choice_parameters = choice_parameters(
        b = 1:2, Omega = matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2),
        Sigma = 1, gamma = 1:2
      ),
      choice_formula = choice_formula(
        formula = choice ~ A + B, ordered = TRUE, re = c("A", "B")
      ),
      J = 3
    ),
    "choice_parameters"
  )
})
