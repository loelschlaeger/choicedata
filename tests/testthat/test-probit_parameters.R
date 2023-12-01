test_that("input checks for probit parameter work", {
  expect_error(
    probit_parameter(C = 3.1),
    "Assertion on 'C' failed: Must be of type 'count', not 'double'."
  )
  expect_error(
    probit_parameter(s = "not_a_numeric"),
    "Assertion on 's' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    probit_parameter(alpha = "not_a_numeric"),
    "Assertion on 'alpha' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    probit_parameter(b = "not_a_numeric"),
    "Assertion on 'b' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    probit_parameter(Omega = "not_a_numeric"),
    "Assertion on 'Omega' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    probit_parameter(Sigma = "not_a_numeric"),
    "Assertion on 'Sigma' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    probit_parameter(Sigma_diff = "not_a_numeric"),
    "Assertion on 'Sigma_diff' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    probit_parameter(diff_alt = 3.1),
    "Assertion on 'diff_alt' failed: Must be of type 'count', not 'double'."
  )
  expect_error(
    probit_parameter(beta = "not_a_numeric"),
    "Assertion on 'beta' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    probit_parameter(z = "not_a_numeric"),
    "Assertion on 'z' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    probit_parameter(d = "not_a_numeric"),
    "Assertion on 'd' failed: Must be of type 'numeric', not 'character'."
  )
})

test_that("probit parameter can be created", {
  x <- probit_parameter()
  expect_true(is.probit_parameter(x))
  expect_s3_class(x, "probit_parameter")
  formula <- choice ~ A | B
  re <- "A"
  J <- 3
  N <- 100
  x <- sample_probit_parameter(
    x, formula = formula, re = re, J = J, N = N, seed = 1
  )
  expect_s3_class(x, "probit_parameter")
  expect_snapshot(print(x))
  expect_snapshot(print(x, "beta"))
  expect_error(
    print.probit_parameter(1),
    "Assertion on 'x' failed: Must inherit from class 'probit_parameter', but has class 'numeric'."
  )
  x <- sample_probit_parameter(
    formula = choice ~ A + B, re = c("A", "B"), ordered = TRUE, J = 3, N = 10,
    seed = 1
  )
  expect_s3_class(x, "probit_parameter")
  expect_snapshot(print(x))
  expect_snapshot(print(x, "d"))
})

test_that("missing probit parameters can be simulated", {
  expect_error(
    sample_probit_parameter(x = "bad_object"),
    "Assertion on 'x' failed: Must inherit from class 'probit_parameter', but has class 'character'."
  )
  expect_error(
    sample_probit_parameter(x = probit_parameter()),
    "Please specify the model 'formula'."
  )
  expect_error(
    sample_probit_parameter(x = probit_parameter(), formula = A ~ B),
    "Please specify the number 'J' of choice alternatives."
  )
  expect_error(
    sample_probit_parameter(x = probit_parameter(), formula = A ~ B, J = 3),
    "Please specify the number of deciders 'N'."
  )
  expect_true(
    is.probit_parameter(
      sample_probit_parameter(
        x = probit_parameter(C = 2), formula = A ~ B, J = 3, N = 10
      )
    )
  )
  expect_true(
    is.probit_parameter(
      sample_probit_parameter(
        x = probit_parameter(C = 2), formula = A ~ B, J = 3, N = 10,
        ordered = TRUE
      )
    )
  )
  expect_true(
    is.probit_parameter(
      sample_probit_parameter(
        x = probit_parameter(C = 2, Sigma = diag(3)), formula = A ~ B, J = 3,
        N = 10, ordered = FALSE
      )
    )
  )
})

test_that("probit parameter can be validated", {
  expect_error(
    validate_probit_parameter(x = "bad_object"),
    "Assertion on 'x' failed: Must inherit from class 'probit_parameter', but has class 'character'."
  )
  expect_error(
    validate_probit_parameter(x = probit_parameter()),
    "Please specify the input 'formula'."
  )
  expect_error(
    validate_probit_parameter(x = probit_parameter(), formula = A ~ B),
    "Please specify the number of choice alternatives 'J'."
  )
  expect_error(
    validate_probit_parameter(x = probit_parameter(), formula = A ~ B, J = 3),
    "Please specify the number of deciders 'N'."
  )
  expect_error(
    validate_probit_parameter(
      x = probit_parameter(C = 3, s = 1:3), formula = A ~ B, J = 3, N = 10
    ),
    "'s' must sum up to 1."
  )
  expect_error(
    validate_probit_parameter(
      x = probit_parameter(C = 3, s = c(0.5, 0.1, 0.4)), formula = A ~ B, J = 3, N = 10
    ),
    "'s' must be descending."
  )
  expect_s3_class(
    validate_probit_parameter(
      x = probit_parameter(
        alpha = 1:3, Sigma = diag(3), z = rep(1, 10)
      ), formula = A ~ B, J = 3, N = 10
    ),
    "probit_parameter"
  )
  expect_s3_class(
    validate_probit_parameter(
      x = probit_parameter(b = matrix(1:6, ncol = 2), C = 2),
      formula = choice ~ A + B + C, J = 3, N = 10, ordered = TRUE, re = c("A", "B", "C")
    ),
    "probit_parameter"
  )
  expect_s3_class(
    validate_probit_parameter(
      x = probit_parameter(C = 2, alpha = 1:2),
      formula = choice ~ A + B + C, J = 3, N = 10, ordered = TRUE, re = c("A", "B")
    ),
    "probit_parameter"
  )
  expect_s3_class(
    validate_probit_parameter(
      x = probit_parameter(C = 1, b = 1:2, Omega = c(1, 0.5, 0.5, 1)),
      formula = choice ~ A + B + C, J = 3, N = 10, ordered = TRUE, re = c("A", "B")
    ),
    "probit_parameter"
  )
  expect_s3_class(
    validate_probit_parameter(
      x = probit_parameter(C = 2, b = 1:2, Omega = 1:2),
      formula = choice ~ A + B + C, J = 3, N = 10, ordered = TRUE, re = c("A")
    ),
    "probit_parameter"
  )
  expect_s3_class(
    validate_probit_parameter(
      x = probit_parameter(C = 1, b = 1:2, beta = matrix(1:20, nrow = 2, ncol = 10), Omega = c(1, 0.5, 0.5, 1)),
      formula = choice ~ A + B + C, J = 3, N = 10, ordered = TRUE, re = c("A", "B")
    ),
    "probit_parameter"
  )
  expect_s3_class(
    validate_probit_parameter(
      x = probit_parameter(C = 2, b = 1:2, beta = matrix(1:10, nrow = 1, ncol = 10), Omega = 1:2),
      formula = choice ~ A + B + C, J = 3, N = 10, ordered = TRUE, re = c("A")
    ),
    "probit_parameter"
  )
  expect_s3_class(
    validate_probit_parameter(
      x = probit_parameter(C = 2, beta = 1:10),
      formula = choice ~ A + B + C, J = 3, N = 10, ordered = TRUE, re = c("A")
    ),
    "probit_parameter"
  )
  expect_s3_class(
    validate_probit_parameter(
      x = probit_parameter(beta = 1:2),
      formula = choice ~ A + B + C, J = 3, N = 1, ordered = TRUE, re = c("A", "B")
    ),
    "probit_parameter"
  )
})

test_that("coefficient vector for decider can be extracted", {
  probit_parameter <- sample_probit_parameter(
    formula = A ~ B | C, re = "B", J = 3, N = 10
  )
  checkmate::expect_numeric(
    get_coefficient_vector(probit_parameter, decider_id = 1),
    any.missing = FALSE, len = 5
  )
  expect_error(
    get_coefficient_vector(probit_parameter, decider_id = 11),
    "Assertion on 'decider_id' failed: Element 1 is not <= 10."
  )
})


