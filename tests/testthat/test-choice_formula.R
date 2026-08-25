test_that("choice_formula can be specified", {
  expect_choice_formula <- function(
      object, error_term, choice, covariate_types, as_covariates, ac_covariates,
      ASC, random_effects
    ) {
    expect(is.choice_formula(object), "bad class")
    expect(Formula::is.Formula(object$formula), "bad formula")
    expect(identical(object$error_term, error_term), "bad error_term")
    expect(identical(object$choice, choice), "bad choice")
    expect(
      identical(object$covariate_types, covariate_types),
      "bad covariate_types"
    )
    expect(identical(object$ASC, ASC), "bad ASC")
    expect(
      identical(object$random_effects, random_effects),
      "bad random_effects"
    )
  }
  expect_choice_formula(
    choice_formula(
      formula = choice ~ A
    ),
    error_term = "probit",
    choice = "choice",
    covariate_types = list("A", character(), character()),
    as_covariates = "A",
    ac_covariates = character(),
    ASC = TRUE,
    random_effects = structure(character(0), names = character(0))
  )
  expect_choice_formula(
    choice_formula(
      formula = choice ~ I(A + B)
    ),
    error_term = "probit",
    choice = "choice",
    covariate_types = list("I(A+B)", character(), character()),
    as_covariates = c("A", "B"),
    ac_covariates = character(),
    ASC = TRUE,
    random_effects = structure(character(0), names = character(0))
  )
  expect_choice_formula(
    choice_formula(
      formula = choice ~ A | B,
      random_effects = c("A" = "cn")
    ),
    error_term = "probit",
    choice = "choice",
    covariate_types = list("A", "B", character()),
    as_covariates = "A",
    ac_covariates = "B",
    ASC = TRUE,
    random_effects = c("A" = "cn")
  )
  expect_choice_formula(
    choice_formula(
      formula = choice ~ A | B,
      random_effects = c("A" = "cn", "B" = "cln")
    ),
    error_term = "probit",
    choice = "choice",
    covariate_types = list("A", "B", character()),
    as_covariates = "A",
    ac_covariates = "B",
    ASC = TRUE,
    random_effects = c("A" = "cn", "B" = "cln")
  )
  expect_choice_formula(
    choice_formula(
      formula = choice ~ A | B,
      random_effects = c("B" = "cln", "ASC" = "cln-")
    ),
    error_term = "probit",
    choice = "choice",
    covariate_types = list("A", "B", character()),
    as_covariates = "A",
    ac_covariates = "B",
    ASC = TRUE,
    random_effects = c("B" = "cln", "ASC" = "cln-")
  )
  uncorrelated <- choice_formula(
    formula = choice ~ A | B + 0 | C,
    random_effects = c("A" = "n", "B" = "ln", "C" = "ln-")
  )
  expect_identical(
    uncorrelated$random_effects,
    c("A" = "n", "B" = "ln", "C" = "ln-")
  )
  expect_choice_formula(
    choice_formula(
      formula = choice ~ 0 | 0 | C
    ),
    error_term = "probit",
    choice = "choice",
    covariate_types = list(character(), character(), "C"),
    as_covariates = "C",
    ac_covariates = character(),
    ASC = FALSE,
    random_effects = structure(character(0), names = character(0))
  )
  expect_choice_formula(
    choice_formula(
      formula = choice ~ 0 | 1 | C
    ),
    error_term = "probit",
    choice = "choice",
    covariate_types = list(character(), character(), "C"),
    as_covariates = "C",
    ac_covariates = character(),
    ASC = TRUE,
    random_effects = structure(character(0), names = character(0))
  )
  expect_choice_formula(
    choice_formula(
      formula = choice ~ A + B
    ),
    error_term = "probit",
    choice = "choice",
    covariate_types = list(c("A", "B"), character(), character()),
    as_covariates = c("A", "B"),
    ac_covariates = character(),
    ASC = TRUE,
    random_effects = structure(character(0), names = character(0))
  )
  expect_choice_formula(
    choice_formula(
      formula = choice ~ A + B,
      random_effects = c("B" = "cn", "ASC" = "cn")
    ),
    error_term = "probit",
    choice = "choice",
    covariate_types = list(c("A", "B"), character(), character()),
    as_covariates = c("A", "B"),
    ac_covariates = character(),
    ASC = TRUE,
    random_effects = c("B" = "cn", "ASC" = "cn")
  )
  expect_choice_formula(
    choice_formula(
      formula = choice ~ I(A^2 + 1) | B | I(log(C)),
      random_effects = c("I(A^2 + 1)" = "cn", "B" = "cn")
    ),
    error_term = "probit",
    choice = "choice",
    covariate_types = list("I(A^2+1)", "B", "I(log(C))"),
    as_covariates = c("A", "C"),
    ac_covariates = "B",
    ASC = TRUE,
    random_effects = c("I(A^2+1)" = "cn", "B" = "cn")
  )
})

test_that("misspecifications in choice_formula can be detected", {
  expect_error(
    choice_formula(),
    "Please specify the input `formula`"
  )
  expect_error(
    choice_formula(
      formula = "not_a_formula"
    ),
    "Input `formula` is bad: Must be a formula, not character"
  )
  expect_error(
    choice_formula(
      formula = choice ~ A,
      error_term = "bad"
    ),
    "Input `error_term` is bad"
  )
  expect_error(
    choice_formula(
      formula = choice ~ A,
      random_effects = 1
    ),
    "Input `random_effects` is bad"
  )
  expect_error(
    choice_formula(
      formula = choice ~ A,
      random_effects = c("A" = "log-normal")
    ),
    "Input `random_effects` is bad"
  )
  expect_error(
    choice_formula(
      formula = choice ~ A,
      random_effects = c("bad_covariate" = "cn")
    ),
    "contains"
  )
  expect_error(
    choice_formula(
      formula = I(choice^2) ~ A,
    ),
    "Transformation of the left-hand side of `formula` is not allowed"
  )
  expect_error(
    choice_formula(
      formula = choice ~ ASC,
    ),
    "is not allowed"
  )
  expect_error(
    choice_formula(
      formula = choice ~ A,
      random_effects = c("B" = "cn")
    ),
    "but it is not on the right-hand side of `formula`"
  )
  expect_error(
    choice_formula(
      formula = choice ~ A,
      random_effects = c("." = "cn")
    ),
    "cannot use"
  )
  expect_error(
    choice_formula(
      formula = choice ~ A | 0,
      random_effects = c("ASC" = "cn")
    ),
    "but it is not on the right-hand side of `formula`"
  )
  expect_error(
    choice_formula(
      formula = ~ bad,
      error_term = "probit"
    ),
    "Input `formula` must have exactly one left-hand side"
  )
  expect_error(
    choice_formula(
      formula = A ~ B | C | D | too_much,
      error_term = "probit"
    ),
    "Input `formula` must have no more than two '|' separators"
  )
})

test_that("choice_formula can be printed", {
  expect_error(
    print.choice_formula(1),
    "Input `x` is bad"
  )
  messages <- capture.output(
    print(choice_formula(
      formula = choice ~ A | B
    )),
    type = "message"
  )
  text <- paste(messages, collapse = "\n")
  expect_match(text, "choice ~ A | B | 0", fixed = TRUE)
  messages <- capture.output(
    print(choice_formula(
      formula = choice ~ A + B,
      random_effects = c("A" = "cn", "B" = "cn")
    )),
    type = "message"
  )
  text <- paste(messages, collapse = "\n")
  expect_match(text, "random effects", fixed = TRUE)
  expect_match(text, "A: cn", fixed = TRUE)
  expect_match(text, "B: cn", fixed = TRUE)
})

test_that("choice_formula can be resolved", {
  data_frame <- data.frame(
    deciderID = c(1, 2),
    choice = c("X", "Y"),
    price_X = c(10, 20),
    price_Y = c(30, 40),
    brand = factor(c("A", "B"))
  )
  choice_data <- choice_data(
    data_frame = data_frame,
    format = "wide"
  )
  choice_formula <- choice_formula(
    formula = choice ~ 0 | brand | price:brand,
    random_effects = c("brand" = "cln")
  )
  choice_formula_resolved <- resolve_choice_formula(
    choice_formula = choice_formula, x = choice_data
  )

  ### check covariate types
  new_covariate_types <- choice_formula_resolved$covariate_types
  expect_identical(new_covariate_types[[1]], character(0))
  expect_identical(new_covariate_types[[2]], "brandB")
  expect_identical(new_covariate_types[[3]], c("price:brandA", "price:brandB"))

  ### check random effects
  new_random_effects <- choice_formula_resolved$random_effects
  expect_identical(names(new_random_effects), "brandB")
  expect_identical(unname(new_random_effects), "cln")

  ### resolve '.'
  choice_formula_2 <- choice_formula(formula = choice ~ .)
  choice_formula_resolved_2 <- resolve_choice_formula(
    choice_formula = choice_formula_2, x = choice_data
  )
  new_covariate_types_2 <- choice_formula_resolved_2$covariate_types
  expect_identical(
    new_covariate_types_2[[1]],
    c("deciderID2", "choice", "alternativeY", "brandB", "price")
  )

  double <- function(x) 2 * x
  local_formula <- choice_formula(choice ~ I(double(price)))
  local_formula <- resolve_choice_formula(local_formula, choice_data)
  expect_identical(
    local_formula$covariate_types[[1]],
    "I(double(price))"
  )
  expect_error(
    resolve_choice_formula(choice_formula(choice ~ unknown), choice_data),
    "unknown"
  )
})
