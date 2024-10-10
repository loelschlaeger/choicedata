test_that("choice_formula can be specified", {
  expect_choice_formula <- function(
      object, var_types, ASC, mixing_types, latent_classes
    ) {
    expect(is.choice_formula(object), "bad class")
    expect(identical(object$var_types, var_types), "bad variable types")
    expect(identical(object$ASC, ASC), "bad ASC")
    expect(identical(object$mixing_types, mixing_types), "bad random effects")
    expect(identical(object$latent_classes, latent_classes), "bad latent classes")
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
  re1 <- character()
  re2 <- "A"
  re3 <- "A+"
  re4 <- c("A+", "B")
  re5 <- c("B", "ASC")
  re6 <- c("B", "ASC+")
  re7 <- c("A", "A+")
  lc1 <- character()
  lc2 <- re2
  lc3 <- re3
  expect_choice_formula(
    choice_formula(
      formula = f1, random_effects = re1, error_term = "probit", latent_classes = lc1
    ),
    var_types = list("A", character(), character()), ASC = TRUE,
    mixing_types = character(), latent_classes = character()
  )
  expect_choice_formula(
    choice_formula(
      formula = f1, error_term = "probit", latent_classes = lc2
    ),
    var_types = list("A", character(), character()), ASC = TRUE,
    mixing_types = character(), latent_classes = c("A")
  )
  expect_choice_formula(
    choice_formula(formula = f2, error_term = "probit", random_effects = re2),
    var_types = list("A", "B", character()), ASC = TRUE,
    mixing_types = c("A" = "normal"), latent_classes = character()
  )
  expect_choice_formula(
    choice_formula(formula = f2, error_term = "logit", random_effects = re3),
    var_types = list("A", "B", character()), ASC = TRUE,
    mixing_types = c("A" = "log-normal"), latent_classes = character()
  )
  expect_choice_formula(
    choice_formula(formula = f2, error_term = "logit", random_effects = re4),
    var_types = list("A", "B", character()), ASC = TRUE,
    mixing_types = c("A" = "log-normal", "B" = "normal"),
    latent_classes = character()
  )
  expect_choice_formula(
    choice_formula(formula = f6, error_term = "logit", random_effects = character()),
    var_types = list(character(), character(), "C"), ASC = FALSE,
    mixing_types = character(), latent_classes = character()
  )
  expect_choice_formula(
    choice_formula(formula = f7, error_term = "logit", random_effects = character()),
    var_types = list(character(), character(), "C"), ASC = TRUE,
    mixing_types = character(), latent_classes = character()
  )
  expect_choice_formula(
    choice_formula(formula = f8, error_term = "logit", random_effects = character()),
    var_types = list(c("A", "B"), character(), character()), ASC = TRUE,
    mixing_types = character(), latent_classes = character()
  )
  expect_choice_formula(
    choice_formula(formula = f8, error_term = "logit", random_effects = re5),
    var_types = list(c("A", "B"), character(), character()), ASC = TRUE,
    mixing_types = c("B" = "normal", "ASC" = "normal"),
    latent_classes = character()
  )
  expect_choice_formula(
    choice_formula(formula = f8, error_term = "logit", random_effects = re6),
    var_types = list(c("A", "B"), character(), character()), ASC = TRUE,
    mixing_types = c("B" = "normal", "ASC" = "log-normal"),
    latent_classes = character()
  )
})

test_that("misspecifications in choice_formula can be detected", {
  expect_error(
    choice_formula(),
    "Please specify the input `formula`"
  )
  expect_error(
    choice_formula(formula = "not_a_formula"),
    "Input `formula` is bad: Must be a formula, not character"
  )
  expect_error(
    choice_formula(formula = choice ~ A),
    "Please specify the input `error_term`"
  )
  expect_error(
    choice_formula(formula = choice ~ A, error_term = "bad"),
    "Input `error_term` is bad"
  )
  expect_error(
    choice_formula(formula = choice ~ A, error_term = "probit", random_effects = 1),
    "Input `random_effects` is bad"
  )
  expect_error(
    choice_formula(
      formula = choice ~ A, error_term = "logit", random_effects = "bad_covariate"
    ),
    "contains"
  )
  expect_error(
    choice_formula(formula = choice ~ ASC, error_term = "logit"),
    "are not allowed"
  )
  expect_error(
    choice_formula(formula = choice ~ A, error_term = "logit", random_effects = "B"),
    "but it is not on the right hand side of `formula`"
  )
  expect_error(
    choice_formula(formula = choice ~ A, error_term = "logit", latent_classes = "B"),
    "but it is not on the right hand side of `formula`"
  )
  expect_error(
    choice_formula(formula = choice ~ A, error_term = "probit", latent_classes = c("A", "A")),
    "Must have unique names, but element 2 is duplicated"
  )
  expect_error(
    choice_formula(formula = choice ~ A | 0, error_term = "probit", random_effects = "ASC"),
    "but it is not on the right hand side of `formula`"
  )
  expect_error(
    choice_formula(formula = choice ~ A, error_term = "probit", random_effects = c("A", "A+")),
    "Multiple random effects specifications"
  )
  expect_error(
    choice_formula(formula = ~ bad, error_term = "probit",),
    "`formula` must be of the form"
  )
  expect_error(
    choice_formula(formula = A ~ B | C | D | too_much, error_term = "probit",),
    "`formula` should have no more than two of '|' separators"
  )
  expect_error(
    choice_formula(formula = A ~ A, error_term = "logit"),
    "cannot occur on both sides of `formula`"
  )
  expect_error(
    choice_formula(formula = A ~ B | B, error_term = "logit"),
    "`formula` contains covariate"
  )
})

test_that("choice_formula can be printed", {
  expect_error(
    print.choice_formula(1),
    "Input `x` must be an object of class"
  )
  expect_snapshot(
    choice_formula(
      formula = choice ~ A | B, error_term = "logit", random_effects = character()
    )
  )
  expect_snapshot(
    choice_formula(
      formula = choice ~ A + B, error_term = "probit",
      random_effects = c("A+", "B")
    )
  )
  expect_snapshot(
    choice_formula(
      formula = choice ~ A + B, error_term = "logit",
      random_effects = c("A+", "B"), latent_class = c("A", "B")
    )
  )
})

