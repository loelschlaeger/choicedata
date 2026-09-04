test_that("effect overview can be created", {

  ### test 1: MMNP with type-1 covariates only
  choice_formula <- choice_formula(
    formula = choice ~ cov,
    random_effects = c("cov" = "cln", "ASC" = "cln-")
  )
  choice_alternatives <- choice_alternatives(
    J = 3,
    alternatives = c("C", "B", "A"),
    base = "B"
  )
  expect_equal(
    choice_effects(
      choice_formula = choice_formula,
      choice_alternatives = choice_alternatives
    ),
    structure(
      list(
        effect_name = c("cov", "ASC_A", "ASC_C"),
        generic_name = c("beta_1", "beta_2", "beta_3"),
        covariate = c("cov", NA, NA),
        alternative = c(NA, "A", "C"),
        as_covariate = c(TRUE, FALSE, FALSE),
        as_effect = c(FALSE, TRUE, TRUE),
        mixing = structure(
          c(2L, 3L, 3L),
          levels = c("cn", "cln", "cln-", "n", "ln", "ln-"),
          class = c("ordered", "factor")
        )
      ),
      row.names = c(NA, 3L),
      class = c("choice_effects", "data.frame"),
      choice_formula = choice_formula,
      choice_alternatives = choice_alternatives,
      delimiter = "_"
    )
  )

  effect_data <- choice_data(
    data_frame = data.frame(
      deciderID = rep(1:2, each = 3),
      alternative = rep(c("A", "B", "C"), 2),
      choice = c(1, 0, 0, 0, 1, 0),
      cov = seq_len(6)
    ),
    format = "long",
    column_decider = "deciderID",
    column_alternative = "alternative",
    column_as_covariates = "cov"
  )
  resolved <- choice_effects(
    choice_formula = choice_formula,
    choice_alternatives = choice_alternatives,
    choice_data = effect_data
  )
  expect_equal(
    as.character(resolved$mixing),
    c("cln", "cln-", "cln-")
  )

  ### test 2: MNP with different types
  choice_formula <- choice_formula(
    formula = choice ~ A | B + 0 | C
  )
  choice_alternatives <- choice_alternatives(
    J = 2,
    alternatives = c("A", "B")
  )
  expect_equal(
    choice_effects(
      choice_formula = choice_formula,
      choice_alternatives = choice_alternatives,
      delimiter = "*"
    ),
    structure(
      list(
        effect_name = c("A", "B*B", "C*A", "C*B"),
        generic_name = c("beta_1", "beta_2", "beta_3", "beta_4"),
        covariate = c("A", "B", "C", "C"),
        alternative = c(NA, "B", "A", "B"),
        as_covariate = c(TRUE, FALSE, TRUE, TRUE),
        as_effect = c(FALSE, TRUE, TRUE, TRUE),
        mixing = structure(
          c(NA_integer_, NA_integer_, NA_integer_, NA_integer_),
          levels = c("cn", "cln", "cln-", "n", "ln", "ln-"),
          class = c("ordered", "factor")
        )
      ),
      row.names = c(NA, -4L),
      class = c("choice_effects", "data.frame"),
      choice_formula = choice_formula,
      choice_alternatives = choice_alternatives,
      delimiter = "*"
    )
  )
})

test_that("misspecified effects can be detected", {
  expect_error(
    choice_effects(),
    "Please specify the input `choice_formula`"
  )
  expect_error(
    choice_effects(choice_formula = choice ~ A),
    "Input `choice_formula` is bad"
  )
  expect_error(
    choice_effects(
      choice_formula = choice_formula(formula = A ~ B)
    ),
    "Please specify the input `choice_alternatives`"
  )
  expect_error(
    choice_effects(
      choice_formula = choice_formula(formula = A ~ B),
      choice_alternatives = 2
    ),
    "Input `choice_alternatives` is bad"
  )
  expect_error(
    choice_effects(
      choice_formula = choice_formula(formula = A ~ B),
      choice_alternatives = choice_alternatives(J = 3),
      delimiter = 1
    ),
    "Input `delimiter` is bad: Must be of type 'string', not 'double'"
  )
  expect_error(
    is.choice_effects(1),
    "is bad"
  )
  expect_error(
    choice_effects(
      choice_formula(choice ~ x_A | 0 | x),
      choice_alternatives(J = 2)
    ),
    "effect names"
  )
})

test_that("ordered alternatives restrict effect specification", {
  expect_error(
    choice_effects(
      choice_formula = choice_formula(
        formula = choice ~ 0 | 0 | C
      ),
      choice_alternatives = choice_alternatives(J = 3, ordered = TRUE)
    ),
    "Ordered choice models only support covariates in the first part"
  )
  expect_s3_class(
    choice_effects(
      choice_formula = choice_formula(
        formula = choice ~ A | 0
      ),
      choice_alternatives = choice_alternatives(J = 3, ordered = TRUE)
    ),
    "choice_effects"
  )
})

test_that("printing effects works", {
  messages <- capture.output(
    output <- capture.output(print(choice_effects(
      choice_formula = choice_formula(
        formula = choice ~ price | income | comfort,
        random_effects = c("price" = "cn", "income" = "cn")
      ),
      choice_alternatives = choice_alternatives(J = 3)
    )), type = "output"),
    type = "message"
  )
  text <- paste(c(messages, output), collapse = "\n")
  expect_match(text, "Choice effects", fixed = TRUE)
  expect_match(text, "ASC_B", fixed = TRUE)
})

test_that("number of effects can be computed", {
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ A | B + 0 | C + D,
      random_effects = c("A" = "cn", "D" = "cln")
    ),
    choice_alternatives = choice_alternatives(
      J = 3
    )
  )
  expect_equal(compute_P(choice_effects), 9)
  expect_equal(compute_P_d(choice_effects), 5)
  expect_equal(compute_P_r(choice_effects), 4)
  expect_identical(
    as.character(utils::tail(choice_effects$mixing, 4)),
    c("cn", "cln", "cln", "cln")
  )
})

test_that("effects can be created with resolving", {
  expect_identical(
    choice_effects(
      choice_formula = choice_formula(
        form = choice ~ comfort
      ),
      choice_alternatives = choice_alternatives(
        J = 2, alternatives = c("A", "B")
      ),
      choice_data = choice_data(
        data_frame = train_choice,
        format = "wide",
        column_occasion = "occasionID"
      )
    )$effect_name,
    c("comfort1", "comfort2", "ASC_B")
  )
})
