test_that("effect overview can be created", {

  ### test 1: MMNP with type-1 covariates only
  choice_formula <- choice_formula(
    formula = choice ~ cov,
    random_effects = c("cov" = "cn", "ASC" = "cn")
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
          c(1L, 1L, 1L),
          levels = c("cn"),
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
          levels = c("cn"),
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
})

test_that("ordered alternatives restrict effect specification", {
  expect_error(
    choice_effects(
      choice_formula = choice_formula(
        formula = choice ~ 0 | 0 | C
      ),
      choice_alternatives = choice_alternatives(J = 3, ordered = TRUE)
    ),
    "Ordered choice models only support alternative-constant covariates."
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
  expect_snapshot(
    choice_effects(
      choice_formula = choice_formula(
        formula = choice ~ price | income | comfort,
        random_effects = c("price" = "cn", "income" = "cn")
      ),
      choice_alternatives = choice_alternatives(J = 3)
    )
  )
})

test_that("number of effects can be computed", {
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ A | B + 0 | C + D,
      random_effects = c("A" = "cn", "D" = "cn")
    ),
    choice_alternatives = choice_alternatives(
      J = 3
    )
  )
  expect_equal(compute_P(choice_effects), 9)
  expect_equal(compute_P_d(choice_effects), 5)
  expect_equal(compute_P_r(choice_effects), 4)
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
