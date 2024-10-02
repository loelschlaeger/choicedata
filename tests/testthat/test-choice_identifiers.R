test_that("checks for choice identifiers work", {
  expect_error(
    choice_identifiers(
      data.frame("id" = 1), column_decider = "id", column_occasion = "id"
    ),
    "Inputs `column_decider` and `column_occasion` must be different"
  )
  expect_error(
    choice_identifiers(
      data.frame("id" = 1:3, "idc" = 1), column_decider = "bad"
    ),
    "Columns of input `data_frame` are bad"
  )
  expect_error(
    choice_identifiers(
      data.frame("id" = c(1, NA, 3), "idc" = 1), column_decider = "id",
      column_occasion = "idc"
    ),
    "must not have NAs"
  )
  expect_error(
    choice_identifiers(
      data.frame("id" = c(1, 1, 3), "idc" = 1), column_decider = "id",
      column_occasion = "idc"
    ),
    "must have unique values for any decider"
  )
  expect_error(
    choice_identifiers(
      data.frame("occasionID" = c(1, 2, 3)), column_decider = "occasionID"
    ),
    "`column_decider` must not equal"
  )
  expect_error(
    choice_identifiers(
      data.frame("id" = c(1, 2, 3)), column_decider = "id", column_occasion = "idc"
    ),
    "must include"
  )
  expect_error(
    choice_identifiers(
      data.frame("id" = c(1, 2, 3), "idc" = c(1, NA, 1)), column_decider = "id",
      column_occasion = "idc"
    ),
    "must not have NAs"
  )
  expect_error(
    choice_identifiers(
      data.frame("id" = c(1, 1), "idc" = c(1, 1)), column_decider = "id",
      column_occasion = "idc"
    ),
    "must have unique values for any decider"
  )
  expect_true(
    is.choice_identifiers(
      choice_identifiers(data.frame("deciderID" = 1, "occasionID" = 1))
    )
  )
})

test_that("choice identifiers work for cross-sectional case", {
  expect_error(
    choice_identifiers(
      generate_choice_identifiers(N = 3, Tp = 2),
      column_occasion = NULL,
      as_cross_section = TRUE
    ),
    "must not have duplicated values"
  )
  expect_equal(
    choice_identifiers(
      generate_choice_identifiers(N = 3, Tp = 1),
      column_occasion = NULL,
      as_cross_section = TRUE
    ),
    structure(
      list(
        deciderID = structure(
          1:3,
          levels = c("1", "2", "3"),
          class = "factor"
        )
      ),
      class = c("choice_identifiers", "data.frame"),
      row.names = c(NA, 3L),
      column_decider = "deciderID",
      as_cross_section = TRUE
    )
  )
  expect_equal(
    choice_identifiers(
      generate_choice_identifiers(N = 3, Tp = 2),
      as_cross_section = TRUE
    ),
    structure(
      list(
        deciderID = structure(
          1:6,
          levels = c("1.1", "1.2", "2.1", "2.2", "3.1", "3.2"),
          class = "factor"
        )
      ),
      class = c("choice_identifiers", "data.frame"),
      row.names = c(NA, 6L),
      column_decider = "deciderID",
      as_cross_section = TRUE
    )
  )
})

test_that("choice_identifiers can be printed", {
  expect_error(
    print.choice_identifiers(data.frame("deciderID" = 1, "occasionID" = 1)),
    "Input `x` must be an object of class"
  )
  expect_snapshot(
    choice_identifiers(data.frame("deciderID" = 1, "occasionID" = 1))
  )
  expect_snapshot(
    print(generate_choice_identifiers(N = 1000, Tp = 2), rows = 5)
  )
})

test_that("choice identifiers can be generated", {
  expect_equal(
    generate_choice_identifiers(N = 3, column_occasion = NULL),
    structure(
      list(
        deciderID = structure(
          1:3,
          levels = c("1", "2", "3"),
          class = "factor"
        )
      ),
      class = c("choice_identifiers", "data.frame"),
      row.names = c(NA, 3L),
      column_decider = "deciderID",
      as_cross_section = TRUE
    )
  )
  expect_equal(
    generate_choice_identifiers(N = 3, Tp = 1:3),
    structure(
      list(
        deciderID = structure(
          c(1L, 2L, 2L, 3L, 3L, 3L),
          levels = c("1", "2", "3"),
          class = "factor"
        ),
        occasionID = structure(
          c(1L, 1L, 2L, 1L, 2L, 3L),
          levels = c("1", "2", "3"),
          class = "factor"
        )
      ),
      class = c("choice_identifiers", "data.frame"),
      row.names = c(NA, 6L),
      column_decider = "deciderID",
      column_occasion = "occasionID",
      as_cross_section = FALSE
    )
  )
  expect_error(
    generate_choice_identifiers(N = 3, Tp = 1:3, column_decider = "occasionID"),
    "`column_decider` must not equal"
  )
  expect_error(
    generate_choice_identifiers(
      N = 3, Tp = 1:3, column_decider = "id", column_occasion = "id"
    ),
    "Inputs `column_decider` and `column_occasion` must be different"
  )
})

test_that("Tp can be expanded", {
  expect_error(
    expand_Tp(),
    "Please specify the input `N`"
  )
  expect_error(
    expand_Tp(N = 3.5),
    "Input `N` is bad: Must be of type 'single integerish value', not 'double'"
  )
  expect_error(
    expand_Tp(N = 10, Tp = "one"),
    "Input `Tp` is bad: Must be of type 'integerish', not 'character'"
  )
  expect_error(
    expand_Tp(N = 10, Tp = 1:9),
    "Input `Tp` is bad: Must have length 10, but has length 9"
  )
  expect_error(
    expand_Tp(N = 10, Tp = 1.5),
    "Input `Tp` is bad: Must be of type 'integerish', but element 1 is not close to an integer"
  )
  expect_equal(
    expand_Tp(N = 10, Tp = 1),
    c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
  )
  expect_equal(
    expand_Tp(N = 10, Tp = 1:10),
    1:10
  )
})

test_that("Tp can be read", {
  choice_identifiers <- generate_choice_identifiers(N = 3, Tp = 1:3)
  expect_equal(
    read_Tp(choice_identifiers),
    1:3
  )
  choice_identifiers <- generate_choice_identifiers(N = 3, Tp = 2, column_occasion = NULL)
  expect_equal(
    read_Tp(choice_identifiers),
    rep(1L, 6)
  )
})
