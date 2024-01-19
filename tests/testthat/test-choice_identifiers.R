test_that("checks for choice identifiers work", {
  expect_error(
    choice_identifiers(
      data.frame("id" = 1), column_decider = "id", column_occasion = "id"
    ),
    "Names for `column_decider` and `column_occasion` must be different."
  )
  expect_error(
    choice_identifiers(
      data.frame("id" = 1:3, "idc" = 1), column_decider = "bad"
    ),
    "not found in `data_frame`."
  )
  expect_error(
    choice_identifiers(
      data.frame("id" = c(1, NA, 3), "idc" = 1), column_decider = "id"
    ),
    "must not have NAs"
  )
  expect_error(
    choice_identifiers(
      data.frame("id" = c(1, 1, 3), "idc" = 1), column_decider = "id"
    ),
    "must not have duplicated values if there are no identifiers for the choice occasions."
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
    "not found in `data_frame`."
  )
  expect_error(
    choice_identifiers(
      data.frame("id" = c(1, 2, 3), "idc" = c(1, NA, 1)), column_decider = "id", column_occasion = "idc"
    ),
    "of `data_frame` must not have NAs."
  )
  expect_error(
    choice_identifiers(
      data.frame("id" = c(1, 1), "idc" = c(1, 1)), column_decider = "id", column_occasion = "idc"
    ),
    "must have unique values for a given decider"
  )
})

test_that("choice identifiers can be generated", {
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
      row.names = c(NA, 6L)
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
    "Names for `column_decider` and `column_occasion` must be different."
  )
})

test_that("choice identifiers can be read", {
  data_frame <- data.frame(
    "decider" = c("A", "B", "A"),
    "occasion" = 1:3
  )
  expect_equal(
    read_choice_identifiers(
      data_frame = data_frame, column_decider = "decider",
      column_occasion = "occasion"
    ),
    structure(
      list(
        decider = structure(
          c(1L, 2L, 1L),
          levels = c("A", "B"),
          class = "factor"
        ),
        occasion = structure(
          1:3,
          levels = c("1", "2", "3"),
          class = "factor")
        ),
      class = c("choice_identifiers", "data.frame"),
      row.names = c(NA, 3L)
    )
  )
  expect_equal(
    read_choice_identifiers(
      data_frame = data_frame, column_decider = "decider",
      column_occasion = "occasion", as_cs = TRUE
    ),
    structure(
      list(
        decider = structure(
          c(1L, 3L, 2L),
          levels = c("A.1", "A.3", "B.2"),
          class = "factor"
        ),
        occasion = structure(
          c(1L, 1L, 1L),
          levels = c("1"),
          class = "factor"
        )
      ),
      class = c("choice_identifiers", "data.frame"),
      row.names = c(NA, 3L)
    )
  )
})

test_that("Tp can be expanded", {
  expect_error(
    expand_Tp(),
    "Please specify the number `N` of deciders."
  )
  expect_error(
    expand_Tp(N = 3.5),
    "Assertion on 'N' failed: Must be of type 'single integerish value', not 'double'."
  )
  expect_error(
    expand_Tp(N = 10, Tp = "one"),
    "Assertion on 'Tp' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    expand_Tp(N = 10, Tp = 1:9),
    "Assertion on 'Tp' failed: Must have length 10, but has length 9."
  )
  expect_error(
    expand_Tp(N = 10, Tp = 1.5),
    "Assertion on 'Tp' failed: Must be of type 'integerish', but element 1 is not close to an integer."
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
