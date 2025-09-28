test_that("choice_identifiers can be defined", {

  ### long format
  expect_true(
    choice_identifiers(
      data_frame = travel_mode_choice,
      format = "long",
      column_decider = "individual",
      column_occasion = NULL,
      cross_section = TRUE
    ) |> is.choice_identifiers()
  )

  ### wide format
  expect_true(
    choice_identifiers(
      data_frame = train_choice,
      format = "wide",
      column_decider = "deciderID",
      column_occasion = "occasionID",
      cross_section = FALSE
    ) |> is.choice_identifiers()
  )
  expect_true(
    choice_identifiers(
      data_frame = train_choice,
      format = "wide",
      column_decider = "deciderID",
      column_occasion = "occasionID",
      cross_section = TRUE
    ) |> is.choice_identifiers()
  )

})

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
    "Input `data_frame` is bad"
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
    "Inputs `column_decider` and `column_occasion` must be different"
  )
  expect_error(
    choice_identifiers(
      data.frame("id" = c(1, 2, 3)),
      column_decider = "id",
      column_occasion = "idc"
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

test_that("choice_identifiers work for cross-sectional case", {
  expect_error(
    choice_identifiers(
      generate_choice_identifiers(N = 3, Tp = 2),
      column_occasion = NULL,
      cross_section = TRUE
    ),
    "must not have duplicated values"
  )
  expect_identical(
    choice_identifiers(
      generate_choice_identifiers(N = 3, Tp = 1),
      column_occasion = NULL,
      cross_section = TRUE
    ),
    structure(
      list(
        deciderID = c("1", "2", "3")
      ),
      class = c("choice_identifiers", "tbl_df", "tbl", "data.frame"),
      row.names = c(NA, 3L),
      column_decider = "deciderID",
      cross_section = TRUE
    )
  )
  expect_identical(
    choice_identifiers(
      generate_choice_identifiers(N = 3, Tp = 2),
      cross_section = TRUE
    ),
    structure(
      list(
        deciderID = c("1.1", "1.2", "2.1", "2.2", "3.1", "3.2")
      ),
      class = c("choice_identifiers", "tbl_df", "tbl", "data.frame"),
      row.names = c(NA, 6L),
      column_decider = "deciderID",
      cross_section = TRUE
    )
  )
})

test_that("choice_identifiers can be generated", {
  expect_identical(
    generate_choice_identifiers(N = 3, column_occasion = NULL),
    structure(
      list(
        deciderID = c("1", "2", "3")
      ),
      class = c("choice_identifiers", "tbl_df", "tbl", "data.frame"),
      row.names = c(NA, 3L),
      column_decider = "deciderID",
      cross_section = TRUE
    )
  )
  expect_identical(
    generate_choice_identifiers(N = 3, Tp = 1:3),
    structure(
      list(
        deciderID = c("1", "2", "2", "3", "3", "3"),
        occasionID = c("1", "1", "2", "1", "2", "3")
      ),
      class = c("choice_identifiers", "tbl_df", "tbl", "data.frame"),
      row.names = c(NA, 6L),
      column_decider = "deciderID",
      column_occasion = "occasionID",
      cross_section = FALSE
    )
  )
  expect_error(
    generate_choice_identifiers(N = 3, Tp = 1:3, column_decider = "occasionID"),
    "Inputs `column_decider` and `column_occasion` must be different"
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
    expand_Tp(N = 3.5),
    "Input `N` is bad: Must be of type 'single integerish value', not 'double'"
  )
  expect_error(
    expand_Tp(N = 10, Tp = "one"),
    "Input `Tp` is bad"
  )
  expect_error(
    expand_Tp(N = 10, Tp = 1:9),
    "Input `Tp` is bad"
  )
  expect_error(
    expand_Tp(N = 10, Tp = 1.5),
    "Input `Tp` is bad"
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

test_that("position from identifier can be extracted", {
  expect_identical(
    get_position_from_identifier(
      N = 10, Tp = 4, decider_number = 2, occasion_number = 3
    ),
    7L
  )
})

test_that("decider identifiers can be obtained", {
  expect_identical(
    get_decider_identifiers(
      choice_identifiers = generate_choice_identifiers(N = 2, Tp = 1:2)
    ),
    c("1", "2")
  )
})

test_that("choice_identifiers can be extracted", {

  ### wide choice_data (panel)
  x <- choice_data(train_choice, column_occasion = "occasionID")
  expect_true(
    extract_choice_identifiers(x) |> is.choice_identifiers()
  )

  ### wide choice_data (cross-sectional)
  x <- choice_data(
    train_choice, column_occasion = "occasionID", cross_section = TRUE
  )
  expect_true(
    extract_choice_identifiers(x) |> is.choice_identifiers()
  )

  ### long choice_data
  x <- choice_data(
    data_frame = travel_mode_choice,
    format = "long",
    column_decider = "individual",
    column_alternative = "mode"
  )
  expect_true(
    extract_choice_identifiers(x) |> is.choice_identifiers()
  )

})

