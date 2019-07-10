
context("Test the rank conversion function")

test_that("test for NA data input", {
  expect_equal(suppressWarnings(ranks_to_numeric(NA_character_)), list(NA_real_))
  expect_warning(ranks_to_numeric(NA_character_),
                 "Sorry it looks like you have one or more NA values in your input dataset")
})


test_that("test the function works", {
  expect_is(ranks_to_numeric(c("S1", "SX", "S2S4")), 'list')
  expect_is(ranks_to_numeric(c("S1", "SX", "S2S4"), simplify = TRUE),
            'numeric')
  expect_equal(ranks_to_numeric(c("S1", "SX", "S2S4")),
               list(1, 0, c(2, 3, 4)))
})


test_that("test the simplify function works", {
  expect_equal(ranks_to_numeric(c("S1", "SX", "S2S4"), simplify = TRUE),
               c(1, 0, 3))
  expect_equal(ranks_to_numeric(c("S1", "SX", "S2S4"), simplify = TRUE,
                                round_fun = min), c(1, 0, 2))
  expect_equal(ranks_to_numeric(c("S1", "SX", "S2S4"), simplify = TRUE,
                                round_fun = max), c(1, 0, 4))

})


test_that("test fails on non character input", {
  expect_error(ranks_to_numeric(1),"'ranks' should be a character vector")
  expect_error(ranks_to_numeric("S1", simplify = "text"),
               "simplify should be TRUE or FALSE")
  expect_error(ranks_to_numeric("S1", round_fun = "min"),
               "round_fun should be a function")
})