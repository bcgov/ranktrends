
context("Test the rli function")


test_that("test the function works", {
  expect_is(rli(c(0,2,3)), "numeric")
  })


test_that("test fails on non character input", {
  expect_error(rli('S1'),"status scores should be a numeric vector")
})

# this one is not working correctly
test_that("test for NA data input", {
  expect_equal(suppressWarnings(rli(NA_character_)), c(NA_real_))
  expect_warning(rli(NA_character_),
                 "Sorry it looks like you have one or more NA values in your input dataset")
})
