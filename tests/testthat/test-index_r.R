
context("Test the rli function")

test_that("test the function works", {
  expect_is(rli(c(0,2,3)), "numeric")
  expect_equal(rli(c(0,2,3,3)), 0.6)
  expect_error(rli(c(6,2)),
      "Numeric rank must be below the maxiumum weighted score for extinct species")
  })


test_that("test fails on non character input", {
  expect_error(rli('S1'),"status scores should be a numeric vector")
})


test_that("test for NA data input", {
  expect_error(rli(NA_real_),
                 "It looks like you have one or more NA values in your input dataset")
})

