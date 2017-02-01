context("Predicting Bills")

test_that("predictbills outputs a numeric value", {
  expect_that(predictbills(2016), is_a("numeric"))
})
