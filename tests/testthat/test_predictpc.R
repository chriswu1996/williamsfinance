context("Predicting Percentage Growth")

test_that("predictpc outputs a numeric value", {
  expect_that(predictpc(2016), is_a("numeric"))
})
