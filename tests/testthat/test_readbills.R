context("Reading Bills from Text File")

test_that("readbills outputs a dataframe with integer columns", {
  expect_that(readbills(2016), is_a("data.frame"))
  expect_that(readbills(2016)[,1], is_a("integer"))
  expect_that(readbills(2016)[,2], is_a("integer"))
  expect_that(readbills(2016)[,3], is_a("integer"))
  expect_that(readbills(2016)[,4], is_a("integer"))
  expect_that(readbills(2016)[,5], is_a("integer"))
})
