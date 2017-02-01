context("Reading Inflation Rates from Text File")

test_that("readinflation outputs numeric values", {
  expect_that(readinflation(), is_a("numeric"))
})
