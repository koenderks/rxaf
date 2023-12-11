context("Test read.xaf function")

test_that(desc = "ExactOnline", {
  file <- system.file("example", "example.xaf", package = "rxaf")
  result <- read.xaf(file)
  expect_equal(nrow(result), 167)
  expect_equal(ncol(result), 21)
})

test_that(desc = "ExactOnline", {
  file <- system.file("example", "example2.xaf", package = "rxaf")
  result <- read.xaf(file)
  expect_equal(nrow(result), 10)
  expect_equal(ncol(result), 21)
})
