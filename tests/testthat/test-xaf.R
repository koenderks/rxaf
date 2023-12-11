test_that(desc = "ExactOnline", {
  result <- read_xaf("ExactOnline.xaf")
  expect_equal(nrow(result), 167)
  expect_equal(ncol(result), 22)
  sub <- xaf_subtables(result)
  expect_equal(length(sub), 4)
})

test_that(desc = "osFinancials", {
  result <- read_xaf("osFinancials.xaf")
  expect_equal(nrow(result), 10)
  expect_equal(ncol(result), 22)
  sub <- xaf_subtables(result)
  expect_equal(length(sub), 3)
})

test_that(desc = "AFAS", {
  result <- read_xaf("AFAS.xaf")
  expect_equal(nrow(result), 31309)
  expect_equal(ncol(result), 22)
  sub <- xaf_subtables(result)
  expect_equal(length(sub), 4)
})

test_that(desc = "Twinfield", {
  result <- read_xaf("Twinfield.xaf")
  expect_equal(nrow(result), 181)
  expect_equal(ncol(result), 22)
  sub <- xaf_subtables(result)
  expect_equal(length(sub), 4)
})

test_that(desc = "Multivers", {
  result <- read_xaf("Multivers.xaf")
  expect_equal(nrow(result), 371)
  expect_equal(ncol(result), 22)
  sub <- xaf_subtables(result)
  expect_equal(length(sub), 4)
})
