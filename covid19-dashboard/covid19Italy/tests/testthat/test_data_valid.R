test_that(desc = "Test the dates", {

  expect_equal(base::class(italy_total$date) == "Date", TRUE)
  expect_equal(base::class(italy_region$date) == "Date", TRUE)
  expect_equal(base::class(italy_province$date) == "Date", TRUE)
  expect_equal(base::min(italy_total$date) == as.Date("2020-02-24"), TRUE)
})


test_that(desc = "Test the structure", {

  expect_equal(base::ncol(italy_total) == 14, TRUE)
  expect_equal(base::ncol(italy_region) == 19, TRUE)
  expect_equal(base::ncol(italy_province) == 11, TRUE)

  expect_equal(base::nrow(italy_total) >= 140, TRUE)
  expect_equal(base::nrow(italy_region) >= 3000, TRUE)
  expect_equal(base::nrow(italy_province) >= 18000, TRUE)

})
