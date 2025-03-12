test_that("z_score calculates correctly", {
  # Test with default parameters
  x <- c(1, 2, 3, 4, 5)
  expected <- (x - mean(x)) / sd(x)
  expect_equal(z_score(x), expected)

  # Test with provided mean and sd
  expect_equal(z_score(c(10, 20, 30), mean = 20, sd = 10), c(-1, 0, 1))

  # Test error handling
  expect_error(z_score(c(1, 1, 1)))
})

test_that("confidence_interval calculates correctly", {
  # Simple test with known values
  set.seed(123)
  x <- rnorm(100)
  ci <- confidence_interval(x)
  expect_length(ci, 2)
  expect_named(ci, c("lower", "upper"))
  expect_true(ci[1] < mean(x) && ci[2] > mean(x))

  # Test error handling
  expect_error(confidence_interval(5))
})

test_that("detect_outliers identifies outliers correctly", {
  # Simple case with clear outlier
  x <- c(1, 2, 3, 4, 100)
  outliers <- detect_outliers(x)
  expect_equal(outliers, c(FALSE, FALSE, FALSE, FALSE, TRUE))

  # Case with no outliers
  x <- c(1, 2, 3, 4, 5)
  outliers <- detect_outliers(x)
  expect_false(any(outliers))

  # Test with different multiplier
  x <- c(1, 2, 3, 4, 10)
  outliers <- detect_outliers(x, multiplier = 1)
  expect_true(outliers[5])
})
