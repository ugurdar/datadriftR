test_that("ProfileDifference constructor works", {
  skip_if_not_installed("fda.usc")
  skip_if_not_installed("doremi")
  
  pd <- ProfileDifference$new()
  expect_s3_class(pd, "R6")
})

test_that("ProfileDifference detects difference between profiles", {
  skip_if_not_installed("fda.usc")
  skip_if_not_installed("doremi")
  
  set.seed(555)
  # Generate two different profiles
  x <- 1:100
  profile1 <- list(x = x, y = sin(x / 10))
  profile2 <- list(x = x, y = sin(x / 10) + 2)  # Shifted profile
  
  pd <- ProfileDifference$new(method = "L2")
  pd$set_profiles(profile1, profile2)
  
  # Suppress foreach parallel backend warning from fda.usc
  result <- suppressWarnings(pd$calculate_difference())
  
  expect_true(result$distance > 0)
  expect_equal(result$method, "L2")
})

test_that("ProfileDifference with pdi method works", {
  skip_if_not_installed("fda.usc")
  skip_if_not_installed("doremi")
  
  set.seed(666)
  x <- 1:100
  profile1 <- list(x = x, y = sin(x / 10))
  profile2 <- list(x = x, y = cos(x / 10))  # Different shape
  
  pd <- ProfileDifference$new(method = "pdi", deriv = "gold")
  pd$set_profiles(profile1, profile2)
  
  result <- pd$calculate_difference()
  
  expect_true(is.numeric(result$distance))
  expect_equal(result$method, "pdi")
})
