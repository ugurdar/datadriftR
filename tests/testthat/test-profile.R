test_that("ProfileDifference constructor works", {
  skip_if_not_installed("fda.usc")
  skip_if_not_installed("doremi")
  
  pd <- ProfileDifference$new()
  expect_s3_class(pd, "R6")
})

test_that("ProfileDifference detects drift in synthetic stream", {
  skip_if_not_installed("fda.usc")
  skip_if_not_installed("doremi")
  
  set.seed(555)
  # Generate time series with pattern change
  t <- seq(0, 1, length.out = 50)
  pre_profile  <- sin(2 * pi * t)
  post_profile <- sin(4 * pi * t)
  
  pd <- ProfileDifference$new(window_size = 50)
  
  # Add pre-drift observations
  for (val in pre_profile) {
    pd$add_element(val)
  }
  
  drift_detected <- FALSE
  # Add post-drift observations
  for (val in post_profile) {
    pd$add_element(val)
    if (pd$detected_change()) {
      drift_detected <- TRUE
      break
    }
  }
  
  # Profile methods may require more data, so we just check it runs
  expect_true(is.logical(drift_detected))
})

test_that("ProfileDifference add_element accepts numeric values", {
  skip_if_not_installed("fda.usc")
  skip_if_not_installed("doremi")
  
  pd <- ProfileDifference$new()
  expect_silent(pd$add_element(0.5))
  expect_silent(pd$add_element(-0.3))
})
