test_that("KLHistogram constructor works", {
  kl <- KLHistogram$new()
  expect_s3_class(kl, "R6")
  expect_false(kl$detected_change())
})

test_that("KLHistogram detects drift in synthetic stream", {
  set.seed(444)
  pre  <- rnorm(500, mean = 0, sd = 1)
  post <- rnorm(500, mean = 2, sd = 1)
  stream <- c(pre, post)
  
  kl <- KLHistogram$new(window_size = 100, num_bins = 10, threshold = 0.5)
  drift_detected <- FALSE
  
  for (i in seq_along(stream)) {
    kl$add_element(stream[i])
    if (kl$detected_change()) {
      drift_detected <- TRUE
      break
    }
  }
  
  expect_true(drift_detected)
})

test_that("KLHistogram add_element accepts numeric values", {
  kl <- KLHistogram$new()
  expect_silent(kl$add_element(0.5))
  expect_silent(kl$add_element(3.2))
})
