test_that("KSWIN constructor works", {
  kswin <- KSWIN$new()
  expect_s3_class(kswin, "R6")
  expect_false(kswin$detected_change())
})

test_that("KSWIN detects drift in synthetic stream", {
  set.seed(333)
  pre  <- rnorm(500, mean = 0, sd = 1)
  post <- rnorm(500, mean = 2, sd = 1)
  stream <- c(pre, post)
  
  kswin <- KSWIN$new(window_size = 100, stat_size = 30)
  drift_detected <- FALSE
  
  for (i in seq_along(stream)) {
    kswin$add_element(stream[i])
    if (kswin$detected_change()) {
      drift_detected <- TRUE
      break
    }
  }
  
  expect_true(drift_detected)
})

test_that("KSWIN add_element accepts numeric values", {
  kswin <- KSWIN$new()
  expect_silent(kswin$add_element(0.5))
  expect_silent(kswin$add_element(-1.2))
})
