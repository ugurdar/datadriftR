test_that("PageHinkley constructor works", {
  ph <- PageHinkley$new()
  expect_s3_class(ph, "R6")
  expect_false(ph$detected_change())
})

test_that("PageHinkley detects drift in synthetic stream", {
  set.seed(456)
  pre  <- rnorm(500, mean = 0, sd = 1)
  post <- rnorm(500, mean = 2, sd = 1)
  stream <- c(pre, post)
  
  ph <- PageHinkley$new(delta = 0.005, threshold = 50)
  drift_detected <- FALSE
  
  for (i in seq_along(stream)) {
    ph$add_element(stream[i])
    if (ph$detected_change()) {
      drift_detected <- TRUE
      break
    }
  }
  
  expect_true(drift_detected)
})

test_that("PageHinkley add_element accepts numeric values", {
  ph <- PageHinkley$new()
  expect_silent(ph$add_element(0.5))
  expect_silent(ph$add_element(-0.3))
})

test_that("PageHinkley handles single observation", {
  ph <- PageHinkley$new()
  expect_silent(ph$add_element(1.0))
  expect_false(ph$detected_change())
})

test_that("PageHinkley reset works", {
  ph <- PageHinkley$new()
  ph$add_element(1.0)
  ph$add_element(2.0)
  ph$reset()
  expect_false(ph$detected_change())
})
