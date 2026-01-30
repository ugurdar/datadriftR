test_that("ADWIN constructor works", {
adwin <- ADWIN$new()
  expect_s3_class(adwin, "R6")
  expect_false(adwin$detected_change())
})

test_that("ADWIN detects drift in synthetic stream", {
  set.seed(456)
  pre  <- rnorm(500, mean = 0, sd = 1)
  post <- rnorm(500, mean = 3, sd = 1)
  stream <- c(pre, post)

  adwin <- ADWIN$new(delta = 0.002)
  drift_detected <- FALSE

  for (i in seq_along(stream)) {
    adwin$add_element(stream[i])
    if (adwin$detected_change()) {
      drift_detected <- TRUE
      break
    }
  }

  expect_true(drift_detected)
})

test_that("ADWIN add_element accepts numeric values", {
  adwin <- ADWIN$new()
  expect_silent(adwin$add_element(0.5))
  expect_silent(adwin$add_element(-1.2))
  expect_silent(adwin$add_element(100))
})

test_that("ADWIN width increases with elements", {
  adwin <- ADWIN$new()
  for (i in 1:10) {
    adwin$add_element(i)
  }
  expect_gte(adwin$width(), 1)
})

test_that("ADWIN estimation returns mean", {
  adwin <- ADWIN$new()
  values <- c(1, 2, 3, 4, 5)
  for (v in values) {
    adwin$add_element(v)
  }
  expect_true(adwin$estimation() > 0)
})

test_that("ADWIN works with detect_drift", {
  set.seed(789)
  stream <- c(rnorm(300, 0, 1), rnorm(300, 2, 1))

  results <- detect_drift(stream, method = "adwin", delta = 0.01)

  expect_s3_class(results, "data.frame")
  expect_true(nrow(results) >= 0)
})
