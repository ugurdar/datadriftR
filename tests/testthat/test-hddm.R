test_that("HDDM_A constructor works", {
  hddm_a <- HDDM_A$new()
  expect_s3_class(hddm_a, "R6")
  expect_false(hddm_a$detected_change())
})

test_that("HDDM_A detects drift in synthetic stream", {
  set.seed(111)
  pre  <- rnorm(500, mean = 0, sd = 1)
  post <- rnorm(500, mean = 1.5, sd = 1)
  stream <- c(pre, post)
  
  hddm_a <- HDDM_A$new()
  drift_detected <- FALSE
  
  for (i in seq_along(stream)) {
    hddm_a$add_element(stream[i])
    if (hddm_a$detected_change()) {
      drift_detected <- TRUE
      break
    }
  }
  
  expect_true(drift_detected)
})

test_that("HDDM_W constructor works", {
  hddm_w <- HDDM_W$new()
  expect_s3_class(hddm_w, "R6")
  expect_false(hddm_w$detected_change())
})

test_that("HDDM_W detects drift in synthetic stream", {
  set.seed(222)
  pre  <- rnorm(500, mean = 0, sd = 1)
  post <- rnorm(500, mean = 1.5, sd = 1)
  stream <- c(pre, post)
  
  hddm_w <- HDDM_W$new()
  drift_detected <- FALSE
  
  for (i in seq_along(stream)) {
    hddm_w$add_element(stream[i])
    if (hddm_w$detected_change()) {
      drift_detected <- TRUE
      break
    }
  }
  
  expect_true(drift_detected)
})

test_that("HDDM add_element accepts numeric values", {
  hddm_a <- HDDM_A$new()
  expect_silent(hddm_a$add_element(0.5))
  
  hddm_w <- HDDM_W$new()
  expect_silent(hddm_w$add_element(-0.3))
})
