test_that("EDDM constructor works", {
  eddm <- EDDM$new()
  expect_s3_class(eddm, "R6")
  expect_false(eddm$eddm_drift)
})

test_that("EDDM detects drift in synthetic stream", {
  set.seed(789)
  pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.9, 0.1))
  post <- sample(c(0,1), 500, replace = TRUE, prob = c(0.1, 0.9))
  stream <- c(pre, post)
  
  eddm <- EDDM$new()
  drift_detected <- FALSE
  
  for (i in seq_along(stream)) {
    eddm$add_element(stream[i])
    if (eddm$eddm_drift) {
      drift_detected <- TRUE
      break
    }
  }
  
  expect_true(drift_detected)
})

test_that("EDDM add_element accepts binary values", {
  eddm <- EDDM$new()
  expect_silent(eddm$add_element(0))
  expect_silent(eddm$add_element(1))
})
