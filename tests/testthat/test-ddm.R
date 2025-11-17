test_that("DDM constructor works", {
  ddm <- DDM$new()
  expect_s3_class(ddm, "R6")
  expect_false(ddm$change_detected)
})

test_that("DDM detects drift in synthetic stream", {
  set.seed(123)
  pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.9, 0.1))
  post <- sample(c(0,1), 500, replace = TRUE, prob = c(0.1, 0.9))
  stream <- c(pre, post)
  
  ddm <- DDM$new()
  drift_detected <- FALSE
  
  for (i in seq_along(stream)) {
    ddm$add_element(stream[i])
    if (ddm$change_detected) {
      drift_detected <- TRUE
      break
    }
  }
  
  expect_true(drift_detected)
})

test_that("DDM add_element accepts numeric values", {
  ddm <- DDM$new()
  expect_silent(ddm$add_element(0))
  expect_silent(ddm$add_element(1))
})
