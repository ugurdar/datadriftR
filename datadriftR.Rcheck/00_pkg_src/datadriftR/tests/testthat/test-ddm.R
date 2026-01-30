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

test_that("DDM detects drift at expected indices on dataset0_binary", {
  dataset_path <- testthat::test_path("fixtures", "dataset0_binary.txt")
  skip_if_not(file.exists(dataset_path), "dataset0_binary.txt not found")
  
  dataset0 <- as.numeric(readLines(dataset_path))
  
  ddm <- DDM$new()
  drift_indices <- c()
  
  for (i in seq_along(dataset0)) {
    ddm$add_element(dataset0[i])
    if (ddm$change_detected) {
      drift_indices <- c(drift_indices, i - 1)  # 0-indexed
    }
  }
  
  expected <- 1838
  expect_equal(drift_indices, expected)
})

test_that("DDM detects drift at expected indices on dataset1_binary", {
  dataset_path <- testthat::test_path("fixtures", "dataset1_binary.txt")
  skip_if_not(file.exists(dataset_path), "dataset1_binary.txt not found")
  
  dataset1 <- as.numeric(readLines(dataset_path))
  
  ddm <- DDM$new()
  drift_indices <- c()
  
  for (i in seq_along(dataset1)) {
    ddm$add_element(dataset1[i])
    if (ddm$change_detected) {
      drift_indices <- c(drift_indices, i - 1)  # 0-indexed
    }
  }
  
  expected <- c(55, 139, 248, 412, 446, 1024)
  expect_equal(drift_indices, expected)
})
