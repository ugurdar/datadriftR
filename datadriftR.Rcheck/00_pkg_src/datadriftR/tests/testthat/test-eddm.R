test_that("EDDM constructor works", {
  eddm <- EDDM$new()
  expect_s3_class(eddm, "R6")
  expect_false(eddm$change_detected)
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
    if (eddm$change_detected) {
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

test_that("EDDM detects drift at expected indices on dataset0_binary", {
  dataset_path <- testthat::test_path("fixtures", "dataset0_binary.txt")
  skip_if_not(file.exists(dataset_path), "dataset0_binary.txt not found")
  
  dataset0 <- as.numeric(readLines(dataset_path))
  
  eddm <- EDDM$new()
  drift_indices <- c()
  
  for (i in seq_along(dataset0)) {
    eddm$add_element(dataset0[i])
    if (eddm$change_detected) {
      drift_indices <- c(drift_indices, i - 1)  # 0-indexed
    }
  }
  
  expected <- c(56, 106, 174, 245, 310, 453, 1639, 1854)
  expect_equal(drift_indices, expected)
})

test_that("EDDM detects drift at expected indices on dataset1_binary", {
  dataset_path <- testthat::test_path("fixtures", "dataset1_binary.txt")
  skip_if_not(file.exists(dataset_path), "dataset1_binary.txt not found")
  
  dataset1 <- as.numeric(readLines(dataset_path))
  
  eddm <- EDDM$new()
  drift_indices <- c()
  
  for (i in seq_along(dataset1)) {
    eddm$add_element(dataset1[i])
    if (eddm$change_detected) {
      drift_indices <- c(drift_indices, i - 1)  # 0-indexed
    }
  }
  
  expected <- c(485, 836, 1037, 1104, 1166, 1363, 1444)
  expect_equal(drift_indices, expected)
})
