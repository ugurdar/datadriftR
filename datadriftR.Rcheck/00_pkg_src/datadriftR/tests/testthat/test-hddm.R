test_that("HDDM_A constructor works", {
  hddm_a <- HDDM_A$new()
  expect_s3_class(hddm_a, "R6")
  expect_false(hddm_a$change_detected)
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
    if (hddm_a$change_detected) {
      drift_detected <- TRUE
      break
    }
  }
  
  expect_true(drift_detected)
})

test_that("HDDM_W constructor works", {
  hddm_w <- HDDM_W$new()
  expect_s3_class(hddm_w, "R6")
  expect_false(hddm_w$change_detected)
})

test_that("HDDM_W detects drift in synthetic stream", {
  set.seed(333)
  pre  <- rnorm(500, mean = 0, sd = 1)
  post <- rnorm(500, mean = 2.0, sd = 1)
  stream <- c(pre, post)
  
  hddm_w <- HDDM_W$new()
  drift_detected <- FALSE
  
  for (i in seq_along(stream)) {
    hddm_w$add_element(stream[i])
    if (hddm_w$change_detected) {
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

test_that("HDDM_A detects drift at expected indices on dataset0_binary", {
  dataset_path <- testthat::test_path("fixtures", "dataset0_binary.txt")
  skip_if_not(file.exists(dataset_path), "dataset0_binary.txt not found")
  
  dataset0 <- as.numeric(readLines(dataset_path))
  
  hddm_a <- HDDM_A$new()
  drift_indices <- c()
  
  for (i in seq_along(dataset0)) {
    hddm_a$add_element(dataset0[i])
    if (hddm_a$change_detected) {
      drift_indices <- c(drift_indices, i - 1)  # 0-indexed
    }
  }
  
  expected <- c(993, 1509)
  expect_equal(drift_indices, expected)
})

test_that("HDDM_A detects drift at expected indices on dataset1_binary", {
  dataset_path <- testthat::test_path("fixtures", "dataset1_binary.txt")
  skip_if_not(file.exists(dataset_path), "dataset1_binary.txt not found")
  
  dataset1 <- as.numeric(readLines(dataset_path))
  
  hddm_a <- HDDM_A$new()
  drift_indices <- c()
  
  for (i in seq_along(dataset1)) {
    hddm_a$add_element(dataset1[i])
    if (hddm_a$change_detected) {
      drift_indices <- c(drift_indices, i - 1)  # 0-indexed
    }
  }
  
  expected <- c(1007)
  expect_equal(drift_indices, expected)
})

test_that("HDDM_W detects drift at expected indices on dataset0_binary", {
  dataset_path <- testthat::test_path("fixtures", "dataset0_binary.txt")
  skip_if_not(file.exists(dataset_path), "dataset0_binary.txt not found")
  
  dataset0 <- as.numeric(readLines(dataset_path))
  
  hddm_w <- HDDM_W$new()
  drift_indices <- c()
  
  for (i in seq_along(dataset0)) {
    hddm_w$add_element(dataset0[i])
    if (hddm_w$change_detected) {
      drift_indices <- c(drift_indices, i - 1)  # 0-indexed
    }
  }
  
  expected <- c(992, 1532)
  expect_equal(drift_indices, expected)
})

test_that("HDDM_W detects drift at expected indices on dataset1_binary", {
  dataset_path <- testthat::test_path("fixtures", "dataset1_binary.txt")
  skip_if_not(file.exists(dataset_path), "dataset1_binary.txt not found")
  
  dataset1 <- as.numeric(readLines(dataset_path))
  
  hddm_w <- HDDM_W$new()
  drift_indices <- c()
  
  for (i in seq_along(dataset1)) {
    hddm_w$add_element(dataset1[i])
    if (hddm_w$change_detected) {
      drift_indices <- c(drift_indices, i - 1)  # 0-indexed
    }
  }
  
  expected <- c(1023)
  expect_equal(drift_indices, expected)
})
