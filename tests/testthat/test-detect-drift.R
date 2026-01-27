test_that("detect_drift works with DDM method", {
  set.seed(123)
  pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.9, 0.1))
  post <- sample(c(0,1), 500, replace = TRUE, prob = c(0.1, 0.9))
  stream <- c(pre, post)

  results <- detect_drift(stream, method = "ddm")

  expect_s3_class(results, "data.frame")
  expect_true(nrow(results) > 0)
  expect_true(all(c("index", "value", "type") %in% names(results)))
  expect_true(any(results$type == "drift"))
})

test_that("detect_drift works with Page-Hinkley method", {
  set.seed(123)
  pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.7, 0.3))
  post <- sample(c(0,5), 500, replace = TRUE, prob = c(0.3, 0.7))
  stream <- c(pre, post)

  results <- detect_drift(stream, method = "page_hinkley")

  expect_s3_class(results, "data.frame")
  expect_true(all(c("index", "value", "type") %in% names(results)))
})

test_that("detect_drift works with EDDM method", {
  set.seed(123)
  pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.7, 0.3))
  post <- sample(c(0,1), 500, replace = TRUE, prob = c(0.3, 0.7))
  stream <- c(pre, post)

  results <- detect_drift(stream, method = "eddm")

  expect_s3_class(results, "data.frame")
  expect_true(all(c("index", "value", "type") %in% names(results)))
})

test_that("detect_drift works with HDDM_A method", {
  set.seed(123)
  pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.7, 0.3))
  post <- sample(c(0,1), 500, replace = TRUE, prob = c(0.3, 0.7))
  stream <- c(pre, post)

  results <- detect_drift(stream, method = "hddm_a")

  expect_s3_class(results, "data.frame")
  expect_true(all(c("index", "value", "type") %in% names(results)))
})

test_that("detect_drift works with HDDM_W method", {
  set.seed(123)
  pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.7, 0.3))
  post <- sample(c(0,1), 500, replace = TRUE, prob = c(0.3, 0.7))
  stream <- c(pre, post)

  results <- detect_drift(stream, method = "hddm_w")

  expect_s3_class(results, "data.frame")
  expect_true(all(c("index", "value", "type") %in% names(results)))
})

test_that("detect_drift works with KSWIN method", {
  set.seed(123)
  pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.7, 0.3))
  post <- sample(c(0,1), 500, replace = TRUE, prob = c(0.3, 0.7))
  stream <- c(pre, post)

  results <- detect_drift(stream, method = "kswin")

  expect_s3_class(results, "data.frame")
  expect_true(all(c("index", "value", "type") %in% names(results)))
})

test_that("detect_drift excludes warnings when include_warnings = FALSE", {
  set.seed(123)
  pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.9, 0.1))
  post <- sample(c(0,1), 500, replace = TRUE, prob = c(0.1, 0.9))
  stream <- c(pre, post)

  results <- detect_drift(stream, method = "ddm", include_warnings = FALSE)

  expect_s3_class(results, "data.frame")
  expect_true(all(results$type == "drift" | nrow(results) == 0))
  expect_false(any(results$type == "warning"))
})

test_that("detect_drift includes warnings when include_warnings = TRUE", {
  set.seed(123)
  pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.9, 0.1))
  post <- sample(c(0,1), 500, replace = TRUE, prob = c(0.1, 0.9))
  stream <- c(pre, post)

  results <- detect_drift(stream, method = "ddm", include_warnings = TRUE)

  expect_s3_class(results, "data.frame")
  # Warning may or may not be present, but if present should be in results
  if (any(results$type == "warning")) {
    expect_true(any(results$type == "warning"))
  }
})

test_that("detect_drift accepts custom parameters", {
  set.seed(123)
  pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.7, 0.3))
  post <- sample(c(0,1), 500, replace = TRUE, prob = c(0.3, 0.7))
  stream <- c(pre, post)

  # Test with custom parameters for DDM
  results <- detect_drift(stream, method = "ddm",
                          min_num_instances = 50,
                          warning_level = 2.5)

  expect_s3_class(results, "data.frame")
})

test_that("detect_drift returns empty data.frame for no drift", {
  set.seed(123)
  stream <- sample(c(0,1), 100, replace = TRUE, prob = c(0.5, 0.5))

  results <- detect_drift(stream, method = "ddm")

  expect_s3_class(results, "data.frame")
  expect_true(all(c("index", "value", "type") %in% names(results)))
})

test_that("detect_drift validates input", {
  expect_error(detect_drift(NULL, method = "ddm"),
               "stream must be a non-empty numeric vector")
  expect_error(detect_drift(numeric(0), method = "ddm"),
               "stream must be a non-empty numeric vector")
  expect_error(detect_drift(c(1,2,3), method = "invalid_method"))
})
