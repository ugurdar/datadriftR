test_that("KLDivergence constructor works", {
  kl <- KLDivergence$new()
  expect_s3_class(kl, "R6")
  expect_false(kl$is_drift_detected())
})

test_that("KLDivergence detects drift in synthetic distributions", {
  set.seed(444)
  # Create two different distributions
  pre  <- rnorm(100, mean = 0, sd = 1)
  post <- rnorm(100, mean = 3, sd = 1)
  
  kl <- KLDivergence$new(bins = 10, drift_level = 0.5)
  kl$set_initial_distribution(pre)
  kl$add_distribution(post)
  
  expect_true(kl$is_drift_detected())
  expect_true(kl$get_kl_result() > 0)
})

test_that("KLDivergence does not detect drift in similar distributions", {
  set.seed(555)
  # Create two similar distributions
  dist1 <- rnorm(100, mean = 0, sd = 1)
  dist2 <- rnorm(100, mean = 0, sd = 1)
  
  kl <- KLDivergence$new(bins = 10, drift_level = 0.5)
  kl$set_initial_distribution(dist1)
  kl$add_distribution(dist2)
  
  # KL divergence should be low for similar distributions
  expect_true(kl$get_kl_result() < 0.5)
})

test_that("KLDivergence accepts numeric vectors", {
  kl <- KLDivergence$new()
  dist1 <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
  dist2 <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1)
  
  expect_silent(kl$set_initial_distribution(dist1))
  expect_silent(kl$add_distribution(dist2))
})
