#' ADWIN (ADaptive WINdowing) Drift Detector
#'
#' @description
#' ADWIN is a drift detection method that maintains a variable-length window
#' of recent data to detect concept drift with mathematical guarantees.
#' Based on the algorithm from Bifet and Gavalda (2007).
#'
#' @details
#' The algorithm maintains a sliding window W with the most recent elements.
#' It compares the distribution of two sub-windows (W0 and W1) and detects
#' drift when their means differ significantly according to the Hoeffding bound.
#' Uses bucket compression for memory efficiency.
#'
#' @references
#' Bifet, A., & Gavalda, R. (2007). Learning from time-changing data with
#' adaptive windowing. In Proceedings of the 2007 SIAM International Conference
#' on Data Mining (pp. 443-448).
#'
#' @examples
#' set.seed(12345)
#' adwin <- ADWIN$new()
#'
#' # Stream: 1000 values from {0,1}, then 1000 values from {4,5,6,7}
#' stream <- c(sample(0:1, 1000, replace = TRUE),
#'             sample(4:7, 1000, replace = TRUE))
#'
#' for (i in seq_along(stream)) {
#'   adwin$add_element(stream[i])
#'   if (adwin$detected_change()) {
#'     message("Change detected at index ", i, ", input value: ", stream[i])
#'   }
#' }
#'
#' @import R6
#' @export
ADWIN <- R6Class(
  "ADWIN",
  public = list(
    #' @field delta Significance threshold for drift detection
    delta = 0.002,
    #' @field clock Frequency of drift checks
    clock = 32,
    #' @field max_buckets Maximum buckets per level
    max_buckets = 5,
    #' @field min_window_length Minimum window length for comparison
    min_window_length = 5,
    #' @field grace_period Initial period before detection starts
    grace_period = 10,

    #' @description

    #' Initialize ADWIN detector
    #' @param delta Significance threshold (default 0.002)
    #' @param clock Drift check frequency (default 32)
    #' @param max_buckets Max buckets per level (default 5)
    #' @param min_window_length Min window for comparison (default 5)
    #' @param grace_period Startup period (default 10)
    initialize = function(delta = 0.002,
                          clock = 32,
                          max_buckets = 5,
                          min_window_length = 5,
                          grace_period = 10) {
      self$delta <- delta
      self$clock <- clock
      self$max_buckets <- max_buckets
      self$min_window_length <- min_window_length
      self$grace_period <- grace_period
      self$reset()
    },

    #' @description
    #' Reset the detector state
    reset = function() {
      private$.n <- 0
      private$.sum <- 0
      private$.variance <- 0
      private$.width <- 0
      private$.n_detections <- 0
      private$.drift_detected <- FALSE
      private$.tick <- 0
      private$.buckets <- list()
      private$.bucket_count <- integer()
    },

    #' @description
    #' Add a new element to the detector
    #' @param value Numeric value to add
    add_element = function(value) {
      # Reset if drift was detected in previous step (like River)
      if (private$.drift_detected) {
        self$reset()
      }

      private$.drift_detected <- FALSE
      private$.n <- private$.n + 1
      private$.tick <- private$.tick + 1

      # Insert new bucket
      private$insert_bucket(value)

      # Compress buckets if needed
      private$compress_buckets()

      # Check for drift periodically
      if (private$.tick >= self$clock &&
          private$.width > self$grace_period) {
        private$.tick <- 0

        # Keep checking until no more drift or window too small
        while (private$.width > self$min_window_length * 2) {
          if (private$detect_drift_once()) {
            private$.drift_detected <- TRUE
            private$.n_detections <- private$.n_detections + 1
          } else {
            break
          }
        }
      }
    },

    #' @description
    #' Check if drift was detected
    #' @return Logical indicating drift detection
    detected_change = function() {
      return(private$.drift_detected)
    },

    #' @description
    #' Get current window width
    #' @return Integer window width
    width = function() {
      return(private$.width)
    },

    #' @description
    #' Get number of detections
    #' @return Integer count of detections
    n_detections = function() {
      return(private$.n_detections)
    },

    #' @description
    #' Get current mean estimate
    #' @return Numeric mean
    estimation = function() {
      if (private$.width == 0) return(0)
      return(private$.sum / private$.width)
    },

    #' @description
    #' Get current variance
    #' @return Numeric variance
    variance = function() {
      if (private$.width <= 1) return(0)
      return(private$.variance / private$.width)
    }
  ),

  active = list(
    #' @field drift_detected Check if drift was detected (active binding)
    drift_detected = function() {
      return(private$.drift_detected)
    }
  ),

  private = list(
    .n = 0,
    .sum = 0,
    .variance = 0,
    .width = 0,
    .n_detections = 0,
    .drift_detected = FALSE,
    .tick = 0,
    .buckets = list(),
    .bucket_count = integer(),

    insert_bucket = function(value) {
      # Add new bucket at level 0
      if (length(private$.buckets) == 0) {
        private$.buckets <- list(list(list(total = value, variance = 0, n = 1)))
        private$.bucket_count <- 1
      } else {
        if (length(private$.buckets) < 1) {
          private$.buckets[[1]] <- list()
          private$.bucket_count[1] <- 0
        }
        private$.buckets[[1]] <- c(
          private$.buckets[[1]],
          list(list(total = value, variance = 0, n = 1))
        )
        private$.bucket_count[1] <- private$.bucket_count[1] + 1
      }

      # Update statistics
      private$.width <- private$.width + 1
      old_mean <- if (private$.width > 1) {
        private$.sum / (private$.width - 1)
      } else {
        0
      }
      private$.sum <- private$.sum + value
      new_mean <- private$.sum / private$.width

      if (private$.width > 1) {
        private$.variance <- private$.variance +
          (value - old_mean) * (value - new_mean)
      }
    },

    compress_buckets = function() {
      level <- 1
      while (level <= length(private$.bucket_count)) {
        if (is.na(private$.bucket_count[level]) ||
            private$.bucket_count[level] <= self$max_buckets) {
          break
        }

        # Need to compress - merge two oldest buckets
        if (length(private$.buckets[[level]]) >= 2) {
          b1 <- private$.buckets[[level]][[1]]
          b2 <- private$.buckets[[level]][[2]]

          # Combined bucket
          n_combined <- b1$n + b2$n
          total_combined <- b1$total + b2$total
          mean1 <- b1$total / b1$n
          mean2 <- b2$total / b2$n
          mean_combined <- total_combined / n_combined
          var_combined <- b1$variance + b2$variance +
            b1$n * (mean1 - mean_combined)^2 +
            b2$n * (mean2 - mean_combined)^2

          new_bucket <- list(
            total = total_combined,
            variance = var_combined,
            n = n_combined
          )

          # Remove merged buckets
          private$.buckets[[level]] <- private$.buckets[[level]][-(1:2)]
          private$.bucket_count[level] <- private$.bucket_count[level] - 2

          # Add to next level
          next_level <- level + 1
          if (next_level > length(private$.buckets)) {
            private$.buckets[[next_level]] <- list()
            private$.bucket_count[next_level] <- 0
          }
          private$.buckets[[next_level]] <- c(
            private$.buckets[[next_level]],
            list(new_bucket)
          )
          private$.bucket_count[next_level] <-
            private$.bucket_count[next_level] + 1
        }

        level <- level + 1
      }
    },

    detect_drift_once = function() {
      if (private$.width < 2 * self$min_window_length) {
        return(FALSE)
      }

      # Traverse buckets from oldest to newest to find cut point
      n0 <- 0
      sum0 <- 0

      for (level in seq_along(private$.buckets)) {
        buckets_at_level <- private$.buckets[[level]]
        if (length(buckets_at_level) == 0) next

        for (j in seq_along(buckets_at_level)) {
          bucket <- buckets_at_level[[j]]

          n0 <- n0 + bucket$n
          sum0 <- sum0 + bucket$total

          n1 <- private$.width - n0
          sum1 <- private$.sum - sum0

          if (n0 < self$min_window_length || n1 < self$min_window_length) {
            next
          }

          mean0 <- sum0 / n0
          mean1 <- sum1 / n1

          # Hoeffding bound - matching River's formula
          delta_prime <- log(2 * log(private$.width) / self$delta)
          m_recip <- (1 / (n0 - self$min_window_length + 1)) +
                     (1 / (n1 - self$min_window_length + 1))
          variance_in_window <- private$.variance / private$.width
          eps <- sqrt(2 * m_recip * variance_in_window * delta_prime) +
                 (2 / 3) * delta_prime * m_recip

          if (abs(mean0 - mean1) > eps) {
            # Drift detected - remove old part (W0)
            private$remove_old_buckets(n0)
            return(TRUE)
          }
        }
      }

      return(FALSE)
    },

    remove_old_buckets = function(n_to_remove) {
      removed <- 0

      for (level in seq_along(private$.buckets)) {
        while (length(private$.buckets[[level]]) > 0 && removed < n_to_remove) {
          bucket <- private$.buckets[[level]][[1]]

          if (removed + bucket$n <= n_to_remove) {
            private$.sum <- private$.sum - bucket$total
            private$.width <- private$.width - bucket$n
            removed <- removed + bucket$n

            private$.buckets[[level]] <- private$.buckets[[level]][-1]
            private$.bucket_count[level] <- max(0, private$.bucket_count[level] - 1)
          } else {
            break
          }
        }
        if (removed >= n_to_remove) break
      }

      # Recalculate variance
      private$recalculate_variance()
    },

    recalculate_variance = function() {
      if (private$.width <= 1) {
        private$.variance <- 0
        return()
      }

      mean_val <- private$.sum / private$.width
      var_sum <- 0

      for (level in seq_along(private$.buckets)) {
        for (bucket in private$.buckets[[level]]) {
          bucket_mean <- bucket$total / bucket$n
          var_sum <- var_sum + bucket$variance +
            bucket$n * (bucket_mean - mean_val)^2
        }
      }

      private$.variance <- var_sum
    }
  )
)
