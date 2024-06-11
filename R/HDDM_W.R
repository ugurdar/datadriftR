#' KSWIN (Kolmogorov-Smirnov WINdowing) for Change Detection
#'
#' @description
#' Implements the Kolmogorov-Smirnov test for detecting distribution changes within
#' a window of streaming data. KSWIN is a non-parametric method for change detection that compares
#' two samples to determine if they come from the same distribution.
#'
#' @details
#' KSWIN is effective for detecting changes in the underlying distribution of data streams.
#' It is particularly useful in scenarios where data properties may evolve over time, allowing for
#' early detection of changes that might affect subsequent data processing.
#'
#' @references
#' Frías-Blanco I, del Campo-Ávila J, Ramos-Jimenez G, et al. Online and non-parametric drift detection methods based
#' on Hoeffding’s bounds. IEEE Transactions on Knowledge and Data Engineering, 2014, 27(3): 810-823.
#'
#' Albert Bifet, Geoff Holmes, Richard Kirkby, Bernhard Pfahringer. MOA: Massive Online Analysis;
#' Journal of Machine Learning Research 11: 1601-1604, 2010.
#' Implementation: https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d1cc79988a6df40da35312e00f6c4eabb2/src/skmultiflow/drift_detection/hddm_w.py
#' @import R6
#' @export
HDDM_W <- R6Class(
  "HDDM_W",
  public = list(
    #' @field drift_confidence Confidence level for detecting a drift (default: 0.001).
    drift_confidence = 0.001,
    #' @field warning_confidence Confidence level for warning detection (default: 0.005).
    warning_confidence = 0.005,
    #' @field lambda_option Decay rate for the EWMA statistic, smaller values give less weight to recent data (default: 0.050).
    lambda_option = 0.050,
    #' @field two_side_option Boolean flag for one-sided or two-sided error monitoring (default: TRUE).
    two_side_option = TRUE,
    #' @field total Container for the EWMA estimator and its bounded conditional sum.
    total = NULL,
    #' @field sample1_decr_monitor First sample monitor for detecting decrements.
    sample1_decr_monitor = NULL,
    #' @field sample1_incr_monitor First sample monitor for detecting increments.
    sample1_incr_monitor = NULL,
    #' @field sample2_decr_monitor Second sample monitor for detecting decrements.
    sample2_decr_monitor = NULL,
    #' @field sample2_incr_monitor Second sample monitor for detecting increments.
    sample2_incr_monitor = NULL,
    #' @field incr_cutpoint Cutpoint for deciding increments.
    incr_cutpoint = Inf,
    #' @field decr_cutpoint Cutpoint for deciding decrements.
    decr_cutpoint = Inf,
    #' @field width Current width of the window.
    width = 0,
    #' @field delay Delay count since last reset.
    delay = 0,
    #' @field change_detected Boolean indicating if a change was detected.
    change_detected = FALSE,
    #' @field warning_detected Boolean indicating if currently in a warning zone.
    warning_detected = FALSE,
    #' @field estimation The current estimation of the stream's mean.
    estimation = NA,
    #' @description
    #' Initializes the HDDM_W detector with specific parameters.
    #' @param drift_confidence Confidence level for drift detection.
    #' @param warning_confidence Confidence level for issuing warnings.
    #' @param lambda_option Decay rate for the EWMA statistic.
    #' @param two_side_option Whether to monitor both increases and decreases.
    initialize = function(drift_confidence = 0.001,
                          warning_confidence = 0.005,
                          lambda_option = 0.050,
                          two_side_option = TRUE) {
      self$drift_confidence <- drift_confidence
      self$warning_confidence <- warning_confidence
      self$lambda_option <- lambda_option
      self$two_side_option <- two_side_option
      self$reset()
    },
    #' @description
    #' Adds a new element to the data stream and updates the detection status.
    #' @param prediction The new data value to add.
    add_element = function(prediction) {
      aux_decay_rate <- 1.0 - self$lambda_option
      self$width <- self$width + 1
      if (is.null(self$total$EWMA_estimator) ||
          self$total$EWMA_estimator < 0) {
        self$total$EWMA_estimator <- prediction
        self$total$indp_bounded_cond_sum <- 1
      } else {
        self$total$EWMA_estimator <- self$lambda_option *
          prediction + aux_decay_rate * self$total$EWMA_estimator
        self$total$indp_bounded_cond_sum <-
          self$lambda_option ^ 2 + aux_decay_rate ^ 2 *
          self$total$indp_bounded_cond_sum
      }

      self$update_incr_statistics(prediction,
                                  self$drift_confidence)
      if (self$monitor_mean_incr(self$drift_confidence)) {
        self$reset()
        self$change_detected <- TRUE
        self$warning_detected <- FALSE
      } else if (self$monitor_mean_incr(self$warning_confidence)) {
        self$change_detected <- FALSE
        self$warning_detected <- TRUE
      } else {
        self$change_detected <- FALSE
        self$warning_detected <- FALSE
      }

      self$update_decr_statistics(prediction,
                                  self$drift_confidence)
      if (self$two_side_option &&
          self$monitor_mean_decr(self$drift_confidence)) {
        self$reset()
      }
      self$estimation <- self$total$EWMA_estimator
    },
    #' @description
    #' Provides current information about the monitoring samples, typically used for debugging or monitoring.
    SampleInfo = function() {
      list(EWMA_estimator = -1.0,
           indp_bounded_cond_sum = 1)
    },
    #' @description
    #' Resets the internal state to initial conditions.
    reset = function() {
      self$total <- self$SampleInfo()
      self$sample1_decr_monitor <- self$SampleInfo()
      self$sample1_incr_monitor <- self$SampleInfo()
      self$sample2_decr_monitor <- self$SampleInfo()
      self$sample2_incr_monitor <- self$SampleInfo()
      self$incr_cutpoint <- Inf
      self$decr_cutpoint <- Inf
      self$width <- 0
      self$delay <- 0
      self$change_detected <- FALSE
      self$warning_detected <- FALSE
    },
    #' @description
    #' Detects an increment in the mean between two samples based on the provided confidence level.
    #' @param sample1 First sample information, containing EWMA estimator and bounded conditional sum.
    #' @param sample2 Second sample information, containing EWMA estimator and bounded conditional sum.
    #' @param confidence The confidence level used for calculating the bound.
    #' @return Boolean indicating if an increment in mean was detected.
    detect_mean_increment = function(sample1, sample2,
                                     confidence) {
      if (sample1$EWMA_estimator < 0 ||
          sample2$EWMA_estimator < 0) {
        return(FALSE)
      }
      ibc_sum <- sample1$indp_bounded_cond_sum +
        sample2$indp_bounded_cond_sum
      bound <-
        sqrt(ibc_sum * log(1 / confidence) / 2)
      return(sample2$EWMA_estimator -
               sample1$EWMA_estimator > bound)
    },
    #' @description
    #' Monitors the data stream for an increase in the mean based on the set confidence level.
    #' @param confidence The confidence level used to detect changes in the mean.
    #' @return Boolean indicating if an increase in the mean was detected.
    monitor_mean_incr = function(confidence) {
      return(
        self$detect_mean_increment(
          self$sample1_incr_monitor,
          self$sample2_incr_monitor,
          confidence
        )
      )
    },
    #' @description
    #' Monitors the data stream for a decrease in the mean based on the set confidence level.
    #' @param confidence The confidence level used to detect changes in the mean.
    #' @return Boolean indicating if a decrease in the mean was detected.
    monitor_mean_decr = function(confidence) {
      return(
        self$detect_mean_increment(
          self$sample2_decr_monitor,
          self$sample1_decr_monitor,
          confidence
        )
      )
    },
    #' @description
    #' Updates increment statistics for drift monitoring based on new values and confidence.
    #' This method adjusts the cutpoint for increments and updates the monitoring samples.
    #' @param value The new value to update statistics.
    #' @param confidence The confidence level for the update.
    update_incr_statistics = function(value, confidence) {
      aux_decay <- 1.0 - self$lambda_option
      bound <- sqrt(self$total$indp_bounded_cond_sum *
                      log(1.0 / confidence) / 2)

      if (self$total$EWMA_estimator + bound <
          self$incr_cutpoint) {
        self$incr_cutpoint <- self$total$EWMA_estimator + bound
        self$sample1_incr_monitor$EWMA_estimator <-
          self$total$EWMA_estimator
        self$sample1_incr_monitor$indp_bounded_cond_sum <-
          self$total$indp_bounded_cond_sum
        self$sample2_incr_monitor <-
          self$SampleInfo()
        self$delay <- 0
      } else {
        self$delay <- self$delay + 1
        if (self$sample2_incr_monitor$EWMA_estimator < 0) {
          self$sample2_incr_monitor$EWMA_estimator <- value
          self$sample2_incr_monitor$indp_bounded_cond_sum <-
            1
        } else {
          self$sample2_incr_monitor$EWMA_estimator <-
            self$lambda_option * value + aux_decay *
            self$sample2_incr_monitor$EWMA_estimator
          self$sample2_incr_monitor$indp_bounded_cond_sum <-
            self$lambda_option ^ 2 + aux_decay ^ 2 *
            self$sample2_incr_monitor$indp_bounded_cond_sum
        }
      }
    },
    #' @description
    #' Updates decrement statistics for drift monitoring based on new values and confidence.
    #' This method adjusts the cutpoint for decrements and updates the monitoring samples.
    #' @param value The new value to update statistics.
    #' @param confidence The confidence level for the update.
    update_decr_statistics = function(value, confidence) {
      aux_decay <- 1.0 - self$lambda_option
      epsilon <-
        sqrt(self$total$indp_bounded_cond_sum *log(1.0 / confidence) / 2)


      if (length(self$total$EWMA_estimator) == 0 ||
          length(epsilon) == 0) {
        cat("Error: Variables not properly initialized.\n")
        return(NULL)
      }

      if ((self$total$EWMA_estimator - epsilon) >
          self$decr_cutpoint) {
        self$decr_cutpoint <-
          self$total$EWMA_estimator - epsilon
        self$sample1_decr_monitor$EWMA_estimator <-
          self$total$EWMA_estimator
        self$sample1_decr_monitor$indp_bounded_cond_sum <-
          self$total$indp_bounded_cond_sum
        self$sample2_decr_monitor <-
          self$SampleInfo()
      } else {
        if (self$sample2_decr_monitor$EWMA_estimator < 0) {
          self$sample2_decr_monitor$EWMA_estimator <- value
          self$sample2_decr_monitor$indp_bounded_cond_sum <-
            1
        } else {
          self$sample2_decr_monitor$EWMA_estimator <-
            self$lambda_option * value + aux_decay *
            self$sample2_decr_monitor$EWMA_estimator
          self$sample2_decr_monitor$indp_bounded_cond_sum <-
            self$lambda_option ^ 2 + aux_decay ^ 2 *
            self$sample2_decr_monitor$indp_bounded_cond_sum
        }
      }
    }
  )
)
