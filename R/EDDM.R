#' EDDM (Early Drift Detection Method)
#'
#' @description
#' This class implements the Early Drift Detection Method (EDDM), designed to detect
#' concept drifts in online learning scenarios by monitoring the distances between consecutive errors.
#' EDDM is particularly useful for detecting gradual drifts earlier than abrupt changes.
#'
#' @details
#' EDDM is a statistical process control method that is more sensitive to changes that happen
#' more slowly and can provide early warnings of deterioration before the error rate increases
#' significantly.
#'
#' @references
#' Early Drift Detection Method. Manuel Baena-Garcia, Jose Del Campo-Avila,
#' Raúl Fidalgo, Albert Bifet, Ricard Gavalda, Rafael Morales-Bueno. In Fourth
#' International Workshop on Knowledge Discovery from Data Streams, 2006.
#' Implementation: https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d1cc79988a6df40da35312e00f6c4eabb2/src/skmultiflow/drift_detection/eddm.py
#' @import R6
#' @export
EDDM <- R6Class(
  classname = "EDDM",
  public = list(
    #' @field eddm_warning Warning threshold setting.
    eddm_warning = NULL,
    #' @field eddm_outcontrol Out-of-control threshold setting.
    eddm_outcontrol = NULL,
    #' @field m_num_errors Current number of errors encountered.
    m_num_errors = NULL,
    #' @field m_min_num_errors Minimum number of errors to initialize drift detection.
    m_min_num_errors = 30,
    #' @field m_n Total instances processed.
    m_n = NULL,
    #' @field m_d Distance to the last error from the current instance.
    m_d = NULL,
    #' @field m_lastd Distance to the previous error from the last error.
    m_lastd = NULL,
    #' @field m_mean Mean of the distances between errors.
    m_mean = NULL,
    #' @field m_std_temp Temporary standard deviation accumulator for the distances.
    m_std_temp = NULL,
    #' @field m_m2s_max Maximum mean plus two standard deviations observed.
    m_m2s_max = NULL,
    #' @field delay Delay count since the last detected change.
    delay = NULL,
    #' @field estimation Current estimated mean distance between errors.
    estimation = NULL,
    #' @field warning_detected Boolean indicating if a warning has been detected.
    warning_detected = FALSE,
    #' @field change_detected Boolean indicating if a change has been detected.
    change_detected = FALSE,

    #' @description
    #' Initializes the EDDM detector with specific parameters.
    #' @param min_num_instances Minimum number of errors before drift detection starts.
    #' @param eddm_warning Threshold for warning level.
    #' @param eddm_outcontrol Threshold for out-of-control level.
    initialize = function(min_num_instances = 30,
                          eddm_warning = 0.95,
                          eddm_outcontrol = 0.9) {
      self$reset()
      self$m_min_num_errors <- min_num_instances
      self$eddm_warning <- eddm_warning
      self$eddm_outcontrol <- eddm_outcontrol
    },

    #' @description
    #' Resets the internal state of the EDDM detector.
    reset = function() {
      self$m_n <- 1
      self$m_num_errors <- 0
      self$m_d <- 0
      self$m_lastd <- 0
      self$m_mean <- 0.0
      self$m_std_temp <- 0.0
      self$m_m2s_max <- 0.0
      self$change_detected <- FALSE
      self$warning_detected <- FALSE
    },

    #' @description
    #' Adds a new observation and updates the drift detection status.
    #' @param prediction Numeric value representing a new error (usually 0 or 1).
    add_element = function(prediction) {
      if (self$change_detected)
        self$reset()

      self$m_n <- self$m_n + 1

      if (prediction == 1) {
        self$warning_detected <- FALSE
        self$delay <- 0
        self$m_num_errors <- self$m_num_errors + 1
        self$m_lastd <- self$m_d
        self$m_d <- self$m_n - 1
        distance <- self$m_d - self$m_lastd

        old_mean <- self$m_mean
        self$m_mean <- self$m_mean + (distance - self$m_mean) /
          self$m_num_errors
        self$estimation <- self$m_mean
        self$m_std_temp <-
          self$m_std_temp + (distance - self$m_mean) *
          (distance - old_mean)
        std <- sqrt(self$m_std_temp / self$m_num_errors)
        m2s <- self$m_mean + 2 * std

        if (self$m_n < self$m_min_num_errors)
          return(NULL)

        if (m2s > self$m_m2s_max) {
          self$m_m2s_max <- m2s
        } else {
          p <- m2s / self$m_m2s_max

          if (self$m_num_errors > self$m_min_num_errors &&
              p < self$eddm_outcontrol) {
            self$change_detected <- TRUE
          } else if (self$m_num_errors > self$m_min_num_errors &&
                     p < self$eddm_warning) {
            self$warning_detected <- TRUE
          } else {
            self$warning_detected <- FALSE
          }
        }
      }
    }
  )
)
