#' DDM (Drift Detection Method)
#'
#' @description
#' Implements the Drift Detection Method (DDM), used for detecting concept drift in data streams
#' by analyzing the performance of online learners. The method monitors changes in the error rate
#' of a learner, signaling potential concept drift.
#'
#' @details
#' DDM is designed to be simple yet effective for detecting concept drift by monitoring
#' the error rate of any online classifier. The method is particularly sensitive to
#' increases in the error rate, which is typically a strong indicator of concept drift.
#'
#' @references
#' João Gama, Pedro Medas, Gladys Castillo, Pedro Pereira Rodrigues: Learning
#' with Drift Detection. SBIA 2004: 286-295
#'
#' Implementation: https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d1cc79988a6df40da35312e00f6c4eabb2/src/skmultiflow/drift_detection/ddm.py
#' @examples
#' set.seed(123)  # Setting a seed for reproducibility
#' data_part1 <- sample(c(0, 1), size = 100, replace = TRUE, prob = c(0.7, 0.3))
#'
#' # Introduce a change in data distribution
#' data_part2 <- sample(c(0, 1), size = 100, replace = TRUE, prob = c(0.3, 0.7))
#'
#' # Combine the two parts
#' data_stream <- c(data_part1, data_part2)
#' ddm <- DDM$new()
#'# Iterate through the data stream
#' for (i in seq_along(data_stream)) {
#'   ddm$add_element(data_stream[i])
#'   if (ddm$change_detected) {
#'     message(paste("Drift detected!", i))
#'   } else if (ddm$warning_detected) {
#'     # message(paste("Warning detected at position:", i))
#'   }
#'}
#' @import R6
#' @export
DDM <- R6::R6Class(
  "DDM",
  public = list(
    #' @field min_instances Minimum number of instances required before drift detection begins.
    min_instances = NULL,

    #' @field warning_level Multiplier for the standard deviation to set the warning threshold.
    warning_level = NULL,

    #' @field out_control_level Multiplier for the standard deviation to set the out-of-control threshold.
    out_control_level = NULL,

    #' @field sample_count Counter for the number of samples processed.
    sample_count = NULL,

    #' @field miss_prob Current estimated probability of misclassification.
    miss_prob = NULL,

    #' @field miss_std Current estimated standard deviation of misclassification probability.
    miss_std = NULL,

    #' @field miss_prob_sd_min Minimum recorded value of misclassification probability plus its standard deviation.
    miss_prob_sd_min = NULL,

    #' @field miss_prob_min Minimum recorded misclassification probability.
    miss_prob_min = NULL,

    #' @field miss_sd_min Minimum recorded standard deviation.
    miss_sd_min = NULL,

    #' @field estimation Current estimation of misclassification probability.
    estimation = NULL,

    #' @field change_detected Boolean indicating if a drift has been detected.
    change_detected = FALSE,

    #' @field warning_detected Boolean indicating if a warning level has been reached.
    warning_detected = FALSE,

    #' @field delay Delay since the last relevant sample.
    delay = NULL,

    #' @description
    #' Initializes the DDM detector with specific parameters.
    #' @param min_num_instances Minimum number of samples required before starting drift detection.
    #' @param warning_level Threshold multiplier for setting a warning level.
    #' @param out_control_level Threshold multiplier for setting the out-of-control level.
    initialize = function(min_num_instances = 30,
                          warning_level = 2.0,
                          out_control_level = 3.0) {
      self$min_instances <- min_num_instances
      self$warning_level <- warning_level
      self$out_control_level <- out_control_level
      self$reset()
    },

    #' @description
    #' Resets the internal state of the DDM detector.
    reset = function() {
      self$sample_count <- 1
      self$miss_prob <- 1
      self$miss_std <- 0
      self$miss_prob_sd_min <- Inf
      self$miss_prob_min <- Inf
      self$miss_sd_min <- Inf
    },

    #' @description
    #' Adds a new prediction error value to the model, updates the calculation of the misclassification
    #' probability and its standard deviation, and checks for warnings or drifts based on updated statistics.
    #' @param prediction The new data point (prediction error) to be added to the model.
    add_element = function(prediction) {
      if (self$change_detected)
        self$reset()

      self$miss_prob <-
        (self$miss_prob + (prediction - self$miss_prob) /
           as.numeric(self$sample_count))
      self$miss_std <-
        sqrt(self$miss_prob * (1 - self$miss_prob) /
               as.numeric(self$sample_count))
      self$sample_count <- self$sample_count + 1

      self$estimation <- self$miss_prob
      self$change_detected <- FALSE
      self$warning_detected <- FALSE
      self$delay <- 0

      if (self$sample_count < self$min_instances)
        return(NULL)

      if (self$miss_prob + self$miss_std <= self$miss_prob_sd_min) {
        self$miss_prob_min <- self$miss_prob
        self$miss_sd_min <- self$miss_std
        self$miss_prob_sd_min <-
          self$miss_prob + self$miss_std
      }

      if (self$miss_prob + self$miss_std > self$miss_prob_min +
          self$out_control_level * self$miss_sd_min) {
        self$change_detected <- TRUE
      } else if (self$miss_prob + self$miss_std > self$miss_prob_min +
                 self$warning_level * self$miss_sd_min) {
        self$warning_detected <- TRUE
      } else {
        self$warning_detected <- FALSE
      }

    },

    #' @description
    #' Returns a boolean indicating whether a drift has been detected based on the monitored statistics.
    detected_change = function() {
      return(self$change_detected)
    }
  )
)
