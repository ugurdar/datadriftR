#' Page-Hinkley Test for Change Detection
#'
#' @description
#' Implements the Page-Hinkley test, a sequential analysis technique used to detect
#' changes in the average value of a continuous signal or process. It is effective
#' in detecting small but persistent changes over time, making it suitable for real-time
#' monitoring applications.
#'
#' @details
#' The Page-Hinkley test is a type of cumulative sum (CUSUM) test that accumulates differences
#' between data points and a reference value (running mean). It triggers a change detection
#' signal when the cumulative sum exceeds a predefined threshold. This test is especially
#' useful for early detection of subtle shifts in the behavior of the monitored process.
#'
#' @references
#' E. S. Page. 1954. Continuous Inspection Schemes.
#' Biometrika 41, 1/2 (1954), 100–115.
#'
#' Montiel, Jacob, et al. "Scikit-Multiflow: A Multi-output Streaming Framework." Journal of Machine
#' Learning Research, 2018. This framework provides tools for multi-output and stream data mining
#' and was an inspiration for some of the implementations in this class.
#'
#' Implementation: https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d1cc79988a6df40da35312e00f6c4eabb2/src/skmultiflow/drift_detection/page_hinkley.py
#' @examples
#' set.seed(123)  # Setting a seed for reproducibility
#' data_part1 <- sample(c(0, 1), size = 100, replace = TRUE, prob = c(0.7, 0.3))
#'
#' # Introduce a change in data distribution
#' data_part2 <- sample(c(0, 5), size = 100, replace = TRUE, prob = c(0.3, 0.7))
#'
#' # Combine the two parts
#' data_stream <- c(data_part1, data_part2)
#' ph <- PageHinkley$new()
#' for (i in seq_along(data_stream)) {
#'   ph$add_element(data_stream[i])
#'   if (ph$detected_change()) {
#'     cat(sprintf("Change has been detected in data: %s - at index: %d\n", data_stream[i], i))
#'   }
#' }
#' @import R6
#' @export
PageHinkley <- R6Class(
  "PageHinkley",
  public = list(
    #' @field min_instances Minimum number of instances required to start detection.
    min_instances = 30,
    #' @field delta Minimal change considered significant for detection.
    delta = 0.005,
    #' @field threshold Decision threshold for signaling a change.
    threshold = 50,
    #' @field alpha Forgetting factor for the cumulative sum calculation.
    alpha = 1 - 0.0001,
    #' @field x_mean Running mean of the observed values.
    x_mean = NULL,
    #' @field sample_count Counter for the number of samples seen.
    sample_count = NULL,
    #' @field sum Cumulative sum used in the change detection.
    sum = NULL,
    #' @field change_detected Boolean indicating if a drift has been detected.
    change_detected = FALSE,

    #' @description
    #' Initializes the Page-Hinkley test with specific parameters.
    #' @param min_instances Minimum number of samples before detection starts.
    #' @param delta Change magnitude to trigger detection.
    #' @param threshold Cumulative sum threshold for change detection.
    #' @param alpha Weight for older data in cumulative sum.
    initialize = function(min_instances = 30,
                          delta = 0.005,
                          threshold = 50,
                          alpha = 1 - 0.0001) {
      self$min_instances <- min_instances
      self$delta <- delta
      self$threshold <- threshold
      self$alpha <- alpha
      self$reset()
    },
    #' @description
    #' Resets all the internal states of the detector to initial values.
    reset = function() {
      self$sample_count <- 1
      self$x_mean <- 0.0
      self$sum <- 0.0
      self$change_detected <- FALSE
    },
    #' @description
    #' Adds a new element to the data stream and updates the detection status based on the Page-Hinkley test.
    #' @param x New data value to add and evaluate.
    add_element = function(x) {
      if (self$change_detected)
        self$reset()

      self$x_mean <-
        self$x_mean + (x - self$x_mean) /
        self$sample_count
      self$sum <-
        max(0, self$alpha * self$sum +
              (x - self$x_mean - self$delta))
      self$sample_count <-
        self$sample_count + 1

      if (self$sample_count < self$min_instances)
        return(NULL)

      if (self$sum > self$threshold)
        self$change_detected <- TRUE
    },
    #' @description
    #' Checks if a change has been detected based on the last update.
    #' @return Boolean indicating whether a change was detected.
    detected_change = function() {
      return(self$change_detected)
    }
  )
)
