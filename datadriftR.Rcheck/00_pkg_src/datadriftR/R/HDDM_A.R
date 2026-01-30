#' HDDM_A: Drift Detection Method based on Adaptive Windows
#'
#' @description
#' This class implements the HDDM_A drift detection method that uses adaptive windows
#' to detect changes in the mean of a data stream. It is designed to monitor online streams
#' of data and can detect increases or decreases in the process mean in a non-parametric and
#' online manner.
#'
#' @details
#' HDDM_A adapts to changes in the data stream by adjusting its internal windows to track
#' the minimum and maximum values of the process mean. It triggers alerts when a significant
#' drift from these benchmarks is detected.
#'
#' @references
#' Frías-Blanco I, del Campo-Ávila J, Ramos-Jimenez G, et al. Online and non-parametric drift
#' detection methods based on Hoeffding’s bounds. IEEE Transactions on Knowledge and Data
#' Engineering, 2014, 27(3): 810-823.
#'
#' Albert Bifet, Geoff Holmes, Richard Kirkby, Bernhard Pfahringer. MOA: Massive Online Analysis;
#' Journal of Machine Learning Research 11: 1601-1604, 2010.
#'
#' Implementation: https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d1cc79988a6df40da35312e00f6c4eabb2/src/skmultiflow/drift_detection/hddm_a.py
#' @examples
#' set.seed(123)  # Setting a seed for reproducibility
#' data_part1 <- sample(c(0, 1), size = 100, replace = TRUE, prob = c(0.7, 0.3))
#'
#' # Introduce a change in data distribution
#' data_part2 <- sample(c(0, 1), size = 100, replace = TRUE, prob = c(0.3, 0.7))
#'
#' # Combine the two parts
#' data_stream <- c(data_part1, data_part2)
#'
#' # Initialize the hddm_a object
#' hddm_a_instance <- HDDM_A$new()
#'
#' # Iterate through the data stream
#' for(i in seq_along(data_stream)) {
#'   hddm_a_instance$add_element(data_stream[i])
#'   if(hddm_a_instance$warning_detected) {
#'     message(paste("Warning detected at index:", i))
#'   }
#'   if(hddm_a_instance$change_detected) {
#'     message(paste("Concept drift detected at index:", i))
#'   }
#' }
#' @import R6
#' @export
HDDM_A <- R6Class(
  "HDDM_A",
  public = list(
    #' @field drift_confidence Confidence level for detecting a drift.
    drift_confidence = 0.001,
    #' @field warning_confidence Confidence level for warning detection.
    warning_confidence = 0.005,
    #' @field two_side_option Boolean flag for one-sided or two-sided mean monitoring.
    two_side_option = TRUE,
    #' @field total_n Total number of samples seen.
    total_n = 0,
    #' @field total_c Total cumulative sum of the samples.
    total_c = 0,
    #' @field n_max Maximum window end for sample count.
    n_max = 0,
    #' @field c_max Maximum window end for cumulative sum.
    c_max = 0,
    #' @field n_min Minimum window start for sample count.
    n_min = 0,
    #' @field c_min Minimum window start for cumulative sum.
    c_min = 0,
    #' @field n_estimation Number of samples since the last detected change.
    n_estimation = 0,
    #' @field c_estimation Cumulative sum since the last detected change.
    c_estimation = 0,
    #' @field change_detected Boolean indicating if a change was detected.
    change_detected = FALSE,
    #' @field warning_detected Boolean indicating if a warning has been detected.
    warning_detected = FALSE,
    #' @field estimation Current estimated mean of the stream.
    estimation = 0,
    #' @field delay Current delay since the last update.
    delay = 0,

    #' @description
    #' Initializes the HDDM_A detector with specific settings.
    #' @param drift_confidence Confidence level for drift detection.
    #' @param warning_confidence Confidence level for issuing warnings.
    #' @param two_side_option Whether to monitor both increases and decreases.
    initialize = function(drift_confidence = 0.001,
                          warning_confidence = 0.005,
                          two_side_option = TRUE) {
      self$drift_confidence <- drift_confidence
      self$warning_confidence <- warning_confidence
      self$two_side_option <- two_side_option
      self$reset()
    },

    #' @description
    #' Adds an element to the data stream and updates the detection status.
    #' @param prediction Numeric, the new data value to add.
    add_element = function(prediction) {
      self$total_n <- self$total_n + 1
      self$total_c <- self$total_c + prediction

      if (self$n_min == 0) {
        self$n_min <- self$total_n
        self$c_min <- self$total_c
      }
      if (self$n_max == 0) {
        self$n_max <- self$total_n
        self$c_max <- self$total_c
      }

      cota <-
        sqrt(1 / (2 * self$n_min) *
               log(1 / self$drift_confidence))
      cota1 <-
        sqrt(1 / (2 * self$total_n) *
               log(1 / self$drift_confidence))

      if (self$c_min / self$n_min + cota >=
          self$total_c / self$total_n + cota1) {
        self$c_min <- self$total_c
        self$n_min <- self$total_n
      }

      cota <- sqrt(1 / (2 * self$n_max) *
                     log(1 / self$drift_confidence))
      if (self$c_max / self$n_max - cota <=
          self$total_c / self$total_n - cota1) {
        self$c_max <- self$total_c
        self$n_max <- self$total_n
      }

      if (self$mean_incr(self$c_min,
                         self$n_min,
                         self$total_c,
                         self$total_n,
                         self$drift_confidence)) {
        self$n_estimation <- self$total_n - self$n_min
        self$c_estimation <-
          self$total_c - self$c_min
        self$n_min <-
          self$n_max <- self$total_n <- 0
        self$c_min <-
          self$c_max <- self$total_c <- 0
        self$change_detected <- TRUE
        self$warning_detected <- FALSE
      } else if (self$mean_incr(self$c_min,
                                self$n_min,
                                self$total_c,
                                self$total_n,
                                self$warning_confidence)) {
        self$change_detected <- FALSE
        self$warning_detected <- TRUE
      } else {
        self$change_detected <- FALSE
        self$warning_detected <- FALSE
      }

      if (self$two_side_option &&
          self$mean_decr(self$c_max, self$n_max,
                         self$total_c, self$total_n)) {
        self$n_estimation <- self$total_n - self$n_max
        self$c_estimation <-
          self$total_c - self$c_max
        self$n_min <-
          self$n_max <- self$total_n <- 0
        self$c_min <-
          self$c_max <- self$total_c <- 0
      }

      self$update_estimations()
    },
    #' @description
    #' Calculates if there is an increase in the mean.
    #' @param c_min Minimum cumulative sum.
    #' @param n_min Minimum count of samples.
    #' @param total_c Total cumulative sum.
    #' @param total_n Total number of samples.
    #' @param confidence Confidence threshold for detection.
    mean_incr = function(c_min, n_min, total_c, total_n, confidence) {
      if (n_min == total_n)
        return(FALSE)


      m <- (total_n - n_min) / n_min * (1 / total_n)
      cota <- sqrt(m / 2 * log(2 / confidence))
      return(total_c / total_n - c_min / n_min >= cota)
    },

    #' @description
    #' Calculates if there is a decrease in the mean.
    #' @param c_max Maximum cumulative sum.
    #' @param n_max Maximum count of samples.
    #' @param total_c Total cumulative sum.
    #' @param total_n Total number of samples.
    mean_decr = function(c_max, n_max, total_c, total_n) {
      if (n_max == total_n)
        return(FALSE)


      m <- (total_n - n_max) / n_max * (1 / total_n)
      cota <-
        sqrt(m / 2 * log(2 / self$drift_confidence))
      return(c_max / n_max - total_c / total_n >= cota)
    },

    #' @description
    #' Resets all internal counters and accumulators to their initial state.
    reset = function() {
      self$n_min <- 0
      self$c_min <- 0
      self$total_n <- 0
      self$total_c <- 0
      self$n_max <- 0
      self$c_max <- 0
      self$c_estimation <- 0
      self$n_estimation <- 0
    },

    #' @description
    #' Updates estimations of the mean after detecting changes.
    update_estimations = function() {
      if (self$total_n >= self$n_estimation) {
        self$c_estimation <- self$n_estimation <- 0
        self$estimation <-
          self$total_c / self$total_n
        self$delay <- self$total_n
      } else {
        self$estimation <- self$c_estimation / self$n_estimation
        self$delay <- self$n_estimation
      }
    }
  )
)
