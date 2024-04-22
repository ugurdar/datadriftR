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
#' Christoph Raab, Moritz Heusinger, Frank-Michael Schleif, Reactive
#' Soft Prototype Computing for Concept Drift Streams, Neurocomputing, 2020.
#' @import R6
#' @export
KSWIN <- R6Class(
  classname = "KSWIN",
  public = list(
    #' @field alpha Significance level for the KS test.
    alpha = 0.005,
    #' @field window_size Total size of the data window used for testing.
    window_size = 100,
    #' @field stat_size Number of data points sampled from the window for the KS test.
    stat_size = 30,
    #' @field window Current data window used for change detection.
    window = NULL,
    #' @field change_detected Boolean flag indicating whether a change has been detected.
    change_detected = FALSE,
    #' @field p_value P-value of the most recent KS test.
    p_value = NULL,

    #' @description
    #' Initializes the KSWIN detector with specific settings.
    #' @param alpha The significance level for the KS test.
    #' @param window_size The size of the data window for change detection.
    #' @param stat_size The number of samples in the statistical test window.
    #' @param data Initial data to populate the window, if provided.
    initialize = function(alpha = 0.005,
                          window_size = 100,
                          stat_size = 30,
                          data = NULL) {
      if (alpha < 0 || alpha > 1) {
        stop("Alpha must be between 0 and 1.")
      }
      if (window_size <= 0) {
        stop("Window size must be greater than 0.")
      }
      if (window_size < stat_size) {
        stop("Stat size must be smaller than window size.")
      }
      self$alpha <- alpha
      self$window_size <- window_size
      self$stat_size <- stat_size
      self$window <- if (is.null(data)) numeric() else data
      self$p_value <- 0
    },
    #' @description
    #' Resets the internal state of the detector to its initial conditions.
    reset = function() {
      self$window <- numeric()
      self$change_detected <- FALSE
      self$p_value <- 0
    },
    #' @description
    #' Adds a new element to the data window and updates the detection status based on the KS test.
    #' @param x The new data value to add to the window.
    add_element = function(x) {
      if (length(self$window) >= self$window_size) {
        self$window <- self$window[-1]
        rnd_indices <- sample(1:(length(self$window) - self$stat_size),
                              self$stat_size,replace=TRUE)
        rnd_window <- self$window[rnd_indices]

        last_window <- tail(self$window, self$stat_size)

        test_result <- suppressWarnings(ks.test(rnd_window, last_window))
        self$p_value <- test_result$p.value

        if (self$p_value <= self$alpha & test_result$statistic > 0.1) {
          self$change_detected <- TRUE
          self$window <- tail(self$window, self$stat_size)
        } else {
          self$change_detected <- FALSE
        }
      }else{
        self$change_detected <- FALSE
      }
      self$window <- c(self$window, x)
    },
    #' @description
    #' Checks if a change has been detected based on the most recent KS test.
    #' @return Boolean indicating whether a change was detected.
    detected_change = function() {
      return(self$change_detected)
    }
  )
)
