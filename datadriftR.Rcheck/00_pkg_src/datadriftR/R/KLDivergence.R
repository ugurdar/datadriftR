#' Kullback-Leibler Divergence (KLD) for Change Detection
#'
#' @description
#' Implements the Kullback-Leibler Divergence (KLD) calculation between two probability distributions
#' using histograms. The class can detect drift by comparing the divergence to a predefined threshold.
#'
#' @details
#' The Kullback-Leibler Divergence (KLD) is a measure of how one probability distribution diverges from a second,
#' expected probability distribution. This class uses histograms to approximate the distributions and calculates the
#' KLD to detect changes over time. If the divergence exceeds a predefined threshold, it signals a detected drift.
#'
#' @references
#' Kullback, S., and Leibler, R.A. (1951). On Information and Sufficiency. Annals of Mathematical Statistics, 22(1), 79-86.
#'
#' @examples
#' set.seed(123)  # Setting a seed for reproducibility
#' initial_data <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
#' kld <- KLDivergence$new(bins = 10, drift_level = 0.2)
#' kld$set_initial_distribution(initial_data)
#'
#' new_data <- c(0.2, 0.2, 0.3, 0.4, 0.4, 0.5, 0.6, 0.7, 0.7, 0.8)
#' kld$add_distribution(new_data)
#'
#' kl_result <- kld$get_kl_result()
#' message(paste("KL Divergence:", kl_result))
#'
#' if (kld$is_drift_detected()) {
#'   message("Drift detected.")
#' }
#' @import R6
#' @export
KLDivergence <- R6Class(
  "KLDivergence",
  public = list(
    #' @field epsilon Value to add to small probabilities to avoid log(0) issues.
    epsilon = 1e-10,
    #' @field base The base of the logarithm used in KLD calculation.
    base = exp(1),
    #' @field bins Number of bins used for the histogram.
    bins = 10,
    #' @field drift_level The threshold for detecting drift.
    drift_level = 0.2,
    #' @field drift_detected Boolean indicating if drift has been detected.
    drift_detected = FALSE,
    #' @field p Initial distribution.
    p = NULL,
    #' @field kl_result The result of the KLD calculation.
    kl_result = NULL,

    #' @description
    #' Initializes the KLDivergence class.
    #' @param epsilon Value to add to small probabilities to avoid log(0) issues.
    #' @param base The base of the logarithm used in KLD calculation.
    #' @param bins Number of bins used for the histogram.
    #' @param drift_level The threshold for detecting drift.
    initialize = function(epsilon = 1e-10,
                          base = exp(1),
                          bins = 10,
                          drift_level = 0.2) {
      self$epsilon <- epsilon
      self$base <- base
      self$bins <- bins
      self$drift_level <- drift_level
      self$reset()
    },

    #' @description
    #' Resets the internal state of the detector.
    reset = function() {
      self$p <- NULL
      self$drift_detected <- FALSE
      self$kl_result <- NULL
    },

    #' @description
    #' Sets the initial distribution.
    #' @param initial_p The initial distribution.
    set_initial_distribution = function(initial_p) {
      self$p <- initial_p
    },

    #' @description
    #' Adds a new distribution and calculates the KLD.
    #' @param q The new distribution.
    add_distribution = function(q) {
      if (!is.null(self$p)) {
        kl_div <- self$calculate_kld(self$p, q)
        self$kl_result <- kl_div

        # Detect drift
        if (kl_div > self$drift_level) {
          self$drift_detected <- TRUE
        } else {
          self$drift_detected <- FALSE
        }
      }
    },

    #' @description
    #' Calculates the KLD between two distributions.
    #' @param p The initial distribution.
    #' @param q The new distribution.
    #' @return The KLD value.
    calculate_kld = function(p, q) {
      # Determine common bin boundaries
      bin_breaks <- seq(min(c(p, q)), max(c(p, q)), length.out = self$bins + 1)

      # Create histograms
      p_hist <- hist(p, breaks = bin_breaks, plot = FALSE)
      q_hist <- hist(q, breaks = bin_breaks, plot = FALSE)

      # Normalize histograms (probability mass function)
      p_prob <- p_hist$counts / sum(p_hist$counts)
      q_prob <- q_hist$counts / sum(q_hist$counts)

      # Add epsilon to small values
      p_prob[p_prob < self$epsilon] <- self$epsilon
      q_prob[q_prob < self$epsilon] <- self$epsilon

      # Calculate KL Divergence
      sum(p_prob * log(p_prob / q_prob) / log(self$base))
    },

    #' @description
    #' Returns the current KLD result.
    #' @return The current KLD value.
    get_kl_result = function() {
      return(self$kl_result)
    },

    #' @description
    #' Checks if drift has been detected.
    #' @return TRUE if drift is detected, otherwise FALSE.
    is_drift_detected = function() {
      return(self$drift_detected)
    }
  )
)
