#' Detect Data Drift in Streaming Data
#'
#' @description
#' A user-friendly wrapper function that detects data drift in streaming data
#' without requiring explicit for-loops. This function processes the entire
#' data stream and returns all detected drift points (and optionally warning points)
#' as a dataframe.
#'
#' @details
#' This function provides a simplified interface to all streaming drift detectors
#' in the datadriftR package. Instead of manually iterating through the stream
#' and checking for drift at each point, users can simply pass the entire stream
#' to this function and receive a dataframe with all drift detection results.
#'
#' The function supports the following drift detection methods:
#' \itemize{
#'   \item \code{ddm}: Drift Detection Method
#'   \item \code{eddm}: Early Drift Detection Method
#'   \item \code{page_hinkley}: Page-Hinkley Test
#'   \item \code{hddm_a}: HDDM with Adaptive Windows
#'   \item \code{hddm_w}: HDDM with Weighted Windows
#'   \item \code{kswin}: Kolmogorov-Smirnov Windowing
#'   \item \code{adwin}: ADaptive WINdowing
#' }
#'
#' @param stream Numeric vector representing the data stream to monitor.
#' @param method Character string specifying the drift detection method to use.
#'   Options are: "ddm", "eddm", "page_hinkley", "hddm_a", "hddm_w", "kswin".
#'   Default is "ddm".
#' @param include_warnings Logical indicating whether to include warning detections
#'   in the results (applicable for DDM, EDDM, HDDM_A, and HDDM_W). Default is TRUE.
#' @param ... Additional parameters to pass to the specific detector constructor.
#'   See individual detector documentation for available parameters.
#'
#' @return A data.frame with the following columns:
#' \describe{
#'   \item{index}{Integer index in the stream where detection occurred}
#'   \item{value}{Numeric value at that index}
#'   \item{type}{Character indicating "drift" or "warning"}
#' }
#' If no drift or warnings are detected, returns an empty data.frame with the same structure.
#'
#' @examples
#' library(datadriftR)
#' set.seed(123)
#'
#' # Generate synthetic stream with drift at index 501
#' pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.7, 0.3))
#' post <- sample(c(0,1), 500, replace = TRUE, prob = c(0.3, 0.7))
#' stream <- c(pre, post)
#'
#' # Detect drift using DDM
#' results <- detect_drift(stream, method = "ddm")
#' print(results)
#'
#' # Detect drift using Page-Hinkley with custom parameters
#' results <- detect_drift(stream, method = "page_hinkley",
#'                         delta = 0.005, threshold = 50)
#' print(results)
#'
#' # Detect drift using KSWIN
#' results <- detect_drift(stream, method = "kswin")
#' print(results)
#'
#' # Get only drift detections, exclude warnings
#' results <- detect_drift(stream, method = "ddm", include_warnings = FALSE)
#' print(results)
#'
#' @export
detect_drift <- function(stream,
                         method = c("ddm", "eddm", "page_hinkley",
                                   "hddm_a", "hddm_w", "kswin", "adwin"),
                         include_warnings = TRUE,
                         ...) {

  # Validate inputs
  if (!is.numeric(stream) || length(stream) == 0) {
    stop("stream must be a non-empty numeric vector")
  }

  method <- match.arg(method)

  # Initialize results storage
  results <- data.frame(
    index = integer(),
    value = numeric(),
    type = character(),
    stringsAsFactors = FALSE
  )

  # Create detector based on method
  detector <- switch(
    method,
    ddm = DDM$new(...),
    eddm = EDDM$new(...),
    page_hinkley = PageHinkley$new(...),
    hddm_a = HDDM_A$new(...),
    hddm_w = HDDM_W$new(...),
    kswin = KSWIN$new(...),
    adwin = ADWIN$new(...),
    stop("Unknown method: ", method)
  )

  # Process stream
  for (i in seq_along(stream)) {
    detector$add_element(stream[i])

    # Check for drift detection
    drift_detected <- if (method %in% c("ddm", "eddm", "hddm_a", "hddm_w")) {
      detector$change_detected
    } else if (method %in% c("page_hinkley", "kswin", "adwin")) {
      detector$detected_change()
    } else {
      FALSE
    }

    if (drift_detected) {
      results <- rbind(results, data.frame(
        index = i,
        value = stream[i],
        type = "drift",
        stringsAsFactors = FALSE
      ))
    }

    # Check for warning detection (only for methods that support it)
    if (include_warnings && method %in% c("ddm", "eddm", "hddm_a", "hddm_w")) {
      if (detector$warning_detected) {
        results <- rbind(results, data.frame(
          index = i,
          value = stream[i],
          type = "warning",
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Return results
  return(results)
}
