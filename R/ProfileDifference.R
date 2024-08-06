#' Profile Difference Calculation for Change Detection
#'
#' @description
#' Implements the calculation of profile differences using various methods such as PDI, L2, and L2 derivative.
#' The class provides methods for setting profiles and calculating the differences.
#'
#' @details
#' The class supports multiple methods for calculating profile differences, including the Profile Disparity Index (PDI)
#' using gold or simple derivative methods, and L2 norm and L2 derivative calculations. It allows for customization
#' of various parameters such as embedding dimensions, derivative orders, and thresholds.
#'
#' @references
#' Kobylińska, K., Krzyziński, M., Machowicz, R., Adamek, M., & Biecek, P. (2023). Exploration of the Rashomon Set Assists Trustworthy Explanations for Medical Data. arXiv e-prints, arXiv-2308.
#'
#' @examples
#' set.seed(123)  # Setting a seed for reproducibility
#' profile1 <- list(x = 1:100, y = sin(1:100))
#' profile2 <- list(x = 1:100, y = sin(1:100) + rnorm(100, 0, 0.1))
#' pd <- ProfileDifference$new(method = "pdi", deriv = "gold")
#' pd$set_profiles(profile1, profile2)
#' result <- pd$calculate_difference()
#' message(result)
#' @import R6
#' @import fda.usc
#' @import doremi
#' @import stats
#' @export
ProfileDifference <- R6Class(
  "ProfileDifference",
  public = list(
    #' @field method The method used for profile difference calculation.
    method = "pdi",
    #' @field deriv The method used for derivative calculation.
    deriv = "gold",
    #' @field gold_spline Boolean indicating if cubic spline should be used in gold method.
    gold_spline = TRUE,
    #' @field gold_embedding Embedding dimension for gold method.
    gold_embedding = 4,
    #' @field nderiv Order of the derivative for simple method.
    nderiv = 4,
    #' @field gold_spline_threshold Threshold for cubic spline in gold method.
    gold_spline_threshold = 0.01,
    #' @field epsilon Small value to avoid numerical issues.
    epsilon = NULL,
    #' @field profile1 The first profile.
    profile1 = NULL,
    #' @field profile2 The second profile.
    profile2 = NULL,

    #' @description
    #' Initializes the ProfileDifference class.
    #' @param method The method used for profile difference calculation.
    #' @param deriv The method used for derivative calculation.
    #' @param gold_spline Boolean indicating if cubic spline should be used in gold method.
    #' @param gold_embedding Embedding dimension for gold method.
    #' @param nderiv Order of the derivative for simple method.
    #' @param gold_spline_threshold Threshold for cubic spline in gold method.
    #' @param epsilon Small value to avoid numerical issues.
    initialize = function(method = "pdi",
                          deriv = "gold",
                          gold_spline = TRUE,
                          gold_embedding = 4,
                          nderiv = 4,
                          gold_spline_threshold = 0.01,
                          epsilon = NULL) {
      self$method <- method
      self$deriv <- deriv
      self$gold_spline <- gold_spline
      self$gold_embedding <- gold_embedding
      self$nderiv <- nderiv
      self$gold_spline_threshold <- gold_spline_threshold
      self$epsilon <- epsilon
      self$reset()
    },

    #' @description
    #' Resets the internal state of the detector.
    reset = function() {
      self$profile1 <- NULL
      self$profile2 <- NULL
    },

    #' @description
    #' Sets the profiles for comparison.
    #' @param profile1 The first profile.
    #' @param profile2 The second profile.
    set_profiles = function(profile1, profile2) {
      self$profile1 <- profile1
      self$profile2 <- profile2
    },

    #' @description
    #' Calculates the difference between the profiles.
    #' @return A list containing the method details and the calculated distance.
    calculate_difference = function() {
      if (is.null(self$profile1) || is.null(self$profile2)) {
        stop("Both profiles must be set before calculating the difference.")
      }

      x <- self$profile2$x
      calculated_y <- stats::approx(self$profile1$x, self$profile1$y, xout = x, rule = 2)$y

      if (self$method == "pdi") {
        if (self$deriv == "gold") {
          profile1der <- doremi::calculate.gold(
            time = x,
            signal = calculated_y,
            embedding = self$gold_embedding,
            n = 1
          )
          profile2der <- doremi::calculate.gold(
            time = x,
            signal = self$profile2$y,
            embedding = self$gold_embedding,
            n = 1
          )

          if (self$gold_spline) {
            spline1 <- stats::spline(profile1der$dsignal[, 2] ~ profile1der$dtime)
            spline2 <- stats::spline(profile2der$dsignal[, 2] ~ profile2der$dtime)

            if (!is.null(self$gold_spline_threshold)) {
              threshold1 <- self$gold_spline_threshold * max(abs(spline1$y))
              spline1$y[abs(spline1$y) < threshold1] <- 0

              threshold2 <- self$gold_spline_threshold * max(abs(spline2$y))
              spline2$y[abs(spline2$y) < threshold2] <- 0
            }

            if (!is.null(self$epsilon)) {
              spline1$y[abs(spline1$y) < self$epsilon] <- 0
              spline2$y[abs(spline2$y) < self$epsilon] <- 0
            }
            m <- mean(sign(spline1$y) != sign(spline2$y))
            return(list(
              method = self$method,
              method_detail = "gold_spline",
              distance = m,
              der1 = spline1$y,
              der2 = spline2$y,
              x = x
            ))

          } else {
            m <- mean(sign((profile1der$dsignal[, 2])) != sign((profile2der$dsignal[, 2])), na.rm = TRUE)
            return(list(
              method = self$method,
              method_detail = "gold",
              distance = m,
              der1 = profile1der$dsignal[, 2],
              der2 = profile2der$dsignal[, 2],
              x = x
            ))
          }

        } else if (self$deriv == "simple") {
          derivative1 <- fda.usc::fdata.deriv(fda.usc::fdata(calculated_y, argvals = x), nderiv = self$nderiv)$data[1,]
          derivative2 <- fda.usc::fdata.deriv(fda.usc::fdata(self$profile2$y, argvals = x), nderiv = self$nderiv)$data[1,]

          if (!is.null(self$epsilon)) {
            derivative1$y[abs(derivative1$y) < self$epsilon] <- 0
            derivative2$y[abs(derivative2$y) < self$epsilon] <- 0
          }
          m <- mean(sign(derivative1) != sign(derivative2))
          return(list(
            method = self$method,
            method_detail = "simple",
            distance = m,
            der1 = derivative1,
            der2 = derivative2,
            x = x
          ))

        } else {
          warning("Please choose one of them: 'simple' or 'gold'")
        }
      } else if (self$method == "L2") {
        m <- as.numeric(fda.usc::metric.lp(
          fda.usc::fdata(calculated_y, argvals = x),
          fda.usc::fdata(self$profile2$y, argvals = x)
        ))
        return(list(method = self$method, method_detail = "L2", distance = m, x = x))

      } else if (self$method == "L2_derivative") {
        m <- as.numeric(fda.usc::semimetric.deriv(
          fda.usc::fdata(calculated_y, argvals = x),
          fda.usc::fdata(self$profile2$y, argvals = x)
        ))
        return(list(method = self$method, method_detail = "L2_derivative", distance = m, x = x))
      }
    }
  )
)
