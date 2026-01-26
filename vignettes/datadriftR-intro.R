## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4,
  message = FALSE,
  warning = FALSE
)

## ----covariate-shift-viz, fig.cap="Covariate Shift: Input distribution changes", fig.height=4----
set.seed(42)
par(mfrow = c(1, 2))

# Training data
x_train <- rnorm(500, mean = 2, sd = 1)
hist(x_train, breaks = 30, col = "#2A9D8F", border = "white",
     main = "Training Distribution", xlab = "Feature X", xlim = c(-2, 10))

# Test data (shifted)
x_test <- rnorm(500, mean = 5, sd = 1.5)
hist(x_test, breaks = 30, col = "#E63946", border = "white",
     main = "Production Distribution", xlab = "Feature X", xlim = c(-2, 10))

par(mfrow = c(1, 1))

## ----concept-drift-viz, fig.cap="Concept Drift: Decision boundary shifts", fig.height=4----
set.seed(123)
par(mfrow = c(1, 2))

# Before concept drift
x <- seq(0, 10, length.out = 100)
y_before <- 0.5 * x + rnorm(100, sd = 0.5)
plot(x, y_before, pch = 19, col = "#2A9D8F", cex = 0.7,
     main = "Before Concept Drift", xlab = "X", ylab = "Y")
abline(lm(y_before ~ x), col = "#1D3557", lwd = 2)

# After concept drift (relationship changed)
y_after <- -0.3 * x + 8 + rnorm(100, sd = 0.5)
plot(x, y_after, pch = 19, col = "#E63946", cex = 0.7,
     main = "After Concept Drift", xlab = "X", ylab = "Y")
abline(lm(y_after ~ x), col = "#1D3557", lwd = 2)

par(mfrow = c(1, 1))

## ----drift-patterns, fig.cap="Different drift patterns over time", fig.height=5----
set.seed(456)
n <- 500
t <- 1:n

par(mfrow = c(2, 2))

# 1. Abrupt drift
abrupt <- c(rnorm(250, 0, 1), rnorm(250, 3, 1))
plot(t, abrupt, type = "l", col = "#457B9D", lwd = 1.5,
     main = "Abrupt Drift", xlab = "Time", ylab = "Value")
abline(v = 250, col = "#E63946", lty = 2, lwd = 2)

# 2. Gradual drift
gradual <- sapply(t, function(i) {
  p <- min(1, max(0, (i - 200) / 100))
  rnorm(1, mean = p * 3, sd = 1)
})
plot(t, gradual, type = "l", col = "#457B9D", lwd = 1.5,
     main = "Gradual Drift", xlab = "Time", ylab = "Value")
rect(200, -5, 300, 8, col = rgb(0.9, 0.3, 0.3, 0.2), border = NA)

# 3. Incremental drift
incremental <- rnorm(n, mean = t/100, sd = 1)
plot(t, incremental, type = "l", col = "#457B9D", lwd = 1.5,
     main = "Incremental Drift", xlab = "Time", ylab = "Value")

# 4. Recurring drift (seasonal)
recurring <- sin(t / 30) * 2 + rnorm(n, 0, 0.5)
plot(t, recurring, type = "l", col = "#457B9D", lwd = 1.5,
     main = "Recurring/Seasonal Drift", xlab = "Time", ylab = "Value")

par(mfrow = c(1, 1))

## ----simple-example-----------------------------------------------------------
library(datadriftR)
set.seed(123)

n_stable <- 500
n_drift <- 500

stream <- c(
  sample(c(0, 1), n_stable, replace = TRUE, prob = c(0.7, 0.3)),
  sample(c(0, 1), n_drift, replace = TRUE, prob = c(0.3, 0.7))
)

drift_point <- n_stable + 1

results <- detect_drift(stream, method = "ddm", include_warnings = FALSE)
head(results, 3)

## ----all-methods--------------------------------------------------------------
methods <- c("ddm", "eddm", "hddm_a", "hddm_w", "kswin", "adwin", "page_hinkley")

comparison <- do.call(rbind, lapply(methods, function(m) {
  res <- detect_drift(stream, method = m, include_warnings = FALSE)
  data.frame(
    Method = toupper(m),
    Detections = nrow(res),
    First = if (nrow(res) > 0) min(res$index) else NA
  )
}))

comparison

## ----comparison-plot, fig.cap="Detection points by method", fig.height=5------
roll_mean <- sapply(seq_along(stream), function(i) mean(stream[max(1,i-50):i]))

plot(roll_mean, type = "l", col = "gray40", lwd = 2,
     xlab = "Observation", ylab = "Rolling Mean",
     main = "Drift Detection Comparison")
abline(v = drift_point, col = "black", lty = 2, lwd = 2)

colors <- c("#E63946", "#F4A261", "#2A9D8F", "#9B5DE5", "#00BBF9", "#8B4513", "#FF69B4")
for (i in seq_len(nrow(comparison))) {
  if (!is.na(comparison$First[i])) {
    abline(v = comparison$First[i], col = colors[i], lwd = 2)
  }
}

legend("bottomleft",
       legend = c(paste0("Actual (", drift_point, ")"),
                  paste0(comparison$Method, " (", comparison$First, ")")),
       col = c("black", colors),
       lty = c(2, rep(1, 6)), lwd = 2, cex = 0.8, bg = "white")

## ----online-ddm---------------------------------------------------------------
ddm <- DDM$new()

for (i in seq_along(stream)) {
  ddm$add_element(stream[i])
  if (ddm$change_detected) {
    cat("DDM detected drift at index:", i, "\n")
    ddm$reset()
  }
}

## ----online-kswin-------------------------------------------------------------
kswin <- KSWIN$new(alpha = 0.005, window_size = 100)

for (i in seq_along(stream)) {
  kswin$add_element(stream[i])
  if (kswin$detected_change()) {
    cat("KSWIN detected drift at index:", i, "\n")
  }
}

## ----online-pagehinkley-------------------------------------------------------
ph <- PageHinkley$new(threshold = 50)

for (i in seq_along(stream)) {
  ph$add_element(stream[i])
  if (ph$detected_change()) {
    cat("Page-Hinkley detected drift at index:", i, "\n")
    ph$reset()
  }
}

## ----online-adwin-------------------------------------------------------------
adwin <- ADWIN$new(delta = 0.002)

for (i in seq_along(stream)) {
  adwin$add_element(stream[i])
  if (adwin$detected_change()) {
    cat("ADWIN detected drift at index:", i, "\n")
  }
}

## ----continuous, fig.cap="Variance change detection in continuous data"-------
set.seed(111)

n_before <- 200
n_after <- 200

before <- rnorm(n_before, mean = 0, sd = 1)
after <- rnorm(n_after, mean = 0, sd = 3)
cont_stream <- c(before, after)
cont_drift <- n_before + 1

results_cont <- detect_drift(cont_stream, method = "kswin",
                              alpha = 0.001, window_size = 50, stat_size = 25)

plot(cont_stream, type = "l", col = "gray50",
     xlab = "Observation", ylab = "Value", main = "Variance Change Detection")
abline(v = cont_drift, col = "black", lty = 2, lwd = 2)
if (nrow(results_cont) > 0) {
  abline(v = results_cont$index[1], col = "red", lwd = 2)
  legend("topleft",
         c(paste0("Actual (", cont_drift, ")"),
           paste0("Detected (", results_cont$index[1], ")")),
         col = c("black", "red"), lty = c(2, 1), lwd = 2)
}

## ----scenario-classification--------------------------------------------------
set.seed(789)

# Simulate prediction errors: 0 = correct, 1 = error
# Model works well initially (5% error), then degrades (25% error)
errors <- c(
  rbinom(400, 1, prob = 0.05),  # Good performance
  rbinom(600, 1, prob = 0.25)   # Degraded performance
)

# Detect when model starts failing
drift_results <- detect_drift(errors, method = "ddm", include_warnings = TRUE)
print(drift_results)

## ----scenario-sensor, fig.cap="Sensor drift detection"------------------------
set.seed(321)

# Normal operation: 20°C with some noise
normal_readings <- rnorm(300, mean = 20, sd = 1)

# Equipment malfunction: readings drift upward
faulty_readings <- rnorm(200, mean = 28, sd = 2)

sensor_stream <- c(normal_readings, faulty_readings)

# Use Page-Hinkley for continuous data
ph <- PageHinkley$new(threshold = 20, delta = 0.01)
drift_points <- c()

for (i in seq_along(sensor_stream)) {
  ph$add_element(sensor_stream[i])
  if (ph$detected_change()) {
    drift_points <- c(drift_points, i)
    ph$reset()
  }
}

# Visualize
plot(sensor_stream, type = "l", col = "#457B9D",
     xlab = "Time", ylab = "Temperature (°C)",
     main = "Sensor Drift Detection")
abline(v = 300, col = "black", lty = 2, lwd = 2)
if (length(drift_points) > 0) {
  abline(v = drift_points[1], col = "#E63946", lwd = 2)
  legend("topleft", c("Actual malfunction (300)", paste0("Detected (", drift_points[1], ")")),
         col = c("black", "#E63946"), lty = c(2, 1), lwd = 2)
}

## ----scenario-gradual, fig.cap="Detecting gradual drift with ADWIN"-----------
set.seed(555)

# Gradual shift in user engagement
n <- 1000
engagement <- numeric(n)
for (i in 1:n) {
  # Engagement slowly decreases over time
  base_rate <- 0.6 - (i / n) * 0.4  # From 60% to 20%
  engagement[i] <- rbinom(1, 1, prob = base_rate)
}

# ADWIN adapts its window to detect gradual changes
adwin <- ADWIN$new(delta = 0.001)
drift_points <- c()

for (i in seq_along(engagement)) {
  adwin$add_element(engagement[i])
  if (adwin$detected_change()) {
    drift_points <- c(drift_points, i)
  }
}

cat("ADWIN detected gradual drift at indices:", drift_points, "\n")

# Visualize with rolling average
roll_avg <- sapply(seq_along(engagement), function(i) mean(engagement[max(1,i-50):i]))
plot(roll_avg, type = "l", col = "#457B9D", lwd = 2,
     xlab = "Time", ylab = "Rolling Engagement Rate",
     main = "Gradual Drift in User Engagement")
if (length(drift_points) > 0) {
  abline(v = drift_points, col = "#E63946", lty = 2)
}

## ----ddm-deep-----------------------------------------------------------------
ddm <- DDM$new(
  min_num_instances = 30,  # Minimum observations before checking
  warning_level = 2.0,     # Standard deviations for warning
  out_control_level = 3.0  # Standard deviations for drift
)

# Process stream
for (i in seq_along(stream)) {
  ddm$add_element(stream[i])
  
  if (ddm$warning_detected && !ddm$change_detected) {
    cat("DDM WARNING at index:", i, "\n")
  }
  if (ddm$change_detected) {
    cat("DDM DRIFT at index:", i, "\n")
    ddm$reset()
  }
}

## ----adwin-deep---------------------------------------------------------------
adwin <- ADWIN$new(
  delta = 0.002,           # Confidence parameter (smaller = more sensitive)
  clock = 32,              # Check frequency
  max_buckets = 5,         # Memory parameter
  min_window_length = 5,   # Minimum window
  grace_period = 10        # Initial warm-up
)

drift_indices <- c()
for (i in seq_along(stream)) {
  adwin$add_element(stream[i])
  if (adwin$detected_change()) {
    drift_indices <- c(drift_indices, i)
  }
}
cat("ADWIN detected drift at:", drift_indices, "\n")

## ----kswin-deep---------------------------------------------------------------
kswin <- KSWIN$new(
  alpha = 0.005,      # Significance level
  window_size = 100,  # Reference window size
  stat_size = 30      # Sliding window size
)

for (i in seq_along(stream)) {
  kswin$add_element(stream[i])
  if (kswin$detected_change()) {
    cat("KSWIN detected drift at index:", i, "\n")
  }
}

## ----pagehinkley-deep---------------------------------------------------------
ph <- PageHinkley$new(
  delta = 0.005,    # Minimum change magnitude to detect
  threshold = 50,   # Detection threshold (lambda)
  alpha = 0.9999    # Forgetting factor for mean
)

for (i in seq_along(stream)) {
  ph$add_element(stream[i])
  if (ph$detected_change()) {
    cat("Page-Hinkley detected drift at index:", i, "\n")
    ph$reset()
  }
}

## ----comparison-drift-types, fig.cap="Method performance on different drift types", fig.height=6----
set.seed(999)

# Create two streams
n <- 800

# Abrupt drift
abrupt_stream <- c(
  rbinom(400, 1, 0.3),
  rbinom(400, 1, 0.7)
)

# Gradual drift  
gradual_stream <- sapply(1:n, function(i) {
  p <- 0.3 + (i > 300) * min(0.4, (i - 300) / 250)
  rbinom(1, 1, p)
})

methods <- c("ddm", "eddm", "adwin", "page_hinkley")

results_abrupt <- lapply(methods, function(m) {
  res <- detect_drift(abrupt_stream, method = m, include_warnings = FALSE)
  if (nrow(res) > 0) min(res$index) else NA
})

results_gradual <- lapply(methods, function(m) {
  res <- detect_drift(gradual_stream, method = m, include_warnings = FALSE)
  if (nrow(res) > 0) min(res$index) else NA
})

comparison_df <- data.frame(
  Method = toupper(methods),
  Abrupt_First = unlist(results_abrupt),
  Gradual_First = unlist(results_gradual)
)

print(comparison_df)

# Visualization
par(mfrow = c(2, 1))

roll_abrupt <- sapply(seq_along(abrupt_stream), function(i) mean(abrupt_stream[max(1,i-30):i]))
plot(roll_abrupt, type = "l", col = "#457B9D", lwd = 2,
     main = "Abrupt Drift Detection", xlab = "Time", ylab = "Rolling Mean")
abline(v = 400, col = "black", lty = 2, lwd = 2)
colors <- c("#E63946", "#F4A261", "#2A9D8F", "#9B5DE5")
for (i in seq_along(methods)) {
  if (!is.na(comparison_df$Abrupt_First[i])) {
    abline(v = comparison_df$Abrupt_First[i], col = colors[i], lwd = 2)
  }
}
legend("bottomleft", c("Actual", comparison_df$Method), 
       col = c("black", colors), lty = c(2, rep(1, 4)), lwd = 2, cex = 0.7)

roll_gradual <- sapply(seq_along(gradual_stream), function(i) mean(gradual_stream[max(1,i-30):i]))
plot(roll_gradual, type = "l", col = "#457B9D", lwd = 2,
     main = "Gradual Drift Detection", xlab = "Time", ylab = "Rolling Mean")
abline(v = 300, col = "black", lty = 2, lwd = 2)
for (i in seq_along(methods)) {
  if (!is.na(comparison_df$Gradual_First[i])) {
    abline(v = comparison_df$Gradual_First[i], col = colors[i], lwd = 2)
  }
}
legend("bottomleft", c("Drift Start", comparison_df$Method), 
       col = c("black", colors), lty = c(2, rep(1, 4)), lwd = 2, cex = 0.7)

par(mfrow = c(1, 1))

## ----best-practice-multiple---------------------------------------------------
run_all_detectors <- function(stream) {
  methods <- c("ddm", "eddm", "hddm_a", "adwin", "kswin", "page_hinkley")
  
  results <- lapply(methods, function(m) {
    res <- detect_drift(stream, method = m, include_warnings = FALSE)
    if (nrow(res) > 0) res$index else integer(0)
  })
  names(results) <- methods
  
  # Consensus: drift confirmed if majority agree
  all_detections <- unlist(results)
  if (length(all_detections) > 0) {
    hist(all_detections, breaks = 20, main = "Detection Distribution",
         xlab = "Index", col = "#2A9D8F")
  }
  
  return(results)
}

detections <- run_all_detectors(stream)

## ----best-practice-tuning-----------------------------------------------------
# More sensitive detection (more false positives)
sensitive <- detect_drift(stream, method = "ddm", 
                          out_control_level = 2.5)

# Less sensitive (fewer false positives)
conservative <- detect_drift(stream, method = "ddm", 
                             out_control_level = 3.5)

cat("Sensitive detections:", nrow(sensitive), "\n")
cat("Conservative detections:", nrow(conservative), "\n")

## ----best-practice-multivariate-----------------------------------------------
# Simulate multivariate data
set.seed(123)
feature1 <- c(rnorm(300, 0, 1), rnorm(200, 2, 1))
feature2 <- c(rnorm(300, 5, 2), rnorm(200, 5, 2))  # No drift
feature3 <- c(rnorm(400, 10, 1), rnorm(100, 15, 1))

features <- list(feature1 = feature1, feature2 = feature2, feature3 = feature3)

for (name in names(features)) {
  result <- detect_drift(features[[name]], method = "kswin", 
                         alpha = 0.01, window_size = 50)
  cat(name, ": ", nrow(result), "drift(s) detected\n")
}

