knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4,
  message = FALSE,
  warning = FALSE
)

library(datadriftR)

set.seed(42)
n <- 400
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Abrupt
plot(c(rnorm(200, 0, 1), rnorm(200, 3, 1)), type = "l", col = "#457B9D",
     main = "Abrupt Drift", xlab = "Time", ylab = "Value")
abline(v = 200, col = "#E63946", lty = 2, lwd = 2)

# Gradual
gradual <- sapply(1:n, function(i) rnorm(1, mean = min(3, max(0, (i-150)/80)), sd = 1))
plot(gradual, type = "l", col = "#457B9D",
     main = "Gradual Drift", xlab = "Time", ylab = "Value")
rect(150, -4, 230, 6, col = rgb(0.9, 0.2, 0.2, 0.15), border = NA)

# Incremental
plot(rnorm(n, mean = (1:n)/100, sd = 1), type = "l", col = "#457B9D",
     main = "Incremental Drift", xlab = "Time", ylab = "Value")

# Recurring
plot(sin((1:n)/25) * 2 + rnorm(n, 0, 0.3), type = "l", col = "#457B9D",
     main = "Recurring Drift", xlab = "Time", ylab = "Value")
par(mfrow = c(1, 1))

set.seed(1)
x <- c(rnorm(300, 0, 1), rnorm(200, 3, 1))

detect_drift(x, method = "page_hinkley", delta = 0.05, threshold = 50)

set.seed(123)

n_good <- 500
n_bad <- 500
error_stream <- c(
  rbinom(n_good, 1, prob = 0.05),
  rbinom(n_bad, 1, prob = 0.30)
)
true_drift_error <- n_good + 1

error_methods <- c("ddm", "eddm", "hddm_a", "hddm_w")

first_index <- function(res, type) {
  idx <- res$index[res$type == type]
  if (length(idx) == 0) NA_integer_ else idx[1]
}

error_results <- do.call(rbind, lapply(error_methods, function(m) {
  res <- detect_drift(error_stream, method = m, include_warnings = TRUE)
  warning_idx <- first_index(res, "warning")
  drift_idx <- first_index(res, "drift")
  data.frame(
    Method = gsub("_", "-", toupper(m)),
    Warning = warning_idx,
    Drift = drift_idx,
    DriftDelay = if (!is.na(drift_idx)) drift_idx - true_drift_error else NA,
    stringsAsFactors = FALSE
  )
}))

error_results

window <- 50
error_rate <- sapply(seq_along(error_stream), function(i) {
  mean(error_stream[max(1, i-window+1):i])
})

plot(error_rate, type = "l", col = "gray50", lwd = 2,
     xlab = "Observation", ylab = paste0("Error Rate (", window, "-obs window)"),
     main = "Error-Rate Method Comparison")
abline(v = true_drift_error, col = "black", lty = 2, lwd = 2)

colors <- c("#E63946", "#F4A261", "#2A9D8F", "#9B5DE5")
for (i in seq_len(nrow(error_results))) {
  if (!is.na(error_results$Warning[i])) {
    abline(v = error_results$Warning[i], col = colors[i], lwd = 2, lty = 3)
  }
  if (!is.na(error_results$Drift[i])) {
    abline(v = error_results$Drift[i], col = colors[i], lwd = 2)
  }
}

legend("topleft", c("True drift", error_results$Method),
       col = c("black", colors),
       lty = c(2, rep(1, nrow(error_results))),
       lwd = 2, cex = 0.8)
legend("bottomright", c("Warning", "Drift"),
       lty = c(3, 1), col = "gray30", lwd = 2, bty = "n", cex = 0.8)

ddm <- DDM$new()
drifts <- c()

for (i in seq_along(error_stream)) {
  ddm$add_element(error_stream[i])
  if (ddm$change_detected) {
    drifts <- c(drifts, i)
    ddm$reset()
  }
}

data.frame(Method = "DDM", True = true_drift_error, Detected = drifts)

ddm_res <- detect_drift(error_stream, method = "ddm", include_warnings = FALSE)
ddm_res

set.seed(456)

n_normal <- 300
n_faulty <- 200
sensor_stream <- c(
  rnorm(n_normal, mean = 20, sd = 1),
  rnorm(n_faulty, mean = 28, sd = 2)
)
true_drift_sensor <- n_normal + 1

dist_methods <- c("kswin", "adwin", "page_hinkley")

dist_results <- do.call(rbind, lapply(dist_methods, function(m) {
  res <- detect_drift(sensor_stream, method = m)
  data.frame(
    Method = gsub("_", "-", toupper(m)),
    Detected = if (nrow(res) > 0) res$index[1] else NA,
    Delay = if (nrow(res) > 0) res$index[1] - true_drift_sensor else NA,
    stringsAsFactors = FALSE
  )
}))

dist_results

plot(sensor_stream, type = "l", col = "gray50",
     xlab = "Time", ylab = "Temperature (°C)",
     main = "Distribution Method Comparison")
abline(v = true_drift_sensor, col = "black", lty = 2, lwd = 2)

colors <- c("#E63946", "#2A9D8F", "#9B5DE5")
for (i in seq_along(dist_results$Detected)) {
  if (!is.na(dist_results$Detected[i])) abline(v = dist_results$Detected[i], col = colors[i], lwd = 2)
}

legend("topleft", c("True drift", dist_results$Method),
       col = c("black", colors), lty = c(2, rep(1, 3)), lwd = 2, cex = 0.8)

set.seed(789)

n_ref <- 400
n_shift <- 400
latency_ms <- c(
  rlnorm(n_ref, meanlog = log(100), sdlog = 0.25),
  rlnorm(n_shift, meanlog = log(180), sdlog = 0.30)
)
true_drift_kld <- n_ref + 1

window <- 200
kld <- KLDivergence$new(bins = 30, drift_level = 0.15)
kld$set_initial_distribution(latency_ms[1:window])

kl <- rep(NA_real_, length(latency_ms))
for (t in (window + 1):length(latency_ms)) {
  current <- latency_ms[(t - window + 1):t]
  kld$add_distribution(current)
  kl[t] <- kld$get_kl_result()
}

detected_kld <- which(kl > kld$drift_level)[1]
data.frame(True = true_drift_kld, Detected = detected_kld, Threshold = kld$drift_level)

plot(kl, type = "l", col = "gray50", lwd = 2,
     xlab = "Time", ylab = "KL divergence",
     main = "KL Divergence vs. Reference Window")
abline(v = true_drift_kld, col = "black", lty = 2, lwd = 2)
abline(h = kld$drift_level, col = "#9B5DE5", lty = 2, lwd = 2)
if (!is.na(detected_kld)) abline(v = detected_kld, col = "#E63946", lwd = 2)

legend("topright",
       c("True drift", "Detected", "Threshold"),
       col = c("black", "#E63946", "#9B5DE5"),
       lty = c(2, 1, 2),
       lwd = 2,
       cex = 0.8)

library(dynaTree)
library(ranger)

elec2_env <- new.env(parent = emptyenv())
data("elec2", package = "dynaTree", envir = elec2_env)
elec2_df <- get("elec2", envir = elec2_env)
stopifnot(is.data.frame(elec2_df))

names(elec2_df) <- c("nswprice", "nswdemand", "vicprice", "vicdemand", "class_raw")
elec2_df$class <- factor(elec2_df$class_raw, levels = c(1, 2), labels = c("DOWN", "UP"))
elec2_df$class_raw <- NULL

split_idx <- floor(nrow(elec2_df) / 2)
period1_data <- elec2_df[1:split_idx, ]
period2_data <- elec2_df[(split_idx + 1):nrow(elec2_df), ]

n_train <- min(2000, nrow(period1_data), nrow(period2_data))
period1_train <- period1_data[1:n_train, ]
period2_train <- period2_data[1:n_train, ]

rf1 <- ranger(class ~ nswprice + nswdemand + vicprice + vicdemand,
	             data = period1_train, probability = TRUE, num.trees = 200, seed = 1)
rf2 <- ranger(class ~ nswprice + nswdemand + vicprice + vicdemand,
	             data = period2_train, probability = TRUE, num.trees = 200, seed = 1)

compute_pdp_rf <- function(model, data, var, grid) {
  preds <- sapply(grid, function(val) {
    newdata <- data
    newdata[[var]] <- val
    mean(predict(model, newdata)$predictions[, "UP"])
  })
  list(x = grid, y = preds)
}

demand_grid <- seq(min(elec2_df$nswdemand), max(elec2_df$nswdemand), length.out = 50)
pdp1 <- compute_pdp_rf(rf1, period1_train, "nswdemand", demand_grid)
pdp2 <- compute_pdp_rf(rf2, period2_train, "nswdemand", demand_grid)

plot(pdp1$x, pdp1$y, type = "l", lwd = 2, col = "#2A9D8F",
     xlab = "NSW Demand", ylab = "P(Price UP)",
     main = "PDP Drift: NSW Demand Effect on Price",
     ylim = range(c(pdp1$y, pdp2$y)))
lines(pdp2$x, pdp2$y, lwd = 2, col = "#E63946")
legend("topright", c("Period 1", "Period 2"), col = c("#2A9D8F", "#E63946"), lwd = 2)

# PDI (Profile Disparity Index)
pd_pdi <- ProfileDifference$new(method = "pdi", deriv = "gold")
pd_pdi$set_profiles(pdp1, pdp2)
res_pdi <- pd_pdi$calculate_difference()

# L2 norm
pd_l2 <- ProfileDifference$new(method = "L2")
pd_l2$set_profiles(pdp1, pdp2)
res_l2 <- pd_l2$calculate_difference()

# L2 derivative
pd_l2d <- ProfileDifference$new(method = "L2_derivative")
pd_l2d$set_profiles(pdp1, pdp2)
res_l2d <- pd_l2d$calculate_difference()

data.frame(
  Method = c("PDI", "L2", "L2_derivative"),
  Distance = round(c(res_pdi$distance, res_l2$distance, res_l2d$distance), 4)
)
