pkgname <- "datadriftR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "datadriftR-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('datadriftR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ADWIN")
### * ADWIN

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ADWIN
### Title: ADWIN (ADaptive WINdowing) Drift Detector
### Aliases: ADWIN

### ** Examples

set.seed(12345)
adwin <- ADWIN$new()

# Stream: 1000 values from {0,1}, then 1000 values from {4,5,6,7}
stream <- c(sample(0:1, 1000, replace = TRUE),
            sample(4:7, 1000, replace = TRUE))

for (i in seq_along(stream)) {
  adwin$add_element(stream[i])
  if (adwin$detected_change()) {
    message("Change detected at index ", i, ", input value: ", stream[i])
  }
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ADWIN", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("DDM")
### * DDM

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DDM
### Title: DDM (Drift Detection Method)
### Aliases: DDM

### ** Examples

set.seed(123)  # Setting a seed for reproducibility
data_part1 <- sample(c(0, 1), size = 100, replace = TRUE, prob = c(0.7, 0.3))

# Introduce a change in data distribution
data_part2 <- sample(c(0, 1), size = 100, replace = TRUE, prob = c(0.3, 0.7))

# Combine the two parts
data_stream <- c(data_part1, data_part2)
ddm <- DDM$new()
# Iterate through the data stream
for (i in seq_along(data_stream)) {
  ddm$add_element(data_stream[i])
  if (ddm$change_detected) {
    message(paste("Drift detected!", i))
  } else if (ddm$warning_detected) {
    # message(paste("Warning detected at position:", i))
  }
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DDM", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("EDDM")
### * EDDM

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: EDDM
### Title: EDDM (Early Drift Detection Method)
### Aliases: EDDM

### ** Examples

set.seed(123)  # Setting a seed for reproducibility
data_part1 <- sample(c(0, 1), size = 100, replace = TRUE, prob = c(0.7, 0.3))

# Introduce a change in data distribution
data_part2 <- sample(c(0, 1), size = 100, replace = TRUE, prob = c(0.3, 0.7))

# Combine the two parts
data_stream <- c(data_part1, data_part2)
eddm <- EDDM$new()
for (i in 1:length(data_stream)) {
  eddm$add_element(data_stream[i])
  if (eddm$change_detected) {
    message(paste("Drift detected!",i))
  } else if (eddm$warning_detected) {
    message(paste("Warning detected!",i))
  }
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("EDDM", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("HDDM_A")
### * HDDM_A

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: HDDM_A
### Title: HDDM_A: Drift Detection Method based on Adaptive Windows
### Aliases: HDDM_A

### ** Examples

set.seed(123)  # Setting a seed for reproducibility
data_part1 <- sample(c(0, 1), size = 100, replace = TRUE, prob = c(0.7, 0.3))

# Introduce a change in data distribution
data_part2 <- sample(c(0, 1), size = 100, replace = TRUE, prob = c(0.3, 0.7))

# Combine the two parts
data_stream <- c(data_part1, data_part2)

# Initialize the hddm_a object
hddm_a_instance <- HDDM_A$new()

# Iterate through the data stream
for(i in seq_along(data_stream)) {
  hddm_a_instance$add_element(data_stream[i])
  if(hddm_a_instance$warning_detected) {
    message(paste("Warning detected at index:", i))
  }
  if(hddm_a_instance$change_detected) {
    message(paste("Concept drift detected at index:", i))
  }
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("HDDM_A", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("HDDM_W")
### * HDDM_W

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: HDDM_W
### Title: KSWIN (Kolmogorov-Smirnov WINdowing) for Change Detection
### Aliases: HDDM_W

### ** Examples

set.seed(123)  # Setting a seed for reproducibility
data_part1 <- sample(c(0, 1), size = 100, replace = TRUE, prob = c(0.7, 0.3))

# Introduce a change in data distribution
data_part2 <- sample(c(0, 1), size = 100, replace = TRUE, prob = c(0.3, 0.7))

# Combine the two parts
data_stream <- c(data_part1, data_part2)

# Initialize the HDDM_W object
hddm_w_instance <- HDDM_W$new()

# Iterate through the data stream
for(i in seq_along(data_stream)) {
  hddm_w_instance$add_element(data_stream[i])
  if(hddm_w_instance$warning_detected) {
    message(paste("Warning detected at index:", i))
  }
  if(hddm_w_instance$change_detected) {
    message(paste("Concept drift detected at index:", i))
  }
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("HDDM_W", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("KLDivergence")
### * KLDivergence

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: KLDivergence
### Title: Kullback-Leibler Divergence (KLD) for Change Detection
### Aliases: KLDivergence

### ** Examples

set.seed(123)  # Setting a seed for reproducibility
initial_data <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
kld <- KLDivergence$new(bins = 10, drift_level = 0.2)
kld$set_initial_distribution(initial_data)

new_data <- c(0.2, 0.2, 0.3, 0.4, 0.4, 0.5, 0.6, 0.7, 0.7, 0.8)
kld$add_distribution(new_data)

kl_result <- kld$get_kl_result()
message(paste("KL Divergence:", kl_result))

if (kld$is_drift_detected()) {
  message("Drift detected.")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("KLDivergence", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("KSWIN")
### * KSWIN

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: KSWIN
### Title: KSWIN (Kolmogorov-Smirnov WINdowing) for Change Detection
### Aliases: KSWIN

### ** Examples

set.seed(123)
x <- c(rnorm(100, mean = 0, sd = 1), rnorm(100, mean = 3, sd = 1))

# High-level interface (returns a data.frame of detections)
detect_drift(x, method = "kswin", alpha = 0.001, window_size = 50, stat_size = 20)

# Online usage (update one observation at a time)
kswin <- KSWIN$new(alpha = 0.001, window_size = 50, stat_size = 20)
drift_idx <- integer()
for (i in seq_along(x)) {
  kswin$add_element(x[i])
  if (kswin$detected_change()) {
    drift_idx <- c(drift_idx, i)
  }
}
drift_idx



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("KSWIN", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("PageHinkley")
### * PageHinkley

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: PageHinkley
### Title: Page-Hinkley Test for Change Detection
### Aliases: PageHinkley

### ** Examples

set.seed(123)  # Setting a seed for reproducibility
data_part1 <- sample(c(0, 1), size = 100, replace = TRUE, prob = c(0.7, 0.3))

# Introduce a change in data distribution
data_part2 <- sample(c(0, 5), size = 100, replace = TRUE, prob = c(0.3, 0.7))

# Combine the two parts
data_stream <- c(data_part1, data_part2)
ph <- PageHinkley$new()
for (i in seq_along(data_stream)) {
  ph$add_element(data_stream[i])
  if (ph$detected_change()) {
    cat(sprintf("Change has been detected in data: %s - at index: %d\n", data_stream[i], i))
  }
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("PageHinkley", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ProfileDifference")
### * ProfileDifference

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ProfileDifference
### Title: Profile Difference Calculation for Change Detection
### Aliases: ProfileDifference

### ** Examples

set.seed(123)
profile1 <- list(x = 1:100, y = sin(1:100))
profile2 <- list(x = 1:100, y = sin(1:100) + rnorm(100, 0, 0.1))
pd <- ProfileDifference$new(method = "pdi", deriv = "gold")
pd$set_profiles(profile1, profile2)
result <- pd$calculate_difference()
message("Method:", result$method)
message("Distance:", round(result$distance, 4))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ProfileDifference", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("detect_drift")
### * detect_drift

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: detect_drift
### Title: Detect Data Drift in Streaming Data
### Aliases: detect_drift

### ** Examples

library(datadriftR)
set.seed(123)

# Generate synthetic stream with drift at index 501
pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.7, 0.3))
post <- sample(c(0,1), 500, replace = TRUE, prob = c(0.3, 0.7))
stream <- c(pre, post)

# Detect drift using DDM
results <- detect_drift(stream, method = "ddm")
print(results)

# Detect drift using Page-Hinkley with custom parameters
results <- detect_drift(stream, method = "page_hinkley",
                        delta = 0.005, threshold = 50)
print(results)

# Detect drift using KSWIN
results <- detect_drift(stream, method = "kswin")
print(results)

# Get only drift detections, exclude warnings
results <- detect_drift(stream, method = "ddm", include_warnings = FALSE)
print(results)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("detect_drift", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
