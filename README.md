
<img align="right" width="220" height="240" src="https://drive.google.com/uc?export=download&id=1w6vmd9972c1TMAsKKofzmJT47_D-TSQO">
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datadriftR

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/datadriftR)](https://cran.r-project.org/package=datadriftR)
[![](https://cranlogs.r-pkg.org/badges/datadriftR)](https://cran.rstudio.com/web/packages/datadriftR/index.html)
[![](http://cranlogs.r-pkg.org/badges/last-week/datadriftR?color=green)](https://cran.r-project.org/package=datadriftR)
<!-- badges: end -->

A system designed for detecting data drift in streaming datasets,
offering a suite of statistical methods to track variations in data
behavior.

## Installation

``` r
install.packages("datadriftR")
```

``` r
remotes::install_github("ugurdar/datadriftR@main")
```

## Examples

#### DDM

``` r
library(datadriftR)
# Generate a sample data stream of 1000 elements with approximately equal probabilities for 0 and 1
set.seed(123)  # Setting a seed for reproducibility
data_part1 <- sample(c(0, 1), size = 500, replace = TRUE, prob = c(0.7, 0.3))

# Introduce a change in data distribution
data_part2 <- sample(c(0, 1), size = 500, replace = TRUE, prob = c(0.3, 0.7))

# Combine the two parts
data_stream <- c(data_part1, data_part2)
# Initialize the DDM object
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
#> Drift detected! 560
```

#### EDDM

``` r
eddm <- EDDM$new()
for (i in 1:length(data_stream)) {
  eddm$add_element(data_stream[i])
  if (eddm$change_detected) {
    message(paste("Drift detected!",i))
  } else if (eddm$warning_detected) {
    # message(paste("Warning detected!",i))
  }
}
#> Drift detected! 403
#> Drift detected! 505
#> Drift detected! 800
```

#### HDDM-A

``` r
hddm_a <- HDDM_A$new()
for(i in seq_along(data_stream)) {
  hddm_a$add_element(data_stream[i])
  if (hddm_a$warning_detected) {
    cat(sprintf("Warning zone has been detected in data: %s - at index: %d\n", data_stream[i], i))
  }
  if (hddm_a$change_detected) {
    cat(sprintf("Change has been detected in data: %s - at index: %d\n", data_stream[i], i))
    hddm_a$reset() # Reset after detecting change
  }
}
#> Warning zone has been detected in data: 1 - at index: 511
#> Warning zone has been detected in data: 1 - at index: 512
#> Warning zone has been detected in data: 0 - at index: 513
#> Warning zone has been detected in data: 1 - at index: 514
#> Warning zone has been detected in data: 0 - at index: 515
#> Warning zone has been detected in data: 1 - at index: 516
#> Change has been detected in data: 1 - at index: 517
```

#### HDDM-W

``` r
hddm_w_instance <- HDDM_W$new()
for(i in seq_along(data_stream)) {
  hddm_w_instance$add_element(data_stream[i])
  if(hddm_w_instance$warning_detected) {
    cat(sprintf("Warning zone detected at index: %d\n", i))
  }
  if(hddm_w_instance$change_detected) {
    cat(sprintf("Concept drift detected at index: %d\n", i))
  }
}
#> Warning zone detected at index: 507
#> Warning zone detected at index: 508
#> Warning zone detected at index: 509
#> Warning zone detected at index: 510
#> Concept drift detected at index: 511
```
