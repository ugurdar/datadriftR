# ADWIN (ADaptive WINdowing) Drift Detector

ADWIN is a drift detection method that maintains a variable-length
window of recent data to detect concept drift with mathematical
guarantees. Based on the algorithm from Bifet and Gavalda (2007).

## Details

The algorithm maintains a sliding window W with the most recent
elements. It compares the distribution of two sub-windows (W0 and W1)
and detects drift when their means differ significantly according to the
Hoeffding bound. Uses bucket compression for memory efficiency.

## References

Bifet, A., & Gavalda, R. (2007). Learning from time-changing data with
adaptive windowing. In Proceedings of the 2007 SIAM International
Conference on Data Mining (pp. 443-448).

## Public fields

- `delta`:

  Significance threshold for drift detection

- `clock`:

  Frequency of drift checks

- `max_buckets`:

  Maximum buckets per level

- `min_window_length`:

  Minimum window length for comparison

- `grace_period`:

  Initial period before detection starts

## Active bindings

- `drift_detected`:

  Check if drift was detected (active binding)

## Methods

### Public methods

- [`ADWIN$new()`](#method-ADWIN-new)

- [`ADWIN$reset()`](#method-ADWIN-reset)

- [`ADWIN$add_element()`](#method-ADWIN-add_element)

- [`ADWIN$detected_change()`](#method-ADWIN-detected_change)

- [`ADWIN$width()`](#method-ADWIN-width)

- [`ADWIN$n_detections()`](#method-ADWIN-n_detections)

- [`ADWIN$estimation()`](#method-ADWIN-estimation)

- [`ADWIN$variance()`](#method-ADWIN-variance)

- [`ADWIN$clone()`](#method-ADWIN-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize ADWIN detector

#### Usage

    ADWIN$new(
      delta = 0.002,
      clock = 32,
      max_buckets = 5,
      min_window_length = 5,
      grace_period = 10
    )

#### Arguments

- `delta`:

  Significance threshold (default 0.002)

- `clock`:

  Drift check frequency (default 32)

- `max_buckets`:

  Max buckets per level (default 5)

- `min_window_length`:

  Min window for comparison (default 5)

- `grace_period`:

  Startup period (default 10)

------------------------------------------------------------------------

### Method `reset()`

Reset the detector state

#### Usage

    ADWIN$reset()

------------------------------------------------------------------------

### Method `add_element()`

Add a new element to the detector

#### Usage

    ADWIN$add_element(value)

#### Arguments

- `value`:

  Numeric value to add

------------------------------------------------------------------------

### Method `detected_change()`

Check if drift was detected

#### Usage

    ADWIN$detected_change()

#### Returns

Logical indicating drift detection

------------------------------------------------------------------------

### Method `width()`

Get current window width

#### Usage

    ADWIN$width()

#### Returns

Integer window width

------------------------------------------------------------------------

### Method `n_detections()`

Get number of detections

#### Usage

    ADWIN$n_detections()

#### Returns

Integer count of detections

------------------------------------------------------------------------

### Method `estimation()`

Get current mean estimate

#### Usage

    ADWIN$estimation()

#### Returns

Numeric mean

------------------------------------------------------------------------

### Method `variance()`

Get current variance

#### Usage

    ADWIN$variance()

#### Returns

Numeric variance

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ADWIN$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
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
#> Change detected at index 1024, input value: 6
```
