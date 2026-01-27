# KSWIN (Kolmogorov-Smirnov WINdowing) for Change Detection

Implements the Kolmogorov-Smirnov test for detecting distribution
changes within a window of streaming data. KSWIN is a non-parametric
method for change detection that compares two samples to determine if
they come from the same distribution.

## Details

KSWIN is effective for detecting changes in the underlying distribution
of data streams. It is particularly useful in scenarios where data
properties may evolve over time, allowing for early detection of changes
that might affect subsequent data processing.

## References

Christoph Raab, Moritz Heusinger, Frank-Michael Schleif, Reactive Soft
Prototype Computing for Concept Drift Streams, Neurocomputing, 2020.

Implementation:
https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d1cc79988a6df40da35312e00f6c4eabb2/src/skmultiflow/drift_detection/kswin.py

## Public fields

- `alpha`:

  Significance level for the KS test.

- `window_size`:

  Total size of the data window used for testing.

- `stat_size`:

  Number of data points sampled from the window for the KS test.

- `window`:

  Current data window used for change detection.

- `change_detected`:

  Boolean flag indicating whether a change has been detected.

- `p_value`:

  P-value of the most recent KS test.

## Methods

### Public methods

- [`KSWIN$new()`](#method-KSWIN-new)

- [`KSWIN$reset()`](#method-KSWIN-reset)

- [`KSWIN$add_element()`](#method-KSWIN-add_element)

- [`KSWIN$detected_change()`](#method-KSWIN-detected_change)

- [`KSWIN$clone()`](#method-KSWIN-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes the KSWIN detector with specific settings.

#### Usage

    KSWIN$new(alpha = 0.005, window_size = 100, stat_size = 30, data = NULL)

#### Arguments

- `alpha`:

  The significance level for the KS test.

- `window_size`:

  The size of the data window for change detection.

- `stat_size`:

  The number of samples in the statistical test window.

- `data`:

  Initial data to populate the window, if provided.

------------------------------------------------------------------------

### Method `reset()`

Resets the internal state of the detector to its initial conditions.

#### Usage

    KSWIN$reset()

------------------------------------------------------------------------

### Method `add_element()`

Adds a new element to the data window and updates the detection status
based on the KS test.

#### Usage

    KSWIN$add_element(x)

#### Arguments

- `x`:

  The new data value to add to the window.

------------------------------------------------------------------------

### Method `detected_change()`

Checks if a change has been detected based on the most recent KS test.

#### Usage

    KSWIN$detected_change()

#### Returns

Boolean indicating whether a change was detected.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    KSWIN$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
set.seed(123)
x <- c(rnorm(100, mean = 0, sd = 1), rnorm(100, mean = 3, sd = 1))

# High-level interface (returns a data.frame of detections)
detect_drift(x, method = "kswin", alpha = 0.001, window_size = 50, stat_size = 20)
#>   index    value  type
#> 1   110 3.918997 drift

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
#> [1] 109
```
