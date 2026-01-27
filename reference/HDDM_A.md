# HDDM_A: Drift Detection Method based on Adaptive Windows

This class implements the HDDM_A drift detection method that uses
adaptive windows to detect changes in the mean of a data stream. It is
designed to monitor online streams of data and can detect increases or
decreases in the process mean in a non-parametric and online manner.

## Details

HDDM_A adapts to changes in the data stream by adjusting its internal
windows to track the minimum and maximum values of the process mean. It
triggers alerts when a significant drift from these benchmarks is
detected.

## References

Frías-Blanco I, del Campo-Ávila J, Ramos-Jimenez G, et al. Online and
non-parametric drift detection methods based on Hoeffding’s bounds. IEEE
Transactions on Knowledge and Data Engineering, 2014, 27(3): 810-823.

Albert Bifet, Geoff Holmes, Richard Kirkby, Bernhard Pfahringer. MOA:
Massive Online Analysis; Journal of Machine Learning Research 11:
1601-1604, 2010.

Implementation:
https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d1cc79988a6df40da35312e00f6c4eabb2/src/skmultiflow/drift_detection/hddm_a.py

## Public fields

- `drift_confidence`:

  Confidence level for detecting a drift.

- `warning_confidence`:

  Confidence level for warning detection.

- `two_side_option`:

  Boolean flag for one-sided or two-sided mean monitoring.

- `total_n`:

  Total number of samples seen.

- `total_c`:

  Total cumulative sum of the samples.

- `n_max`:

  Maximum window end for sample count.

- `c_max`:

  Maximum window end for cumulative sum.

- `n_min`:

  Minimum window start for sample count.

- `c_min`:

  Minimum window start for cumulative sum.

- `n_estimation`:

  Number of samples since the last detected change.

- `c_estimation`:

  Cumulative sum since the last detected change.

- `change_detected`:

  Boolean indicating if a change was detected.

- `warning_detected`:

  Boolean indicating if a warning has been detected.

- `estimation`:

  Current estimated mean of the stream.

- `delay`:

  Current delay since the last update.

## Methods

### Public methods

- [`HDDM_A$new()`](#method-HDDM_A-new)

- [`HDDM_A$add_element()`](#method-HDDM_A-add_element)

- [`HDDM_A$mean_incr()`](#method-HDDM_A-mean_incr)

- [`HDDM_A$mean_decr()`](#method-HDDM_A-mean_decr)

- [`HDDM_A$reset()`](#method-HDDM_A-reset)

- [`HDDM_A$update_estimations()`](#method-HDDM_A-update_estimations)

- [`HDDM_A$clone()`](#method-HDDM_A-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes the HDDM_A detector with specific settings.

#### Usage

    HDDM_A$new(
      drift_confidence = 0.001,
      warning_confidence = 0.005,
      two_side_option = TRUE
    )

#### Arguments

- `drift_confidence`:

  Confidence level for drift detection.

- `warning_confidence`:

  Confidence level for issuing warnings.

- `two_side_option`:

  Whether to monitor both increases and decreases.

------------------------------------------------------------------------

### Method `add_element()`

Adds an element to the data stream and updates the detection status.

#### Usage

    HDDM_A$add_element(prediction)

#### Arguments

- `prediction`:

  Numeric, the new data value to add.

------------------------------------------------------------------------

### Method `mean_incr()`

Calculates if there is an increase in the mean.

#### Usage

    HDDM_A$mean_incr(c_min, n_min, total_c, total_n, confidence)

#### Arguments

- `c_min`:

  Minimum cumulative sum.

- `n_min`:

  Minimum count of samples.

- `total_c`:

  Total cumulative sum.

- `total_n`:

  Total number of samples.

- `confidence`:

  Confidence threshold for detection.

------------------------------------------------------------------------

### Method `mean_decr()`

Calculates if there is a decrease in the mean.

#### Usage

    HDDM_A$mean_decr(c_max, n_max, total_c, total_n)

#### Arguments

- `c_max`:

  Maximum cumulative sum.

- `n_max`:

  Maximum count of samples.

- `total_c`:

  Total cumulative sum.

- `total_n`:

  Total number of samples.

------------------------------------------------------------------------

### Method `reset()`

Resets all internal counters and accumulators to their initial state.

#### Usage

    HDDM_A$reset()

------------------------------------------------------------------------

### Method `update_estimations()`

Updates estimations of the mean after detecting changes.

#### Usage

    HDDM_A$update_estimations()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    HDDM_A$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
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
#> Warning detected at index: 123
#> Warning detected at index: 124
#> Warning detected at index: 125
#> Warning detected at index: 126
#> Warning detected at index: 127
#> Concept drift detected at index: 128
```
