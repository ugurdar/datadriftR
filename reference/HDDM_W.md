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

Frías-Blanco I, del Campo-Ávila J, Ramos-Jimenez G, et al. Online and
non-parametric drift detection methods based on Hoeffding’s bounds. IEEE
Transactions on Knowledge and Data Engineering, 2014, 27(3): 810-823.

Albert Bifet, Geoff Holmes, Richard Kirkby, Bernhard Pfahringer. MOA:
Massive Online Analysis; Journal of Machine Learning Research 11:
1601-1604, 2010. Implementation:
https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d1cc79988a6df40da35312e00f6c4eabb2/src/skmultiflow/drift_detection/hddm_w.py

## Public fields

- `drift_confidence`:

  Confidence level for detecting a drift (default: 0.001).

- `warning_confidence`:

  Confidence level for warning detection (default: 0.005).

- `lambda_option`:

  Decay rate for the EWMA statistic, smaller values give less weight to
  recent data (default: 0.050).

- `two_side_option`:

  Boolean flag for one-sided or two-sided error monitoring (default:
  TRUE).

- `total`:

  Container for the EWMA estimator and its bounded conditional sum.

- `sample1_decr_monitor`:

  First sample monitor for detecting decrements.

- `sample1_incr_monitor`:

  First sample monitor for detecting increments.

- `sample2_decr_monitor`:

  Second sample monitor for detecting decrements.

- `sample2_incr_monitor`:

  Second sample monitor for detecting increments.

- `incr_cutpoint`:

  Cutpoint for deciding increments.

- `decr_cutpoint`:

  Cutpoint for deciding decrements.

- `width`:

  Current width of the window.

- `delay`:

  Delay count since last reset.

- `change_detected`:

  Boolean indicating if a change was detected.

- `warning_detected`:

  Boolean indicating if currently in a warning zone.

- `estimation`:

  The current estimation of the stream's mean.

## Methods

### Public methods

- [`HDDM_W$new()`](#method-HDDM_W-new)

- [`HDDM_W$add_element()`](#method-HDDM_W-add_element)

- [`HDDM_W$SampleInfo()`](#method-HDDM_W-SampleInfo)

- [`HDDM_W$reset()`](#method-HDDM_W-reset)

- [`HDDM_W$detect_mean_increment()`](#method-HDDM_W-detect_mean_increment)

- [`HDDM_W$monitor_mean_incr()`](#method-HDDM_W-monitor_mean_incr)

- [`HDDM_W$monitor_mean_decr()`](#method-HDDM_W-monitor_mean_decr)

- [`HDDM_W$update_incr_statistics()`](#method-HDDM_W-update_incr_statistics)

- [`HDDM_W$update_decr_statistics()`](#method-HDDM_W-update_decr_statistics)

- [`HDDM_W$clone()`](#method-HDDM_W-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes the HDDM_W detector with specific parameters.

#### Usage

    HDDM_W$new(
      drift_confidence = 0.001,
      warning_confidence = 0.005,
      lambda_option = 0.05,
      two_side_option = TRUE
    )

#### Arguments

- `drift_confidence`:

  Confidence level for drift detection.

- `warning_confidence`:

  Confidence level for issuing warnings.

- `lambda_option`:

  Decay rate for the EWMA statistic.

- `two_side_option`:

  Whether to monitor both increases and decreases.

------------------------------------------------------------------------

### Method `add_element()`

Adds a new element to the data stream and updates the detection status.

#### Usage

    HDDM_W$add_element(prediction)

#### Arguments

- `prediction`:

  The new data value to add.

------------------------------------------------------------------------

### Method `SampleInfo()`

Provides current information about the monitoring samples, typically
used for debugging or monitoring.

#### Usage

    HDDM_W$SampleInfo()

------------------------------------------------------------------------

### Method `reset()`

Resets the internal state to initial conditions.

#### Usage

    HDDM_W$reset()

------------------------------------------------------------------------

### Method `detect_mean_increment()`

Detects an increment in the mean between two samples based on the
provided confidence level.

#### Usage

    HDDM_W$detect_mean_increment(sample1, sample2, confidence)

#### Arguments

- `sample1`:

  First sample information, containing EWMA estimator and bounded
  conditional sum.

- `sample2`:

  Second sample information, containing EWMA estimator and bounded
  conditional sum.

- `confidence`:

  The confidence level used for calculating the bound.

#### Returns

Boolean indicating if an increment in mean was detected.

------------------------------------------------------------------------

### Method `monitor_mean_incr()`

Monitors the data stream for an increase in the mean based on the set
confidence level.

#### Usage

    HDDM_W$monitor_mean_incr(confidence)

#### Arguments

- `confidence`:

  The confidence level used to detect changes in the mean.

#### Returns

Boolean indicating if an increase in the mean was detected.

------------------------------------------------------------------------

### Method `monitor_mean_decr()`

Monitors the data stream for a decrease in the mean based on the set
confidence level.

#### Usage

    HDDM_W$monitor_mean_decr(confidence)

#### Arguments

- `confidence`:

  The confidence level used to detect changes in the mean.

#### Returns

Boolean indicating if a decrease in the mean was detected.

------------------------------------------------------------------------

### Method `update_incr_statistics()`

Updates increment statistics for drift monitoring based on new values
and confidence. This method adjusts the cutpoint for increments and
updates the monitoring samples.

#### Usage

    HDDM_W$update_incr_statistics(value, confidence)

#### Arguments

- `value`:

  The new value to update statistics.

- `confidence`:

  The confidence level for the update.

------------------------------------------------------------------------

### Method `update_decr_statistics()`

Updates decrement statistics for drift monitoring based on new values
and confidence. This method adjusts the cutpoint for decrements and
updates the monitoring samples.

#### Usage

    HDDM_W$update_decr_statistics(value, confidence)

#### Arguments

- `value`:

  The new value to update statistics.

- `confidence`:

  The confidence level for the update.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    HDDM_W$clone(deep = FALSE)

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
#> Warning detected at index: 122
#> Warning detected at index: 123
#> Warning detected at index: 124
#> Concept drift detected at index: 125
```
