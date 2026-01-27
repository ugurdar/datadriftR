# EDDM (Early Drift Detection Method)

This class implements the Early Drift Detection Method (EDDM), designed
to detect concept drifts in online learning scenarios by monitoring the
distances between consecutive errors. EDDM is particularly useful for
detecting gradual drifts earlier than abrupt changes.

## Details

EDDM is a statistical process control method that is more sensitive to
changes that happen more slowly and can provide early warnings of
deterioration before the error rate increases significantly.

## References

Early Drift Detection Method. Manuel Baena-Garcia, Jose Del Campo-Avila,
Raúl Fidalgo, Albert Bifet, Ricard Gavalda, Rafael Morales-Bueno. In
Fourth International Workshop on Knowledge Discovery from Data Streams,
2006.

Implementation:
https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d1cc79988a6df40da35312e00f6c4eabb2/src/skmultiflow/drift_detection/eddm.py

## Public fields

- `eddm_warning`:

  Warning threshold setting.

- `eddm_outcontrol`:

  Out-of-control threshold setting.

- `m_num_errors`:

  Current number of errors encountered.

- `m_min_num_errors`:

  Minimum number of errors to initialize drift detection.

- `m_n`:

  Total instances processed.

- `m_d`:

  Distance to the last error from the current instance.

- `m_lastd`:

  Distance to the previous error from the last error.

- `m_mean`:

  Mean of the distances between errors.

- `m_std_temp`:

  Temporary standard deviation accumulator for the distances.

- `m_m2s_max`:

  Maximum mean plus two standard deviations observed.

- `delay`:

  Delay count since the last detected change.

- `estimation`:

  Current estimated mean distance between errors.

- `warning_detected`:

  Boolean indicating if a warning has been detected.

- `change_detected`:

  Boolean indicating if a change has been detected.

## Methods

### Public methods

- [`EDDM$new()`](#method-EDDM-new)

- [`EDDM$reset()`](#method-EDDM-reset)

- [`EDDM$add_element()`](#method-EDDM-add_element)

- [`EDDM$clone()`](#method-EDDM-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes the EDDM detector with specific parameters.

#### Usage

    EDDM$new(min_num_instances = 30, eddm_warning = 0.95, eddm_outcontrol = 0.9)

#### Arguments

- `min_num_instances`:

  Minimum number of errors before drift detection starts.

- `eddm_warning`:

  Threshold for warning level.

- `eddm_outcontrol`:

  Threshold for out-of-control level.

------------------------------------------------------------------------

### Method `reset()`

Resets the internal state of the EDDM detector.

#### Usage

    EDDM$reset()

------------------------------------------------------------------------

### Method `add_element()`

Adds a new observation and updates the drift detection status.

#### Usage

    EDDM$add_element(prediction)

#### Arguments

- `prediction`:

  Numeric value representing a new error (usually 0 or 1).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    EDDM$clone(deep = FALSE)

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
eddm <- EDDM$new()
for (i in 1:length(data_stream)) {
  eddm$add_element(data_stream[i])
  if (eddm$change_detected) {
    message(paste("Drift detected!",i))
  } else if (eddm$warning_detected) {
    message(paste("Warning detected!",i))
  }
}
#> Warning detected! 108
#> Warning detected! 109
#> Warning detected! 110
#> Warning detected! 111
#> Warning detected! 112
#> Warning detected! 113
#> Warning detected! 114
#> Warning detected! 115
#> Drift detected! 116
#> Warning detected! 157
#> Warning detected! 158
#> Warning detected! 159
#> Warning detected! 160
#> Warning detected! 161
#> Warning detected! 162
#> Drift detected! 163
```
