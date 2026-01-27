# DDM (Drift Detection Method)

Implements the Drift Detection Method (DDM), used for detecting concept
drift in data streams by analyzing the performance of online learners.
The method monitors changes in the error rate of a learner, signaling
potential concept drift.

## Details

DDM is designed to be simple yet effective for detecting concept drift
by monitoring the error rate of any online classifier. The method is
particularly sensitive to increases in the error rate, which is
typically a strong indicator of concept drift.

## References

João Gama, Pedro Medas, Gladys Castillo, Pedro Pereira Rodrigues:
Learning with Drift Detection. SBIA 2004: 286-295

Implementation:
https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d1cc79988a6df40da35312e00f6c4eabb2/src/skmultiflow/drift_detection/ddm.py

## Public fields

- `min_instances`:

  Minimum number of instances required before drift detection begins.

- `warning_level`:

  Multiplier for the standard deviation to set the warning threshold.

- `out_control_level`:

  Multiplier for the standard deviation to set the out-of-control
  threshold.

- `sample_count`:

  Counter for the number of samples processed.

- `miss_prob`:

  Current estimated probability of misclassification.

- `miss_std`:

  Current estimated standard deviation of misclassification probability.

- `miss_prob_sd_min`:

  Minimum recorded value of misclassification probability plus its
  standard deviation.

- `miss_prob_min`:

  Minimum recorded misclassification probability.

- `miss_sd_min`:

  Minimum recorded standard deviation.

- `estimation`:

  Current estimation of misclassification probability.

- `change_detected`:

  Boolean indicating if a drift has been detected.

- `warning_detected`:

  Boolean indicating if a warning level has been reached.

- `delay`:

  Delay since the last relevant sample.

## Methods

### Public methods

- [`DDM$new()`](#method-DDM-new)

- [`DDM$reset()`](#method-DDM-reset)

- [`DDM$add_element()`](#method-DDM-add_element)

- [`DDM$detected_change()`](#method-DDM-detected_change)

- [`DDM$clone()`](#method-DDM-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes the DDM detector with specific parameters.

#### Usage

    DDM$new(min_num_instances = 30, warning_level = 2, out_control_level = 3)

#### Arguments

- `min_num_instances`:

  Minimum number of samples required before starting drift detection.

- `warning_level`:

  Threshold multiplier for setting a warning level.

- `out_control_level`:

  Threshold multiplier for setting the out-of-control level.

------------------------------------------------------------------------

### Method `reset()`

Resets the internal state of the DDM detector.

#### Usage

    DDM$reset()

------------------------------------------------------------------------

### Method `add_element()`

Adds a new prediction error value to the model, updates the calculation
of the misclassification probability and its standard deviation, and
checks for warnings or drifts based on updated statistics.

#### Usage

    DDM$add_element(prediction)

#### Arguments

- `prediction`:

  The new data point (prediction error) to be added to the model.

------------------------------------------------------------------------

### Method `detected_change()`

Returns a boolean indicating whether a drift has been detected based on
the monitored statistics.

#### Usage

    DDM$detected_change()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DDM$clone(deep = FALSE)

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
#> Drift detected! 129
```
