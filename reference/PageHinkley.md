# Page-Hinkley Test for Change Detection

Implements the Page-Hinkley test, a sequential analysis technique used
to detect changes in the average value of a continuous signal or
process. It is effective in detecting small but persistent changes over
time, making it suitable for real-time monitoring applications.

## Details

The Page-Hinkley test is a type of cumulative sum (CUSUM) test that
accumulates differences between data points and a reference value
(running mean). It triggers a change detection signal when the
cumulative sum exceeds a predefined threshold. This test is especially
useful for early detection of subtle shifts in the behavior of the
monitored process.

## References

E. S. Page. 1954. Continuous Inspection Schemes. Biometrika 41, 1/2
(1954), 100–115.

Montiel, Jacob, et al. "Scikit-Multiflow: A Multi-output Streaming
Framework." Journal of Machine Learning Research, 2018. This framework
provides tools for multi-output and stream data mining and was an
inspiration for some of the implementations in this class.

Implementation:
https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d1cc79988a6df40da35312e00f6c4eabb2/src/skmultiflow/drift_detection/page_hinkley.py

## Public fields

- `min_instances`:

  Minimum number of instances required to start detection.

- `delta`:

  Minimal change considered significant for detection.

- `threshold`:

  Decision threshold for signaling a change.

- `alpha`:

  Forgetting factor for the cumulative sum calculation.

- `x_mean`:

  Running mean of the observed values.

- `sample_count`:

  Counter for the number of samples seen.

- `sum`:

  Weighted cumulative sum used for mean calculation.

- `PH`:

  Page-Hinkley statistic.

- `min_PH`:

  Minimum value of PH statistic observed.

- `change_detected`:

  Boolean indicating if a drift has been detected.

## Methods

### Public methods

- [`PageHinkley$new()`](#method-PageHinkley-new)

- [`PageHinkley$reset()`](#method-PageHinkley-reset)

- [`PageHinkley$add_element()`](#method-PageHinkley-add_element)

- [`PageHinkley$detected_change()`](#method-PageHinkley-detected_change)

- [`PageHinkley$get_PH()`](#method-PageHinkley-get_PH)

- [`PageHinkley$clone()`](#method-PageHinkley-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes the Page-Hinkley test with specific parameters.

#### Usage

    PageHinkley$new(min_instances = 30, delta = 0.05, threshold = 50, alpha = 1)

#### Arguments

- `min_instances`:

  Minimum number of samples before detection starts.

- `delta`:

  Change magnitude to trigger detection.

- `threshold`:

  Cumulative sum threshold for change detection.

- `alpha`:

  Weight for older data in cumulative sum.

------------------------------------------------------------------------

### Method `reset()`

Resets all the internal states of the detector to initial values.

#### Usage

    PageHinkley$reset()

------------------------------------------------------------------------

### Method `add_element()`

Adds a new element to the data stream and updates the detection status
based on the Page-Hinkley test.

#### Usage

    PageHinkley$add_element(x)

#### Arguments

- `x`:

  New data value to add and evaluate.

------------------------------------------------------------------------

### Method `detected_change()`

Checks if a change has been detected based on the last update.

#### Usage

    PageHinkley$detected_change()

#### Returns

Boolean indicating whether a change was detected.

------------------------------------------------------------------------

### Method `get_PH()`

Returns the current Page-Hinkley statistic.

#### Usage

    PageHinkley$get_PH()

#### Returns

The current PH value.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PageHinkley$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
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
#> Change has been detected in data: 5 - at index: 120
```
