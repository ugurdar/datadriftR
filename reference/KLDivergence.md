# Kullback-Leibler Divergence (KLD) for Change Detection

Implements the Kullback-Leibler Divergence (KLD) calculation between two
probability distributions using histograms. The class can detect drift
by comparing the divergence to a predefined threshold.

## Details

The Kullback-Leibler Divergence (KLD) is a measure of how one
probability distribution diverges from a second, expected probability
distribution. This class uses histograms to approximate the
distributions and calculates the KLD to detect changes over time. If the
divergence exceeds a predefined threshold, it signals a detected drift.

## References

Kullback, S., and Leibler, R.A. (1951). On Information and Sufficiency.
Annals of Mathematical Statistics, 22(1), 79-86.

## Public fields

- `epsilon`:

  Value to add to small probabilities to avoid log(0) issues.

- `base`:

  The base of the logarithm used in KLD calculation.

- `bins`:

  Number of bins used for the histogram.

- `drift_level`:

  The threshold for detecting drift.

- `drift_detected`:

  Boolean indicating if drift has been detected.

- `p`:

  Initial distribution.

- `kl_result`:

  The result of the KLD calculation.

## Methods

### Public methods

- [`KLDivergence$new()`](#method-KLDivergence-new)

- [`KLDivergence$reset()`](#method-KLDivergence-reset)

- [`KLDivergence$set_initial_distribution()`](#method-KLDivergence-set_initial_distribution)

- [`KLDivergence$add_distribution()`](#method-KLDivergence-add_distribution)

- [`KLDivergence$calculate_kld()`](#method-KLDivergence-calculate_kld)

- [`KLDivergence$get_kl_result()`](#method-KLDivergence-get_kl_result)

- [`KLDivergence$is_drift_detected()`](#method-KLDivergence-is_drift_detected)

- [`KLDivergence$clone()`](#method-KLDivergence-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes the KLDivergence class.

#### Usage

    KLDivergence$new(epsilon = 1e-10, base = exp(1), bins = 10, drift_level = 0.2)

#### Arguments

- `epsilon`:

  Value to add to small probabilities to avoid log(0) issues.

- `base`:

  The base of the logarithm used in KLD calculation.

- `bins`:

  Number of bins used for the histogram.

- `drift_level`:

  The threshold for detecting drift.

------------------------------------------------------------------------

### Method `reset()`

Resets the internal state of the detector.

#### Usage

    KLDivergence$reset()

------------------------------------------------------------------------

### Method `set_initial_distribution()`

Sets the initial distribution.

#### Usage

    KLDivergence$set_initial_distribution(initial_p)

#### Arguments

- `initial_p`:

  The initial distribution.

------------------------------------------------------------------------

### Method `add_distribution()`

Adds a new distribution and calculates the KLD.

#### Usage

    KLDivergence$add_distribution(q)

#### Arguments

- `q`:

  The new distribution.

------------------------------------------------------------------------

### Method `calculate_kld()`

Calculates the KLD between two distributions.

#### Usage

    KLDivergence$calculate_kld(p, q)

#### Arguments

- `p`:

  The initial distribution.

- `q`:

  The new distribution.

#### Returns

The KLD value.

------------------------------------------------------------------------

### Method `get_kl_result()`

Returns the current KLD result.

#### Usage

    KLDivergence$get_kl_result()

#### Returns

The current KLD value.

------------------------------------------------------------------------

### Method `is_drift_detected()`

Checks if drift has been detected.

#### Usage

    KLDivergence$is_drift_detected()

#### Returns

TRUE if drift is detected, otherwise FALSE.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    KLDivergence$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
set.seed(123)  # Setting a seed for reproducibility
initial_data <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
kld <- KLDivergence$new(bins = 10, drift_level = 0.2)
kld$set_initial_distribution(initial_data)

new_data <- c(0.2, 0.2, 0.3, 0.4, 0.4, 0.5, 0.6, 0.7, 0.7, 0.8)
kld$add_distribution(new_data)

kl_result <- kld$get_kl_result()
message(paste("KL Divergence:", kl_result))
#> KL Divergence: 6.00903559691594

if (kld$is_drift_detected()) {
  message("Drift detected.")
}
#> Drift detected.
```
