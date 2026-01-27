# Profile Difference Calculation for Change Detection

Implements the calculation of profile differences using various methods
such as PDI, L2, and L2 derivative. The class provides methods for
setting profiles and calculating the differences.

## Details

The class supports multiple methods for calculating profile differences,
including the Profile Disparity Index (PDI) using gold or simple
derivative methods, and L2 norm and L2 derivative calculations. It
allows for customization of various parameters such as embedding
dimensions, derivative orders, and thresholds.

## References

Kobylińska, K., Krzyziński, M., Machowicz, R., Adamek, M., & Biecek, P.
(2023). Exploration of the Rashomon Set Assists Trustworthy Explanations
for Medical Data. arXiv e-prints, arXiv-2308.

## Public fields

- `method`:

  The method used for profile difference calculation.

- `deriv`:

  The method used for derivative calculation.

- `gold_spline`:

  Boolean indicating if cubic spline should be used in gold method.

- `gold_embedding`:

  Embedding dimension for gold method.

- `nderiv`:

  Order of the derivative for simple method.

- `gold_spline_threshold`:

  Threshold for cubic spline in gold method.

- `epsilon`:

  Small value to avoid numerical issues.

- `profile1`:

  The first profile.

- `profile2`:

  The second profile.

## Methods

### Public methods

- [`ProfileDifference$new()`](#method-ProfileDifference-new)

- [`ProfileDifference$reset()`](#method-ProfileDifference-reset)

- [`ProfileDifference$set_profiles()`](#method-ProfileDifference-set_profiles)

- [`ProfileDifference$calculate_difference()`](#method-ProfileDifference-calculate_difference)

- [`ProfileDifference$clone()`](#method-ProfileDifference-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes the ProfileDifference class.

#### Usage

    ProfileDifference$new(
      method = "pdi",
      deriv = "gold",
      gold_spline = TRUE,
      gold_embedding = 4,
      nderiv = 4,
      gold_spline_threshold = 0.01,
      epsilon = NULL
    )

#### Arguments

- `method`:

  The method used for profile difference calculation.

- `deriv`:

  The method used for derivative calculation.

- `gold_spline`:

  Boolean indicating if cubic spline should be used in gold method.

- `gold_embedding`:

  Embedding dimension for gold method.

- `nderiv`:

  Order of the derivative for simple method.

- `gold_spline_threshold`:

  Threshold for cubic spline in gold method.

- `epsilon`:

  Small value to avoid numerical issues.

------------------------------------------------------------------------

### Method `reset()`

Resets the internal state of the detector.

#### Usage

    ProfileDifference$reset()

------------------------------------------------------------------------

### Method `set_profiles()`

Sets the profiles for comparison.

#### Usage

    ProfileDifference$set_profiles(profile1, profile2)

#### Arguments

- `profile1`:

  The first profile.

- `profile2`:

  The second profile.

------------------------------------------------------------------------

### Method `calculate_difference()`

Calculates the difference between the profiles.

#### Usage

    ProfileDifference$calculate_difference()

#### Returns

A list containing the method details and the calculated distance.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ProfileDifference$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
set.seed(123)
profile1 <- list(x = 1:100, y = sin(1:100))
profile2 <- list(x = 1:100, y = sin(1:100) + rnorm(100, 0, 0.1))
pd <- ProfileDifference$new(method = "pdi", deriv = "gold")
pd$set_profiles(profile1, profile2)
result <- pd$calculate_difference()
message("Method:", result$method)
#> Method:pdi
message("Distance:", round(result$distance, 4))
#> Distance:0.0309
```
