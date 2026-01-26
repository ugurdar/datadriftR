# datadriftR

[![CRAN status](https://www.r-pkg.org/badges/version/datadriftR)](https://CRAN.R-project.org/package=datadriftR)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**datadriftR** is an R package for detecting data drift in streaming data. It monitors when statistical properties of your data change over time, which is essential for maintaining machine learning model performance in production.

## Available Methods

| Method | Description |
|--------|-------------|
| `ddm` | Drift Detection Method |
| `eddm` | Early Drift Detection Method |
| `hddm_a` | Hoeffding's bound with averaging |
| `hddm_w` | Hoeffding's bound with weighting |
| `kswin` | Kolmogorov-Smirnov Windowing |
| `adwin` | ADaptive WINdowing |
| `page_hinkley` | Page-Hinkley Test |
| `kl_divergence` | KL Divergence |
| `profile_difference` | Profile Difference |

## Installation

```r
# Install from CRAN
install.packages("datadriftR")

# Or install the development version from GitHub with pak
# install.packages("pak")
pak::pak("ugurdar/datadriftR")

# Or install the development version from GitHub with remotes
# install.packages("remotes")
# remotes::install_github("ugurdar/datadriftR")
```

## Simple DDM Example

```r
library(datadriftR)

# Create a stream with drift at position 501
set.seed(123)
stream <- c(

  sample(c(0, 1), 500, replace = TRUE, prob = c(0.7, 0.3)),
  sample(c(0, 1), 500, replace = TRUE, prob = c(0.3, 0.7))
)

# Detect drift
results <- detect_drift(stream, method = "ddm")
print(results)
```



## Documentation

- [Getting Started](https://ugurdar.github.io/datadriftR/articles/datadriftR-intro.html)
- [Reference](https://ugurdar.github.io/datadriftR/reference/)

## Citation

```bibtex
@article{dar2025datadriftr,
  title={datadriftR: Drift Detection Methods for Stream Data},
  author={Dar, Ugur and Cavus, Mustafa},
  journal={Journal of Open Source Software},
  year={2025}
}
```

## Authors

- [Ugur Dar](https://github.com/ugurdar) - Eskisehir Technical University
- [Mustafa Cavus](https://github.com/mcavs) - Eskisehir Technical University
