<img align="right" width="220" height="240" src="https://drive.google.com/uc?export=download&id=1w6vmd9972c1TMAsKKofzmJT47_D-TSQO">
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datadriftR

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CRAN status](https://www.r-pkg.org/badges/version/datadriftR)](https://CRAN.R-project.org/package=datadriftR)

**datadriftR** is an R package for detecting concept drift in streaming data. It helps you monitor when the statistical properties of your data change over time — a common problem in production machine learning systems.

## Features

- **8 detection methods**: DDM, EDDM, HDDM-A, HDDM-W, KSWIN, Page-Hinkley, KL-Divergence, Profile Difference
- **Simple interface**: Just use `detect_drift()` and get results
- **Real-time processing**: Analyze data one observation at a time

## Installation

Install from CRAN:

```r
install.packages("datadriftR")
```

Or install the development version:

```r
# install.packages("remotes")
remotes::install_github("ugurdar/datadriftR")
```

## Quick Start

```r
library(datadriftR)
set.seed(123)

# Create a stream with drift at position 501
stable <- sample(c(0, 1), 500, replace = TRUE, prob = c(0.7, 0.3))
drift  <- sample(c(0, 1), 500, replace = TRUE, prob = c(0.3, 0.7))
stream <- c(stable, drift)

# Detect drift
results <- detect_drift(stream, method = "ddm")
print(results)

# Try different methods
detect_drift(stream, method = "kswin")
detect_drift(stream, method = "page_hinkley")
```

## Available Methods

| Method | Description |
|--------|-------------|
| `ddm` | Drift Detection Method - monitors error rate |
| `eddm` | Early DDM - faster detection |
| `hddm_a` | Hoeffding bound with averaging |
| `hddm_w` | Hoeffding bound with weighting |
| `kswin` | Kolmogorov-Smirnov windowing |
| `page_hinkley` | Page-Hinkley test |
| `kl_divergence` | KL divergence based |
| `profile_difference` | Functional data comparison |

## Documentation

- [Getting Started](https://ugurdar.github.io/datadriftR/articles/datadriftR-intro.html)
- [Function Reference](https://ugurdar.github.io/datadriftR/reference/index.html)
- [CRAN Page](https://cran.r-project.org/package=datadriftR)

## Citation

If you use datadriftR in your research, please cite:

```bibtex
@article{dar2025datadriftr,
  title={datadriftR: an R package for streaming data drift detection},
  author={Dar, Ugur and Cavus, Mustafa},
  journal={Journal of Open Source Software},
  year={2025},
  note={Submitted}
}
```

## License

MIT License

## Authors

- Ugur Dar (Eskisehir Technical University)
- Mustafa Cavus (Eskisehir Technical University)
