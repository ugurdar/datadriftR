<img align="right" width="220" height="240" src="https://drive.google.com/uc?export=download&id=1w6vmd9972c1TMAsKKofzmJT47_D-TSQO">
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datadriftR

[![R-CMD-check](https://github.com/yourusername/datadriftR/workflows/R-CMD-check/badge.svg)](https://github.com/yourusername/datadriftR/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**datadriftR** is an R package for real-time detection of data drift in univariate streaming data. It provides a unified interface to multiple online drift detectors, enabling reproducible monitoring workflows in production machine learning systems.

## Features

- **8 streaming drift detectors**: DDM, EDDM, HDDM-A, HDDM-W, KSWIN, Page-Hinkley, KL-histogram, ProfileDifference
- **Unified R6 interface**: All detectors expose `add_element()` for streaming updates
- **Single-observation processing**: No batch reprocessing required
- **Lightweight**: Core detectors depend only on base R and R6

## Installation

Install from GitHub:

```r
# install.packages("remotes")
remotes::install_github("yourusername/datadriftR")
```

## Quick Start

```r
library(datadriftR)
set.seed(123)

# Generate synthetic stream with drift at index 501
pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.7, 0.3))
post <- sample(c(0,1), 500, replace = TRUE, prob = c(0.3, 0.7))
stream <- c(pre, post)

# DDM detector
ddm <- DDM$new()
for (i in seq_along(stream)) {
  ddm$add_element(stream[i])
  if (ddm$change_detected) {
    message("DDM drift detected at index ", i)
    break
  }
}

# Page-Hinkley detector
ph <- PageHinkley$new()
for (i in seq_along(stream)) {
  ph$add_element(stream[i])
  if (ph$detected_change()) {
    message("Page-Hinkley drift detected at index ", i)
    break
  }
}
```

## Documentation

- [Package website](https://yourusername.github.io/datadriftR) (if using pkgdown)
- [JOSS paper](paper/paper.md)
- Function documentation: `?DDM`, `?KSWIN`, etc.

## Testing

Run tests locally:

```r
devtools::test()
```

Or from command line:

```bash
Rscript run_tests_simple.R
```

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

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

MIT License - see [LICENSE](LICENSE) file.

## Authors

- Ugur Dar (Eskisehir Technical University)
- Mustafa Cavus (Eskisehir Technical University)
