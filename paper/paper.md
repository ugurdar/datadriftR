---
title: "datadriftR: Concept Drift Detection Methods for Streaming Data"
authors:
  - name: Ugur Dar
    orcid: 0009-0005-8076-2199
    affiliation: 1
  - name: Mustafa Cavus
    orcid: 0000-0002-6172-5449
    affiliation: 2
affiliations:
  - name: Eskisehir Technical University, Department of Statistics, Eskisehir, Turkey
    index: 1
  - name: Eskisehir Technical University, Department of Statistics, Eskisehir, Turkey
    index: 2
date: 2025-11-09
tags:
  - R
  - concept-drift
  - streaming-data
  - online-learning
bibliography: paper.bib
---

# Summary

datadriftR is an R package for detecting concept drift in streaming data. It provides a suite of widely used univariate drift detection algorithms, enabling practitioners to monitor and react to changes in data-generating processes. The package is designed with simple R6 classes and consistent APIs to integrate into streaming pipelines and online learning workflows.

# Statement of need

Detecting distributional shifts in streaming data is critical for the reliability of deployed machine learning systems. While Python ecosystems (e.g., scikit-multiflow, river) offer several options, there is limited, consolidated support in base R for online drift detection with consistent interfaces. datadriftR fills this gap by offering reference implementations of:

- DDM (Drift Detection Method) [@gama2004]
- EDDM (Early Drift Detection Method) [@baena2006]
- HDDM-A and HDDM-W [@frias2014; @raab2020]
- KSWIN (Kolmogorov–Smirnov Windowing) [@montiel2018]
- Page-Hinkley [@page1954]
- KL Divergence-based monitoring [@kullback1951]

These methods share a simple interface (add_element, reset, change_detected, warning_detected) that enables quick experimentation and operational monitoring in streaming contexts.

# Implementation

The package uses R6 classes to maintain online statistics per method and update them in O(1) or amortized O(1) time per element. Core algorithms follow the canonical references and are verified with targeted examples. The package has no heavy dependencies and works with base R (>= 3.5.2).

# Example

An example for DDM is shown below; similar usage patterns apply to other detectors.

```r
library(datadriftR)
set.seed(123)
pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.7, 0.3))
post <- sample(c(0,1), 500, replace = TRUE, prob = c(0.3, 0.7))
stream <- c(pre, post)

m <- DDM$new()
for (i in seq_along(stream)) {
  m$add_element(stream[i])
  if (m$change_detected) {
    message(sprintf("Drift detected at index %d", i))
    break
  }
}
```

# Acknowledgements

We thank the maintainers of the referenced algorithms and the R community for helpful discussions. Any remaining issues are our own.

# References
