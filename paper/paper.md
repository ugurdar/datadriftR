---

---
title: "datadriftR: an R package for streaming data drift detection"
authors:
- affiliation: 1
  name: Ugur Dar
  orcid: 0009-0005-8076-2199
- affiliation: 1
  name: Mustafa Cavus
  orcid: 0000-0002-6172-5449
date: "9 November 2025"
output:
  pdf_document:
    fig_caption: yes
  html_document:
    df_print: paged
    fig_caption: true
  word_document: default
bibliography: paper.bib
tags:
- R
- data drift
- concept drift
- change detection
affiliations:
- index: 1
  name: Eskisehir Technical University, Department of Statistics, Eskisehir, Turkey
---

# Summary

datadriftR is an open-source R package for detecting data drift (also known as concept drift) in univariate data streams. The package implements a suite of established statistical drift detectors, each encapsulated in an R6 class with identical method names and usage patterns, enabling continuous monitoring of distributional changes in real-time systems. The detectors provided include: DDM (Drift Detection Method) [@gama2004], EDDM (Early Drift Detection Method) [@baena2006], HDDM-A and HDDM-W (Hoeffding-bound-based methods) [@frias2014], KSWIN (Kolmogorov–Smirnov Windowing) [@raab2020], Page–Hinkley [@page1954], KL divergence-based monitoring [@kullback1951], and ProfileDifference (functional derivative-based comparison) [@kobylinska2023]. All detectors maintain internal sufficient statistics and perform constant-time or amortized constant-time updates per observation, making the package suitable for operational deployment with minimal computational overhead. The design emphasizes minimal dependencies (base R ≥ 3.5.2, with optional support from `R6`, `fda.usc`, and `doremi` for advanced methods), consistent method names across all detectors (`add_element`, `reset`, `change_detected`, `warning_detected`), and seamless integration with R-based streaming and online-learning workflows.

# Statement of need

Data drift (concept drift) detection is a fundamental challenge in deployed machine learning systems and adaptive analytics [@kobylinska2023]. When the underlying data-generating process changes over time, model performance can deteriorate silently, leading to incorrect predictions and suboptimal decision-making. Early detection of such shifts enables timely interventions—such as model retraining, recalibration, or triggering alerts—thereby maintaining system reliability in production environments.

Established methods for drift detection include error-rate monitors (DDM [@gama2004], EDDM [@baena2006]), statistical tests over adaptive windows (HDDM-A and HDDM-W based on Hoeffding bounds [@frias2014]), sliding-window Kolmogorov–Smirnov tests (KSWIN [@raab2020]), cumulative-sum approaches (Page–Hinkley [@page1954]), and divergence measures (KL divergence [@kullback1951]). These detectors are well studied and widely deployed in Java (MOA [@bifet2010moa]) and Python (scikit-multiflow [@montiel2018]) ecosystems, yet R users lack a unified, lightweight toolkit that provides these algorithms with a consistent usage pattern.

datadriftR addresses this gap by providing reference implementations of canonical drift detectors with the following design goals:

1. **Consistent interface**: All detectors are R6 objects with the same method names (`add_element`, `reset`, `change_detected`, `warning_detected`), enabling users to switch between methods or compare results without rewriting code.
2. **Efficient online updates**: Sufficient statistics are maintained in constant time or amortized constant time per observation, supporting real-time monitoring.
3. **Minimal dependencies**: The package works with base R (≥ 3.5.2) and leverages R6 for clean object-oriented design; advanced methods (ProfileDifference) optionally use `fda.usc` and `doremi`.
4. **Reproducibility and transparency**: Implementations follow the canonical references, with links to corresponding scikit-multiflow sources provided in code documentation for verification and cross-checking.

By consolidating these methods in a single package with identical method names, datadriftR lowers the barrier to deploying drift detection in R-based production systems and supports reproducible research in streaming data analysis.

# Examples of Use

Below, we construct a single synthetic binary stream with an abrupt distributional shift at index 501 and demonstrate minimal usage for each detector. All examples use the same data for direct comparability. Each detector's implementation references the original algorithmic papers cited in the package documentation.

```r
library(datadriftR)
set.seed(123)
# Generate pre-drift and post-drift segments
pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.7, 0.3))
post <- sample(c(0,1), 500, replace = TRUE, prob = c(0.3, 0.7))
stream <- c(pre, post)

# 1) DDM (Drift Detection Method) – Gama et al. (2004)
#    Monitors error rate and signals drift when it exceeds control limits.
ddm <- DDM$new()
for (i in seq_along(stream)) {
  ddm$add_element(stream[i])
  if (ddm$change_detected) {
    message("DDM drift detected at index ", i)
    break
  }
}

# 2) EDDM (Early Drift Detection Method) – Baena-García et al. (2006)
#    Tracks distances between errors for earlier detection of gradual drift.
eddm <- EDDM$new()
for (i in seq_along(stream)) {
  eddm$add_element(stream[i])
  if (eddm$change_detected) {
    message("EDDM drift detected at index ", i)
    break
  }
}

# 3) HDDM-A (Hoeffding Drift Detection Method - Adaptive) – Frías-Blanco et al. (2015)
#    Uses Hoeffding bounds to detect mean shifts in adaptive windows.
hddm_a <- HDDM_A$new()
for (i in seq_along(stream)) {
  hddm_a$add_element(stream[i])
  if (hddm_a$change_detected) {
    message("HDDM-A drift detected at index ", i)
    break
  }
}

# 4) HDDM-W (Hoeffding Drift Detection Method - Weighted EWMA) – Frías-Blanco et al. (2015)
#    Applies exponentially weighted moving averages with Hoeffding bounds.
hddm_w <- HDDM_W$new()
for (i in seq_along(stream)) {
  hddm_w$add_element(stream[i])
  if (hddm_w$change_detected) {
    message("HDDM-W drift detected at index ", i)
    break
  }
}

# 5) KSWIN (Kolmogorov–Smirnov Windowing) – Raab et al. (2020)
#    Performs a two-sample KS test over sliding windows.
kswin <- KSWIN$new()
for (i in seq_along(stream)) {
  kswin$add_element(stream[i])
  if (kswin$change_detected) {
    message("KSWIN drift detected at index ", i)
    break
  }
}

# 6) Page–Hinkley – Page (1954)
#    Cumulative-sum test for detecting persistent shifts in the mean.
ph <- PageHinkley$new()
for (i in seq_along(stream)) {
  ph$add_element(stream[i])
  if (ph$detected_change()) {
    message("Page–Hinkley drift detected at index ", i)
    break
  }
}

# 7) KL Divergence monitor – Kullback & Leibler (1951)
#    Compares empirical distributions via histogram-based KL divergence.
kl <- KLDivergence$new(bins = 10, drift_level = 0.2)
kl$set_initial_distribution(pre)
kl$add_distribution(post[1:100])
if (kl$is_drift_detected()) {
  message("KL divergence drift detected; KL value: ", kl$get_kl_result())
}

# 8) ProfileDifference (functional derivative-based) – Kobyliń ska et al. (2023)
#    Compares profiles using gold or simple derivative methods (PDI, L2).
#    Note: ProfileDifference expects profile objects (x, y lists), not raw streams.
profile1 <- list(x = 1:500, y = cumsum(pre - mean(pre)))
profile2 <- list(x = 1:500, y = cumsum(post[1:500] - mean(post[1:500])))
pd <- ProfileDifference$new(method = "pdi", deriv = "gold")
pd$set_profiles(profile1, profile2)
result <- pd$calculate_difference()
if (result$distance > 0.5) {
  message("ProfileDifference drift detected; distance: ", result$distance)
}
```

Each detector's decision logic and parameter defaults are drawn directly from the corresponding reference implementation in scikit-multiflow (links provided in the package documentation) and the original algorithmic papers.

# References

