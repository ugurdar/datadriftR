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

Deployed machine learning models frequently encounter degradation in predictive accuracy when the statistical properties of incoming data evolve over time, a condition known as data drift. This phenomenon can manifest in several forms, most notably concept drift, which occurs when the functional relationship linking predictor variables to the outcome changes, thereby undermining model reliability. Conventional drift detection strategies often rely on aggregate performance indicators or univariate distributional summaries, approaches that may overlook nuanced yet consequential shifts in the data-generating mechanism. Within the broader machine learning operations (MLOps) framework, continuous model monitoring has emerged as a critical practice for safeguarding the stability and dependability of production systems [@biecek2019; @mougan2023]. datadriftR is an open-source R package designed to address these challenges by providing real-time detection of data drift in univariate streaming data. The package implements a comprehensive suite of widely recognized statistical methods for monitoring distributional changes, including error-rate-based detectors that track classification performance (DDM [@gama2004], EDDM [@baena2006]), Hoeffding-bound methods that employ adaptive windowing to detect mean shifts (HDDM-A and HDDM-W [@frias2014]), a sliding-window Kolmogorov–Smirnov test for distribution comparison (KSWIN [@raab2020]), the cumulative-sum-based Page–Hinkley test for detecting persistent shifts [@page1954], histogram-based Kullback–Leibler divergence monitoring for measuring distributional divergence [@kullback1951], and a functional profile comparison method for analyzing temporal patterns [@kobylinska2023]. 

# Statement of need

Data drift detection is a fundamental challenge in deployed machine learning systems and adaptive analytics [@kobylinska2023]. When the underlying data-generating process changes over time, model performance can deteriorate silently, leading to incorrect predictions and suboptimal decision-making. Early detection of such shifts enables timely interventions—such as model retraining, recalibration or triggering alerts—thereby maintaining system reliability in production environments.

The R ecosystem lacks a dedicated package for streaming drift detection despite widespread availability in Java (MOA [@bifet2010moa]) and Python (scikit-multiflow [@montiel2018]). While individual R packages address specific aspects of change-point detection or distribution testing, no existing toolkit consolidates canonical online detectors—DDM [@gama2004], EDDM [@baena2006], HDDM-A and HDDM-W [@frias2014], KSWIN [@raab2020], Page–Hinkley [@page1954], and KL divergence [@kullback1951]—under a unified framework for incremental analysis.

datadriftR fills this gap by implementing eight detectors as R6 classes sharing a common protocol (`add_element()`, `reset()`, `change_detected`, `warning_detected`). The package design emphasizes:

- **Single-observation updates**: Each detector processes one observation at a time, maintaining internal state (e.g., running mean and standard deviation in DDM, adaptive windows in HDDM-A) without requiring batch reprocessing.
- **Interchangeable algorithms**: Because all detectors expose the same methods, users can swap implementations (e.g., replace EDDM with KSWIN) by changing a single constructor call, facilitating comparative experiments.
- **Lightweight footprint**: Core detectors depend only on base R and R6; ProfileDifference optionally loads `fda.usc` and `doremi` for functional data analysis.

By unifying these methods, datadriftR enables reproducible monitoring workflows in R-based production pipelines and supports systematic benchmarking studies on streaming data.

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



# References

