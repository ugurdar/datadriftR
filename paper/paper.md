---
title: "datadriftR: An R package for data drift detection"
authors:
- affiliation: 1
  name: Ugur Dar
  orcid: "0009-0005-8076-2199"
- affiliation: 1
  name: Mustafa Cavus
  orcid: "0000-0002-6172-5449"
date: "17 November 2025"
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

datadriftR brings these methods together in a single R package, offering drift detectors that can be updated observation by observation and used with minimal dependencies. This makes it straightforward to incorporate drift monitoring into streaming workflows and to compare alternative detectors on the same data.

# Examples of Use

To illustrate the package's unified interface, we generate a synthetic binary stream with an abrupt distributional shift at index 501 and demonstrate minimal usage for representative detectors. All examples process the same stream to enable direct comparison.

```r
library(datadriftR)
set.seed(123)
# Generate pre-drift and post-drift segments
pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.7, 0.3))
post <- sample(c(0,1), 500, replace = TRUE, prob = c(0.3, 0.7))
stream <- c(pre, post)

# 1) DDM (Drift Detection Method) 
ddm <- DDM$new()
for (i in seq_along(stream)) {
  ddm$add_element(stream[i])
  if (ddm$change_detected) {
    message("DDM drift detected at index ", i)
    break
  }
}

# 2) Page–Hinkley 
ph <- PageHinkley$new()
for (i in seq_along(stream)) {
  ph$add_element(stream[i])
  if (ph$detected_change()) {
    message("Page–Hinkley drift detected at index ", i)
    break
  }
}

```

The package also includes HDDM-A, HDDM-W, KL-divergence histogram, and ProfileDifference detectors. Each follows the same instantiate–update–check pattern. For complete examples, comparison of detection methods, and detailed usage demonstrations, see the package vignette (`vignette("datadriftR-intro")`), the README, and the individual detector documentation pages available via CRAN.

# References



