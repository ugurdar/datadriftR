# Package index

## High-Level Interface

Simple function for quick drift detection

- [`detect_drift()`](https://ugurdar.github.io/datadriftR/reference/detect_drift.md)
  : Detect Data Drift in Streaming Data

## Error-Rate Based Detectors

Detectors that monitor error rates

- [`DDM`](https://ugurdar.github.io/datadriftR/reference/DDM.md) : DDM
  (Drift Detection Method)
- [`EDDM`](https://ugurdar.github.io/datadriftR/reference/EDDM.md) :
  EDDM (Early Drift Detection Method)

## Hoeffding Bound Methods

Detectors using Hoeffding’s inequality

- [`HDDM_A`](https://ugurdar.github.io/datadriftR/reference/HDDM_A.md) :
  HDDM_A: Drift Detection Method based on Adaptive Windows
- [`HDDM_W`](https://ugurdar.github.io/datadriftR/reference/HDDM_W.md) :
  KSWIN (Kolmogorov-Smirnov WINdowing) for Change Detection

## Distribution-Based Detectors

Statistical test-based methods

- [`KSWIN`](https://ugurdar.github.io/datadriftR/reference/KSWIN.md) :
  KSWIN (Kolmogorov-Smirnov WINdowing) for Change Detection
- [`KLDivergence`](https://ugurdar.github.io/datadriftR/reference/KLDivergence.md)
  : Kullback-Leibler Divergence (KLD) for Change Detection
- [`ADWIN`](https://ugurdar.github.io/datadriftR/reference/ADWIN.md) :
  ADWIN (ADaptive WINdowing) Drift Detector

## Sequential Analysis Methods

Cumulative statistics methods

- [`PageHinkley`](https://ugurdar.github.io/datadriftR/reference/PageHinkley.md)
  : Page-Hinkley Test for Change Detection

## Profile-Based Methods

Functional data analysis methods

- [`ProfileDifference`](https://ugurdar.github.io/datadriftR/reference/ProfileDifference.md)
  : Profile Difference Calculation for Change Detection
