# Detect Data Drift in Streaming Data

A user-friendly wrapper function that detects data drift in streaming
data without requiring explicit for-loops. This function processes the
entire data stream and returns all detected drift points (and optionally
warning points) as a dataframe.

## Usage

``` r
detect_drift(
  stream,
  method = c("ddm", "eddm", "page_hinkley", "hddm_a", "hddm_w", "kswin", "adwin"),
  include_warnings = TRUE,
  ...
)
```

## Arguments

- stream:

  Numeric vector representing the data stream to monitor.

- method:

  Character string specifying the drift detection method to use. Options
  are: "ddm", "eddm", "page_hinkley", "hddm_a", "hddm_w", "kswin".
  Default is "ddm".

- include_warnings:

  Logical indicating whether to include warning detections in the
  results (applicable for DDM, EDDM, HDDM_A, and HDDM_W). Default is
  TRUE.

- ...:

  Additional parameters to pass to the specific detector constructor.
  See individual detector documentation for available parameters.

## Value

A data.frame with the following columns:

- index:

  Integer index in the stream where detection occurred

- value:

  Numeric value at that index

- type:

  Character indicating "drift" or "warning"

If no drift or warnings are detected, returns an empty data.frame with
the same structure.

## Details

This function provides a simplified interface to all streaming drift
detectors in the datadriftR package. Instead of manually iterating
through the stream and checking for drift at each point, users can
simply pass the entire stream to this function and receive a dataframe
with all drift detection results.

The function supports the following drift detection methods:

- `ddm`: Drift Detection Method

- `eddm`: Early Drift Detection Method

- `page_hinkley`: Page-Hinkley Test

- `hddm_a`: HDDM with Adaptive Windows

- `hddm_w`: HDDM with Weighted Windows

- `kswin`: Kolmogorov-Smirnov Windowing

- `adwin`: ADaptive WINdowing

## Examples

``` r
library(datadriftR)
set.seed(123)

# Generate synthetic stream with drift at index 501
pre  <- sample(c(0,1), 500, replace = TRUE, prob = c(0.7, 0.3))
post <- sample(c(0,1), 500, replace = TRUE, prob = c(0.3, 0.7))
stream <- c(pre, post)

# Detect drift using DDM
results <- detect_drift(stream, method = "ddm")
print(results)
#>     index value    type
#> 1     514     1 warning
#> 2     516     1 warning
#> 3     517     1 warning
#> 4     518     0 warning
#> 5     519     1 warning
#> 6     520     0 warning
#> 7     521     1 warning
#> 8     522     1 warning
#> 9     523     1 warning
#> 10    524     1 warning
#> 11    525     1 warning
#> 12    526     1 warning
#> 13    527     0 warning
#> 14    528     1 warning
#> 15    529     0 warning
#> 16    530     1 warning
#> 17    531     0 warning
#> 18    532     1 warning
#> 19    533     1 warning
#> 20    534     0 warning
#> 21    535     1 warning
#> 22    536     0 warning
#> 23    537     1 warning
#> 24    538     0 warning
#> 25    539     1 warning
#> 26    540     1 warning
#> 27    541     0 warning
#> 28    542     1 warning
#> 29    543     0 warning
#> 30    544     1 warning
#> 31    545     0 warning
#> 32    546     0 warning
#> 33    547     1 warning
#> 34    548     0 warning
#> 35    549     0 warning
#> 36    550     1 warning
#> 37    551     1 warning
#> 38    552     1 warning
#> 39    553     1 warning
#> 40    554     0 warning
#> 41    555     1 warning
#> 42    556     1 warning
#> 43    557     1 warning
#> 44    558     1 warning
#> 45    559     1 warning
#> 46    560     1   drift
#> 47    700     1 warning
#> 48    703     1 warning
#> 49    706     1 warning
#> 50    707     1 warning
#> 51    708     1 warning
#> 52    709     1 warning
#> 53    710     1 warning
#> 54    711     1 warning
#> 55    712     1 warning
#> 56    713     1 warning
#> 57    714     0 warning
#> 58    715     1 warning
#> 59    716     1 warning
#> 60    717     1 warning
#> 61    718     1 warning
#> 62    719     0 warning
#> 63    720     1 warning
#> 64    721     1 warning
#> 65    722     0 warning
#> 66    723     0 warning
#> 67    724     0 warning
#> 68    725     1 warning
#> 69    726     0 warning
#> 70    727     0 warning
#> 71    728     1 warning
#> 72    730     1 warning
#> 73    731     1 warning
#> 74    733     1 warning
#> 75    736     1 warning
#> 76    745     1 warning
#> 77    746     1 warning
#> 78    748     1 warning
#> 79    749     1 warning
#> 80    751     1 warning
#> 81    752     1 warning
#> 82    753     1 warning
#> 83    754     1 warning
#> 84    755     1 warning
#> 85    756     1 warning
#> 86    757     1 warning
#> 87    758     0 warning
#> 88    759     1 warning
#> 89    760     1 warning
#> 90    761     1 warning
#> 91    762     1 warning
#> 92    763     1 warning
#> 93    764     1 warning
#> 94    765     1 warning
#> 95    766     1 warning
#> 96    767     1 warning
#> 97    768     1 warning
#> 98    769     1 warning
#> 99    770     1 warning
#> 100   771     0 warning
#> 101   772     0 warning
#> 102   773     1 warning
#> 103   774     0 warning
#> 104   775     1 warning
#> 105   776     1 warning
#> 106   777     1 warning
#> 107   778     1 warning
#> 108   779     1 warning
#> 109   780     0 warning
#> 110   781     1 warning
#> 111   782     0 warning
#> 112   783     1 warning
#> 113   784     1 warning
#> 114   785     0 warning
#> 115   786     1 warning
#> 116   787     1 warning
#> 117   788     1 warning
#> 118   789     1 warning
#> 119   790     0 warning
#> 120   791     0 warning
#> 121   792     1 warning
#> 122   793     1 warning
#> 123   794     1 warning
#> 124   795     0 warning
#> 125   796     1 warning
#> 126   797     1 warning
#> 127   798     1 warning
#> 128   799     1 warning
#> 129   800     1 warning
#> 130   801     1 warning
#> 131   802     1 warning
#> 132   803     1 warning
#> 133   804     1 warning
#> 134   805     1 warning
#> 135   806     0 warning
#> 136   807     1 warning
#> 137   808     1 warning
#> 138   809     0 warning
#> 139   810     1 warning
#> 140   811     1 warning
#> 141   812     1 warning
#> 142   813     1 warning
#> 143   814     0 warning
#> 144   815     0 warning
#> 145   816     1 warning
#> 146   817     1 warning
#> 147   818     1 warning
#> 148   819     1 warning
#> 149   820     1 warning
#> 150   821     1 warning
#> 151   822     1 warning
#> 152   823     1 warning
#> 153   824     0 warning
#> 154   825     0 warning
#> 155   826     1 warning
#> 156   827     1 warning
#> 157   828     1 warning
#> 158   829     1 warning
#> 159   830     0 warning
#> 160   831     1 warning
#> 161   832     0 warning
#> 162   833     1 warning
#> 163   834     0 warning
#> 164   835     1 warning
#> 165   836     1 warning
#> 166   837     0 warning
#> 167   838     1 warning
#> 168   839     1 warning
#> 169   840     1 warning
#> 170   841     0 warning
#> 171   842     0 warning
#> 172   843     1 warning
#> 173   844     1 warning
#> 174   845     1 warning
#> 175   846     1 warning
#> 176   847     0 warning
#> 177   848     1 warning
#> 178   849     1 warning
#> 179   850     1 warning
#> 180   851     1 warning
#> 181   852     0 warning
#> 182   853     0 warning
#> 183   854     0 warning
#> 184   855     1 warning
#> 185   856     1 warning
#> 186   857     0 warning
#> 187   858     1 warning
#> 188   859     1 warning
#> 189   860     0 warning
#> 190   861     1 warning
#> 191   862     1 warning
#> 192   863     1 warning
#> 193   864     1 warning
#> 194   865     1 warning
#> 195   866     1 warning
#> 196   867     1 warning
#> 197   868     1 warning
#> 198   869     1 warning
#> 199   870     1 warning
#> 200   871     1 warning
#> 201   872     1 warning
#> 202   873     0 warning
#> 203   874     1 warning
#> 204   875     0 warning
#> 205   876     1 warning
#> 206   877     1 warning
#> 207   878     1 warning
#> 208   879     0 warning
#> 209   880     1 warning
#> 210   881     1 warning
#> 211   882     0 warning
#> 212   883     1 warning
#> 213   884     1 warning
#> 214   885     0 warning
#> 215   886     0 warning
#> 216   887     1 warning
#> 217   888     1 warning
#> 218   889     1 warning
#> 219   890     1 warning
#> 220   891     1 warning
#> 221   892     0 warning
#> 222   893     1 warning
#> 223   894     1 warning
#> 224   895     0 warning
#> 225   896     0 warning
#> 226   897     1 warning
#> 227   898     1 warning
#> 228   899     1 warning
#> 229   900     0 warning
#> 230   901     0 warning
#> 231   902     1 warning
#> 232   903     0 warning
#> 233   904     1 warning
#> 234   905     1 warning
#> 235   906     1 warning
#> 236   907     0 warning
#> 237   908     0 warning
#> 238   909     0 warning
#> 239   910     1 warning
#> 240   911     1 warning
#> 241   912     1 warning
#> 242   913     0 warning
#> 243   914     1 warning
#> 244   915     1 warning
#> 245   916     0 warning
#> 246   917     1 warning
#> 247   918     1 warning
#> 248   919     1 warning
#> 249   920     1 warning
#> 250   921     0 warning
#> 251   922     1 warning
#> 252   923     1 warning
#> 253   924     0 warning
#> 254   925     1 warning
#> 255   926     1 warning
#> 256   927     1 warning
#> 257   928     1 warning
#> 258   929     1 warning
#> 259   930     0 warning
#> 260   931     0 warning
#> 261   932     1 warning
#> 262   933     0 warning
#> 263   934     1 warning
#> 264   935     0 warning
#> 265   936     1 warning
#> 266   937     1 warning
#> 267   938     0 warning
#> 268   939     0 warning
#> 269   940     1 warning
#> 270   941     0 warning
#> 271   942     1 warning
#> 272   943     1 warning
#> 273   944     1 warning
#> 274   945     1 warning
#> 275   946     1 warning
#> 276   947     1 warning
#> 277   948     1 warning
#> 278   949     1 warning
#> 279   950     1 warning
#> 280   951     1 warning
#> 281   952     1 warning
#> 282   953     0 warning
#> 283   954     1 warning
#> 284   955     1 warning
#> 285   956     0 warning
#> 286   957     1 warning
#> 287   958     1 warning
#> 288   959     1 warning
#> 289   960     1 warning
#> 290   961     1 warning
#> 291   962     1 warning
#> 292   963     1 warning
#> 293   964     1 warning
#> 294   965     0 warning
#> 295   966     1 warning
#> 296   967     1 warning
#> 297   968     1 warning
#> 298   969     0 warning
#> 299   970     1 warning
#> 300   971     0 warning
#> 301   972     1 warning
#> 302   973     0 warning
#> 303   974     1 warning
#> 304   975     1 warning
#> 305   976     1 warning
#> 306   977     0 warning
#> 307   978     1 warning
#> 308   979     1 warning
#> 309   980     1 warning
#> 310   981     1 warning
#> 311   982     0 warning
#> 312   983     0 warning
#> 313   984     1 warning
#> 314   985     0 warning
#> 315   986     0 warning
#> 316   987     1 warning
#> 317   988     1 warning
#> 318   989     1 warning
#> 319   990     1 warning
#> 320   991     1 warning
#> 321   992     1 warning
#> 322   993     1 warning
#> 323   994     1 warning
#> 324   995     1 warning
#> 325   996     0 warning
#> 326   997     1 warning
#> 327   998     1 warning
#> 328   999     0 warning
#> 329  1000     1 warning

# Detect drift using Page-Hinkley with custom parameters
results <- detect_drift(stream, method = "page_hinkley",
                        delta = 0.005, threshold = 50)
print(results)
#>   index value  type
#> 1   650     1 drift

# Detect drift using KSWIN
results <- detect_drift(stream, method = "kswin")
print(results)
#>   index value  type
#> 1   516     1 drift
#> 2   715     1 drift

# Get only drift detections, exclude warnings
results <- detect_drift(stream, method = "ddm", include_warnings = FALSE)
print(results)
#>   index value  type
#> 1   560     1 drift
```
