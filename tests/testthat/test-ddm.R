testthat::test_that("DDM R6 class basic behavior", {
  testthat::skip_if_not_installed("datadriftR")
  # yükle
  library(datadriftR)
  # Constructor çalışmalı
  ddm <- tryCatch(DDM$new(), error = function(e) NULL)
  testthat::expect_true(!is.null(ddm), info = "DDM$new() should construct without error")
  if (!is.null(ddm)) {
    # add_element() hata vermemeli
    ok <- tryCatch({ ddm$add_element(0); TRUE }, error = function(e) FALSE)
    testthat::expect_true(ok, info = "DDM$add_element should accept a value without error")
    # change_detected alanına erişim sağlanabilmeli (mantıksal veya NULL)
    field_ok <- tryCatch({ val <- ddm$change_detected; TRUE }, error = function(e) FALSE)
    testthat::expect_true(field_ok, info = "DDM should expose a change_detected field or active binding")
  }
})
