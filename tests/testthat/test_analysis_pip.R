library(CellpixR)
library(testthat)


test_that("analysis_pip() errors on missing folder", {
  expect_error(analysis_pip(folder = "Not_existing"), "exist|found")
})


test_that("analysis_pip() runs and returns valid results run with GPU", {
  skip_on_cran()
  skip_if_not(tryCatch({
    reticulate::py_run_string("import torch; torch.cuda.is_available()")
    TRUE
  },
  error = function(e) FALSE), "No GPU available")

  res <- analysis_pip(
    folder = system.file("images", package = "CellpixR"),
    write_csv = TRUE,
    scale_info = TRUE,
    interactive = FALSE,
    gpu = TRUE,
    method_bp = "edge")

  expect_type(res,
               "list")

  expect_named(res,
               c("by_image", "converted", "pixel"),
               ignore.order = TRUE)

  expect_s3_class(res$converted, "data.frame")
  expect_s3_class(res$pixel, "data.frame")

  expect_gt(nrow(res$pixel), 0)
  expect_gt(nrow(res$converted), 0)

  expect_true(all(res$pixel$ares > 0))
  expect_true(all(res$pixel$mean_intensity >= 0))
})
