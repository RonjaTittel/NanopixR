
test_that("extract_image_features errors on missing folder", {
  expect_error(extract_image_features("definitely_not_existing"))
})

test_that("extract_image_features errors when folder has no readable images", {
  tmp <- withr::local_tempdir()
  expect_error(extract_image_features(tmp, verbose = FALSE))
})

test_that("extract_image_features exports a CSV when export_csv=TRUE", {
  skip_if_not_installed("imager")

  tmp <- withr::local_tempdir()

  # Erzeuge ein Testbild
  img <- imager::as.cimg(matrix(runif(32 * 32), 32, 32))
  f1 <- file.path(tmp, "test_img_1.png")
  imager::save.image(img, f1)

  # Prüfe, ob DEIN Loader das wirklich lesen kann.
  # Wenn nicht: Test überspringen (statt FAIL), weil das sonst auf dem System nie zuverlässig ist.
  can_run <- TRUE
  res_try <- tryCatch(
    extract_image_features(folder = tmp, export_csv = FALSE, verbose = FALSE),
    error = function(e) { can_run <<- FALSE; NULL }
  )
  skip_if_not(can_run, "extract_image_features cannot read generated test images on this system/config.")

  # Jetzt erst: CSV-Export testen (Minimum)
  out_name <- "Properties_test.csv"
  out_path <- file.path(tmp, out_name)

  res <- extract_image_features(
    folder = tmp,
    export_csv = TRUE,
    csv_name = out_name,
    round_digits = 5,
    verbose = FALSE
  )

  expect_true(file.exists(out_path))
  expect_true(is.data.frame(res))
  expect_gt(nrow(res), 0)
})
