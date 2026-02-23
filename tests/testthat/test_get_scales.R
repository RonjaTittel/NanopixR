
skip_if_not(is_integration_enabled(), "Integration tests disabled (set CELLPIXR_INTEGRATION=true).")

test_that("get_scales errors on invalid folder (checked before python)", {
  expect_error(get_scales(1), "single character", ignore.case = TRUE)
  expect_error(get_scales("definitely_not_existing"), "does not exist", ignore.case = TRUE)
})

test_that("get_scales returns empty list when no image files are present (requires python+ncempy)", {
  skip_if_not_installed("reticulate")
  skip_if_not_installed("withr")

  # get_scales checks python before listing files:
  skip_if_not(reticulate::py_available(initialize = FALSE), "Python not configured for reticulate.")
  skip_if_not(reticulate::py_module_available("ncempy"), "Python module 'ncempy' not available.")

  tmp <- withr::local_tempdir()
  # no image files
  res <- get_scales(tmp)

  expect_type(res, "list")
  expect_equal(length(res), 0)
})

test_that("get_scales normalizes names and returns no_dm3_found for missing dm3 (requires python+ncempy)", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_available(initialize = FALSE), "Python not configured for reticulate.")
  skip_if_not(reticulate::py_module_available("ncempy"), "Python module 'ncempy' not available.")

  tmp <- withr::local_tempdir()

  # Dummy image file (doesn't need to be a real image)
  file.create(file.path(tmp, "TeSt_Image.TIF"))

  res <- get_scales(tmp)

  expect_type(res, "list")
  expect_true("test_image" %in% names(res))  # normalized: lowercase, no extension

  one <- res[["test_image"]]
  expect_type(one, "list")

  # expected fields exist
  expect_setequal(
    names(one),
    c("scale_x", "scale_y", "unit_x", "unit_y", "mm_per_pixel_x", "mm_per_pixel_y", "source")
  )

  expect_true(is.na(one$scale_x))
  expect_true(is.na(one$unit_x))
  expect_equal(one$source, "no_dm3_found")
})

test_that("get_scales matches expected output for known inst/images set (requires python+ncempy)", {
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_available(initialize = FALSE), "Python not configured for reticulate.")
  skip_if_not(reticulate::py_module_available("ncempy"), "Python module 'ncempy' not available.")

  pkg_root <- testthat::test_path("..", "..")
  img_dir  <- file.path(pkg_root, "inst", "images")
  skip_if_not(dir.exists(img_dir), "inst/images not found.")

  # Run
  scales <- get_scales(img_dir)
  expect_type(scales, "list")

  # We only assert on the four images you showed (robust if inst/images contains more)
  expected_names <- c("beads2", "ch_097_5", "ch_600_10_no_scale_bar", "droplets_beads3")
  expect_true(all(expected_names %in% names(scales)))

  scales <- scales[expected_names]

  # Helper: check a "no_dm3_found" entry
  expect_no_dm3 <- function(x) {
    expect_setequal(names(x), c("scale_x","scale_y","unit_x","unit_y","mm_per_pixel_x","mm_per_pixel_y","source"))
    expect_true(is.na(x$scale_x))
    expect_true(is.na(x$scale_y))
    expect_true(is.na(x$unit_x))
    expect_true(is.na(x$unit_y))
    expect_true(is.na(x$mm_per_pixel_x))
    expect_true(is.na(x$mm_per_pixel_y))
    expect_equal(x$source, "no_dm3_found")
  }

  expect_no_dm3(scales$beads2)
  expect_no_dm3(scales$ch_600_10_no_scale_bar)
  expect_no_dm3(scales$droplets_beads3)

  # ch_097_5: the one with dm3 info
  x <- scales$ch_097_5
  expect_setequal(names(x), c("scale_x","scale_y","unit_x","unit_y","mm_per_pixel_x","mm_per_pixel_y","source"))

  expect_equal(round(x$scale_x, 7), 0.4198725)
  expect_equal(round(x$scale_y, 7), 0.4198725)
  expect_equal(x$unit_x, "nm")
  expect_equal(x$unit_y, "nm")

  # 0.4198725 nm * (1e-6 mm / nm) = 4.198725e-07 mm
  expect_equal(x$mm_per_pixel_x, x$scale_x * 1e-6, tolerance = 1e-14)
  expect_equal(x$mm_per_pixel_y, x$scale_y * 1e-6, tolerance = 1e-14)

  expect_equal(x$source, "dm3_pixelSize_pixelUnit")
})
