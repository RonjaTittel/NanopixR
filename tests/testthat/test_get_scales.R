
# ── .normalize_name: unit tests ───────────────────────────────────────────────
test_that(".normalize_name - returns lowercase filename without extension", {
  expect_equal(CellpixR:::.normalize_name("beads2.png"), "beads2")
  expect_equal(CellpixR:::.normalize_name("ch_097_5.tif"), "ch_097_5")
})

test_that(".normalize_name - converts uppercase to lowercase", {
  expect_equal(CellpixR:::.normalize_name("Image_A.PNG"), "image_a")
})

test_that(".normalize_name - strips directory path and keeps filename only", {
  expect_equal(CellpixR:::.normalize_name("/some/path/to/beads2.png"), "beads2")
})

test_that(".normalize_name - throws error for non-character input", {
  expect_error(CellpixR:::.normalize_name(123),
               "path_or_name must be a character vector")
  expect_error(CellpixR:::.normalize_name(NULL),
               "path_or_name must be a character vector")
})

# ── .unit_scale_factor: unit tests ────────────────────────────────────────────
test_that(".unit_scale_factor - nm returns 1e-6", {
  expect_equal(CellpixR:::.unit_scale_factor("nm"), 1e-6)
  expect_equal(CellpixR:::.unit_scale_factor("NM"), 1e-6) # case-insensitive
})

test_that(".unit_scale_factor - um returns 1e-3", {
  expect_equal(CellpixR:::.unit_scale_factor("um"), 1e-3)
  expect_equal(CellpixR:::.unit_scale_factor("\u00b5m"), 1e-3)
})

test_that(".unit_scale_factor - mm returns 1", {
  expect_equal(CellpixR:::.unit_scale_factor("mm"), 1)
  expect_equal(CellpixR:::.unit_scale_factor("MM"), 1)
})

test_that(".unit_scale_factor - unsupported unit returns NA", {
  expect_true(is.na(CellpixR:::.unit_scale_factor("px")))
  expect_true(is.na(CellpixR:::.unit_scale_factor("cm")))
})

test_that(".unit_scale_factor - NA input returns NA", {
  expect_true(is.na(CellpixR:::.unit_scale_factor(NA)))
})

test_that(".unit_scale_factor - empty string returns NA", {
  expect_true(is.na(CellpixR:::.unit_scale_factor("")))
})

# ── get_scales: input validation (no Python required) ─────────────────────────
test_that("get_scales - input validation: folder must be a character string", {
  expect_error(get_scales(123),
               "'folder' must be a single character string.")
  expect_error(get_scales(c("path1", "path2")),
               "'folder' must be a single character string.")
  expect_error(get_scales(NULL),
               "'folder' must be a single character string.")
})

test_that("get_scales - input validation: non-existent folder", {
  expect_error(get_scales("/this/path/does/not/exist"),
               "The specified folder does not exist")
})

# ── get_scales: integration tests (require Python + ncempy) ───────────────────
test_that("get_scales - returns empty list if no image files are found", {
  skip_if_not(reticulate::py_available(initialize = FALSE),
              "Python not available")
  skip_if_not(reticulate::py_module_available("ncempy"),
              "ncempy not available")

  tmp <- file.path(tempdir(), "no_images_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE))
  # place a non-image file to ensure folder is not empty
  writeLines("dummy", file.path(tmp, "dummy.txt"))

  result <- get_scales(tmp)
  expect_equal(result, list())
})

test_that("get_scales - missing DM3 file sets source to 'no_dm3_found' and all values to NA", {
  skip_if_not(reticulate::py_available(initialize = FALSE),
              "Python not available")
  skip_if_not(reticulate::py_module_available("ncempy"),
              "ncempy not available")

  tmp <- file.path(tempdir(), "no_dm3_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE))

  img_dir <- system.file("images", package = "CellpixR")
  file.copy(file.path(img_dir, "beads2.png"), tmp)

  result <- get_scales(tmp)

  expect_true("beads2" %in% names(result))
  entry <- result[["beads2"]]

  expect_equal(entry$source, "no_dm3_found")
  expect_true(is.na(entry$scale_x))
  expect_true(is.na(entry$scale_y))
  expect_true(is.na(entry$unit_x))
  expect_true(is.na(entry$unit_y))
  expect_true(is.na(entry$mm_per_pixel_x))
  expect_true(is.na(entry$mm_per_pixel_y))
})

test_that("get_scales - each result entry contains all 7 expected fields", {
  skip_if_not(reticulate::py_available(initialize = FALSE),
              "Python not available")
  skip_if_not(reticulate::py_module_available("ncempy"),
              "ncempy not available")

  tmp <- file.path(tempdir(), "fields_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE))

  img_dir <- system.file("images", package = "CellpixR")
  file.copy(file.path(img_dir, "beads2.png"), tmp)

  result <- get_scales(tmp)
  entry <- result[["beads2"]]

  expected_fields <- c("scale_x", "scale_y", "unit_x", "unit_y",
                       "mm_per_pixel_x", "mm_per_pixel_y", "source")
  expect_true(all(expected_fields %in% names(entry)))
})

test_that("get_scales - result is a named list with one entry per image", {
  skip_if_not(reticulate::py_available(initialize = FALSE),
              "Python not available")
  skip_if_not(reticulate::py_module_available("ncempy"),
              "ncempy not available")

  tmp <- file.path(tempdir(), "named_list_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE))

  img_dir <- system.file("images", package = "CellpixR")
  file.copy(file.path(img_dir, "beads2.png"), tmp)

  result <- get_scales(tmp)

  expect_true(is.list(result))
  expect_equal(length(result), 1L)
  expect_equal(names(result), "beads2")
})

test_that("get_scales - list entry names are normalized (lowercase, no extension)", {
  skip_if_not(reticulate::py_available(initialize = FALSE),
              "Python not available")
  skip_if_not(reticulate::py_module_available("ncempy"),
              "ncempy not available")

  tmp <- file.path(tempdir(), "norm_names_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE))

  img_dir <- system.file("images", package = "CellpixR")
  file.copy(file.path(img_dir, "beads2.png"), tmp)

  result <- get_scales(tmp)

  # names must be lowercase and without file extension
  expect_true(all(!grepl("\\.", names(result))))
  expect_equal(names(result), tolower(names(result)))
})
