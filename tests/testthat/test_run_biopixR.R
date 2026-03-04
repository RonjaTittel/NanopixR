
# ── .bp_validate_detection_params: unit tests ─────────────────────────────────
test_that(".bp_validate_detection_params - returns normalized params for 'edge'", {
  res <- CellpixR:::.bp_validate_detection_params("edge")
  expect_equal(res$method, "edge")
  expect_equal(res$alpha, "gaussian")
  expect_equal(res$sigma, "gaussian")
  expect_null(res$threshold)
})

test_that(".bp_validate_detection_params - accepts numeric alpha and sigma for 'edge'", {
  res <- CellpixR:::.bp_validate_detection_params("edge", alpha = 1.5, sigma = 2.0)
  expect_equal(res$alpha, 1.5)
  expect_equal(res$sigma, 2.0)
})

test_that(".bp_validate_detection_params - throws error for invalid alpha", {
  expect_error(
    CellpixR:::.bp_validate_detection_params("edge", alpha = "invalid"),
    "'alpha' must be numeric or 'gaussian'"
  )
})

test_that(".bp_validate_detection_params - throws error for invalid sigma", {
  expect_error(
    CellpixR:::.bp_validate_detection_params("edge", sigma = "invalid"),
    "'sigma' must be numeric or 'gaussian'"
  )
})

test_that(".bp_validate_detection_params - returns normalized params for 'threshold'", {
  res <- CellpixR:::.bp_validate_detection_params("threshold")
  expect_equal(res$method, "threshold")
  expect_null(res$alpha)
  expect_null(res$sigma)
  expect_equal(res$threshold, "auto")
})

test_that(".bp_validate_detection_params - throws error for unknown method", {
  expect_error(
    CellpixR:::.bp_validate_detection_params("unknown"),
    "'arg'"
  )
})

# ── .bp_validate_size_filter: unit tests ──────────────────────────────────────
test_that(".bp_validate_size_filter - returns disabled filter when use = FALSE", {
  res <- CellpixR:::.bp_validate_size_filter(use = FALSE)
  expect_false(res$use)
  expect_null(res$lowerlimit)
  expect_null(res$upperlimit)
})

test_that(".bp_validate_size_filter - defaults to 'auto' when use = TRUE and limits are NULL", {
  res <- CellpixR:::.bp_validate_size_filter(use = TRUE)
  expect_true(res$use)
  expect_equal(res$lowerlimit, "auto")
  expect_equal(res$upperlimit, "auto")
})

test_that(".bp_validate_size_filter - accepts numeric limits", {
  res <- CellpixR:::.bp_validate_size_filter(use = TRUE, lowerlimit = 10, upperlimit = 500)
  expect_equal(res$lowerlimit, 10)
  expect_equal(res$upperlimit, 500)
})

test_that(".bp_validate_size_filter - throws error for invalid use argument", {
  expect_error(
    CellpixR:::.bp_validate_size_filter(use = "yes"),
    "'use_sizefilter' must be TRUE or FALSE."
  )
})

test_that(".bp_validate_size_filter - throws error for invalid lowerlimit", {
  expect_error(
    CellpixR:::.bp_validate_size_filter(use = TRUE, lowerlimit = "big"),
    "'size_lowerlimit' must be numeric or 'auto'"
  )
})

test_that(".bp_validate_size_filter - throws error for invalid upperlimit", {
  expect_error(
    CellpixR:::.bp_validate_size_filter(use = TRUE, upperlimit = "big"),
    "'size_upperlimit' must be numeric or 'auto'"
  )
})

# ── .bp_validate_proximity_filter: unit tests ─────────────────────────────────
test_that(".bp_validate_proximity_filter - returns disabled filter when use = FALSE", {
  res <- CellpixR:::.bp_validate_proximity_filter(use = FALSE)
  expect_false(res$use)
  expect_null(res$radius)
  expect_null(res$elongation)
})

test_that(".bp_validate_proximity_filter - defaults to 'auto' radius and 2 elongation", {
  res <- CellpixR:::.bp_validate_proximity_filter(use = TRUE)
  expect_true(res$use)
  expect_equal(res$radius, "auto")
  expect_equal(res$elongation, 2)
})

test_that(".bp_validate_proximity_filter - accepts numeric radius", {
  res <- CellpixR:::.bp_validate_proximity_filter(use = TRUE, radius = 5, elongation = 3)
  expect_equal(res$radius, 5)
  expect_equal(res$elongation, 3)
})

test_that(".bp_validate_proximity_filter - throws error for invalid use argument", {
  expect_error(
    CellpixR:::.bp_validate_proximity_filter(use = "yes"),
    "'use_proxfilter' must be TRUE or FALSE."
  )
})

test_that(".bp_validate_proximity_filter - throws error for invalid radius", {
  expect_error(
    CellpixR:::.bp_validate_proximity_filter(use = TRUE, radius = "big"),
    "'prox_radius' must be numeric or 'auto'"
  )
})

test_that(".bp_validate_proximity_filter - throws error for non-numeric elongation", {
  expect_error(
    CellpixR:::.bp_validate_proximity_filter(use = TRUE, elongation = "long"),
    "'prox_elongation' must be a single numeric value"
  )
})

# ── .bp_prepare_image_table: unit tests ───────────────────────────────────────
test_that(".bp_prepare_image_table - stops if no image files match pattern", {
  tmp <- file.path(tempdir(), "bp_empty_pattern")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE))
  writeLines("dummy", file.path(tmp, "dummy.txt"))

  expect_error(
    CellpixR:::.bp_prepare_image_table(tmp,
                                       pattern = "\\.(png|jpg|jpeg|tif|tiff|bmp)$"),
    "No valid image files found|No files match"
  )
})

test_that(".bp_prepare_image_table - stops if selected_files not found", {
  img_dir <- system.file("images", package = "CellpixR")

  expect_error(
    CellpixR:::.bp_prepare_image_table(img_dir,
                                       pattern = "\\.(png|jpg|jpeg|tif|tiff|bmp)$",
                                       selected_files = "nonexistent_image.png"),
    "None of the specified files were found"
  )
})

test_that(".bp_prepare_image_table - filters to selected_files correctly", {
  img_dir <- system.file("images", package = "CellpixR")

  res <- CellpixR:::.bp_prepare_image_table(img_dir,
                                            pattern = "\\.(png|jpg|jpeg|tif|tiff|bmp)$",
                                            selected_files = "beads2.png")
  expect_equal(nrow(res), 1L)
  expect_equal(res$norm, "beads2")
})

# ── run_biopixR: input validation (no image processing needed) ────────────────
test_that("run_biopixR - throws error for invalid folder type", {
  expect_error(run_biopixR(123),
               "'folder' must be a single character string.")
})

test_that("run_biopixR - throws error for non-existent folder", {
  expect_error(run_biopixR("/this/path/does/not/exist"),
               "The specified folder does not exist")
})

test_that("run_biopixR - throws error if selected_files not found", {
  img_dir <- system.file("images", package = "CellpixR")
  expect_error(
    run_biopixR(img_dir,
                selected_files = "nonexistent.png",
                write_results = FALSE),
    "None of the specified files were found"
  )
})

# ── Shared fixtures: run once, reuse across all integration tests ──────────────
# run_biopixR is slow (~5 min per call) -> execute once and share the result

img_dir <- system.file("images", package = "CellpixR")

beads_dir <- local({
  tmp <- file.path(tempdir(), "bp_beads_test")
  dir.create(tmp, showWarnings = FALSE)
  file.copy(file.path(img_dir, "beads2.png"), tmp)
  tmp
})
withr::defer(unlink(beads_dir, recursive = TRUE), teardown_env())

# single shared result for all output structure and parameter tests
res_edge <- suppressWarnings(suppressMessages(
  run_biopixR(beads_dir,
              method = "edge",
              alpha = "gaussian",
              sigma = "gaussian",
              write_results = FALSE)
))

# second shared result for threshold method tests
res_threshold <- suppressWarnings(suppressMessages(
  run_biopixR(beads_dir,
              method = "threshold",
              write_results = FALSE)
))

# ── run_biopixR: output structure (reusing res_edge) ──────────────────────────
test_that("run_biopixR - returns a list with pixel, converted and parameters", {
  expect_true(is.list(res_edge))
  expect_true(all(c("pixel", "converted", "parameters") %in% names(res_edge)))
})

test_that("run_biopixR - pixel result is a named list with one entry per image", {
  expect_true(is.list(res_edge$pixel))
  expect_equal(length(res_edge$pixel), 1L)
  expect_true("beads2" %in% names(res_edge$pixel))
})

test_that("run_biopixR - pixel data frame contains all expected columns", {
  df <- res_edge$pixel[["beads2"]]
  expected_cols <- c("ROI_ID", "Centroid_X", "Centroid_Y", "Area",
                     "Mean_Intensity", "Perimeter", "Circularity",
                     "Min_Diameter", "Max_Diameter", "Image_name",
                     "Total_ROIs", "Overall_Confidence", "Analysis_method")
  expect_true(all(expected_cols %in% names(df)))
})

test_that("run_biopixR - Image_name column matches normalized image name", {
  df <- res_edge$pixel[["beads2"]]
  expect_true(all(df$Image_name == "beads2"))
})

test_that("run_biopixR - converted is NULL when conversion = FALSE", {
  expect_null(res_edge$converted)
})

# ── run_biopixR: parameters (reusing res_edge and res_threshold) ───────────────
test_that("run_biopixR - parameters list contains all expected fields", {
  expect_true(all(c("folder", "method", "alpha", "sigma",
                    "size_filter", "proximity_filter") %in% names(res_edge$parameters)))
})

test_that("run_biopixR - parameters reflect method = 'edge' with gaussian defaults", {
  expect_equal(res_edge$parameters$method, "edge")
  expect_equal(res_edge$parameters$alpha, "gaussian")
  expect_equal(res_edge$parameters$sigma, "gaussian")
})

test_that("run_biopixR - parameters reflect method = 'threshold'", {
  expect_equal(res_threshold$parameters$method, "threshold")
  expect_null(res_threshold$parameters$alpha)
  expect_null(res_threshold$parameters$sigma)
})

test_that("run_biopixR - size_filter is disabled by default", {
  expect_false(res_edge$parameters$size_filter$use)
})

test_that("run_biopixR - proximity_filter is disabled by default", {
  expect_false(res_edge$parameters$proximity_filter$use)
})

# ── run_biopixR: write_results (own temp folder, runs once) ───────────────────
test_that("run_biopixR - write_results = TRUE creates pixel CSV, FALSE does not", {
  # run once with write_results = TRUE
  tmp_write <- file.path(tempdir(), "bp_csv_test")
  dir.create(tmp_write, showWarnings = FALSE)
  on.exit(unlink(tmp_write, recursive = TRUE))
  file.copy(file.path(img_dir, "beads2.png"), tmp_write)

  run_biopixR(tmp_write, write_results = TRUE) |> suppressWarnings() |> suppressMessages()
  expect_true(file.exists(file.path(tmp_write, "Results",
                                    "Results_biopixR_pixel.csv")))

  # run once with write_results = FALSE
  tmp_no_write <- file.path(tempdir(), "bp_no_csv_test")
  dir.create(tmp_no_write, showWarnings = FALSE)
  on.exit(unlink(tmp_no_write, recursive = TRUE), add = TRUE)
  file.copy(file.path(img_dir, "beads2.png"), tmp_no_write)

  run_biopixR(tmp_no_write, write_results = FALSE) |> suppressWarnings() |> suppressMessages()
  expect_false(file.exists(file.path(tmp_no_write, "Results",
                                     "Results_biopixR_pixel.csv")))
})

# ── run_biopixR: selected_files (reusing img_dir, runs once) ──────────────────
test_that("run_biopixR - selected_files restricts processing to specified image", {
  res_sel <- suppressWarnings(suppressMessages(
    run_biopixR(img_dir,
                selected_files = "beads2.png",
                write_results = FALSE)
  ))
  expect_equal(length(res_sel$pixel), 1L)
  expect_true("beads2" %in% names(res_sel$pixel))
})
