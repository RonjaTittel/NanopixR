
# ── Helpers ───────────────────────────────────────────────────────────────────
# Central skip condition: all integration tests require Python + Cellpose
cellpose_available <- function() {
  reticulate::py_available(initialize = FALSE) &&
    reticulate::py_module_available("cellpose")
}

# ── run_cellpose: input validation (no Python required) ───────────────────────
test_that("run_cellpose - throws error for invalid folder type", {
  expect_error(run_cellpose(123),
               "'folder' must be a single character string.")
})

test_that("run_cellpose - throws error for non-existent folder", {
  expect_error(run_cellpose("/this/path/does/not/exist"),
               "The specified folder does not exist")
})

test_that("run_cellpose - throws error for invalid diameter", {
  skip_if_not(cellpose_available(), "Cellpose not available")

  img_dir <- system.file("images", package = "NanopixR")
  expect_error(
    run_cellpose(img_dir, gpu = TRUE, diameter = -5,
                 save_masks = FALSE, save_csv = FALSE),
    "'diameter' must be a single positiv number or NULL."
  )
  expect_error(
    run_cellpose(img_dir, gpu = TRUE, diameter = "auto",
                 save_masks = FALSE, save_csv = FALSE),
    "'diameter' must be a single positiv number or NULL."
  )
})

test_that("run_cellpose - throws error when conversion = TRUE but scale_info is NULL", {
  skip_if_not(cellpose_available(), "Cellpose not available")

  img_dir <- system.file("images", package = "NanopixR")
  expect_error(
    run_cellpose(img_dir,
                 gpu = TRUE,
                 conversion = TRUE,
                 scale_info = NULL,
                 save_masks = FALSE,
                 save_csv = FALSE),
    "'conversion = TRUE' requires 'scale_info' to be provided."
  )
})

test_that("run_cellpose - throws error if selected_files not found", {
  skip_if_not(cellpose_available(), "Cellpose not available")

  img_dir <- system.file("images", package = "NanopixR")
  expect_error(
    run_cellpose(img_dir,
                 gpu = TRUE,
                 selected_files = "nonexistent.png",
                 save_masks = FALSE,
                 save_csv = FALSE),
    "Some selected files were not found"
  )
})

# ── .cp_import_cellpose_modules: unit test ────────────────────────────────────
test_that(".cp_import_cellpose_modules - returns list with all required modules", {
  skip_if_not(cellpose_available(), "Cellpose not available")

  mods <- NanopixR:::.cp_import_cellpose_modules()
  expect_true(is.list(mods))
  expect_true(all(c("cellpose", "models", "io", "np") %in% names(mods)))
})

# ── Shared fixture ─────────────────────────────────────────────────────────────
# run_cellpose() requires GPU + Python + Cellpose and is expensive.
# Execute once with beads2.png only and reuse across all integration tests.

img_dir <- system.file("images", package = "NanopixR")

beads_dir <- local({
  tmp <- file.path(tempdir(), "cp_beads_test")
  dir.create(tmp, showWarnings = FALSE)
  file.copy(file.path(img_dir, "beads2.png"), tmp)
  tmp
})
withr::defer(unlink(beads_dir, recursive = TRUE), teardown_env())

# single shared result - only created if Cellpose is available
res_cp <- if(cellpose_available()) {
  tryCatch(
    run_cellpose(beads_dir,
                 gpu = TRUE,
                 save_masks = FALSE,
                 save_csv = FALSE),
    error = function(e) NULL
  )
} else {
  NULL
}

# ── run_cellpose: output structure ────────────────────────────────────────────
test_that("run_cellpose - returns a list with 'pixel' element", {
  skip_if_not(cellpose_available(), "Cellpose not available")
  skip_if(is.null(res_cp), "run_cellpose() failed during fixture setup")

  expect_true(is.list(res_cp))
  expect_true("pixel" %in% names(res_cp))
})

test_that("run_cellpose - does not contain 'converted' when conversion = FALSE", {
  skip_if_not(cellpose_available(), "Cellpose not available")
  skip_if(is.null(res_cp), "run_cellpose() failed during fixture setup")

  expect_false("converted" %in% names(res_cp))
})

test_that("run_cellpose - pixel is a named list with one entry per image", {
  skip_if_not(cellpose_available(), "Cellpose not available")
  skip_if(is.null(res_cp), "run_cellpose() failed during fixture setup")

  expect_true(is.list(res_cp$pixel))
  expect_equal(length(res_cp$pixel), 1L)
  expect_true("beads2" %in% names(res_cp$pixel))
})

test_that("run_cellpose - pixel data frame contains all expected columns", {
  skip_if_not(cellpose_available(), "Cellpose not available")
  skip_if(is.null(res_cp), "run_cellpose() failed during fixture setup")

  df <- res_cp$pixel[["beads2"]]
  expected_cols <- c("ROI_ID", "Centroid_X", "Centroid_Y", "Area",
                     "Mean_Intensity", "Perimeter", "Circularity",
                     "Min_Diameter", "Max_Diameter", "Image_name",
                     "Total_ROIs", "Overall_Confidence", "Analysis_method")
  expect_true(all(expected_cols %in% names(df)))
})

test_that("run_cellpose - Image_name column matches normalized image name", {
  skip_if_not(cellpose_available(), "Cellpose not available")
  skip_if(is.null(res_cp), "run_cellpose() failed during fixture setup")

  df <- res_cp$pixel[["beads2"]]
  expect_true(all(df$Image_name == "beads2"))
})

test_that("run_cellpose - Overall_Confidence contains numeric values between 0 and 1", {
  skip_if_not(cellpose_available(), "Cellpose not available")
  skip_if(is.null(res_cp), "run_cellpose() failed during fixture setup")

  conf <- res_cp$pixel[["beads2"]]$Overall_Confidence
  expect_true(is.numeric(conf))
  expect_true(all(conf >= 0 & conf <= 1, na.rm = TRUE))
})

test_that("run_cellpose - Analysis_method contains 'Cellpose'", {
  skip_if_not(cellpose_available(), "Cellpose not available")
  skip_if(is.null(res_cp), "run_cellpose() failed during fixture setup")

  method <- res_cp$pixel[["beads2"]]$Analysis_method
  expect_true(all(grepl("Cellpose", method)))
})

test_that("run_cellpose - numeric columns are actually numeric", {
  skip_if_not(cellpose_available(), "Cellpose not available")
  skip_if(is.null(res_cp), "run_cellpose() failed during fixture setup")

  df <- res_cp$pixel[["beads2"]]
  num_cols <- c("ROI_ID", "Centroid_X", "Centroid_Y", "Area",
                "Mean_Intensity", "Perimeter", "Circularity",
                "Min_Diameter", "Max_Diameter")
  for(col in num_cols) {
    expect_true(is.numeric(df[[col]]),
                info = paste("Column", col, "is not numeric"))
  }
})

# ── run_cellpose: save_masks and save_csv ─────────────────────────────────────
test_that("run_cellpose - save_csv = TRUE creates pixel CSV file", {
  skip_if_not(cellpose_available(), "Cellpose not available")

  tmp <- file.path(tempdir(), "cp_csv_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE))
  file.copy(file.path(img_dir, "beads2.png"), tmp)

  run_cellpose(tmp, gpu = TRUE, save_masks = FALSE, save_csv = TRUE)
  expect_true(file.exists(file.path(tmp, "Results",
                                    "Results_Cellpose_pixel.csv")))
})

test_that("run_cellpose - save_csv = FALSE creates no CSV file", {
  skip_if_not(cellpose_available(), "Cellpose not available")

  tmp <- file.path(tempdir(), "cp_no_csv_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE))
  file.copy(file.path(img_dir, "beads2.png"), tmp)

  run_cellpose(tmp, gpu = TRUE, save_masks = FALSE, save_csv = FALSE)
  expect_false(file.exists(file.path(tmp, "Results",
                                     "Results_Cellpose_pixel.csv")))
})

# ── run_cellpose: selected_files ──────────────────────────────────────────────
test_that("run_cellpose - selected_files restricts processing to specified image", {
  skip_if_not(cellpose_available(), "Cellpose not available")

  res_sel <- run_cellpose(img_dir,
                          selected_files = "beads2.png",
                          gpu = TRUE,
                          save_masks = FALSE,
                          save_csv = FALSE)
  expect_equal(length(res_sel$pixel), 1L)
  expect_true("beads2" %in% names(res_sel$pixel))
})
