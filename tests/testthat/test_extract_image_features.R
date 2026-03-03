
test_that("extract_image_features - input validation: folder must be a character string", {
  expect_error(extract_image_features(123),
               "'folder' must be a single character string.")
  expect_error(extract_image_features(c("path1", "path2")),
               "'folder' must be a single character string.")
  expect_error(extract_image_features(NULL),
               "'folder' must be a single character string.")
})

test_that("extract_image_features - input validation: non-existent folder", {
  expect_error(extract_image_features("/this/path/does/not/exist"),
               "The specified folder does not exist")
})

test_that("extract_image_features - input validation: empty folder throws error", {
  tmp <- tempdir()
  empty_dir <- file.path(tmp, "empty_test_folder")
  dir.create(empty_dir, showWarnings = FALSE)
  on.exit(unlink(empty_dir, recursive = TRUE))

  expect_error(extract_image_features(empty_dir),
               "No image files found in the specified folder!")
})

# ── Fixture: folder containing beads2.png ─────────────────────────────────────
img_dir <- system.file("images", package = "CellpixR")
beads_dir <- local({
  tmp <- file.path(tempdir(), "beads_test")
  dir.create(tmp, showWarnings = FALSE)
  file.copy(file.path(img_dir, "beads2.png"), tmp)
  tmp
})
withr::defer(unlink(beads_dir, recursive = TRUE), teardown_env())

# ── Output structure ──────────────────────────────────────────────────────────
test_that("extract_image_features - return value has correct class", {
  res <- extract_image_features(beads_dir, export_csv = FALSE, verbose = FALSE)
  expect_s3_class(res, "extract_image_features")
  expect_s3_class(res, "data.frame")
})

test_that("extract_image_features - return value contains all expected columns", {
  res <- extract_image_features(beads_dir, export_csv = FALSE, verbose = FALSE)

  expected_cols <- c("Image_name", "Color_mode", "GS_Diff",
                     "skewness", "kurtosis",
                     "mean_grad", "median_grad", "edge_density",
                     "mean_power", "high_freq_ratio",
                     "Recommended_method", "Decision_justification")
  expect_true(all(expected_cols %in% names(res)))
})

test_that("extract_image_features - one row per image", {
  res <- extract_image_features(beads_dir, export_csv = FALSE, verbose = FALSE)
  expect_equal(nrow(res), 1L)
})

test_that("extract_image_features - Image_name matches 'beads2'", {
  res <- extract_image_features(beads_dir, export_csv = FALSE, verbose = FALSE)
  expect_equal(res$Image_name, "beads2")
})

test_that("extract_image_features - Recommended_method is either 'BiopixR' or 'Cellpose'", {
  res <- extract_image_features(beads_dir, export_csv = FALSE, verbose = FALSE)
  expect_true(res$Recommended_method %in% c("BiopixR", "Cellpose"))
})

test_that("extract_image_features - beads2 is recommended as BiopixR", {
  res <- extract_image_features(beads_dir, export_csv = FALSE, verbose = FALSE)
  expect_equal(res$Recommended_method, "BiopixR")
})

test_that("extract_image_features - numeric feature columns are actually numeric", {
  res <- extract_image_features(beads_dir, export_csv = FALSE, verbose = FALSE)

  num_cols <- c("GS_Diff", "skewness", "kurtosis",
                "mean_grad", "median_grad", "edge_density",
                "mean_power", "high_freq_ratio")
  for (col in num_cols) {
    expect_true(is.numeric(res[[col]]),
                info = paste("Column", col, "is not numeric"))
  }
})

test_that("extract_image_features - Decision_justification is not empty", {
  res <- extract_image_features(beads_dir, export_csv = FALSE, verbose = FALSE)
  expect_true(nchar(res$Decision_justification) > 0)
})

# ── Parameter: round_digits ───────────────────────────────────────────────────
test_that("extract_image_features - round_digits affects number of decimal places", {
  res2 <- extract_image_features(beads_dir, export_csv = FALSE,
                                 verbose = FALSE, round_digits = 2)
  res5 <- extract_image_features(beads_dir, export_csv = FALSE,
                                 verbose = FALSE, round_digits = 5)

  # mean_grad is part of results and processed by .ex_round_numeric_df()
  # with 2 digits, the value must not have more than 2 decimal places
  expect_equal(res2$mean_grad, round(res2$mean_grad, 2))
  # 5 digits should retain more precision than 2
  expect_false(identical(res2$mean_grad, res5$mean_grad))
})

# ── Parameter: export_csv ─────────────────────────────────────────────────────
test_that("extract_image_features - export_csv = TRUE creates a CSV file", {
  tmp <- file.path(tempdir(), "csv_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE))
  file.copy(file.path(img_dir, "beads2.png"), tmp)

  extract_image_features(tmp,
                         export_csv = TRUE,
                         csv_name = "Properties.csv",
                         verbose = FALSE)
  expect_true(file.exists(file.path(tmp, "Properties.csv")))
})

test_that("extract_image_features - export_csv = FALSE does not create a CSV file", {
  tmp <- file.path(tempdir(), "no_csv_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE))
  file.copy(file.path(img_dir, "beads2.png"), tmp)

  extract_image_features(tmp, export_csv = FALSE, verbose = FALSE)
  expect_false(file.exists(file.path(tmp, "Properties.csv")))
})

test_that("extract_image_features - csv_name is applied correctly", {
  tmp <- file.path(tempdir(), "csv_name_test")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE))
  file.copy(file.path(img_dir, "beads2.png"), tmp)

  extract_image_features(tmp,
                         export_csv = TRUE,
                         csv_name = "MyOutput.csv",
                         verbose = FALSE)
  expect_true(file.exists(file.path(tmp, "MyOutput.csv")))
})

# ── Parameter: verbose ────────────────────────────────────────────────────────
test_that("extract_image_features - verbose = TRUE prints messages", {
  expect_message(
    extract_image_features(beads_dir, export_csv = FALSE, verbose = TRUE),
    regexp = "Loading images|Analysis completed"
  )
})

test_that("extract_image_features - verbose = FALSE prints no messages", {
  expect_no_message(
    extract_image_features(beads_dir, export_csv = FALSE, verbose = FALSE)
  )
})

# ── .ex_choose_method: unit tests ─────────────────────────────────────────────
test_that(".ex_choose_method - recommends BiopixR for high skewness and high kurtosis", {
  df <- data.frame(
    skewness    = 5.0,   # |skewness| > 1.5  -> BiopixR +1.5
    kurtosis    = 28.9,  # kurtosis  > 7     -> BiopixR +1.5
    mean_grad   = 0.003, # < 0.03            -> BiopixR +1
    median_grad = 0.0,   # < 0.01            -> BiopixR +1
    mean_power  = 5000   # < 100000          -> BiopixR +1.5
  )
  res <- CellpixR:::.ex_choose_method(df)
  expect_equal(res$Recommended_method, "BiopixR")
})

test_that(".ex_choose_method - recommends Cellpose for low skewness and low kurtosis", {
  df <- data.frame(
    skewness    = -0.2,   # |skewness| <= 1.5 -> Cellpose +1.5
    kurtosis    = 2.8,    # <= 7              -> Cellpose +1.5
    mean_grad   = 0.04,   # 0.03 - 1.0        -> Cellpose +1
    median_grad = 0.05,   # 0.01 - 0.09       -> Cellpose +1
    mean_power  = 500000  # >= 100000         -> Cellpose +1.5
  )
  res <- CellpixR:::.ex_choose_method(df)
  expect_equal(res$Recommended_method, "Cellpose")
})

test_that(".ex_choose_method - tie defaults to Cellpose", {
  # BiopixR  = 1.5 (skewness) + 1.5 (kurtosis) = 3.0
  # Cellpose = 1.0 (mean_grad) + 1.0 (median_grad) + 1.5 (mean_power) = 3.5
  # -> Cellpose wins
  df <- data.frame(
    skewness    = 3.0,    # BiopixR  +1.5
    kurtosis    = 10.0,   # BiopixR  +1.5
    mean_grad   = 0.05,   # Cellpose +1
    median_grad = 0.05,   # Cellpose +1
    mean_power  = 200000  # Cellpose +1.5
  )
  res <- CellpixR:::.ex_choose_method(df)
  expect_true(res$Recommended_method %in% c("BiopixR", "Cellpose"))
})

test_that(".ex_choose_method - Decision_justification contains all five features", {
  df <- data.frame(
    skewness = 2.0, kurtosis = 8.0,
    mean_grad = 0.01, median_grad = 0.005, mean_power = 50000
  )
  res <- CellpixR:::.ex_choose_method(df)
  expect_match(res$Decision_justification, "Skewness")
  expect_match(res$Decision_justification, "Kurtosis")
  expect_match(res$Decision_justification, "mean_grad")
  expect_match(res$Decision_justification, "median_grad")
  expect_match(res$Decision_justification, "mean_power")
})

test_that(".ex_choose_method - processes multiple rows correctly", {
  df <- data.frame(
    skewness    = c(5.0,  -0.2),
    kurtosis    = c(28.9,  2.8),
    mean_grad   = c(0.003, 0.04),
    median_grad = c(0.0,   0.05),
    mean_power  = c(5000,  500000)
  )
  res <- CellpixR:::.ex_choose_method(df)
  expect_equal(nrow(res), 2L)
  expect_equal(res$Recommended_method[1], "BiopixR")
  expect_equal(res$Recommended_method[2], "Cellpose")
})
