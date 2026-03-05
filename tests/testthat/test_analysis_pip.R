
# ── .ap_validate_properties: unit tests ──────────────────────────────────────
test_that(".ap_validate_properties - passes for valid properties data frame", {
  df <- data.frame(Image_name = "beads2",
                   Recommended_method = "BiopixR",
                   stringsAsFactors = FALSE)
  expect_true(NanopixR:::.ap_validate_properties(df))
})

test_that(".ap_validate_properties - throws error if Image_name is missing", {
  df <- data.frame(Recommended_method = "BiopixR", stringsAsFactors = FALSE)
  expect_error(NanopixR:::.ap_validate_properties(df),
               "Properties is missing required columns")
})

test_that(".ap_validate_properties - throws error if Recommended_method is missing", {
  df <- data.frame(Image_name = "beads2", stringsAsFactors = FALSE)
  expect_error(NanopixR:::.ap_validate_properties(df),
               "Properties is missing required columns")
})

# ── .ap_group_images_by_method: unit tests ────────────────────────────────────
test_that(".ap_group_images_by_method - correctly separates Cellpose and BiopixR images", {
  properties <- data.frame(
    Image_name         = c("beads2", "ch_097_5"),
    Recommended_method = c("BiopixR", "Cellpose"),
    stringsAsFactors   = FALSE
  )
  lookup <- data.frame(
    norm = c("beads2", "ch_097_5"),
    file = c("/img/beads2.png", "/img/ch_097_5.png"),
    stringsAsFactors = FALSE
  )
  res <- NanopixR:::.ap_group_images_by_method(properties, lookup)
  expect_equal(res$BiopixR, "beads2")
  expect_equal(res$Cellpose, "ch_097_5")
})

test_that(".ap_group_images_by_method - returns empty vectors if no match", {
  properties <- data.frame(
    Image_name         = "beads2",
    Recommended_method = "BiopixR",
    stringsAsFactors   = FALSE
  )
  lookup <- data.frame(
    norm = "beads2",
    file = "/img/beads2.png",
    stringsAsFactors = FALSE
  )
  res <- NanopixR:::.ap_group_images_by_method(properties, lookup)
  expect_length(res$Cellpose, 0)
  expect_equal(res$BiopixR, "beads2")
})

# ── .ap_collect_results: unit tests ───────────────────────────────────────────

# minimal dummy data frame factory
make_roi_df <- function(name) {
  data.frame(ROI_ID = 1L, Image_name = name, stringsAsFactors = FALSE)
}

test_that(".ap_collect_results - merges pixel results from both methods", {
  res_cp <- list(pixel = list(ch_097_5 = make_roi_df("ch_097_5")))
  res_bp <- list(pixel = list(beads2   = make_roi_df("beads2")))
  out <- NanopixR:::.ap_collect_results(res_cp, res_bp, use_conversion = FALSE)
  expect_equal(length(out$pixel), 2L)
  expect_true(all(c("ch_097_5", "beads2") %in% names(out$pixel)))
})

test_that(".ap_collect_results - converted is NULL when use_conversion = FALSE", {
  res_cp <- list(pixel = list(ch_097_5 = make_roi_df("ch_097_5")))
  res_bp <- list(pixel = list(beads2   = make_roi_df("beads2")))
  out <- NanopixR:::.ap_collect_results(res_cp, res_bp, use_conversion = FALSE)
  expect_null(out$converted)
})

test_that(".ap_collect_results - merges converted results when use_conversion = TRUE", {
  res_cp <- list(pixel     = list(ch_097_5 = make_roi_df("ch_097_5")),
                 converted = list(ch_097_5 = make_roi_df("ch_097_5")))
  res_bp <- list(pixel     = list(beads2   = make_roi_df("beads2")),
                 converted = list(beads2   = make_roi_df("beads2")))
  out <- NanopixR:::.ap_collect_results(res_cp, res_bp, use_conversion = TRUE)
  expect_equal(length(out$converted), 2L)
})

test_that(".ap_collect_results - handles NULL results from either method", {
  res_bp <- list(pixel = list(beads2 = make_roi_df("beads2")))
  out <- NanopixR:::.ap_collect_results(NULL, res_bp, use_conversion = FALSE)
  expect_equal(length(out$pixel), 1L)
  expect_equal(names(out$pixel), "beads2")
})

# ── .ap_resolve_scale_handling: unit tests ────────────────────────────────────
test_that(".ap_resolve_scale_handling - returns conversion disabled if scale_info = FALSE", {
  res <- NanopixR:::.ap_resolve_scale_handling(
    folder = tempdir(), scale_info = FALSE,
    interactive = FALSE, verbose = FALSE
  )
  expect_false(res$use_conversion)
  expect_null(res$scale_info)
})

test_that(".ap_resolve_scale_handling - returns conversion disabled if scale_info = NULL", {
  res <- NanopixR:::.ap_resolve_scale_handling(
    folder = tempdir(), scale_info = NULL,
    interactive = FALSE, verbose = FALSE
  )
  expect_false(res$use_conversion)
  expect_null(res$scale_info)
})

test_that(".ap_resolve_scale_handling - enables conversion and calls get_scales when scale_info = TRUE", {
  mock_scales <- list(beads2 = list(mm_per_pixel_x = 4.2e-7,
                                    mm_per_pixel_y = 4.2e-7,
                                    source = "dm3_pixelSize_pixelUnit"))
  # local_mocked_bindings replaces get_scales in the NanopixR namespace
  # for the duration of this test only
  local_mocked_bindings(
    get_scales = function(...) mock_scales,
    .package = "NanopixR"
  )
  res <- NanopixR:::.ap_resolve_scale_handling(
    folder = tempdir(), scale_info = TRUE,
    interactive = FALSE, verbose = FALSE
  )
  expect_true(res$use_conversion)
  expect_equal(res$scale_info, mock_scales)
})

test_that(".ap_resolve_scale_handling - disables conversion if get_scales fails", {
  local_mocked_bindings(
    get_scales = function(...) stop("Python not available"),
    .package = "NanopixR"
  )
  res <- NanopixR:::.ap_resolve_scale_handling(
    folder = tempdir(), scale_info = TRUE,
    interactive = FALSE, verbose = FALSE
  )
  expect_false(res$use_conversion)
  expect_null(res$scale_info)
})

# ── .ap_run_method: unit tests ────────────────────────────────────────────────
test_that(".ap_run_method - returns NULL immediately if files is empty", {
  res <- NanopixR:::.ap_run_method(method = "BiopixR",
                                   folder = tempdir(),
                                   files = character(0),
                                   use_conversion = FALSE,
                                   scale_info = NULL,
                                   verbose = FALSE,
                                   gpu = TRUE,
                                   method_bp = "edge")
  expect_null(res)
})

test_that(".ap_run_method - throws error for unknown method", {
  expect_error(
    NanopixR:::.ap_run_method(method = "unknown",
                              folder = tempdir(),
                              files = "beads2",
                              use_conversion = FALSE,
                              scale_info = NULL,
                              verbose = FALSE,
                              gpu = TRUE,
                              method_bp = "edge"),
    "Unknown analysis method"
  )
})

# ── analysis_pip: input validation ────────────────────────────────────────────
test_that("analysis_pip - throws error for invalid folder type", {
  expect_error(analysis_pip(123),
               "'folder' must be a single character string.")
})

test_that("analysis_pip - throws error for non-existent folder", {
  expect_error(analysis_pip("/this/path/does/not/exist"),
               "The specified folder does not exist")
})

# ── analysis_pip: mocked integration tests ────────────────────────────────────
# All slow steps (.ap_extract_features, .ap_run_method) are mocked.
# This tests the pipeline orchestration logic only.

# shared mock objects
mock_properties <- structure(
  data.frame(
    Image_name         = "beads2",
    Recommended_method = "BiopixR",
    stringsAsFactors   = FALSE
  ),
  class = c("extract_image_features", "data.frame")
)

mock_pixel_df <- data.frame(
  ROI_ID          = 1L,
  Centroid_X      = 5.0,
  Centroid_Y      = 5.0,
  Area            = 25.0,
  Mean_Intensity  = 0.5,
  Perimeter       = 20.0,
  Circularity     = 0.8,
  Min_Diameter    = 4.0,
  Max_Diameter    = 6.0,
  Image_name      = "beads2",
  Total_ROIs      = 1L,
  Overall_Confidence = NA_real_,
  Analysis_method = "BiopixR; method=edge",
  stringsAsFactors = FALSE
)

mock_bp_result <- list(pixel = list(beads2 = mock_pixel_df), converted = NULL)

img_dir <- system.file("images", package = "NanopixR")

test_that("analysis_pip - returns list with pixel, converted and by_image", {
  mockery::stub(analysis_pip, ".ap_extract_features",   mock_properties)
  mockery::stub(analysis_pip, ".ap_run_method",
                function(method, ...) {
                  if(method == "BiopixR") mock_bp_result else NULL
                })

  res <- analysis_pip(img_dir,
                      write_csv = FALSE,
                      scale_info = FALSE,
                      interactive = FALSE,
                      verbose = FALSE,
                      gpu = TRUE)

  expect_true(is.list(res))
  expect_true(all(c("pixel", "converted", "by_image") %in% names(res)))
})

test_that("analysis_pip - pixel is a data frame with results", {
  mockery::stub(analysis_pip, ".ap_extract_features", mock_properties)
  mockery::stub(analysis_pip, ".ap_run_method",
                function(method, ...) {
                  if(method == "BiopixR") mock_bp_result else NULL
                })

  res <- analysis_pip(img_dir,
                      write_csv = FALSE,
                      scale_info = FALSE,
                      interactive = FALSE,
                      verbose = FALSE,
                      gpu = TRUE)

  expect_true(is.data.frame(res$pixel))
  expect_true(nrow(res$pixel) > 0)
})

test_that("analysis_pip - converted is NULL when scale_info = FALSE", {
  mockery::stub(analysis_pip, ".ap_extract_features", mock_properties)
  mockery::stub(analysis_pip, ".ap_run_method",
                function(method, ...) {
                  if(method == "BiopixR") mock_bp_result else NULL
                })

  res <- analysis_pip(img_dir,
                      write_csv = FALSE,
                      scale_info = FALSE,
                      interactive = FALSE,
                      verbose = FALSE,
                      gpu = TRUE)

  expect_null(res$converted)
})

test_that("analysis_pip - by_image contains per-image split of pixel results", {
  mockery::stub(analysis_pip, ".ap_extract_features", mock_properties)
  mockery::stub(analysis_pip, ".ap_run_method",
                function(method, ...) {
                  if(method == "BiopixR") mock_bp_result else NULL
                })

  res <- analysis_pip(img_dir,
                      write_csv = FALSE,
                      scale_info = FALSE,
                      interactive = FALSE,
                      verbose = FALSE,
                      gpu = TRUE)

  expect_true(is.list(res$by_image$pixel))
  expect_true("beads2" %in% names(res$by_image$pixel))
})

test_that("analysis_pip - write_csv = TRUE creates CSV in out_dir", {
  mockery::stub(analysis_pip, ".ap_extract_features", mock_properties)
  mockery::stub(analysis_pip, ".ap_run_method",
                function(method, ...) {
                  if(method == "BiopixR") mock_bp_result else NULL
                })

  tmp_out <- file.path(tempdir(), "ap_csv_test")
  on.exit(unlink(tmp_out, recursive = TRUE))

  analysis_pip(img_dir,
               write_csv = TRUE,
               out_dir = tmp_out,
               scale_info = FALSE,
               interactive = FALSE,
               verbose = FALSE,
               gpu = TRUE)

  expect_true(file.exists(file.path(tmp_out, "Analysis_pixel.csv")))
})

test_that("analysis_pip - write_csv = FALSE creates no CSV", {
  mockery::stub(analysis_pip, ".ap_extract_features", mock_properties)
  mockery::stub(analysis_pip, ".ap_run_method",
                function(method, ...) {
                  if(method == "BiopixR") mock_bp_result else NULL
                })

  tmp_out <- file.path(tempdir(), "ap_no_csv_test")
  on.exit(unlink(tmp_out, recursive = TRUE))

  analysis_pip(img_dir,
               write_csv = FALSE,
               out_dir = tmp_out,
               scale_info = FALSE,
               interactive = FALSE,
               verbose = FALSE,
               gpu = TRUE)

  expect_false(file.exists(file.path(tmp_out, "Analysis_pixel.csv")))
})

test_that("analysis_pip - crashes when no images produce results (known bug: split(NULL) in by_image)", {
  mockery::stub(analysis_pip, ".ap_extract_features", mock_properties)
  mockery::stub(analysis_pip, ".ap_run_method", function(...) NULL)

  expect_error(
    analysis_pip(img_dir,
                 write_csv = FALSE,
                 scale_info = FALSE,
                 interactive = FALSE,
                 verbose = FALSE,
                 gpu = TRUE)
  )
})
