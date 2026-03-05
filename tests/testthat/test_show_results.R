
# ── Shared fixture ─────────────────────────────────────────────────────────────
# Build a minimal temp folder with:
#   - beads2.png  (copied from inst/images)
#   - Results/beads2_bp_masks.tif  (synthetic dummy mask via ijtiff)
#   - a minimal results_list data frame
img_dir <- system.file("images", package = "NanopixR")

show_dir <- local({
  tmp <- file.path(tempdir(), "show_results_test")
  dir.create(file.path(tmp, "Results"), recursive = TRUE, showWarnings = FALSE)

  # copy original image
  file.copy(file.path(img_dir, "beads2.png"), tmp)

  # create a 100x100 dummy mask (one ROI with label 1)
  # must be large enough for plot margins when download = TRUE renders a PNG
  mask_arr <- array(0, dim = c(100, 100, 1, 1))
  mask_arr[30:70, 30:70, 1, 1] <- 1
  ijtiff::write_tif(img = mask_arr,
                    path = file.path(tmp, "Results", "beads2_bp_masks.tif"),
                    bits_per_sample = 32,
                    compression = "none",
                    overwrite = TRUE)
  tmp
})
withr::defer(unlink(show_dir, recursive = TRUE), teardown_env())

# minimal results_list matching the dummy mask
dummy_results <- list(
  beads2 = data.frame(
    ROI_ID            = 1L,
    Centroid_X        = 5.0,
    Centroid_Y        = 5.0,
    Area              = 25.0,
    Mean_Intensity    = 0.5,
    Perimeter         = 20.0,
    Circularity       = 0.8,
    Min_Diameter      = 4.0,
    Max_Diameter      = 6.0,
    Image_name        = "beads2",
    Total_ROIs        = 1L,
    Overall_Confidence = 0.95,
    Analysis_method   = "BiopixR; method=edge; alpha=gaussian; sigma=gaussian",
    stringsAsFactors  = FALSE
  )
)

# ── show_results: input validation ────────────────────────────────────────────
test_that("show_results - throws error for invalid folder type", {
  expect_error(
    show_results(dummy_results, folder = 123, image_name = "beads2"),
    "'folder' must be a single character string."
  )
})

test_that("show_results - throws error for non-existent folder", {
  expect_error(
    show_results(dummy_results,
                 folder = "/this/path/does/not/exist",
                 image_name = "beads2"),
    "The specified folder does not exist"
  )
})

test_that("show_results - throws error for missing image_name", {
  expect_error(
    show_results(dummy_results, folder = show_dir),
    "'image_name' must be a single character string."
  )
})

test_that("show_results - throws error for non-character image_name", {
  expect_error(
    show_results(dummy_results, folder = show_dir, image_name = 123),
    "'image_name' must be a single character string."
  )
})

test_that("show_results - throws error if image_name not found in folder", {
  expect_error(
    show_results(dummy_results, folder = show_dir, image_name = "nonexistent"),
    "No results found for image: nonexistent"
  )
})

test_that("show_results - throws error if image_name not found in results_list", {
  expect_error(
    show_results(list(), folder = show_dir, image_name = "beads2"),
    "No results found for"
  )
})

# ── show_results: return value ────────────────────────────────────────────────
test_that("show_results - returns a data frame invisibly", {
  result <- suppressMessages(
    show_results(dummy_results,
                 folder = show_dir,
                 image_name = "beads2",
                 download = FALSE,
                 plot = FALSE,
                 verbose = FALSE)
  )
  expect_true(is.data.frame(result))
})

test_that("show_results - returned data frame drops metadata columns", {
  result <- suppressMessages(
    show_results(dummy_results,
                 folder = show_dir,
                 image_name = "beads2",
                 download = FALSE,
                 plot = FALSE,
                 verbose = FALSE)
  )
  dropped <- c("Image_name", "Total_ROIs", "Overall_Confidence",
               "Mean_Confidence", "Analysis_method")
  expect_true(!any(dropped %in% names(result)))
})

test_that("show_results - returned data frame retains ROI measurement columns", {
  result <- suppressMessages(
    show_results(dummy_results,
                 folder = show_dir,
                 image_name = "beads2",
                 download = FALSE,
                 plot = FALSE,
                 verbose = FALSE)
  )
  expected <- c("ROI_ID", "Centroid_X", "Centroid_Y", "Area",
                "Mean_Intensity", "Perimeter", "Circularity",
                "Min_Diameter", "Max_Diameter")
  expect_true(all(expected %in% names(result)))
})

# ── show_results: verbose ─────────────────────────────────────────────────────
test_that("show_results - verbose = TRUE prints output to console", {
  expect_output(
    suppressMessages(
      show_results(dummy_results,
                   folder = show_dir,
                   image_name = "beads2",
                   download = FALSE,
                   plot = FALSE,
                   verbose = TRUE)
    ),
    regexp = "beads2"
  )
})

test_that("show_results - verbose = FALSE prints nothing", {
  expect_silent(
    suppressMessages(
      show_results(dummy_results,
                   folder = show_dir,
                   image_name = "beads2",
                   download = FALSE,
                   plot = FALSE,
                   verbose = FALSE)
    )
  )
})

# ── show_results: download ────────────────────────────────────────────────────
test_that("show_results - download = TRUE creates PNG and CSV in Results/", {
  # mock the plot function to avoid 'figure margins too large' in headless test env
  mockery::stub(show_results, ".show_plot_image_mask", function(...) invisible(NULL))

  suppressMessages(
    show_results(dummy_results,
                 folder = show_dir,
                 image_name = "beads2",
                 download = TRUE,
                 plot = FALSE,
                 verbose = FALSE)
  )
  expect_true(file.exists(file.path(show_dir, "Results", "beads2_overview.png")))
  expect_true(file.exists(file.path(show_dir, "Results", "beads2_results.csv")))
})

test_that("show_results - download = FALSE creates no new files in Results/", {
  # record files before call
  before <- list.files(file.path(show_dir, "Results"))

  suppressMessages(
    show_results(dummy_results,
                 folder = show_dir,
                 image_name = "beads2",
                 download = FALSE,
                 plot = FALSE,
                 verbose = FALSE)
  )
  after <- list.files(file.path(show_dir, "Results"))
  # no new files should have appeared
  expect_equal(sort(before), sort(after))
})

# ── .show_find_image_and_mask: unit tests ─────────────────────────────────────
test_that(".show_find_image_and_mask - returns correct paths for beads2", {
  res <- NanopixR:::.show_find_image_and_mask(show_dir, "beads2")
  expect_true(all(c("img_path", "mask_path", "base") %in% names(res)))
  expect_true(file.exists(res$img_path))
  expect_true(file.exists(res$mask_path))
  expect_equal(res$base, "beads2")
})

test_that(".show_find_image_and_mask - throws error if image not in folder", {
  expect_error(
    NanopixR:::.show_find_image_and_mask(show_dir, "nonexistent"),
    "Original image not found"
  )
})

test_that(".show_find_image_and_mask - throws error if no mask file exists", {
  # temp folder with image but no Results/mask
  tmp_nomask <- file.path(tempdir(), "show_no_mask")
  dir.create(tmp_nomask, showWarnings = FALSE)
  on.exit(unlink(tmp_nomask, recursive = TRUE))
  file.copy(file.path(img_dir, "beads2.png"), tmp_nomask)

  expect_error(
    NanopixR:::.show_find_image_and_mask(tmp_nomask, "beads2"),
    "Mask not found for image"
  )
})

test_that(".show_find_image_and_mask - prefers bp mask over cp mask", {
  # folder already has a bp mask from shared fixture
  res <- NanopixR:::.show_find_image_and_mask(show_dir, "beads2")
  expect_true(grepl("_bp_masks\\.tif$", res$mask_path))
})
