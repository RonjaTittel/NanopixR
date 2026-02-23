
skip_if_not(is_integration_enabled(), "Integration tests disabled (set CELLPIXR_INTEGRATION=true).")

test_that("run_cellpose errors on invalid folder", {
  expect_error(
    run_cellpose(folder = "definitely_not_existing"),
    "does not exist|exist",
    ignore.case = TRUE
  )
})

test_that("run_cellpose validates inputs, branches, and return structure (unit; mocked internals)", {
  tmp <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()

  # trackers for side effects
  called <- new.env(parent = emptyenv())
  called$save_masks <- FALSE
  called$write_csv  <- FALSE
  called$convert    <- FALSE

  # minimal dummy "image bundle"
  dummy_bundle <- list(
    imgs_np = list(matrix(0, 2, 2)),
    imgs    = list(matrix(0, 2, 2)),
    files   = c(file.path(tmp, "beads2.png"))
  )

  # mock internal helpers so we don't need Python/cellpose
  testthat::local_mocked_bindings(
    .cp_use_python_from_option = function() invisible(TRUE),

    .cp_import_cellpose_modules = function() {
      list(io = NULL, np = NULL, models = NULL)
    },

    .cp_load_images = function(folder, io, np, selected_files) {
      dummy_bundle
    },

    .cp_create_model = function(models, model_path, gpu) {
      "dummy_model"
    },

    .cp_analysis_method_text = function(model_path, diameter) {
      "Cellpose dummy method"
    },

    .cp_eval_images = function(model, imgs_np, diameter, progress) {
      list("dummy_segmentation_result")
    },

    .cp_save_masks = function(io, imgs_np, res_list, files, output_dir) {
      called$save_masks <- TRUE
      invisible(TRUE)
    },

    .cp_compute_roi_stats = function(res_list, imgs, analysis_method_text, progress) {
      list(beads2 = data.frame(
        ROI_ID = 1L,
        Area = 10,
        Image_name = "beads2",
        stringsAsFactors = FALSE
      ))
    },

    .cp_convert_roi_units = function(Results_pixel, scale_info) {
      called$convert <- TRUE
      list(beads2 = data.frame(
        ROI_ID = 1L,
        Area_um2 = 0.5,
        Image_name = "beads2",
        stringsAsFactors = FALSE
      ))
    },

    .cp_write_results_csv = function(results_pixel, results_converted, output_dir, prefix) {
      called$write_csv <- TRUE
      list(
        pixel = file.path(output_dir, paste0(prefix, "_pixel.csv")),
        converted = if (!is.null(results_converted)) file.path(output_dir, paste0(prefix, "_converted.csv")) else NULL
      )
    },

    .env = asNamespace("CellpixR")
  )

  # --- diameter validation: error ---
  expect_error(
    run_cellpose(folder = tmp, diameter = -1, save_csv = FALSE, save_masks = FALSE),
    "'diameter' must be a single positiv number or NULL\\.",
    fixed = FALSE
  )

  expect_error(
    run_cellpose(folder = tmp, diameter = c(1, 2), save_csv = FALSE, save_masks = FALSE),
    "'diameter' must be a single positiv number or NULL\\.",
    fixed = FALSE
  )

  # --- conversion requires scale_info: error ---
  expect_error(
    run_cellpose(folder = tmp, conversion = TRUE, scale_info = NULL, save_csv = FALSE, save_masks = FALSE),
    "requires 'scale_info' to be provided",
    ignore.case = TRUE
  )

  # --- structure: conversion = FALSE ---
  called$save_masks <- FALSE
  called$write_csv  <- FALSE
  called$convert    <- FALSE

  res1 <- run_cellpose(
    folder = tmp,
    output_dir = out_dir,
    conversion = FALSE,
    save_masks = FALSE,
    save_csv = FALSE
  )

  expect_type(res1, "list")
  expect_named(res1, "pixel")
  expect_type(res1$pixel, "list")
  expect_true("beads2" %in% names(res1$pixel))
  expect_true(is.data.frame(res1$pixel$beads2))
  expect_true(all(c("ROI_ID", "Area", "Image_name") %in% names(res1$pixel$beads2)))

  expect_false(called$save_masks)
  expect_false(called$write_csv)
  expect_false(called$convert)

  # --- structure: conversion = TRUE ---
  called$save_masks <- FALSE
  called$write_csv  <- FALSE
  called$convert    <- FALSE

  res2 <- run_cellpose(
    folder = tmp,
    output_dir = out_dir,
    conversion = TRUE,
    scale_info = list(beads2 = 1),
    save_masks = FALSE,
    save_csv = FALSE
  )

  expect_type(res2, "list")
  expect_named(res2, c("pixel", "converted"))
  expect_type(res2$converted, "list")
  expect_true("beads2" %in% names(res2$converted))
  expect_true(is.data.frame(res2$converted$beads2))
  expect_true(isTRUE(called$convert))

  # --- branch checks: save_masks & save_csv ---
  called$save_masks <- FALSE
  called$write_csv  <- FALSE

  res3 <- run_cellpose(
    folder = tmp,
    output_dir = out_dir,
    conversion = FALSE,
    save_masks = TRUE,
    save_csv = TRUE
  )

  expect_named(res3, "pixel")
  expect_true(isTRUE(called$save_masks))
  expect_true(isTRUE(called$write_csv))
})
