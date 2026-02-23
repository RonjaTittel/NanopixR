
test_that("analysis_pip: calls pipeline helpers with correct args and returns structured results", {

  pkg_env <- environment(analysis_pip)

  # call log
  calls <- new.env(parent = emptyenv())
  calls$check_folder <- 0
  calls$extract_features <- 0
  calls$validate_properties <- 0
  calls$build_lookup <- 0
  calls$resolve_scale <- 0
  calls$group_images <- 0
  calls$resolve_group <- 0
  calls$run_method <- list()
  calls$collect_results <- 0
  calls$write_csv <- 0
  calls$print_summary <- 0

  # fixtures / fake returns
  folder <- "dummy_folder"

  fake_properties <- data.frame(
    Image_name = c("img_a", "img_b"),
    stringsAsFactors = FALSE
  )

  fake_lookup <- data.frame(
    norm = c("img_a", "img_b"),
    file = c("img_a.tif", "img_b.tif"),
    stringsAsFactors = FALSE
  )

  fake_scale <- list(
    use_conversion = TRUE,
    scale_info = list(dummy = TRUE)
  )

  fake_groups <- list(
    Cellpose = c("img_a"),
    BiopixR  = c("img_b")
  )

  resolve_group_call_idx <- 0

  fake_cp_results <- list(
    pixel = list(
      data.frame(Image_name = "img_a", ROI_ID = 1, Area = 10, stringsAsFactors = FALSE)
    ),
    converted = list(
      data.frame(Image_name = "img_a", ROI_ID = 1, Area_um2 = 1.23, stringsAsFactors = FALSE)
    )
  )

  fake_bp_results <- list(
    pixel = list(
      data.frame(Image_name = "img_b", ROI_ID = 2, Area = 20, stringsAsFactors = FALSE)
    ),
    converted = list(
      data.frame(Image_name = "img_b", ROI_ID = 2, Area_um2 = 2.34, stringsAsFactors = FALSE)
    )
  )

  fake_combined <- list(
    pixel = c(fake_cp_results$pixel, fake_bp_results$pixel),
    converted = c(fake_cp_results$converted, fake_bp_results$converted)
  )

  # mocks
  testthat::local_mocked_bindings(

    .check_folder = function(x) {
      calls$check_folder <- calls$check_folder + 1
      expect_identical(x, folder)
      invisible(TRUE)
    },

    .ap_extract_features = function(x) {
      calls$extract_features <- calls$extract_features + 1
      expect_identical(x, folder)
      fake_properties
    },

    .ap_validate_properties = function(props) {
      calls$validate_properties <- calls$validate_properties + 1
      expect_true(is.data.frame(props))
      expect_true("Image_name" %in% names(props))
      invisible(TRUE)
    },

    .build_image_lookup = function(x) {
      calls$build_lookup <- calls$build_lookup + 1
      expect_identical(x, folder)
      fake_lookup
    },

    .ap_resolve_scale_handling = function(folder, scale_info, interactive, verbose) {
      calls$resolve_scale <- calls$resolve_scale + 1
      expect_identical(folder, "dummy_folder")
      expect_true(is.logical(interactive))
      expect_true(is.logical(verbose))
      fake_scale
    },

    .ap_group_images_by_method = function(properties, lookup) {
      calls$group_images <- calls$group_images + 1
      expect_true(is.data.frame(properties))
      expect_true(is.data.frame(lookup))
      fake_groups
    },

    .ap_resolve_group = function(images, recommended, interactive, verbose) {
      calls$resolve_group <- calls$resolve_group + 1
      resolve_group_call_idx <<- resolve_group_call_idx + 1

      # first call = cellpose recommended set, second call = biopixr recommended set
      if (resolve_group_call_idx == 1) {
        expect_identical(recommended, "Cellpose")
        expect_identical(images, c("img_a"))
        return(list(Cellpose = c("img_a"), BiopixR = character(0), Skipped = character(0)))
      } else {
        expect_identical(recommended, "BiopixR")
        expect_identical(images, c("img_b"))
        return(list(Cellpose = character(0), BiopixR = c("img_b"), Skipped = character(0)))
      }
    },

    .ap_print_summary = function(...) {
      calls$print_summary <- calls$print_summary + 1
      invisible(TRUE)
    },

    .ap_run_method = function(method, folder, files, use_conversion, scale_info, verbose, gpu, method_bp) {
      # record each run with args
      calls$run_method[[length(calls$run_method) + 1]] <- list(
        method = method,
        folder = folder,
        files = files,
        use_conversion = use_conversion,
        scale_info = scale_info,
        verbose = verbose,
        gpu = gpu,
        method_bp = method_bp
      )

      if (identical(method, "Cellpose")) {
        expect_identical(files, c("img_a"))
        return(fake_cp_results)
      } else {
        expect_identical(method, "BiopixR")
        expect_identical(files, c("img_b"))
        return(fake_bp_results)
      }
    },

    .ap_collect_results = function(results_cellpose, results_biopixr, use_conversion) {
      calls$collect_results <- calls$collect_results + 1
      expect_true(is.list(results_cellpose))
      expect_true(is.list(results_biopixr))
      expect_true(isTRUE(use_conversion))
      fake_combined
    },

    .ap_write_csv_results = function(df_pixel, df_converted, out_dir, verbose) {
      calls$write_csv <- calls$write_csv + 1
      expect_true(is.data.frame(df_pixel))
      expect_true(is.data.frame(df_converted))
      expect_true(is.character(out_dir) && length(out_dir) == 1)
      expect_true(is.logical(verbose))
      invisible(TRUE)
    },

    .env = pkg_env
  )

  # run
  res <- analysis_pip(
    folder = folder,
    write_csv = TRUE,
    out_dir = file.path(folder, "Results"),
    scale_info = FALSE,
    interactive = FALSE,
    verbose = FALSE,
    gpu = TRUE,
    method_bp = "edge"
  )

  # asserts: calls
  expect_equal(calls$check_folder, 1)
  expect_equal(calls$extract_features, 1)
  expect_equal(calls$validate_properties, 1)
  expect_equal(calls$build_lookup, 1)
  expect_equal(calls$resolve_scale, 1)
  expect_equal(calls$group_images, 1)
  expect_equal(calls$resolve_group, 2)
  expect_equal(length(calls$run_method), 2)
  expect_equal(calls$collect_results, 1)
  expect_equal(calls$write_csv, 1)

  # check that run_method got the correct passthrough args
  expect_true(all(vapply(calls$run_method, function(x) identical(x$folder, folder), logical(1))))
  expect_true(all(vapply(calls$run_method, function(x) identical(x$gpu, TRUE), logical(1))))
  expect_true(all(vapply(calls$run_method, function(x) identical(x$method_bp, "edge"), logical(1))))
  expect_true(all(vapply(calls$run_method, function(x) isTRUE(x$use_conversion), logical(1))))

  # asserts: result structure
  expect_type(res, "list")
  expect_named(res, c("pixel", "converted", "by_image"), ignore.order = TRUE)

  expect_s3_class(res$pixel, "data.frame")
  expect_s3_class(res$converted, "data.frame")

  expect_true(all(c("Image_name", "ROI_ID") %in% names(res$pixel)))
  expect_true(all(c("Image_name", "ROI_ID") %in% names(res$converted)))

  # by_image splits exist and contain both images
  expect_true(is.list(res$by_image))
  expect_true(is.list(res$by_image$pixel))
  expect_true(is.list(res$by_image$converted))

  expect_true(all(c("img_a", "img_b") %in% names(res$by_image$pixel)))
  expect_true(all(c("img_a", "img_b") %in% names(res$by_image$converted)))
})


test_that("analysis_pip: does not write CSV when write_csv = FALSE", {

  pkg_env <- environment(analysis_pip)
  wrote <- FALSE

  testthat::local_mocked_bindings(
    .check_folder = function(...) invisible(TRUE),
    .ap_extract_features = function(...) data.frame(Image_name = "img_a", stringsAsFactors = FALSE),
    .ap_validate_properties = function(...) invisible(TRUE),
    .build_image_lookup = function(...) data.frame(norm = "img_a", file = "img_a.tif", stringsAsFactors = FALSE),
    .ap_resolve_scale_handling = function(...) list(use_conversion = FALSE, scale_info = NULL),
    .ap_group_images_by_method = function(...) list(Cellpose = "img_a", BiopixR = character(0)),
    .ap_resolve_group = function(images, recommended, ...) {
      if (recommended == "Cellpose") list(Cellpose = images, BiopixR = character(0), Skipped = character(0))
      else list(Cellpose = character(0), BiopixR = images, Skipped = character(0))
    },
    .ap_run_method = function(method, folder, files, use_conversion, scale_info, verbose, gpu, method_bp) {
      list(pixel = list(data.frame(Image_name="img_a", ROI_ID=1, stringsAsFactors=FALSE)),
           converted = list())
    },
    .ap_collect_results = function(...) list(pixel = list(data.frame(Image_name="img_a", ROI_ID=1, stringsAsFactors=FALSE)),
                                             converted = list()),
    .ap_write_csv_results = function(...) { wrote <<- TRUE; invisible(TRUE) },
    .env = pkg_env
  )

  res <- analysis_pip(folder = "x", write_csv = FALSE, interactive = FALSE, verbose = FALSE)

  expect_false(wrote)
  expect_true(is.data.frame(res$pixel))
  expect_null(res$converted)
})
