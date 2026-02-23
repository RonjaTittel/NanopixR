
test_that("show_results: minimal validation paths", {

  pkg_env <- environment(show_results)

  # mock the parts that would otherwise depend on real folders
  testthat::local_mocked_bindings(
    .check_folder = function(folder) invisible(TRUE),
    .list_images_sorted = function(folder) data.frame(norm = c("img1"), stringsAsFactors = FALSE),
    .env = pkg_env
  )

  results_list <- list(
    img1 = data.frame(ROI_ID = 1, stringsAsFactors = FALSE)
  )

  # invalid image_name
  expect_error(
    show_results(results_list, folder = "x", image_name = 1, download = FALSE, plot = FALSE, verbose = FALSE),
    "image_name"
  )

  # image not in lookup
  expect_error(
    show_results(results_list, folder = "x", image_name = "nope", download = FALSE, plot = FALSE, verbose = FALSE),
    "No results found for image"
  )

  # image in lookup but missing from results_list
  expect_error(
    show_results(list(other = results_list$img1), folder = "x", image_name = "img1", download = FALSE, plot = FALSE, verbose = FALSE),
    "No results found for: img1"
  )

  # no ROIs in df
  empty_list <- list(img1 = results_list$img1[0, , drop = FALSE])
  expect_error(
    show_results(empty_list, folder = "x", image_name = "img1", download = FALSE, plot = FALSE, verbose = FALSE),
    "No ROIs found"
  )
})


test_that("show_results: control flow (no image IO when df empty; lookup checked first)", {

  pkg_env <- environment(show_results)

  # Case 1: empty df must stop BEFORE image/mask is searched
  testthat::local_mocked_bindings(
    .check_folder = function(folder) invisible(TRUE),
    .list_images_sorted = function(folder) data.frame(norm = "img1", stringsAsFactors = FALSE),

    # if this gets called, the function went too far -> fail test
    .show_find_image_and_mask = function(folder, image_name) {
      stop("BUG: should not try to locate images when df is empty", call. = FALSE)
    },

    .env = pkg_env
  )

  empty_results <- list(img1 = data.frame())
  expect_error(
    show_results(empty_results, folder = "x", image_name = "img1",
                 download = FALSE, plot = FALSE, verbose = FALSE),
    "No ROIs found"
  )

  # Case 2: lookup gate is checked BEFORE results_list name gate
  testthat::local_mocked_bindings(
    .check_folder = function(folder) invisible(TRUE),

    # lookup does NOT contain img1
    .list_images_sorted = function(folder) data.frame(norm = "other", stringsAsFactors = FALSE),

    # must never be reached in this scenario
    .show_find_image_and_mask = function(folder, image_name) {
      stop("BUG: should not try to locate images when lookup fails", call. = FALSE)
    },

    .env = pkg_env
  )

  # results_list DOES contain img1, but lookup doesn't -> expect lookup error (not results_list error)
  results_list <- list(img1 = data.frame(ROI_ID = 1, stringsAsFactors = FALSE))

  expect_error(
    show_results(results_list, folder = "x", image_name = "img1",
                 download = FALSE, plot = FALSE, verbose = FALSE),
    "No results found for image"
  )
})
