library(CellpixR)
library(testthat)


test_that("extract_image_features errors on missing folder", {
  expect_error(extract_image_features("definitely_not_existing"), "does not exist|folder", ignore.case = TRUE)
})

test_that("extract_image_features stops when folder has no readable images", {
  skip_if_not_installed("biopixR")
  skip_if_not_installed("imager")

  tmp <- withr::local_tempdir()

  # keine Bilder drin -> .ex_load_images stoppt (oder später no readable images)
  expect_error(
    extract_image_features(tmp, verbose = FALSE),
    "No image files|No readable images",
    ignore.case = TRUE
  )
})

test_that("extract_image_features returns correct class + structure + values", {

  skip_if_not_installed("biopixR")
  skip_if_not_installed("imager")
  skip_if_not_installed("tibble")

  # --- Bilder aus inst/images ---
  pkg_root <- testthat::test_path("..", "..")
  img_dir  <- file.path(pkg_root, "inst", "images")
  skip_if_not(dir.exists(img_dir), "inst/images not found.")

  res <- extract_image_features(
    folder = img_dir,
    export_csv = FALSE,
    round_digits = 5,
    verbose = FALSE
  )

  # 1) Klasse
  expect_s3_class(res, "extract_image_features")
  expect_true(inherits(res, "data.frame"))

  # 2) Zeilen + Spalten
  expected_cols <- c(
    "Image_name",
    "Color_mode",
    "GS_Diff",
    "skewness",
    "kurtosis",
    "mean_grad",
    "median_grad",
    "edge_density",
    "mean_power",
    "high_freq_ratio",
    "Recommended_method",
    "Decision_justification"
  )
  expect_equal(names(res), expected_cols)

  # Wenn du GENAU diese 4 Bilder erwartest:
  expect_equal(nrow(res), 4)

  # --- stabile Sortierung für Vergleich ---
  res <- res[order(res$Image_name), , drop = FALSE]
  rownames(res) <- NULL

  # 3) Werte identisch (per Hand eingetragen)
  #    -> trage hier die Werte aus deinem Screenshot/CSV ein
  expected <- tibble::tibble(
    Image_name = c(
      "beads2",
      "ch_097_5",
      "ch_600_10_no_scale_bar",
      "droplets_beads3"
    ),
    Color_mode = c(
      "grayscale (3 identical channels)",
      "grayscale (3 identical channels)",
      "grayscale (3 identical channels)",
      "color"
    ),
    GS_Diff = c(0.00000, 0.00000, 0.00000, 0.01956),
    skewness = c(3.084, -0.883, -0.193, 5.117),
    kurtosis = c(11.376, 2.773, 2.986, 28.989),
    mean_grad = c(0.00542, 0.03549, 0.07876, 0.00278),
    median_grad = c(0.00000, 0.03010, 0.07326, 0.00000),
    edge_density = c(0.09977, 0.09989, 0.09997, 0.08461),
    mean_power = c(11622.475, 586288.746, 301803.033, 5092.302),
    high_freq_ratio = c(1.00000, 0.99999, 0.99994, 1.00000),
    Recommended_method = c("BiopixR", "Cellpose", "Cellpose", "BiopixR"),
    Decision_justification = c(
      "Skewness -> BiopixR, Kurtosis -> BiopixR, mean_grad -> BiopixR, median_grad -> BiopixR, mean_power -> BiopixR",
      "Skewness -> Cellpose, Kurtosis -> Cellpose, mean_grad -> Cellpose, median_grad -> Cellpose, mean_power -> Cellpose",
      "Skewness -> Cellpose, Kurtosis -> Cellpose, mean_grad -> Cellpose, median_grad -> Cellpose, mean_power -> Cellpose",
      "Skewness -> BiopixR, Kurtosis -> BiopixR, mean_grad -> BiopixR, median_grad -> BiopixR, mean_power -> BiopixR"
    )
  )

  # --- compare numerics with tolerance (float safety) ---
  # Rundung ist eh auf 5, aber je nach Import/Backend kann’s minimal abweichen.
  num_cols <- names(expected)[vapply(expected, is.numeric, logical(1))]
  for (cc in num_cols) {
    expect_equal(res[[cc]], expected[[cc]], tolerance = 1e-5)
  }

  # --- compare non-numerics exact ---
  char_cols <- setdiff(names(expected), num_cols)
  for (cc in char_cols) {
    expect_equal(as.character(res[[cc]]), as.character(expected[[cc]]))
  }
})


test_that("extract_image_features exports a CSV when export_csv=TRUE", {

  skip_if_not_installed("biopixR")
  skip_if_not_installed("imager")

  pkg_root <- testthat::test_path("..", "..")
  src_dir  <- file.path(pkg_root, "inst", "images")
  skip_if_not(dir.exists(src_dir), "inst/images not found.")

  # Wir schreiben NICHT nach inst/images, sondern kopieren nach temp
  tmp <- withr::local_tempdir()

  img_files <- list.files(
    src_dir,
    pattern = "\\.(png|jpg|jpeg|tif|tiff|bmp)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  skip_if_not(length(img_files) > 0, "No supported images in inst/images.")
  file.copy(img_files, tmp)

  out_name <- "Properties_test.csv"
  out_path <- file.path(tmp, out_name)

  res <- extract_image_features(
    folder = tmp,
    export_csv = TRUE,
    csv_name = out_name,
    round_digits = 5,
    verbose = FALSE
  )

  expect_true(file.exists(out_path))

  # optional: CSV content equals return (after sorting + rounding)
  on_disk <- utils::read.csv(out_path, stringsAsFactors = FALSE, check.names = FALSE)

  normalize <- function(x) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    if ("Image_name" %in% names(x)) {
      x <- x[order(x$Image_name), , drop = FALSE]
      rownames(x) <- NULL
    }
    num_cols <- names(x)[vapply(x, is.numeric, logical(1))]
    for (cc in num_cols) x[[cc]] <- round(x[[cc]], 5)
    x
  }

  expect_equal(normalize(on_disk), normalize(res))
})
