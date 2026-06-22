
#' Run a complete image analysis pipeline with automatic method selection
#'
#' @description
#' \code{analysis_pip()} executes a full image analysis workflow on a folder
#' of microscopy images. The function automatically extracts image features,
#' recommends an appropriate analysis method ('Cellpose' or 'BiopixR'),
#' optionally allows interactive overrides, executes the selected analyses and
#' returns combined result tables. Pixel-level results and, if scale information
#' is available, converted physical measurements can optionally be written to
#' disk.
#'
#' @param folder Character string specifying the directory containing input images.
#' @param write_csv Logical; if \code{TRUE}, combined result tables are written
#'  to CSV files in \code{out_dir}
#' @param out_dir Character string specifying the output directory for results.
#'  Defaults to a subdirectory named \code{"Results"} inside \code{folder}.
#' @param scale_info Logical; controls whether scale information should be used
#'  for unit conversion. If \code{TRUE}, available scale information is
#'  extracted automatically.
#' @param interactive Logical; if \code{TRUE}, the user may interactively
#'  override recommended analysis methods or skip images.
#' @param verbose Logical; controls console output and progress reporting.
#' @param gpu Logical; if \code{TRUE}, GPU acceleration is enabled for
#'  'Cellpose'-based analysis, if available.
#' @param method_bp Character string specifying the 'BiopixR' analysis method to
#'  use. Must be one of \code{"edge"} or \code{"threshold"}.
#'
#' @details
#' The function orchestrates feature extraction, method recommendation,
#' optional interactive decision-making, execution of analysis backends, and
#' result aggregation. The returned object provides both combined result tables
#' and per-image result lists for flexible downstream analysis.
#'
#'
#' @returns A named list with the following elements:
#' \describe{
#'  \item{pixel}{A data frame containing pixel-level measurements for all
#'   analyzed images, or \code{NULL} if no objects were detected.}
#'  \item{converted}{A data frame containing converted physical measurements
#'    (e.g. in nanometers), or \code{NULL} if unit conversion was not applied.}
#'  \item{by_image}{A list of per-image result tables, split by image name,
#'  containing pixel-level and, if applicable, converted measurements.}
#'  }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' img_dir <- system.file("images", package = "NanopixR")
#'
#' results <- analysis_pip(folder = img_dir,
#'   write_csv = TRUE,
#'   scale_info = TRUE,
#'   interactive = FALSE,
#'   gpu = TRUE,
#'   method_bp = "edge")
#'
#' head(results$pixel)
#' head(results$coverted)
#' }
analysis_pip <- function(folder,
                         write_csv = TRUE,
                         out_dir = file.path(folder, "Results"),
                         scale_info = FALSE,
                         interactive = FALSE,
                         verbose = TRUE,
                         gpu = TRUE,
                         method_bp = c("edge", "threshold")) {

  # validate input folder
  .check_folder(folder)

  # step 1: extract image feature properties
  if(verbose) cat("\nExtracting image features...\n")
  properties <- .ap_extract_features(folder)

  # validate extracted properties structure
  .ap_validate_properties(properties)

  # build lookup table linking normalized names to files
  lookup <- .build_image_lookup(folder)

  # resolve scale handling and unit conversion settings
  scale <- .ap_resolve_scale_handling(folder = folder,
                                      scale_info = scale_info,
                                      interactive = interactive,
                                      verbose = verbose)
  use_conversion <- scale$use_conversion
  scale_info <- scale$scale_info

  # group images according to recommended method
  groups <- .ap_group_images_by_method(properties,
                                       lookup)
  cellpose_imgs <- groups$Cellpose
  biopixr_imgs <- groups$BiopixR

  # resolve user decisions for Cellpose-recommended images
  res_cp <- .ap_resolve_group(images = cellpose_imgs,
                              recommended = "Cellpose",
                              interactive = interactive,
                              verbose = verbose)
  cellpose_to_run <- res_cp$Cellpose
  biopixR_to_run <- res_cp$BiopixR
  skipped_imgs <- res_cp$Skipped

  # resolve user decisions for BiopixR-recommended images
  res_bp <- .ap_resolve_group(images = biopixr_imgs,
                              recommended = "BiopixR",
                              interactive = interactive,
                              verbose = verbose)
  cellpose_to_run <- c(cellpose_to_run, res_bp$Cellpose)
  biopixR_to_run <- c(biopixR_to_run, res_bp$BiopixR)
  skipped_imgs <- c(skipped_imgs, res_bp$Skipped)

  # optionally print execution summary
  if(verbose && interactive) {
    .ap_print_summary(cellpose_to_run,
                      biopixR_to_run,
                      skipped_imgs)
  }

  # execute Cellpose analysis
  Results_Cellpose <- .ap_run_method(method = "Cellpose",
                                     folder = folder,
                                     files = cellpose_to_run,
                                     use_conversion = use_conversion,
                                     scale_info = scale_info,
                                     verbose = verbose,
                                     gpu = gpu,
                                     method_bp = method_bp)

  # execute BiopixR analysis
  Results_BiopixR <- .ap_run_method(method = "BiopixR",
                                    folder = folder,
                                    files = biopixR_to_run,
                                    use_conversion = use_conversion,
                                    scale_info = scale_info,
                                    verbose = verbose,
                                    gpu = gpu,
                                    method_bp = method_bp)

  # merge results from both analysis pipelines
  combined <- .ap_collect_results(results_cellpose = Results_Cellpose,
                                  results_biopixr = Results_BiopixR,
                                  use_conversion = use_conversion)
  pixel_all <- combined$pixel
  converted_all <- combined$converted

  # bind per-image pixel results into single data frame
  df_pixel <- if(length(pixel_all) > 0) {
    do.call(rbind, pixel_all)
  } else {NULL}

  # bind converted results if enabled
  df_converted <- if(use_conversion && length(converted_all) > 0) {
    do.call(rbind, converted_all)
  } else {NULL}

  # optionally write combined result tables to disk
  if(write_csv) {
    .ap_write_csv_results(df_pixel = df_pixel,
                          df_converted = df_converted,
                          out_dir = out_dir,
                          verbose = verbose)
  }

  # assemble structured return object
  results <- list(
    pixel = df_pixel,
    converted = if(use_conversion) df_converted else NULL,
    by_image = list(pixel = split(df_pixel, df_pixel$Image_name),
                    converted = if(use_conversion) {
                      split(df_converted, df_converted$Image_name)
                      } else {NULL}))

    return(results)
}
