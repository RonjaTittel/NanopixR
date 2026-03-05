
#' Run image-based object analysis using 'biopixR'
#'
#' Runs a complete 'biopixR' analysis pipeline on a folder of images.
#' The function prepares the images, performs object detection and optional
#' post-processing filters, extracts pixel- and region-level measurements and
#' optionally writes result tables to disk.
#'
#' @param folder Character string specifying the directory containing input images.
#' @param output_dir Character string specifying the output directory for results.
#'  Defaults to a sub directory named \code{"Results"} inside \code{folder}.
#' @param selected_files Optional character vector specifying a subset of images
#'  to analyze. If \code{NULL}, all images in \code{folder} are processed.
#'
#' @param method Character string. Object detection method to use.
#'  Either \code{"edge"} or \code{"threshold"}.
#' @param alpha Numeric value or \code{"gaussian"}. Edge detection paramter
#'  used when \code{method = "edge"}.
#' @param sigma Numeric value or \code{"gaussian"}. Smoothing paramter
#'  used when \code{method = "edge"}.
#'
#' @param use_sizefilter Logical. If \code{TRUE}, applies size-based
#'  object filter after detection.
#' @param size_lowerlimit Numeric value or \code{"auto"}. Lower size limit
#'  for the size filter detection.
#' @param size_upperlimit Numeric value or \code{"auto"}. Upper size limit
#'  for the size filter detection.
#'
#' @param use_proxfilter Logical. If \code{TRUE}, applies a proximity-based
#'  object filter after detection.
#' @param prox_radius Numeric value or \code{"auto"}. Radius parameter
#'  for the proximity filter detection.
#' @param prox_elongation Numeric value. Elongation parameter for the
#'  proximity filter detection.
#'
#' @param conversion Logical; whether to convert pixel-based ROI measurements to
#'  physical units. Defaults to \code{FALSE}.
#' @param scale_info Named list containing image-specific scale information
#'  required for unit conversion. Must be provided f \code{conversion = TRUE}.
#'
#' @param write_results Logical. If \code{TRUE}, writes result tables to disk
#'  as CSV files.
#'
#' @returns A list with the following elements:
#'  \describe{
#'    \item{pixel}{Named list of data frames containing pixel based
#'      measurements for each image.}
#'    \item{converted}{Named list of data frames containing converted
#'      measurements, or \code{NULL} if conversion is disabled.}
#'    \item{parameters}{List of analysis parameters used for the run.}
#'    }
#'
#' @details
#' The analysis is performed independently for each image. Errors occuring during
#' the processing of a single do not abort the entire run.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' img_dir <- system.file("images", package = "NanopixR")
#'
#' results <- run_biopixR(
#' folder = img_dir,
#' method = "edge",
#' alpha = "gaussian",
#' sigma = "gaussian",
#' write_results = FALSE)
#' }
run_biopixR <- function(folder,
                        output_dir = file.path(folder, "Results"),
                        selected_files = NULL,
                        # method
                        method = c("edge", "threshold"),
                        alpha = "gaussian",
                        sigma = "gaussian",
                        # size filter
                        use_sizefilter = FALSE,
                        size_lowerlimit = NULL,
                        size_upperlimit = NULL,
                        # proximity filter
                        use_proxfilter = FALSE,
                        prox_radius = NULL,
                        prox_elongation = NULL,
                        # scaling / conversion
                        conversion = FALSE,
                        scale_info = NULL,
                        # output
                        write_results = TRUE
                        ) {

  # validate input folder
  .check_folder(folder)

  # supported image file pattern
  pattern = "\\.(png|jpg|jpeg|tif|tiff|bmp)$"

  # default output file names
  csv_name_pixel = "Results_biopixR_pixel.csv"
  csv_name_converted = "Results_biopixR_converted.csv"

  # ensure output directory exists
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # prepare image table and apply optional file selection
  img_table <- .bp_prepare_image_table(folder = folder,
                                       pattern = pattern,
                                       selected_files = selected_files)

  files <- img_table$file
  norm_names <- img_table$norm

  # validate and normalize detection method
  method <- match.arg(method)

  params <- .bp_validate_detection_params(method = method,
                                          alpha = alpha,
                                          sigma = sigma)

  method = params$method
  alpha = params$alpha
  sigma = params$sigma

  # validate size filter configuration
  size_filter <- .bp_validate_size_filter(use = use_sizefilter,
                                          lowerlimit = size_lowerlimit,
                                          upperlimit = size_upperlimit)

  # validate proximity filter configuration
  prox_filter <- .bp_validate_proximity_filter(use = use_proxfilter,
                                               radius = prox_radius,
                                               elongation = prox_elongation)

  # store normalized parameter set for traceability
  params <- list(folder = normalizePath(folder),
                 method = method,
                 alpha = alpha,
                 sigma = sigma,
                 size_filter = size_filter,
                 proximity_filter = prox_filter)

  # initialize containers for per-image results
  Results_pixel <- list()
  Results_converted <- list()

  # process each image sequentially
  for(i in seq_along(files)) {
    res <- tryCatch(.bp_process_one_image(img_path = files[i],
                                           normname = norm_names[i],
                                           method = method,
                                           alpha = alpha,
                                           sigma = sigma,
                                           size_filter = size_filter,
                                           prox_filter = prox_filter,
                                           output_dir = output_dir,
                                           conversion = conversion,
                                           scale_info = scale_info))

    # store results if processing succeeded
    if(!is.null(res)) {
      Results_pixel[[norm_names[i]]] <- res$pixel

      if(!is.null(res$converted)) {
        Results_converted[[norm_names[i]]] <- res$converted
      }
    }
  }

  # optionally write combined CSV result tables
  if(write_results) {.bp_write_results_csv(results_pixel = Results_pixel,
                                           results_converted = Results_converted,
                                           output_dir = output_dir,
                                           csv_name_pixel = csv_name_pixel,
                                           csv_name_converted = csv_name_converted,
                                           write_pixel =  TRUE,
                                           write_converted = isTRUE(conversion))
  }

  # return final result structure
  final_output <- list(pixel = Results_pixel,
                       converted = if(isTRUE(conversion)) Results_converted else NULL,
                       parameters = params)
  invisible(final_output)
}
