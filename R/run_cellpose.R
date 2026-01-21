
#' Run 'Cellpose'-based image segmentation and ROI analysis
#'
#' Performing image segmentation using the 'Cellpose' deep learning framework and
#' computing region-of-interest (ROI) statistics for the detected objects. Optionally
#' Converting the results from pixel units to physical units and writting to disk.
#'
#' Workglow consisting of these steps:
#' \enumerate{
#' \item Load input images from directory.
#' \item Run 'Cellpose' segmentation on each image.
#' \item Optionally save segmentation masks ti disk.
#' \item Compute intensity-based and geometric ROI statistics.
#' \item Optionally convert ROI measurements to physical units.
#' \item Optionally write results to CSV files.
#' }
#'
#' @param folder Character string specifying the directory containing input images.
#' @param output_dir Character string specifying the output directory for results.
#'  Defaults to a sub directory named \code{"Results"} inside \code{folder}.
#' @param selected_files Optional character vector specifying a subset of images
#'  to analyze. If \code{NULL}, all images in \code{folder} are processed.
#'
#' @param gpu Logical; whether to enable GPU acceleration for 'Cellpose' (highly recommended)
#'  Defaults to \code{FALSE}.
#' @param diameter Numeric scalar specifying the approximate object diameter in  pixels.
#'  If \code{NULL}, 'Cellpose' automatically estimates the diameter.
#' @param model_path Optional path to a custom 'Cellpose' model. If \code{NULL}, the
#'  standard pretrained 'Cellpose' model is used.
#'
#' @param conversion Logical; whether to convert pixel-based ROI measurements to
#'  physical units. Defaults to \code{FALSE}.
#' @param scale_info Named list containing image-specific scale information
#'  required for unit conversion. Must be provided f \code{conversion = TRUE}.
#'
#' @param save_masks Logical; whether to save segmentation masks to disk.
#'  Defaults to \code{TRUE}.
#' @param save_csv Logical; whether to write ROI statistices to CSV files.
#'  Defaults to \code{TRUE}.
#'
#' @return A list containg the analysis results.
#' \describe{
#' \item{pixel}{A named list of data frames containing ROI statistics in pixel units.}
#' \item{converted}{A named list of data frames containing ROI statistics in
#'  physical units. Only returned if \code{conversion = TRUE}.}
#'  }
#'
#' @details
#' Segmentation is performed using the 'Cellpose' Python package via the \pkg{reticulte}
#' interface. ROI statistics include object area, mean intensity, perimeter,
#' circularity, minimum and maximum diameter, centroid coordinates and confidence
#' measures derived from 'Cellpose' outputs.
#'
#' Images for which no objects are detected are omitted from the results.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' img_dir <- system.file("images", package = "CellpixR")
#'
#' results <- run_cellpose(
#' folder = img_dir,
#' gpu = TRUE
#' )
#'
#' names(results$pixel)
#' head(results$pixel[[1]])
#' }
run_cellpose <- function(
    folder,
    output_dir = file.path(folder, "Results"),
    selected_files = NULL,
    # model & segmentation
    gpu = FALSE,
    diameter = NULL,
    model_path = NULL,
    # scaling / conversion
    conversion = FALSE,
    scale_info = NULL,
    # output
    save_masks = TRUE,
    save_csv = TRUE
) {
  .check_folder(folder)
  .cp_use_python_from_option()
  mods <- .cp_import_cellpose_modules()

  img_bundle <- .cp_load_images(
    folder = folder,
    io = mods$io,
    np = mods$np,
    selected_files = selected_files
  )

  imgs_np <- img_bundle$imgs_np
  imgs <- img_bundle$imgs
  files <- img_bundle$files

  if (!is.null(diameter)) {
    if(!is.numeric(diameter) || length(diameter) != 1 || diameter <= 0) {
      stop("'diameter' must be a single positiv number or NULL.", call. = FALSE)
    }
  }

  model <- .cp_create_model(
    models = mods$models,
    model_path = model_path,
    gpu = gpu
  )

  analysis_method_text <- .cp_analysis_method_text(
    model_path = model_path,
    diameter = diameter
  )

  res_list <- .cp_eval_images(
    model = model,
    imgs_np = imgs_np,
    diameter = diameter,
    progress = interactive()
  )

  if(isTRUE(save_masks)) {
    .cp_save_masks(
      io = mods$io,
      imgs_np = imgs_np,
      res_list = res_list,
      files = files,
      output_dir = output_dir
    )
  }

  Results_pixel <- .cp_compute_roi_stats(
    res_list = res_list,
    imgs = imgs,
    analysis_method_text = analysis_method_text,
    progress = interactive()
  )

  if(isTRUE(conversion) && is.null(scale_info)) {
    stop("'conversion = TRUE' requires 'scale_info' to be provided.", call. = FALSE)
  }

  Results_converted <- NULL

  if(isTRUE(conversion)) {
  Results_converted <- .convert_roi_units(
    Results_pixel = Results_pixel,
    scale_info = scale_info
  )
  }

  csv_paths <- NULL

  if(isTRUE(save_csv)) {
    csv_paths <- .write_results_csv(
      results_pixel = Results_pixel,
      results_converted = Results_converted,
      output_dir = output_dir,
      prefix = "Results_Cellpose"
    )
  }

  if(isTRUE(conversion)) {
    list(
      pixel = Results_pixel,
      converted = Results_converted
    )
  } else {
    list(
      pixel = Results_pixel
    )
  }
}
