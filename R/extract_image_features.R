
#' Extract image features and recommend an analysis method
#'
#' This function analzyses a folder of image files to extract interpretable
#' image characteristics based on intensity distributions, gradient
#' infromation and frequency-domain properties.
#'
#' The extracted features are combined into a properties table and used to
#' heuristically recommend an appropriate downstream image analysis method
#' (e.g BiopixR or Cellpose) for each image.
#'
#' The function does \emph{not} perform object detection or segmentation
#' itself. Insted, it provides a data-driven preperation step to
#' guide subsequent analysis workflows.
#'
#' @param folder Character string specifying the directory containing input images.
#' @param export_csv Logical. If \code{TRUE}, the resulting properties table
#'  is written to a CSV file in \code{folder}. Default is \code{FALSE}.
#' @param csv_name Character string. Name of the CSV file to be written when
#'  \code{export_csv = TRUE}. Default is \code{"Properties.csv"}.
#' @param round_digits Integer. Number of deicmal digits used to round numeric
#'  feature values in the output table. Default is \code{5}.
#' @param verbose Logical. If \code{TRUE}, progress messages are printed during
#'  image loading and analysis. Default is \code{TRUE}.
#'
#' @returns An object of class \code{"extract_image_features"} inheriting
#'  from \code{"data.frame"}.
#'
#' The returned data frame contains one row per image and includes:
#'  \itemize{
#'    \item normalized image names
#'    \item color mode information
#'    \item histogramm-based intensity features
#'    \item gradient- and frequency-based features
#'    \item a recommended analysis method
#'    \item a textual justification for the recommendation
#'    }
#'
#' @details
#' Feature extraction is performed using a combination of histogram statistics,
#' spatial gradient measures and frequency-domain descriptors. The final method
#' recommendation is derived from weighted voting heruistics based on these
#' features.
#'
#' Unreadable image files are skipped automatically.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # set folder
#' img_dir <- system.file("images", package = "CellpixR")
#'
#' # analyze the folder of microscopy images
#' res <- extract_image_features(img_dir)
#'
#' # inspect the recommended methods
#' table(res$Recommended_method)
#' }
extract_image_features <- function(folder,
                                   export_csv = FALSE,
                                   csv_name = "Properties.csv",
                                   round_digits = 5,
                                   verbose = TRUE) {

  .check_folder(folder)

  if(isTRUE(verbose)) {
    message("Loading images")
  }

  imgs <- .ex_load_images(folder)
  norm_names <- names(imgs)

  if(length(imgs) == 0) {
    stop("No readable images found in the folder.")
  }

  color_info <- t(sapply(imgs,
                         .ex_check_color_mode))

  hist_stats <- t(vapply(imgs,
                         .ex_analyze_hist_features,
                         FUN.VALUE = c(skewness = 0, kurtosis = 0)))

  results <- .ex_analyze_image_list(imgs,
                                    edge_q = 0.9,
                                    high_cut = 0.25,
                                    verbose = verbose)

  results_round <- .ex_round_numeric_df(results,
                                        digits = round_digits)

  Properties <- .ex_build_properties_table(norm_names = norm_names,
                                           color_info = color_info,
                                           hist_stats = hist_stats,
                                           results_round = results_round)

  Decision <- .ex_choose_method(Properties)

  Properties$Recommended_method <- Decision$Recommended_method
  Properties$Decision_justification <- Decision$Decision_justification

  if(isTRUE(export_csv)) {
    .ex_export_properties(Properties,
                          folder = folder,
                          csv_name = csv_name,
                          verbose = verbose)
  }

  if(isTRUE(verbose)) {
    message("Analysis completed")
  }

  class(Properties) <- c("extract_image_features", class(Properties))
  return(Properties)
}
