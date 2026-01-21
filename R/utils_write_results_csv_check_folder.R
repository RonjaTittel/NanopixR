# .write_results_csv()
# writes the analysis results to CSV files
# writes pixel-based and optionally converted ROI statistics to CSV files in the
# output diretory
.write_results_csv <- function(results_pixel,
                                  results_converted = NULL,
                                  output_dir,
                                  prefix) {

   if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  out <- list(pixel = NULL, converted = NULL)

  if(length(results_pixel)) {
    df_pixel <- do.call(rbind, results_pixel)
    out$pixel_csv <- file.path(output_dir, paste0(prefix, "_pixel.csv"))
    utils::write.csv(df_pixel, out$pixel_csv, row.names = FALSE)
  }

  if(!is.null(results_converted) && length(results_converted) > 0) {
    df_conv <- do.call(rbind, results_converted)
    out$conv <- file.path(output_dir, paste0(prefix, "_converted.csv"))
    utils::write.csv(df_conv, out$converted_csv, row.names = FALSE)
  }
  invisible(out)
}
#
#
# .check_folder()
# Validates the input image folder
# checks that the folder is a single character string and that it does exist
.check_folder <- function(folder) {
  if(!is.character(folder) || length(folder) != 1) {
    stop("'folder' must be a single character string.", call. = FALSE)
  }
  if(!dir.exists(folder)) {
    stop("The specified folder does not exist: ", folder, call. = FALSE)
  }
  invisible(TRUE)
}
#
#
# Mask path helpers
# .mask_path_cp()
# Building the path for 'Cellpose' masks files
.mask_path_cp <- function(normname, folder) {
  file.path(folder, "Results", paste0(normname, "_cp_masks.tif"))
}
#
# .mask_path_bp()
# Building the path for 'biopixR' masks files
.mask_path_bp <- function(normname, folder) {
  file.path(folder, "Results", paste0(normname, "_bp_masks.tif"))
}
