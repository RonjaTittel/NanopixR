# helper functions used by 'run_biopixR()' to post-process and export analysis
# results
#
# no exported functions
#
#
# .bp_write_results_csv()
# write the analysis results of 'run_biopixR()' to disk. combines per-image result
# tables and writes pixel-level and optionally converted results as CSV-files
.bp_write_results_csv <- function(results_pixel,
                                  results_converted = NULL,
                                  output_dir,
                                  csv_name_pixel,
                                  csv_name_converted,
                                  write_pixel = TRUE,
                                  write_converted = TRUE) {

  if(write_pixel && length(results_pixel)) {
    df_pixel <- do.call(rbind, results_pixel)
    utils::write.csv(df_pixel,
                     file.path(output_dir, csv_name_pixel),
                     row.names = FALSE)
  }

  if(write_converted && !is.null(results_converted) && length(results_converted)) {
    df_conv <- do.call(rbind, results_converted)
    utils::write.csv(df_conv,
                     file.path(output_dir, csv_name_converted),
                     row.names = FALSE)
  }

  invisible(TRUE)
}
#
#
# .bp_build_analysis_method()
# analysis method description
# constructs a reproducible summary of the analysis configuration used for image processing
.bp_build_analysis_method <- function(method,
                                      alpha = NULL,
                                      sigma = NULL,
                                      size_filter,
                                      prox_filter) {
  parts <- c("BiopixR")

  if(method == "edge") {
    parts <- c(parts,
               paste0("method=edge"),
               paste0("alpha=", alpha),
               paste0("sigma=", sigma))
  } else {
    parts <- c(parts, "method=threshold")
  }

  if(size_filter$use) {
    parts <- c(parts,
               paste0("sizeFilter=", size_filter$lowerlimit, "-", size_filter$upperlimit))
  }

  if(prox_filter$use) {
    parts <- c(parts,
               paste0("proximityFilter=", prox_filter$radius, "-", prox_filter$elongation))
  }

  paste(parts, collapse = "; ")
}
