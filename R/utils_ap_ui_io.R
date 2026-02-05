# helper functions used by 'analysis_pip()' to provide optional user feedback
# and progress summaries during interactive execution and to export analysis
# results to disk
#
# no exported functions
#
#
# .ap_print_summary()
# print a concise summary of planned image analysis. Reports the number of
# images assigned to each analysis method and those skipped prior to execution
.ap_print_summary <- function(cellpose,
                              biopixr,
                              skipped) {
  cat("\n=====================================================\n")
  cat("Summary of planned analyses\n")
  cat("-----------------------------------------------------\n")
  cat("Cellpose: ", length(cellpose), " images\n", sep = "")
  cat("BiopixR: ", length(biopixr), " images\n", sep = "")
  cat("Skipped: ", length(skipped), " images\n", sep = "")
  cat("=====================================================\n\n")
}
#
#
# .ap_write_csv_results()
# write the combined analysis results of 'analysis_pip()' to disk. Exports
# pixel-level and optionally converted result tables as CSV files to a user-
# specified output directory
.ap_write_csv_results <- function(df_pixel,
                                  df_converted,
                                  out_dir,
                                  verbose = TRUE) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  if(!is.null(df_pixel)) {
    pixel_csv <- file.path(out_dir, "Analysis_pixel.csv")
    utils::write.csv(df_pixel, pixel_csv, row.names = FALSE)

    if(verbose) {
      cat("Saved combined pixel table:\n ", pixel_csv, "\n", sep = "")
    }
  }

  if(!is.null(df_converted)) {
    conv_csv <- file.path(out_dir, "Analysis_converted.csv")
    utils::write.csv(df_converted, conv_csv, row.names = FALSE)

    if(verbose) {
      cat("Saved combinde converted table:\n ", conv_csv, "\n", sep = "")
    }
  }

  invisible(TRUE)
}
