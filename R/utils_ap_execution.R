# helper functions uses by "analysis_pip()' to execute image analysis methods
# and collect theri results in a unified format
#
# no exported functions
#
#
# .ap_run_method()
# execture the selected image analysis method on a set of images. Acts as a
# unified dispatcher for 'Cellpose'- and 'BiopixR'-based analysis pipelines.
# Handles method-spcific execution logic, optional unit conversion and verbose
# progress reporting. Returns the raw result object produced by the underlying
# analysis function or NULL if no images are provided
.ap_run_method <- function(method,
                           folder,
                           files,
                           use_conversion,
                           scale_info,
                           verbose = TRUE,
                           gpu = TRUE,
                           method_bp) {

  # skip execution if no images assigned to method
  if(length(files) == 0) {
    return(NULL)
  }

  # skip execution if no images assigned to method
  if(verbose) {
    cat("\nStarting ", method, " analysis...\n", sep = "")
  }

  # dispatch to Cellpose pipeline
  if(method == "Cellpose") {
    return(run_cellpose(folder = folder,
                        selected_files = files,
                        conversion = use_conversion,
                        scale_info = scale_info,
                        gpu = gpu))
  }

  # dispatch to BiopixR pipeline
  if(method == "BiopixR") {
    return(run_biopixR(folder = folder,
                       selected_files = files,
                       conversion = use_conversion,
                       scale_info = scale_info,
                       method = method_bp))
  }

  # fail fast for unsupported method
  stop("Unknown analysis method: ", method, call. = FALSE)
}
#
#
# .ap_collect_results()
# collect and merge results returned by the individual analysis methods,
# Combines per-image result tables for pixel-level and optionally converted
# measurements into unified result lists
.ap_collect_results <- function(results_cellpose,
                               results_biopixr,
                               use_conversion) {

  # initialize containers for merged results
  pixel_all <- list()
  converted_all <- list()

  # merge pixel-level results from Cellpose
  if(!is.null(results_cellpose) && !is.null(results_cellpose$pixel)) {
    pixel_all <- c(pixel_all, results_cellpose$pixel)
  }

  # merge pixel-level results from BiopixR
  if(!is.null(results_biopixr) && !is.null(results_biopixr$pixel)) {
    pixel_all <- c(pixel_all, results_biopixr$pixel)
  }

  # merge converted results only if conversion was requested
  if(use_conversion) {

    # from Cellpose
    if(!is.null(results_cellpose) && !is.null(results_cellpose$converted)) {
      converted_all <- c(converted_all, results_cellpose$converted)
    }

    # from BiopixR
    if(!is.null(results_biopixr) && !is.null(results_biopixr$converted)) {
      converted_all <- c(converted_all, results_biopixr$converted)
    }
  }

  # return unified result structure
  list(pixel = pixel_all,
       converted = if(use_conversion) converted_all else NULL)
}
