# Help functions for the 'run_cellpose()' main function
# for 'Cellpose' setup and environment handling
#
# helper function used by 'run_cellpose()' to validate inputs,
# configure the Python/ 'Cellpose' environment and construct
# descriptive metadata about the analysis setup
#
# No exported functions
#
#
# .cp_check_folder()
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
# .cp_use_python_from_option()
# Configures Python
# If the option 'cellpose.python' is set and points to an existing Python executable, it
# is activatet via 'reticulate::use_python()'.
.cp_use_python_from_option <- function() {
  python_path <- getOption("cellpose.python", NULL)
  if(!is.null(python_path) && file.exists(python_path)) {
    reticulate::use_python(python_path, required = TRUE)
  }
  invisible(NULL)
}
#
#
# .cp_import_cellpose_modules()
# Imports the required Cellpose Python moduls
# Importing 'Cellpose' and 'NumPy' Python moduls via reticulate. Stops if 'Cellpose' is not
# available in the Python environment.
.cp_import_cellpose_modules <- function() {
  tryCatch({list(
    cellpose = reticulate::import("cellpose"),
    models = reticulate::import("cellpose.models"),
    io = reticulate::import("cellpose.io"),
    np = reticulate::import("numpy")
  )},
  error = function(e) {
    stop("Cellpose is not available in the configured Python environment.\n",
         "Please restart the R Session and run setup() to configure Python and Cellpose correctly.\n",
         "original error:\n", e$message, call. = FALSE)
  })
}
#
#
# .cp_validate_model_path ()
# Validating the custom Cellpose model path
# Checks if the provided path is a single character string and if it exists.
.cp_validate_model_path <- function(model_path) {
  if(is.null(model_path)) {
    return(NULL)}
  if(!is.character(model_path) || length(model_path) != 1) {
    stop("'model_path' must be NULL or a single character string.", call. = FALSE)
  }
  if(!file.exists(model_path)) {
    stop("The specified 'model_path' does not exist: ", model_path, call. = FALSE)
  }
  normalizePath(model_path, mustWork = TRUE)
}
#
#
# .cp_analysis_method_text ()
# Constructs a string describing the analysing method
# Creates a description of the Cellpose analysis settings, including the model type and the object diameter.
# For reproducible results.
.cp_analysis_method_text <- function(model_path = NULL,
                                     diameter = NULL) {
  model_part <- if(is.null(model_path)) {
    "Standard" } else {
      paste0("Custom (", basename(model_path), ")")
    }
  diameter_part <- if(is.null(diameter)) {
    "auto" } else {
      paste0(diameter, " px")
    }
  paste0("Cellpose | Model: ", model_part, " | Diameter: ", diameter_part)
}
