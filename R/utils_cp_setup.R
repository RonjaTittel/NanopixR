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
# .cp_use_python_from_option()
# Configures Python
# If the option 'cellpose.python' is set and points to an existing Python executable, it
# is activatet via 'reticulate::use_python()'.
.cp_use_python_from_option <- function() {
  # read configured Python path from global option
  python_path <- getOption("cellpose.python", NULL)
  # activate specified Python environment if valid
  if(!is.null(python_path) && file.exists(python_path)) {
    # required = TRUE forces reticulate to bind to this interpreter
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
    # import required Python modules
    cellpose = reticulate::import("cellpose"),
    models = reticulate::import("cellpose.models"),
    io = reticulate::import("cellpose.io"),
    np = reticulate::import("numpy")
  )},
  error = function(e) {
    # provide clean, user-facing error message if import fails
    stop("Cellpose is not available in the configured Python environment.\n",
         "Please restart the R Session and run setup() to configure Python and Cellpose correctly.\n",
         "original error:\n", e$message, call. = FALSE)
  })
}
#
#
# .cp_analysis_method_text ()
# Constructs a string describing the analysing method
# Creates a description of the Cellpose analysis settings, including the model type and the object diameter.
# For reproducible results.
.cp_analysis_method_text <- function(model_path = NULL,
                                     diameter = NULL) {
  # describe model source (standard vs custom model file)
  model_part <- if(is.null(model_path)) {
    "Standard" } else {
      paste0("Custom (", basename(model_path), ")")
    }
  # describe diameter setting (auto-estimation vs fixed value)
  diameter_part <- if(is.null(diameter)) {
    "auto" } else {
      # construct reproducible method descriptor
      paste0(diameter, " px")
    }
  paste0("Cellpose | Model: ", model_part, " | Diameter: ", diameter_part)
}
