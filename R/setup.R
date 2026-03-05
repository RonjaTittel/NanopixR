
#' Interactive environment setup
#'
#'Runs an interactive setup assistant to configure the Python / *Cellpose*
#'environment required for *NanopixR*. This function is intended for interactive
#'use only.
#'
#' @return Invisibly returns TRUE if the setup is completed successfully
#' @export
setup <- function() {

  # ensure function is only executed interactively
  if(!interactive()) {
    stop("setup() must be run in an interactive R session", call. = FALSE)
  }

  # locate bundled setup script inside package
  script <- system.file("scripts", "setup_env.R", package = "NanopixR")
  if( script == "") {
    stop("Setup script not found", call. = FALSE)
  }

  # load script into isolated temporary environment
  # avoids polluting the global workspace
  env <- new.env(parent = baseenv())
  sys.source(script, envir = env)

  # ensure expected setup function exists in script
  if(!exists("setup_env", envir = env,  inherits = FALSE)) {
    stop("setup_env() not found in setup script", call. = FALSE)
  }

  # execute setup routine
  env$setup_env()
}
