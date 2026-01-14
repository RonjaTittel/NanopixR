
#' Interactive environment setup
#'
#'Runs an interactive setup assistant to configure the Python / Cellpose
#'environment required for CellpixR. This function is intended for interactive
#'use only.
#'
#' @return Invisibly returns TRUE if the setup is completet sucsessfully
#' @export
setup <- function() {
  if(!interactive()) {
    stop("setup() ,ust be run in an interactive R session", call. = FALSE)
  }
  script <- system.file("scripts", "setup_env.R", package = "CellpixR")
  if( script == "") {
    stop("Setup script not found", call. = FALSE)
  }

  #Load the script into an temporary environment
  env <- new.env(parent = baseenv())
  sys.source(script, envir = env)

  if(!exists("setup_env", envir = env,  inherits = FALSE)) {
    stop("setup_env() not found in setup script", call. = FALSE)
  }
  env$setup_env()
}
