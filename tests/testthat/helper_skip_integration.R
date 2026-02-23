# Integration tests are opt-in (local only)
is_integration_enabled <- function() {
  identical(Sys.getenv("CELLPIXR_INTEGRATION"), "true")
}
