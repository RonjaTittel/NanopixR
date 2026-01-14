# Normalizing file names to a consistent and comparable format

.normalize_name <- function(path_or_name) {
  if(!is.character(path_or_name)) {
    stop("path_or_name must be a single character vector", call. = FALSE)
  }

  x <- basename(path_or_name)
  x <- tolower(x)
  tools::file_path_sans_ext(x)
}
