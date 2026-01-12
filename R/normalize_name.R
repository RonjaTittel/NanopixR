#' Normalizing a file name
#'
#' Extracting the base name from a file path,
#' Converting it to lower case and
#' Removing the file extension.
#'
#' @param path_or_name Character string containing a file name or a file path
#' @return A normalized, lowercase file name without extensions
#' @export
#'
#' @examples
#' normalize_name("This_Is_A_file_NAME_01.PNG")
#'
#' normalize_name("path/to/data/This_Is_A_file_NAME_02.PNG")
#'
normalize_name <- function(path_or_name) {
  if(!is.character(path_or_name)) {
    stop("path_or_name must be a single character string", call. = FALSE)
  }

  x <- basename(path_or_name)
  x <- tolower(x)
  tools::file_path_sans_ext(x)
}
