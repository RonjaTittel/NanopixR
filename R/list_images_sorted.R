#' List image files from an folder sorted by their normalized name
#'
#' Lists all image files in a folder and returns them sorted by their
#' normalized file names.
#'
#' @param folder Character string specifying a folder path. The folder should include image files.
#'
#' @return Date frame with the original file paths and the normalized names sorted by the normalized names
#' @export
#'
#' @examples
#' folder1 <- system.file("images", package = "CellpixR")
#' list_images_sorted(folder1)
#'
list_images_sorted <- function(folder) {
  if(!is.character(folder) || length(folder) != 1) {
    stop("folder must be a single character string", call. = FALSE)
  }

  if(!dir.exists(folder)) {
    stop("folder does not exist", call. = FALSE)
  }

  files <- list.files(
    folder,
    pattern = "\\.(tif|tiff|png|jpg|jpeg|bmp)$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if(length(files) == 0) {
    return(data.frame(
      file = character(0),
      norm = character(0),
      stringsAsFactors = FALSE
    ))
  }

  norm<- normalize_name(files)
  ord<- order(norm)

  data.frame(
    file = files[ord],
    norm = norm[ord],
    stringsAsFactors = FALSE
  )
}
