#
# .list_images_sorted()
# Lists image files and returns them sorted by their normalized names for best reproducibility
.list_images_sorted <- function(folder) {
  # build full lookup table (file, norm, base)
  lookup <- .build_image_lookup(folder)

  # if no images found, return empty subset with expected columns
  if(nrow(lookup) == 0) {
    return(lookup[ ,c("file", "norm"), drop = FALSE])
  }

  # order by normalized names to ensure stable processing order
  ord <- order(lookup$norm)
  # return only required columns (file + norm)
  lookup[ord, c("file", "norm"), drop = FALSE]
}
#
#
# .build_image_lookup()
# Building a lookup table that connects image files to their normalized names
.build_image_lookup <- function(folder) {

  # validate input folder
  .check_folder(folder)

  # list supported image files (case-insensitive)
  files <- list.files(
    folder,
    pattern = "\\.(png|jpg|jpeg|tif|tiff|bmp)$",
    full.names = TRUE,
    ignore.case = TRUE
  )


  # return empty lookup if no images found
  if(length(files) == 0) {
    return(data.frame(
      norm = character(0),
      file = character(0),
      base = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # extract base filenames (without extension)
  base_names <- tools::file_path_sans_ext(basename(files))
  # normalize names for robust matching (case-insensitive, no extension)
  norm_names <- .normalize_name(base_names)

  # construct lookup table:
  # norm → normalized name used for matching
  # file → full file path
  # base → original base filename (no extension)
  data.frame(
    norm = norm_names,
    file = files,
    base = base_names,
    stringsAsFactors = FALSE
  )
}
#
#
# .normalize_name()
# Normalizing file names to a consistent and comparable format
.normalize_name <- function(path_or_name) {
  # ensure input is character (paths or filenames expected)
  if(!is.character(path_or_name)) {
    stop("path_or_name must be a character vector", call. = FALSE)
  }

  # remove directory part (keep filename only)
  x <- basename(path_or_name)
  # enforce case-insensitive matching
  x <- tolower(x)
  # remove file extension to allow consistent name comparison
  tools::file_path_sans_ext(x)
}
#
#
# .check_folder()
# Validates the input image folder
# checks that the folder is a single character string and that it does exist
.check_folder <- function(folder) {
  if(!is.character(folder) || length(folder) != 1) {
    stop("'folder' must be a single character string.", call. = FALSE)
  }
  if(!dir.exists(folder)) {
    stop("The specified folder does not exist: ", folder, call. = FALSE)
  }
  # return TRUE invisibly to allow use in validation pipelines
  invisible(TRUE)
}
