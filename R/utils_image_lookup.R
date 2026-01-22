#
# .list_images_sorted()
# Lists image files and returns them sorted by their normalized names for best reproducibility
.list_images_sorted <- function(folder) {
  lookup <- .build_image_lookup(folder)

  if(nrow(lookup) == 0) {
    return(lookup[ ,c("file", "norm"), drop = FALSE])
  }

  ord <- order(lookup$norm)
  lookup[ord, c("file", "norm"), drop = FALSE]
}
#
#
# .build_image_lookup()
# Building a lookup table that connects image files to their normalized names
.build_image_lookup <- function(folder) {
  if(!is.character(folder) || length(folder) !=1) {
    stop("folder must be a single character string", call. = FALSE)
  }

  if(!dir.exists(folder)) {
    stop("folder does not exist", call. = FALSE)
  }

  files <- list.files(
    folder,
    pattern = "\\.(png|jpg|jpeg|tif|tiff|bmp)$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  if(length(files) == 0) {
    return(data.frame(
      norm = character(0),
      file = character(0),
      base = character(0),
      stringsAsFactors = FALSE
    ))
  }

  base_names <- tools::file_path_sans_ext(basename(files))
  norm_names <- .normalize_name(base_names)

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
  if(!is.character(path_or_name)) {
    stop("path_or_name must be a character vector", call. = FALSE)
  }

  x <- basename(path_or_name)
  x <- tolower(x)
  tools::file_path_sans_ext(x)
}
