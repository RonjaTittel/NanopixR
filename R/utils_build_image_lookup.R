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
