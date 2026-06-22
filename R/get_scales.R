
#' Extract scale information from DM3 files associated with each image
#'
#' Reads the pixel size and unit information from DM3 files associated with the
#' image files in a folder and returns the scale information converted to
#' millimeters per pixel.
#'
#' @param folder Path to the folder containing the image files and corresponding DM3 files
#' @return A named list. Each element corresponds to an image and contains:
#' \describe{
#'  \item{scale_x}{Pixel size in the x direction.}
#'  \item{scale_y}{Pixel size in the y direction.}
#'  \item{unit_x}{Unit of pixel size in the x direction.}
#'  \item{unit_y}{Unit of pixel size in the y direction.}
#'  \item{mm_per_pixel_x}{Pixel size in millimeters in the x direction.}
#'  \item{mm_per_pixel_y}{Pixel size in millimeters in the y direction.}
#'  \item{source}{Source of the scale information.}
#'  }
#' @export
get_scales <- function(folder) {

  # validate input folder
  .check_folder(folder)

  # validate input folder
  if(!reticulate::py_available(initialize = FALSE)) {
    stop("Python is not configured. Please restart the R Session and run setup() first.", call. = FALSE)
  }

  # ensure required Python module is available
  if(!reticulate::py_module_available("ncempy")) {
    stop("Python module 'ncempy' is not available. Please install and configure einvironment.", call. = FALSE)
  }

  # import Digital Micrograph reader module
  dm_mod <- reticulate::import("ncempy.io.dm", delay_load = TRUE)

  # collect supported image files
  image_files <- list.files(
    folder,
    pattern = "\\.(png|jpg|jpeg|tif|tiff|bmp)$",
    ignore.case = TRUE,
    full.names = TRUE
  )

  # return empty list if no images found
  if(length(image_files) == 0) {
    return(list())
  }

  results <- list()

  # iterate over all image files
  for(img_path in image_files) {
    img_name <- basename(img_path)
    base <- tools::file_path_sans_ext(img_name)
    base_norm <- .normalize_name(base)
    # expected corresponding DM3 file
    dm3_path <- file.path(folder, paste0(base, ".dm3"))

    # handle missing DM3 file
    if(!file.exists(dm3_path)) {
      results[[base_norm]] <- list(
        scale_x = NA,
        scale_y = NA,
        unit_x = NA,
        unit_y = NA,
        mm_per_pixel_x = NA,
        mm_per_pixel_y = NA,
        source = "no_dm3_found"
      )
      next
    }

    # attempt to load DM3 file
    dm_obj <- tryCatch({
      dm_mod$dmReader(dm3_path)
    }, error = function(e) NULL)

    # handle read failure
    if(is.null(dm_obj)) {
      results[[base_norm]] <- list(
        scale_x = NA,
        scale_y = NA,
        unit_x = NA,
        unit_y = NA,
        mm_per_pixel_x = NA,
        mm_per_pixel_y = NA,
        source = "dm3_read_error"
      )
      next
    }

    # extract pixel size and units
    scale <- tryCatch(dm_obj[['pixelSize']], error = function(e) NA)
    units <- tryCatch(dm_obj[['pixelUnit']], error = function(e) NA)

    # validate metadata format
    if(length(scale) != 2 || length(units) != 2) {
      results[[base_norm]] <- list(
        scale_x = NA,
        scale_y = NA,
        unit_x = NA,
        unit_y = NA,
        mm_per_pixel_x = NA,
        mm_per_pixel_y = NA,
        source = "invalid_format"
      )
      next
    }

    # convert scale values to numeric
    scale_x <- suppressWarnings(as.numeric(scale[[1]]))
    scale_y <- suppressWarnings(as.numeric(scale[[2]]))
    unit_x <- as.character(units[[1]])
    unit_y <- as.character(units[[2]])

    # convert units to millimeter scale factors
    fx <- .unit_scale_factor(unit_x)
    fy <- .unit_scale_factor(unit_y)

    # compute millimeter per pixel values
    mm_x <- if(!is.na(fx)) scale_x * fx else NA
    mm_y <- if(!is.na(fy)) scale_y * fy else NA

    # store result for current image
    results[[base_norm]] <- list(
      scale_x = scale_x,
      scale_y = scale_y,
      unit_x = unit_x,
      unit_y = unit_y,
      mm_per_pixel_x = mm_x,
      mm_per_pixel_y = mm_y,
      source = if(is.na(fx) || is.na(fy)) {
        "dm3_pixelSize_pixelUnit_unknown_unit"
        } else {"dm3_pixelSize_pixelUnit"}
    )
  }
  results
}
