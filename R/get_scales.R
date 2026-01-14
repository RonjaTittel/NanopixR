
#' Extracting the scale information from DM3 files belonging to each image
#'
#' Reads the pixel size and unit information from DM3 files associated with the
#' image files in a folder and returns the scale information converted to
#' millimeters per pixel
#'
#' @param folder Path to the folder containing the image files and the corresponding DM3 files
#' @return A named list. Each element corresponds to a image and contains:
#' \describe{
#'  \item{scale_x}{Pixel size in x direction}
#'  \item{scale_y}{Pixel size in y direction}
#'  \item{unit_x}{Unit of pixel size in x direction}
#'  \item{unit_y}{Unit of pixel size in y direction}
#'  \item{mm_per_pixel_x}{Pixel size in millimeters in x direction}
#'  \item{mm_per_pixel_y}{Pixel size in millimeters in y direction}
#'  \item{source}{Source of the scale information}
#'  }
#' @export
get_scales <- function(folder) {
  if(!dir.exists(folder)) {
    stop("Folder does not exist.", call. = FALSE)
  }
  if(!reticulate::py_available(initialize = FALSE)) {
    stop("Python is not configured. Please restart the R Session and run setup() first.", call. = FALSE)
  }
  if(!reticulate::py_module_available("ncempy")) {
    stop("Python module 'ncempy' is not available. Please install and configure einvironment.", call. = FALSE)
  }

  dm_mod <- reticulate::import("ncempy.io.dm", delay_load = TRUE)

  # getting Image files
  image_files <- list.files(
    folder,
    pattern = "\\.(png|jpg|jpeg|tif|tiff|bmp)$",
    ignore.case = TRUE,
    full.names = TRUE
  )

  if(length(image_files) == 0) {
    return(list())
  }

  results <- list()

  for(img_path in image_files) {
    img_name <- basename(img_path)
    base <- tools::file_path_sans_ext(img_name)
    base_norm <- .normalize_name(base)
    dm3_path <- file.path(folder, paste0(base, ".dm3"))

    # No DM3
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

    #DM3 loading
    dm_obj <- tryCatch({
      dm_mod$dmReader(dm3_path)
    }, error = function(e) NULL)

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

    # Pixel size and Pixel unit
    scale <- tryCatch(dm_obj[['pixelSize']], error = function(e) NA)
    units <- tryCatch(dm_obj[['pixelUnit']], error = function(e) NA)

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

    scale_x <- suppressWarnings(as.numeric(scale[[1]]))
    scale_y <- suppressWarnings(as.numeric(scale[[2]]))
    unit_x <- as.character(units[[1]])
    unit_y <- as.character(units[[2]])
    fx <- .unit_scale_factor(unit_x)
    fy <- .unit_scale_factor(unit_y)

    mm_x <- if(!is.na(fx)) scale_x * fx else NA
    mm_y <- if(!is.na(fy)) scale_y * fy else NA

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
