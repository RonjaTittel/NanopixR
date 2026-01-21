# Help functions for the 'run_cellpose()' and 'run_biopixR()' main function
# for converting ROI statistics to physical units
#
# helper functions used by 'run_cellpose()' and 'run_biopixR()' to convert pixel-based ROI measurements
# into physical units based on provided image scale information
#
# No exported functions
#
#
# .unit_scale_factor()
# Getting the factor according to the scale for unit conversion
.unit_scale_factor <- function(unit) {
  if (is.na(unit) || !nzchar(unit)) return(NA_real_)
  u <- tolower(unit)
  if (u == "nm")  return(1e-6)
  if (u %in% c("um", "\u00b5m")) return(1e-3)
  if (u == "mm")  return(1)
  return(NA_real_)
}
#
#
# .convert_roi_units_single()
# convert ROI statistics for a single image to physical units
# pixel based measurments (area, perimeter, diameters) into physical units
# using specific scale information
.convert_roi_units_single <- function(stats,
                                         scale_info) {

  if(is.null(stats) || is.null(scale_info)) {
    return(NULL)
  }

  if(is.na(scale_info$mm_per_pixel_x) || is.na(scale_info$mm_per_pixel_y)) {
    warning("Scale information incomplete - skipping conversion.", call. = FALSE)
    return(NULL)
  }

  unit_raw <- tolower(scale_info$unit_x)
  unit_factor <- switch(
    unit_raw,
    "nm" = 1e6,
    "um" = 1e3,
    "\u00b5m" = 1e3,
    "mm" = 1,
    NA
  )

  if(is.na(unit_factor)) {
    warning("Unknown unit'", unit_raw, "' - skipping conversion.", call. = FALSE)
    return(NULL)
  }

  fx <- scale_info$mm_per_pixel_x * unit_factor
  fy <- scale_info$mm_per_pixel_y * unit_factor
  stats_conv <- stats

  if("Area" %in% names(stats_conv)) {
    stats_conv$Area <- stats_conv$Area * fy * fx
    names(stats_conv)[names(stats_conv) == "Area"] <- paste0("Area_", unit_raw, "2")
  }

  for(col in c("Perimeter", "Min_Diameter", "Max_Diameter")) {
    if(col %in% names(stats_conv)) {
      stats_conv[[col]] <- stats_conv[[col]] * fx
      names(stats_conv)[names(stats_conv) == col] <- paste0(col, "_", unit_raw)
    }
  }
  stats_conv
}
#
#
# .convert_roi_units()
# convert ROI statistics for multiple images to physical units
# pixel based measurments (area, perimeter, diameters) into physical units
# using specific scale information
.convert_roi_units <- function(Results_pixel,
                                  scale_info) {
  Results_converted <- list()

  for(img_name in names(Results_pixel)) {
    info <- scale_info[[img_name]]
    stats <- Results_pixel[[img_name]]
    converted <- .convert_roi_units_single(stats, info)

    if(!is.null(converted)) {
      Results_converted[[img_name]] <- converted
    }
  }
  Results_converted
}
