# Help functions for the 'run_cellpose()' and 'run_biopixR()' main function
# compute geometric properties of segmented ROIs
# calculates the geometric features for each labeld region in an segmentation mask
# perimeter, circulaity, minimum and maximum diameter and center cordinates
.compute_roi_geometry <- function(mask_matrix) {
  mask_matrix[is.na(mask_matrix)] <- 0L
  lab <- mask_matrix
  storage.mode(lab) <- "integer"
  ids <- sort(unique(lab[lab > 0L]))

  if(!length(ids)) {
    return(data.frame(
      ROI_ID = integer(0),
      Perimeter = numeric(0),
      Circularity = numeric(0),
      Min_Diameter = numeric(0),
      Max_Diameter = numeric(0),
      Centroid_X = numeric(0),
      Centroid_Y = numeric(0)
    ))
  }

  mask_img <- EBImage::Image(lab, dim = dim(mask_matrix), colormode = "Grayscale")

  feat_shape <- as.data.frame(EBImage::computeFeatures.shape(mask_img))
  feat_shape$ROI_ID <- as.numeric(rownames(feat_shape))

  feat_moment <- as.data.frame(EBImage::computeFeatures.moment(mask_img))
  feat_moment$ROI_ID <- as.numeric(rownames(feat_moment))

  get_perimeter <- function(id) {
    bin <- mask_img == id
    ctr <- EBImage::ocontour(bin)
    if(!length(ctr)) return(NA_real_)
    k <- which.max(sapply(ctr, nrow))
    xy <- ctr[[k]]
    sum(sqrt(diff(xy[, 1])^2 + diff(xy[, 2])^2))
  }

  perims <- vapply(ids, get_perimeter, numeric(1))
  featP <- data.frame(ROI_ID = ids, Perimeter = as.numeric(perims))

  merged <- merge(feat_shape, featP, by = "ROI_ID", all.x = TRUE)
  merged <- merge(merged, feat_moment[, c("ROI_ID", "m.cx", "m.cy")], by = "ROI_ID", all.x = TRUE)

  ok <- is.finite(merged$Perimeter) & merged$Perimeter > 0 & merged$s.area > 0

  out <- data.frame(
    ROI_ID = merged$ROI_ID,
    Perimeter = merged$Perimeter,
    Circularity = ifelse(ok, (4 * pi * merged$s.area) / (merged$Perimeter^2), NA_real_),
    Min_Diameter = 2 * merged$s.radius.min,
    Max_Diameter = 2 * merged$s.radius.max,
    Centroid_X = merged$m.cx,
    Centroid_Y = merged$m.cy
  )

  return(out)
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
