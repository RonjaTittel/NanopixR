# helper functions used by 'run_biopixR()' to prepare input images
# and process individual images during analysis
#
# no exported functions
#
#
# .bp_prepare_image_table()
# prepares a image table
# builds a reproducile lookup table of input image files combining file discovery,
# filename normalization and optional file selection
.bp_prepare_image_table <- function(folder,
                                    pattern,
                                    selected_files = NULL) {
  img_table <- .list_images_sorted(folder)

  if(nrow(img_table) == 0) {
    stop("No valid image files found in the folder.", call. = FALSE)
  }

  keep <- grepl(pattern, basename(img_table$file), ignore.case = TRUE)
  img_table <- img_table[keep, , drop = FALSE]

  if(nrow(img_table) == 0) {
    stop("No files match the specified filter.", call. = FALSE)
  }

  if(!is.null(selected_files)) {
    sel_norm <- .normalize_name(selected_files)
    idx <- match(sel_norm, img_table$norm)
    found <- !is.na(idx)

    if(!any(found)) {
      stop("None of the specified files were found. Normalized input: ",
           paste(unique(sel_norm), collapse = ", "), call. = FALSE)
    }

    img_table <- img_table[idx[found], , drop = FALSE]
  }

  img_table
}
#
#
# .bp_process_one_image()
# process a single image for 'run_biopixR()'
# performs object detection, optional filtering, feature extraction and mask
# generation for one input image
.bp_process_one_image <- function(img_path,
                                  normname,
                                  method,
                                  alpha, sigma,
                                  size_filter, prox_filter,
                                  output_dir,
                                  conversion = FALSE,
                                  scale_info = NULL) {
  img <- biopixR::importImage(img_path)

  if(method == "edge") {
    det <- biopixR::objectDetection(img,
                                    method = "edge",
                                    alpha = alpha,
                                    sigma = sigma,
                                    vis = FALSE)
  } else {
    det <- biopixR::objectDetection(img,
                                    method = "threshold",
                                    vis = FALSE)
  }

  if(size_filter$use) {
    det <- biopixR::sizeFilter(det$centers,
                               det$coordinates,
                               lowerlimit = size_filter$lowerlimit,
                               upperlimit = size_filter$upperlimit)
  }

  if(prox_filter$use) {
    det <- biopixR::proximityFilter(det$centers,
                                    det$coordinates,
                                    radius = prox_filter$radius,
                                    elongation = prox_filter$elongation)
  }

  coords <- det$coordinates
  w <- dim(img)[1]
  h <- dim(img)[2]
  m_lab <- matrix(0L, nrow = h, ncol = w)
  xy_ok <- !is.na(coords$x) & !is.na(coords$y)
  cx <- as.integer(coords$x[xy_ok])
  cy <- as.integer(coords$y[xy_ok])
  vv <- as.integer(coords$value[xy_ok])
  inside <- cx >= 1 & cx <= w & cy >= 1 & cy <= h
  m_lab[cbind(cy[inside], cx[inside])] <- vv[inside]

  mask_path <- file.path(output_dir, paste0(normname, "_bp_masks.tif"))
  lab_arr <- array(as.numeric(m_lab), dim = c(h, w, 1, 1))
  if(file.exists(mask_path)) file.remove(mask_path)
  ijtiff::write_tif(img = lab_arr,
                    path = mask_path,
                    bits_per_sample = 32,
                    compression = "none")


  img_gray <- apply(as.array(img), c(1, 2), mean)
  df <- data.frame( ROI_ID = as.vector(m_lab),
                    intensity = as.vector(img_gray))
  df <- df[df$ROI_ID > 0, ]
  stats <- stats::aggregate(df["intensity"], by = list(ROI_ID = df$ROI_ID), FUN = mean)
  names(stats)[2] <- "Mean_Intensity"
  stats$Area <- as.numeric(table(df$ROI_ID)[match(stats$ROI_ID, as.numeric(names(table(df$ROI_ID))))])

  geom <- .compute_roi_geometry(m_lab)
  stats <- merge(stats, geom, by = "ROI_ID", all.x = TRUE)

  stats$Image_name <- normname
  stats$Total_ROIs <- length(unique(m_lab[m_lab > 0]))
  stats$Overall_Confidence <- NA
  stats$Analysis_method <- .bp_build_analysis_method(method = method,
                                                     alpha = alpha,
                                                     sigma = sigma,
                                                     size_filter = size_filter,
                                                     prox_filter = prox_filter)
  stats <- stats[, c(
    "ROI_ID",
    "Centroid_X", "Centroid_Y",
    "Area",
    "Mean_Intensity",
    "Perimeter",
    "Circularity",
    "Min_Diameter", "Max_Diameter",
    "Image_name",
    "Total_ROIs",
    "Overall_Confidence",
    "Analysis_method"
  )]

  stats_conv <- NULL

  if(isTRUE(conversion) && !is.null(scale_info)) {
    stats_conv <- .convert_roi_units_single(stats,
                                            scale_info[[normname]])
  }

  list(pixel = stats,
       converted = stats_conv)
}
