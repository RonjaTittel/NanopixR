# Help functions for the 'run_cellpose()' main function
# for ROI-based analysis of 'Cellpose' segmentation
#
# helper functions used by 'run_cellpose()' to compute geometric and intensity-based
# statistics for segmented regions of interest (ROIs) produced by 'Cellpose'
#
# No exported functions
#
#
# .cp_compute_roi_stats_single()
# compute the ROI statistics for a single image
# intensity-based, geometric and confidence-related statistics for all segmented
# regions in a single image
.cp_compute_roi_stats_single <- function(masks,
                                         flows,
                                         img,
                                         image_name,
                                         analysis_method_text) {

  # extract cell probability map from Cellpose output
  cellprob <- flows[[3]]

  # handle case where probability map has leading singleton dimension
  if(length(dim(cellprob)) == 3 && dim(cellprob)[1] == 1) {
    cellprob <- cellprob[1,,]
  }

  # apply sigmoid to convert logits to probabilities
  cellprob <- 1 / (1 + exp(-cellprob))

  # extract all non-background ROI IDs
  roi_ids <- sort(unique(as.vector(masks[masks > 0])))

  # return NULL if no objects were detected
  if(length(roi_ids) == 0) {
    return(NULL)
    }

  # flatten masks, image (mean over channels), and probabilities
  mask_flat <- as.vector(masks)
  img_flat <- as.vector(apply(as.array(img), c(1, 2), mean))
  prob_flat <- as.vector(cellprob)

  # assemble pixel-wise data frame
  df <- data.frame(
    mask = mask_flat,
    intensity = img_flat,
    prob = prob_flat
    )

  # keep only foreground pixels
  df <- df[df$mask > 0, ]

  # compute mean intensity and mean probability per ROI
  agg <- stats::aggregate(
    df[, c("intensity", "prob")],
    by = list(ROI_ID = df$mask),
    FUN = mean
    )

  # assemble core ROI statistics
  stats <- data.frame(
    ROI_ID = agg$ROI_ID,
    Mean_Intensity = agg$intensity,
    Area = as.numeric(table(df$mask)),
    Overall_Confidence = mean(agg$prob, na.rm = TRUE)
  )

  # add image-level metadata
  stats$Image_name <- image_name
  stats$Total_ROIs <- nrow(stats)

  # compute geometric descriptors (centroids, perimeter, circularity, etc.)
  geom <- .compute_roi_geometry(masks)

  # merge intensity-based and geometric features
  stats <- merge(stats, geom, by = "ROI_ID", all.x = TRUE)

  # annotate analysis method for reproducibility
  stats$Analysis_method <- analysis_method_text

  # enforce consistent column order
  desired_order <- c(
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
    )

  stats[, intersect(desired_order, names(stats)), drop = FALSE]
}
#
#
# .cp_compute_roi_stats
# compute the ROI statistics for multiple images
# applies the ROI-based statistical analysis to a list of 'Cellpose' segmentation
# results and returns per-image ROI statistics.
.cp_compute_roi_stats <- function(res_list,
                                  imgs,
                                  analysis_method_text,
                                  progress = FALSE) {
  n_imgs <- length(imgs)

  # initialize result list (one entry per image)
  Results_pixel <- vector("list", n_imgs)
  names(Results_pixel) <- names(imgs)

  # optional progress bar
  if(isTRUE(progress)) {
    pb <- utils::txtProgressBar(min = 0, max = n_imgs, style = 3)
    on.exit(close(pb), add = TRUE)
    }

  # iterate over all images
  for(i in seq_len(n_imgs)) {
    # extract masks and flow outputs from Cellpose result structure
    # (nested Python list → convert to R)
    masks <- reticulate::py_to_r(res_list[[i]][[1]][[1]])
    flows <- reticulate::py_to_r(res_list[[i]][[2]][[1]])

    # compute ROI-level statistics for single image
    stats <- .cp_compute_roi_stats_single(
      masks = masks,
      flows = flows,
      img = imgs[[i]],
      image_name = names(imgs)[i],
      analysis_method_text = analysis_method_text
      )

    # store result only if ROIs were detected
    if(!is.null(stats)) {
      Results_pixel[[names(imgs)[i]]] <- stats
      }

    if(isTRUE(progress)) {
      utils::setTxtProgressBar(pb, i)
      }
    }
Results_pixel
}
#
#
# .cp_convert_roi_units()
# convert ROI statistics for multiple images to physical units
# pixel based measurments (area, perimeter, diameters) into physical units
# using specific scale information
.cp_convert_roi_units <- function(Results_pixel,
                                  scale_info) {
  # initialize container for converted results
  Results_converted <- list()

  # iterate over all images with ROI statistics
  for(img_name in names(Results_pixel)) {
    # retrieve corresponding scale metadata for image
    info <- scale_info[[img_name]]
    stats <- Results_pixel[[img_name]]
    # convert pixel-based measures to physical units
    converted <- .convert_roi_units_single(stats, info)

    # store only successfully converted results
    if(!is.null(converted)) {
      Results_converted[[img_name]] <- converted
    }
  }
  Results_converted
}
