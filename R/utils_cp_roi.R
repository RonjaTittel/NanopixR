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
  cellprob <- flows[[3]]

  if(length(dim(cellprob)) == 3 && dim(cellprob)[1] == 1) {
    cellprob <- cellprob[1,,]
    }

  cellprob <- 1 / (1 + exp(-cellprob))
  roi_ids <- sort(unique(as.vector(masks[masks > 0])))

  if(length(roi_ids) == 0) {
    return(NULL)
    }

  mask_flat <- as.vector(masks)
  img_flat <- as.vector(apply(as.array(img), c(1, 2), mean))
  prob_flat <- as.vector(cellprob)

  df <- data.frame(
    mask = mask_flat,
    intensity = img_flat,
    prob = prob_flat
    )

  df <- df[df$mask > 0, ]

  agg <- stats::aggregate(
    df[, c("intensity", "prob")],
    by = list(ROI_ID = df$mask),
    FUN = mean
    )

  stats <- data.frame(
    ROI_ID = agg$ROI_ID,
    Mean_Intensity = agg$intensity,
    Area = as.numeric(table(df$mask)),
    Overall_Confidence = mean(agg$prob, na.rm = TRUE)
  )

  stats$Image_name <- image_name
  stats$Total_ROIs <- nrow(stats)

  geom <- .compute_roi_geometry(masks)

  stats <- merge(stats, geom, by = "ROI_ID", all.x = TRUE)
  stats$Analysis_method <- analysis_method_text

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
  Results_pixel <- vector("list", n_imgs)
  names(Results_pixel) <- names(imgs)

  if(isTRUE(progress)) {
    pb <- utils::txtProgressBar(min = 0, max = n_imgs, style = 3)
    on.exit(close(pb), add = TRUE)
    }

  for(i in seq_len(n_imgs)) {
    masks <- reticulate::py_to_r(res_list[[i]][[1]][[1]])
    flows <- reticulate::py_to_r(res_list[[i]][[2]][[1]])

    stats <- .cp_compute_roi_stats_single(
      masks = masks,
      flows = flows,
      img = imgs[[i]],
      image_name = names(imgs)[i],
      analysis_method_text = analysis_method_text
      )

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
