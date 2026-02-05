# helper functions used by 'show_results()' to resolve image and mask paths and
# to render combined image-mask visualizations
#
# no exported functions
#
#
# .show_find_image_and_mask()
# resolves the original image file and its corresponding segmentation mask for a
# given normalized image name. Supports both 'BiopixR' and 'Cellpose' mask naming
# conventions and returns a validated file path
.show_find_image_and_mask <- function(folder,
                                      image_name) {
  lookup <- .list_images_sorted(folder)
  idx <- match(image_name, lookup$norm)
  if(is.na(idx)) {stop("Original image not found: ", image_name, call. = FALSE)}
  img_path <- lookup$file[idx]
  base <- tools::file_path_sans_ext(basename(img_path))

  mask_bp <- file.path(folder, "Results", paste0(base, "_bp_masks.tif"))
  mask_cp <- file.path(folder, "Results", paste0(base, "_cp_masks.tif"))

  if(file.exists(mask_bp)) {
    mask_path <- mask_bp
  } else if(file.exists(mask_cp)) {
    mask_path <- mask_cp
  } else {
    stop("Mask not found for image: ", base, call. = FALSE)
  }

  list(img_path = img_path,
       mask_path = mask_path,
       base = base)
}
#
#
# .show_plot_image_mask()
# renders a side-by-side visualization of the original image and its
# corresponding segmentation mask. Overlays coordinate axes and ROI identifiers
# based on centroid positions.
.show_plot_image_mask <- function(img,
                                  mask,
                                  mask_color,
                                  df,
                                  image_name,
                                  draw_axes = TRUE,
                                  draw_ids = TRUE) {
  op <- graphics::par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))
  on.exit(graphics::par(op), add = TRUE)

  dims <- dim(mask)
  img_height <- dims[1]
  img_width <- dims[2]

  # Original image
  plot(0, 0, type = "n",
       xlim = c(0, img_width),
       ylim = c(0, img_height),
       asp = 1,
       axes = FALSE)
  graphics::rasterImage(grDevices::as.raster(suppressWarnings(EBImage::normalize(img))),
              0, 0, img_width, img_height)
  graphics::title(image_name, line = 1.5, cex.main = 1.2)

  # Mask
  plot(0, 0, type = "n",
       xlim = c(0, img_width),
       ylim = c(0, img_height),
       asp = 1,
       axes = FALSE)
  graphics::rasterImage(grDevices::as.raster(mask_color),
              0, 0, img_width, img_height)
  graphics::title("Mask", line = 1.5, cex.main = 1.2)

  # Axes
  if(draw_axes) {
    arrow_len <- img_height * 0.2
    graphics::arrows(0, img_height, arrow_len, img_height, col = "red", lwd = 2)
    graphics::arrows(0, img_height, 0, img_height - arrow_len, col = "red", lwd = 2)
    graphics::text(arrow_len * 1.1, img_height * 1.05, "Y", col = "red", cex = 0.8)
    graphics::text(-arrow_len * 0.1, img_height - arrow_len * 1.1, "X", col = "red", cex = 0.8)
  }

  # IDs
  if(draw_ids && all(c("Centroid_X", "Centroid_Y", "ROI_ID") %in% names(df))) {
    mask_int <- as.integer(EBImage::imageData(mask))
    roi_ids <- sort(unique(mask_int[mask_int > 0]))
    text_pos <- df[df$ROI_ID %in% roi_ids, c("Centroid_X", "Centroid_Y", "ROI_ID")]
    x <- text_pos$Centroid_Y
    y <- img_width - text_pos$Centroid_X
    graphics::text(x, y, labels = text_pos$ROI_ID, col = "grey7", cex = 0.8, font = 2)
  }
}
