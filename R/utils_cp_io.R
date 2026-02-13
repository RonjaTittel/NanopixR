# Help functions for the 'run_cellpose()' main function
# for input and output operations in 'run_cellpose()'
#
# helper functions used by 'run_cellpose()' to load input images,
# save 'Cellpose' segmentation masks and write analysis results to disk
#
# No exported functions
#
#
# .cp_load_images()
# Load and prepare input images for 'Cellpose'
# Loads image files from a specificed folder, optionally selecting a subset of files.
# Images read via 'Cellpose' I/O utilities and converted to 'NumPy' arrays.
.cp_load_images <- function(folder,
                            io,
                            np,
                            selected_files = NULL) {
  # normalize folder path (ensure absolute and existing)
  img_dir_abs <- normalizePath(folder, mustWork = TRUE)

  # retrieve sorted image lookup
  img_table <- .list_images_sorted(img_dir_abs)
  all_files <- img_table$file
  normnames <- img_table$norm

  # optional subset selection via normalized matching
  if(is.null(selected_files)) {
    files <- all_files
    sel_norm <- normnames
  } else {
    # normalize user-provided file names for robust matching
    sel_norm_input <- .normalize_name(selected_files)
    match_idx <- match(sel_norm_input, normnames)

    # stop if any requested files are not found
    if(any(is.na(match_idx))) {
      missing <- selected_files[is.na(match_idx)]
      stop("Some selected files were not found:\n", paste0(" -", missing, collapse = "\n"), call. = FALSE)
    }
    files <- all_files[match_idx]
    sel_norm <- normnames[match_idx]
  }

  # load images using Cellpose I/O (Python side)
  imgs <- lapply(files, io$imread)

  # convert images to NumPy arrays (required for model$eval)
  imgs_np <- lapply(imgs, np$array)

  # assign normalized names for downstream consistency
  names(imgs) <- sel_norm
  names(imgs_np) <- sel_norm

  # return both raw images and NumPy arrays
  list(files = files,
       names = sel_norm,
       imgs = imgs,
       imgs_np = imgs_np)
}
#
#
# .cp_save_masks
# Saves 'Cellpose' segmentation masks to disk
# saves masks and flow fields created by 'Cellpose' as TIFF files using the
# 'Cellpose' I/O backend.
.cp_save_masks <- function(io,
                           imgs_np,
                           res_list,
                           files,
                           output_dir,
                           tif = TRUE,
                           png = FALSE) {

  # ensure output directory exists
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # construct output filenames (without extension)
  out_files <- file.path(output_dir, basename(tools::file_path_sans_ext(files)))

  # handle single-image vs multi-image API behavior
  if(length(imgs_np) == 1) {
    # for single image, Cellpose expects non-list inputs
    io$save_masks(
      images = imgs_np[[1]],
      masks = res_list[[1]][[1]][[1]],
      flows = res_list[[1]][[2]][[1]],
      file_names = out_files[1],
      tif = tif,
      png = png,
      channels = c(0L, 0L)
    )
    } else {
      # for multiple images, pass lists
      io$save_masks(
        images = imgs_np,
        masks = lapply(res_list, function(r) r[[1]][[1]]),
        flows = lapply(res_list, function(r) r[[2]][[1]]),
        file_names = out_files,tif = tif,
        png = png,
        channels = c(0L, 0L)
      )
    }
  # return output file base names (invisibly)
  invisible(out_files)
}
#
#
# .cp_write_results_csv()
# writes the analysis results to CSV files
# writes pixel-based and optionally converted ROI statistics to CSV files in the
# output diretory
.cp_write_results_csv <- function(results_pixel,
                                  results_converted = NULL,
                                  output_dir,
                                  prefix) {
  # ensure output directory exists
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # container for written file paths
  out <- list(pixel = NULL, converted = NULL)

  # write pixel-based ROI statistics
  if(length(results_pixel)) {
    # combine per-image data frames into single table
    df_pixel <- do.call(rbind, results_pixel)
    out$pixel_csv <- file.path(output_dir, paste0(prefix, "_pixel.csv"))
    utils::write.csv(df_pixel, out$pixel_csv, row.names = FALSE)
  }

  # write converted (physical unit) statistics
  if(!is.null(results_converted) && length(results_converted) > 0) {
    df_conv <- do.call(rbind, results_converted)
    out$converted_csv <- file.path(output_dir, paste0(prefix, "_converted.csv"))
    utils::write.csv(df_conv, out$converted_csv, row.names = FALSE)
  }
  # return written file paths invisibly
  invisible(out)
}
