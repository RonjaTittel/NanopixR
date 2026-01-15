# Help functions for the 'run_cellpose()' main function
# for input and output operations in 'run_cellpose()'
#
# helper functions used by 'run_cellpose()' to load input images,
# save Cellpsoe segmentation masks and write analysis results to disk
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
  img_dir_abs <- normalizePath(folder, mustWork = TRUE)
  img_table <- .list_images_sorted(img_dir_abs)
  all_files <- img_table$file
  normnames <- img_table$norm
  if(is.null(selected_files)) {
    files <- all_files
    sel_norm <- normnames
  } else {
    sel_norm_input <- .normalize_name(selected_files)
    match_idx <- match(sel_norm_input, normnames)
    if(any(is.na(match_idx))) {
      missing <- selected_files[is.na(match_idx)]
      stop("Some selected files were not found:\n", paste0(" -", missing, collapse = "\n"), call. = FALSE)
    }
    files <- all_files[match_idx]
    sel_norm <- normnames[match_idx]
  }
  imgs <- lapply(files, io$imread)
  imgs_np <- lapply(imgs, np$array)
  names(imgs) <- sel_norm
  names(imgs_np) <- sel_norm
  list(files = files, names = sel_norm, imgs = imgs, imgs_np = imgs_np)
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
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  out_files <- file.path(output_dir, basename(tools::file_path_sans_ext(files)))
  if(length(imgs_np) == 1) {
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
      io$save_masks(
        images = imgs_np,
        masks = lapply(res_list, function(r) r[[1]][[1]]),
        flows = lapply(res_list, function(r) r[[2]][[1]]),
        file_names = out_files,tif = tif,
        png = png,
        channels = c(0L, 0L)
      )
      }
  invisible(out_files)
}
#
#
#cp_write_results_csv()
# writes the analysis results to CSV files
# writes pixel-based and optionally converted ROI statistics to CSV files in the
# output diretory
.cp_write_results_csv <- function(Results_pixel,
                                  Results_converted = NULL,
                                  outpur_dir,
                                  prefix = "Results_Cellpose") {
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  df_pixel <- do.call(rbind, Results_pixel)
  pixel_csv <- file.path(output_dir, paste0(prefix, "_pixel.csv"))
  utils::write.csv(df_pixel, pixel_csv, row.names = FALSE)
  conv_csv <- NULL
  if(!is.null(Results_converted) && length(Results_converted) > 0) {
    df_conv <- do.call(rbind, Results_converted)
    conv_csv <- file.path(output_dir, paste0(prefix, "_converted.csv"))
    utils::write.csv(df_conv, conv_csv, row.names = FALSE)
  }
  invisible(list(
    pixel = pixel_csv,
    converted = conv_csv))
}



