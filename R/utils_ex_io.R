# helper functions used by 'extract_image_features()' to handle input/
# output operations for image-based analyses
#
# includes loading images from disk using a unified lookup and exporting
# analysis results to CSV files
#
# no exported functions
#
#
# .ex_load_images()
# load and import image files from a folder using normalized names based on
# .list_images_sorted()' with robust handling of unreadable files
.ex_load_images <- function(folder) {

  # retrieve sorted image table (file paths + normalized names)
  img_table <- .list_images_sorted(folder)

  # stop if no supported image files are found
  if(nrow(img_table) == 0) {
    stop(" No image files found in the specified folder!")
  }

  # attempt to import each image;
  # unreadable/corrupt files return NULL instead of stopping execution
  imgs <- lapply(img_table$file, function(f) {
    tryCatch(biopixR::importImage(f), error = function(e) NULL)
  })

  # assign normalized names for consistent downstream referencing
  names(imgs) <- img_table$norm
  # identify successfully loaded images
  ok <- !vapply(imgs, is.null, logical(1))

  # if all imports failed, return empty list
  if(!any(ok)) {return(list())}

  # return only successfully loaded images
  imgs[ok]
}
#
#
# .ex_export_properties()
# export the final properties table to a CSV file with optional verbose status
# output
.ex_export_properties <- function(df,
                                  folder,
                                  csv_name,
                                  verbose = FALSE) {

  # write results table to CSV (UTF-8 for cross-platform compatibility)
  utils::write.csv(df,
                   file.path(folder, csv_name),
                   row.names = FALSE,
                   fileEncoding = "UTF-8")

  # optionally print save location
  if(isTRUE(verbose)) {
    message("Results saved to: ", file.path(folder, csv_name))
  }
}
