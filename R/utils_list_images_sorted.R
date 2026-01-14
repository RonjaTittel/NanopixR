# Lists image files and returns them sorted by their normalized names for best reproducibility

.list_images_sorted <- function(folder) {
  lookup <- .build_image_lookup(folder)

  if(nrow(lookup) == 0) {
    return(lookup[ ,c("file", "norm"), drop = FALSE])
  }

  ord <- order(lookup$norm)
  lookup[ord, c("file", "norm"), drop = FALSE]
}
