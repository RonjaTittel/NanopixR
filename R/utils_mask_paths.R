# Mask path helpers

# Building the path for 'Cellpose' masks files
.mask_path_cp <- function(normname, folder) {
  file.path(folder, "Results", paste0(normname, "_cp_masks.tif"))
}

# Building the path for 'biopixR' masks files
.mask_path_bp <- function(normname, folder) {
  file.path(folder, "Results", paste0(normname, "_bp_masks.tif"))
}
