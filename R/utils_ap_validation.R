# helper functions used by 'analysis_pip()' to extract, validate and prepare
# image-specific metadate and analysis decisions
#
# no exported functions
#
#
# .ap_extract_features()
# extract image features required for method recommendation and downstream
# analysis. Acts as a thin wrapper around feature extraction utilities and
# returns a normalized properties table
.ap_extract_features <- function(folder) {
  tryCatch(extract_image_features(folder),
           error = function(e) {
             stop("Feature extraction failed: ", e$message, call. = FALSE)
           })
}
#
#
# .ap_validate_properties()
# validate the structure and contents od the extracted properties table. Checks
# required columns, data integrity and consisteny of recommendations.
.ap_validate_properties <- function(properties) {
  required_cols <- c("Image_name", "Recommended_method")
  missing <- setdiff(required_cols, names(properties))

  if(length(missing) > 0) {
    stop("Properties is missing required columns: ",
         paste(missing, collapse = ", "),
         call. = FALSE)
  }

  invisible(TRUE)
}
#
#
# .ap_resolve_scale_handling()
# resolve scale bar handling an dunit conversion behavior. Determines whether
# physical unit conversion should be applied based on user input, availability
# of scale information and interactive settings
.ap_resolve_scale_handling <- function(folder,
                                       scale_info,
                                       interactive = FALSE,
                                       verbose = TRUE) {

  if(is.null(scale_info)) {
    if(!interactive) {
      if(verbose) {
        cat("Scale info not specified - conversion disabled.\n")
      }

      return(list(use_conversion = FALSE,
                  scale_info = NULL))
    }

    ans <- tolower(readline("Do the images contain scale information (y/n)?"))

    if(!ans %in% c("y", "yes")) {
      if(verbose) {
        cat("No scale information - conversion disabled.\n")
      }

      return(list(use_conversion = FALSE,
                  scale_info = NULL))
    }
  }

  if(!isTRUE(scale_info)) {
    if(verbose) {
      cat("Scale extraction disabled by parameter.\n")
    }

    return(list(use_conversion = FALSE,
                scale_info = NULL))
  }

  if(verbose) {
    cat("\nExtracting scale information from all images...\n")
  }

  scales <- tryCatch(get_scales(folder),
                     error = function(e) {
                       if(verbose) {
                         cat("Error extracting scales: ", e$message, "\n", sep = "")
                       }

                       NULL
                     })
  if(is.null(scales)) {
    if(verbose) {
      cat("no usable scale info found - conversion disabled. \n")
    }

    return(list(use_conversion = FALSE,
                scale_info = NULL))
  }

  if(verbose) {
    cat("Scale information sucessfully gathered.\n")
  }

  list(use_conversion = TRUE,
       scale_info = scales)
}
#
#
# .ap_group_images_by_method()
# group images by recommended analysis method based on extracted properties.
# Returns normalized image name vectors for each supported method.
.ap_group_images_by_method <- function(properties,
                                       lookup) {
  props_norm <- .normalize_name(properties$Image_name)
  cellpose_imgs <- lookup$norm[
    lookup$norm %in% props_norm[properties$Recommended_method == "Cellpose"]
  ]
  biopixr_imgs <- lookup$norm[
    lookup$norm %in% props_norm[properties$Recommended_method == "BiopixR"]
  ]
  list(Cellpose = cellpose_imgs,
       BiopixR = biopixr_imgs)
}
#
#
# .ap_resolve_group()
# resolve the final execution decision for a group of images. Applies default
# behavior in non-interactive mode and optionally promps the user to override
# the recommended method or skip images
.ap_resolve_group <- function(images,
                              recommended,
                              interactive  = FALSE,
                              verbose = TRUE) {

  if(length(images) == 0) {
    return(list(Cellpose = character(0),
                BiopixR = character(0),
                Skipped = character(0)))
  }

  if(!interactive) {
    return(list(Cellpose = if(recommended == "Cellpose") images else character(0),
                BiopixR = if (recommended == "BiopixR") images else character(0),
                Skipped = character(0)))
  }

  if(verbose) {
    cat("\n=====================================================\n")
    cat("Recommended method: ", recommended, "\n")
    cat("-----------------------------------------------------\n")
    cat("Images:\n")
    print(images)
    cat("-----------------------------------------------------\n")
  }

  prompt <- paste0("Analyse with *", recommended, "* (",
                  tolower(substr(recommended, 1, 1)), "),\n",
                  "with the other method (o), \n",
                  "or skip (s)?")
  ans <- tolower(readline(prompt))

  if(ans  == tolower(substr(recommended, 1, 1))) {
    return(list(Cellpose = if(recommended == "Cellpose") images else character(0),
                BiopixR = if(recommended == "BiopixR") images else character(0),
                Skipped = character(0)))
  }

  if(ans == "o") {
    return(list(
      Cellpose = if(recommended == "BiopixR") images else character(0),
      BiopixR = if(recommended == "Cellpose") images else character(0),
      Skipped = character(0)))
  }

  list(Cellpose = character(0),
       BiopixR = character(0),
       Skipped = images)
}
