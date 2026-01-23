# Help functions for the 'run_cellpose()' main function
# for mode creation and segmentation
#
# helper functions used by 'run_cellpose()' to initialize the 'Cellpose' models
# and perform image segmentation
#
# No exported functions
#
#
# .cp_validate_model_path ()
# Validating the custom Cellpose model path
# Checks if the provided path is a single character string and if it exists.
.cp_validate_model_path <- function(model_path) {
  if(is.null(model_path)) {
    return(NULL)}
  if(!is.character(model_path) || length(model_path) != 1) {
    stop("'model_path' must be NULL or a single character string.", call. = FALSE)
  }
  if(!file.exists(model_path)) {
    stop("The specified 'model_path' does not exist: ", model_path, call. = FALSE)
  }
  normalizePath(model_path, mustWork = TRUE)
}
#
#
# .cp_create_model()
# Create a Cellpose model input
# Initialising the Cellpose model that is beeing used. Either the default pretrained
# model or a user-provided model path. Additionally the GPU usage can be
# enabled or disabled
.cp_create_model <- function(models,
                             model_path = NULL,
                             gpu = FALSE) {
  model_path <- .cp_validate_model_path(model_path)
  if(is.null(model_path)) {
    models$CellposeModel(gpu = gpu)
  } else {
    models$CellposeModel(
      pretrained_model = model_path,
      gpu = gpu)
  }
}
#
#
# .cp_eval_images()
# runs the 'Cellpose' segmentation on a set of images
# applies a 'Cellpose' model to one or more images and returns the segmentation results
.cp_eval_images <- function(model,
                            imgs_np,
                            diameter = NULL,
                            progress = FALSE) {
  if(!is.null(diameter)) {
    if(!is.numeric(diameter) || length(diameter) != 1 || diameter <= 0) {
      stop("'diameter' must be a single positive number or NULL.", call. = FALSE)
    }
    diameter <- as.integer(diameter)
  }
  n_imgs <- length(imgs_np)
  res_list <- vector("list", n_imgs)
  if(isTRUE(progress)) {
    pb <- utils::txtProgressBar(min = 0, max = n_imgs, style = 3)
    on.exit(close(pb), add = TRUE)
  }
  for(i in seq_len(n_imgs)) {
    res_list[[i]] <- model$eval(
      x = list(imgs_np[[i]]),
      diameter = diameter
    )
    if(isTRUE(progress)) {
      utils::setTxtProgressBar(pb, i)
    }
  }
  res_list
}
