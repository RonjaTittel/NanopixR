# helper functions used by 'run_biopixR()' to validate and normalize
# parameters for object detection and filters
#
# no exported functions
#
#
# .bp_vaildate_detection_params()
# validate and normalize object detection parameters
# check consistency of 'method', 'alpha' and 'sigma' and applies method-
# dependent defaults for edge- and threshold-based detection
.bp_validate_detection_params <- function(method,
                                          alpha = NULL,
                                          sigma = NULL) {
  method <- match.arg(method, c("edge", "threshold"))

  if(method == "edge") {
    if(is.null(alpha)) alpha <- "gaussian"
    if(is.null(sigma)) sigma <- "gaussian"

    if(!(is.numeric(alpha) || (is.character(alpha) & alpha == "gaussian"))) {
      stop("'alpha' must be numeric or 'gaussian' for method = 'edge'.", call. = FALSE)
    }

    if(!(is.numeric(sigma) || (is.character(sigma) && sigma == "gaussian"))) {
      stop("'sigma' must be numeric or 'gaussian' for method = 'edge'.", call. = FALSE)
    }

    threshold <- NULL
  }

  if(method == "threshold") {
    alpha <- NULL
    sigma <- NULL
    threshold <- "auto"
  }

  list(method = method,
       alpha = alpha,
       sigma = sigma,
       threshold = threshold)
}
#
#
# .bp_validate_size_filter()
# validate and normalize size filter configuration
# handles activation, default values and validity checks for size-
# based object filtering
.bp_validate_size_filter <- function(use = FALSE,
                                     lowerlimit = NULL,
                                     upperlimit = NULL) {

  if(!is.logical(use) || length(use) != 1) {
    stop("'use_sizefilter' must be TRUE or FALSE.", call. = FALSE)
  }

  if(!use) {
    return(list(use = FALSE,
      lowerlimit = NULL,
      upperlimit = NULL))
  }

  if(is.null(lowerlimit)) lowerlimit <- "auto"
  if(is.null(upperlimit)) upperlimit <- "auto"

  valid_limit <- function(x) {
    is.numeric(x) || (is.character(x) && x == "auto")
  }

  if(!valid_limit(lowerlimit)) {
    stop("'size_lowerlimit' must be numeric or 'auto' when 'use_sizefilter' is TRUE.", call. = FALSE)
  }

  if(!valid_limit(upperlimit)) {
    stop("'size_upperlimit' must be numeric or 'auto' when 'use_sizefilter' is TRUE.", call. = FALSE)
  }

  list(use = TRUE,
       lowerlimit = lowerlimit,
       upperlimit = upperlimit)
}
#
#
# .bp_validate_proximity_filter()
# validate and normalize proximity filter configuration
# handles activation, default values and validity checks for proximity-
# based object filtering
.bp_validate_proximity_filter <- function(use = FALSE,
                                          radius = NULL,
                                          elongation = NULL) {

  if(!is.logical(use) || length(use) != 1) {
    stop("'use_proxfilter' must be TRUE or FALSE.", cal. = FALSE)
  }

  if(!use) {
    return(list(use = FALSE,
                radius = NULL,
                elongation = NULL))
  }

  if(is.null(radius)) radius <- "auto"
  if(is.null(elongation)) elongation <- 2

  if(!is.numeric(radius) || !(is.character(radius) && radius == "auto")) {
    stop("'prox_radius' must be numeric or 'auto' when use_proxfilter' is TRUE.", call. = FALSE)
  }

  if(!is.numeric(elongation) || length(elongation) != 1) {
    stop("'prox_elongation' must be a single numeric value when 'use_proxfilter' is TRUE.", call. = FALSE)
  }

  list(use = TRUE,
       radius = radius,
       elongation = elongation)
}
