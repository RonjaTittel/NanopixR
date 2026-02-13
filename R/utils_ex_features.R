# helper functions used by 'extract_image_features()' to compute
# interpretable image features on single-image and multi-image level
#
# these functions combine low-level image operations into statisically
# meaningful feature representations
#
# no exported functions
#
#
# .ex_analyze_hist_features()
# compute histogram-based intensity features (e.g. skewness and kurtosis)
# from a single image
.ex_analyze_hist_features <- function(img,
                                      breaks = 256) {

  # convert to grayscale and flatten to numeric vector
  gray <- imager::grayscale(img)
  v <- as.numeric(gray)

  # standard deviation required for moment normalization
  sd_int <- stats::sd(v)

  # return NA if image has constant or invalid intensity
  if(!is.finite(sd_int) || sd_int == 0) {
    return(c(skewness = NA_real_, kurtosis = NA_real_))
  }

  # compute histogram without plotting
  h <- graphics::hist(v, breaks = breaks, plot = FALSE)
  mean_int <- mean(v)

  # weighted third central moment (normalized)
  skewness <- sum((h$mids - mean_int)^3 * h$counts) / (sum(h$counts) * sd_int^3)

  # weighted fourth central moment (normalized)
  kurtosis <- sum((h$mids - mean_int)^4 * h$counts) / (sum(h$counts) * sd_int^4)

  c(skewness = skewness,
    kurtosis = kurtosis)
}
#
#
# .ex_analyze_single_image()
# extract a set of gradient- and frequency-based features from a single image
# for downstream decision making
.ex_analyze_single_image <- function(img,
                                     edge_q = 0.9,
                                     high_cut = 0.25) {

  # convert image to normalized grayscale matrix
  m <- .ex_to_gray_matrix(img)

  # handle degenerate images (all zeros)
  if(all(m == 0)) {
    return(c(mean_grad = 0,
             median_grad = 0,
             edge_density = 0,
             mean_power = 0,
             high_freq_ratio = 0))
  }

  # compute gradient magnitude (edge strength)
  gm <- .ex_gradient_magnitude(m)
  v <- as.numeric(gm)

  # basic gradient statistics
  mean_grad <- mean(v)
  median_grad <- stats::median(v)

  # define edge threshold via upper quantile (adaptive per image)
  thr <- stats::quantile(v, edge_q, names = FALSE)
  # proportion of pixels above threshold → edge density
  edge_density <- sum(v > thr) / length(v)

  # frequency-domain descriptors
  fq <- .ex_freq_features(m, high_cut)

  c(mean_grad = mean_grad,
    median_grad = median_grad,
    edge_density = edge_density,
    mean_power = fq["mean_power"],
    high_freq_ratio = fq["high_freq_ratio"])
}
#
#
# .ex_analyze_image_list()
# apply single-image feature extraction to a list of images with robust error
# handling and optional progress reporting
.ex_analyze_image_list <- function(imgs,
                                   edge_q = 0.9,
                                   high_cut = 0.25,
                                   verbose = FALSE) {
  # initialize results table (one row per image)
  results <- data.frame(image_name = names(imgs),
                        mean_grad = NA_real_,
                        median_grad = NA_real_,
                        edge_density = NA_real_,
                        mean_power = NA_real_,
                        high_freq_ratio = NA_real_,
                        stringsAsFactors = FALSE)

  # iterate over image list
  for(i in seq_along(imgs)) {
    nm <- names(imgs)[i]

    # optional progress output
    if(isTRUE(verbose)) {
      message("Analyzing image: ", nm)
    }

    # extract features with error protection
    # failures return NA vector instead of stopping loop
    res <- tryCatch(.ex_analyze_single_image(imgs[[i]],
                                             edge_q = edge_q,
                                             high_cut = high_cut),
                    error = function(e) {rep(NA_real_, 5)})

    # assign computed features (columns 2–6)
    results[i, 2:6] <- res
  }

  results
}
