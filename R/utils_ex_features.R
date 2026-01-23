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
  gray <- imager::grayscale(img)
  v <- as.numeric(gray)

  sd_int <- stats::sd(v)

  if(!is.finite(sd_int) || sd_int == 0) {
    return(c(skewness = NA_real_, kurtosis = NA_real_))
  }

  h <- graphics::hist(v, breaks = breaks, plot = FALSE)
  mean_int <- mean(v)
  skewness <- sum((h$mids - mean_int)^3 * h$counts) / (sum(h$counts) * sd_int^3)
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

  m <- .ex_to_gray_matrix(img)

  if(all(m == 0)) {
    return(c(mean_grad = 0,
             median_grad = 0,
             edge_density = 0,
             mean_power = 0,
             high_freq_ratio = 0))
  }

  gm <- .ex_gradient_magnitude(m)
  v <- as.numeric(gm)

  mean_grad <- mean(v)
  median_grad <- stats::median(v)
  thr <- stats::quantile(v, edge_q, names = FALSE)
  edge_density <- sum(v > thr) / length(v)

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
  results <- data.frame(image_name = names(imgs),
                        mean_grad = NA_real_,
                        median_grad = NA_real_,
                        edge_density = NA_real_,
                        mean_power = NA_real_,
                        high_freq_ratio = NA_real_,
                        stringsAsFactors = FALSE)

  for(i in seq_along(imgs)) {
    nm <- names(imgs)[i]

    if(isTRUE(verbose)) {
      message("Analyzing image: ", nm)
    }

    res <- tryCatch(.ex_analyze_single_image(imgs[[i]],
                                             edge_q = edge_q,
                                             high_cut = high_cut),
                    error = function(e) {rep(NA_real_, 5)})

    results[i, 2:6] <- res
  }

  results
}
