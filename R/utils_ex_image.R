# helper functions used by 'extract_image_features()' to perform low-level
# image and signal processing operations
#
# these functions operate directly on image objects or numeric matrices and
# provide reusable building blocks for feature extraction
#
# no exported functions
#
#
# .ex_check_color_mode()
# determine wether an image is grayscale or color and quantify inter-channel
# differences for RGB images
.ex_check_color_mode <- function(img,
                                 diff_threshold = 0.01) {

  if(imager::spectrum(img) == 3) {
    r <- img[, ,1]
    g <- img[, ,2]
    b <- img[, ,3]
    diff_mean <- mean(abs(r - g) + abs(r - b) + abs(g - b)) / 3

    mode <- if(diff_mean < diff_threshold) {
      "grayscale (3 identical channels)"
    } else {
      "color"
      }

    c(mode = mode,
      diff_mean = round(diff_mean, 5))
  } else {
    c(mode = "grayscale (1 channel)",
      diff_mean = NA_real_)
  }

}
#
#
# .ex_to_gray_matrix()
# convert an image object into a normalized grayscale numeric matrix independent
# of the original channel structure
.ex_to_gray_matrix <- function(img) {
  a <- as.array(img)
  d <- dim(a)

  if(length(d) == 4 && d[4] == 3) {
    m <- 0.299 * a[ , , ,1] + 0.587 * a[ , , ,2] + 0.114 * a[ , , ,3]

  } else if (length(d) == 3 && d[3] == 3) {
    m <- 0.299 * a[ , ,1] + 0.587 * a[ , ,2] + 0.114 * a[ , ,3]

  } else {
    m <- drop(a)
  }

  rng <- range(m, na.rm = TRUE)

  if(!is.finite(rng[2]) || rng[2] <= rng[1]) {
    m[,] <- 0
  } else {
    m <- (m - rng[1]) / (rng[2] - rng[1])
  }

  m
}
#
#
# .ex_gradient_magnitude()
# compute the gradient magnitude matrix of a grayscale image matrix as a
# measure of locel intensity changes
.ex_gradient_magnitude <- function(m) {
  nr <- nrow(m)
  nc <- ncol(m)
  gx <- matrix(0, nr, nc)
  gy <- matrix(0, nr, nc)

  gx[ ,2:nc] <- m[ ,2:nc] - m[ ,1:(nc - 1)]
  gx[ ,1] <- gx[ ,2]

  gy[2:nr, ] <- m[2:nr, ] - m[1:(nr - 1), ]
  gy[1, ] <- gy[2, ]

  sqrt(gx^2 + gy^2)
}
#
#
# .ex_freq_features()
# extract basic frequency-domain desriptors from a grayscale image matrix using
# a Fourier-based power spectrum analysis
.ex_freq_features <- function(m,
                              high_cut = 0.25) {
  F <- stats::fft(m)
  P <- Mod(F)^2
  nr <- nrow(m)
  nc <- ncol(m)
  cx <- nr / 2
  cy <- nc / 2

  yy <- matrix(rep(seq_len(nc), each = nr), nrow = nr)
  xx <- matrix(rep(seq_len(nr), times = nc), nrow = nr)
  r <- sqrt((xx - cx)^2 + (yy - cy)^2)

  mask_high <- r > min(cx, cy) * high_cut

  total_power <- sum(P)
  mean_power <- mean(P)

  high_freq_ratio <- if(total_power >0) {
    sum(P[mask_high]) / total_power
  } else {
    0
  }

  c(mean_power = mean_power,
    high_freq_ratio = high_freq_ratio)
}
