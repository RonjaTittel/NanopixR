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

  # check number of channels (3 → RGB image)
  if(imager::spectrum(img) == 3) {
    # extract individual color channels
    r <- img[, ,1]
    g <- img[, ,2]
    b <- img[, ,3]
    # quantify mean absolute difference between channels
    # (low value → channels nearly identical → effectively grayscale
    diff_mean <- mean(abs(r - g) + abs(r - b) + abs(g - b)) / 3

    # classify based on threshold
    mode <- if(diff_mean < diff_threshold) {
      "grayscale (3 identical channels)"
    } else {
      "color"
      }

    # return classification and numeric difference
    c(mode = mode,
      diff_mean = round(diff_mean, 5))
  } else {
    # single-channel image → grayscale by definition
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

  # convert image to array and inspect dimensions
  a <- as.array(img)
  d <- dim(a)

  # handle different RGB array structures (4D or 3D)
  if(length(d) == 4 && d[4] == 3) {
    # weighted RGB → grayscale (standard luminance conversion)
    m <- 0.299 * a[ , , ,1] + 0.587 * a[ , , ,2] + 0.114 * a[ , , ,3]

  } else if (length(d) == 3 && d[3] == 3) {
    # alternative RGB layout
    m <- 0.299 * a[ , ,1] + 0.587 * a[ , ,2] + 0.114 * a[ , ,3]

  } else {
    # already single-channel → drop extra dimensions
    m <- drop(a)
  }

  # normalize to [0, 1] range
  rng <- range(m, na.rm = TRUE)

  # handle constant or invalid images safely
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
  # initialize gradient matrices (x and y directions)
  gx <- matrix(0, nr, nc)
  gy <- matrix(0, nr, nc)

  # horizontal forward differences (intensity change along columns)
  gx[ ,2:nc] <- m[ ,2:nc] - m[ ,1:(nc - 1)]
  # copy second column to first to handle left boundary
  gx[ ,1] <- gx[ ,2]

  # vertical forward differences (intensity change along rows)
  gy[2:nr, ] <- m[2:nr, ] - m[1:(nr - 1), ]
  # copy second row to first to handle top boundary
  gy[1, ] <- gy[2, ]

  # gradient magnitude (Euclidean norm)
  sqrt(gx^2 + gy^2)
}
#
#
# .ex_freq_features()
# extract basic frequency-domain desriptors from a grayscale image matrix using
# a Fourier-based power spectrum analysis
.ex_freq_features <- function(m,
                              high_cut = 0.25) {
  # compute 2D Fourier transform
  F <- stats::fft(m)

  # power spectrum (magnitude squared)

  P <- Mod(F)^2
  nr <- nrow(m)
  nc <- ncol(m)

  # approximate frequency center (no fftshift applied)
  cx <- nr / 2
  cy <- nc / 2

  # build coordinate grids
  yy <- matrix(rep(seq_len(nc), each = nr), nrow = nr)
  xx <- matrix(rep(seq_len(nr), times = nc), nrow = nr)

  # radial distance from center
  r <- sqrt((xx - cx)^2 + (yy - cy)^2)

  # define high-frequency region as outer ring
  mask_high <- r > min(cx, cy) * high_cut

  # global power statistics
  total_power <- sum(P)
  mean_power <- mean(P)

  # ratio of power contained in high-frequency region
  high_freq_ratio <- if(total_power >0) {
    sum(P[mask_high]) / total_power
  } else {
    0
  }

  c(mean_power = mean_power,
    high_freq_ratio = high_freq_ratio)
}
