#' hog_descriptor
#'
#' Histogram of Orientated Gradients (HOG)..
#'
#'
#' @param img a gray-scale matrix.
#' @param cell the number of HOG windows per bound box.
#' @param n_bins the number of histogram bins.
#'
#' @importFrom dplyr "%>%"
#'
#' @return a feature vector of length `cell^2 * n_bins`.
#' @export
#'
hog_descriptor <- function(img, cell, n_bins) {
# initial vector to hog features
  H <- numeric(cell * cell * n_bins)

  # image dimensions
  rows <- dim(img)[1]
  cols <- dim(img)[2]

  # calculate (approximations of) gradients
  grad_x <- matrix(0, nrow = rows-1, ncol = cols)
  grad_y <- matrix(0, nrow = rows-1, ncol = cols)

  # filters for edge
  D1_mask <- matrix(c(-1,0,1), nrow = 1, ncol = 3)

  for(x in 2:(cols-1)) {
    for(y in 2:(rows-1)) {
      # approx eq. 5.10 and 5.11
      grad_x[y, x] <- sum(img[y, (x - 1):(x + 1)] * D1_mask)
      grad_y[y, x] <- sum(img[(y - 1):(y + 1), y] * D1_mask)
    }
  }


  # subset
  grad_x <- grad_x[2:(rows-1), 2:(cols-1)]
  grad_y <- grad_y[2:(rows-1), 2:(cols-1)]

  # calculate angles and magnitudes
  angles <- atan2(grad_y, grad_x)
  magnit <- sqrt(grad_y^2 + grad_x^2) # eq. 5.8

  # # normalize
  # magnit <- magnit / max(magnit) * 255

  # calculate features per window
  step_x <- floor(ncol(angles) / (cell + 1))
  step_y <- floor(nrow(angles) / (cell + 1))
  count <- 0
  # loop
  for (n in 0:(cell-1)) {
    for (m in 0:(cell-1)) {
      # increment iterator
      count <- count + 1
      # subset
      angles_sub <- angles[(n*step_y+1):((n+2)*step_y),
                           (m*step_x+1):((m+2)*step_x)]  %>%
        as.vector()
      magnit_sub <- magnit[(n*step_y+1):((n+2)*step_y),
                           (m*step_x+1):((m+2)*step_x)]  %>%
        as.vector()
      #
      bin_count <- 0
      H_sub <- numeric(n_bins)
      # assembling the histogram with 9 bins (range of 20 degrees per bin)
      for (ang_lim in seq(from = -pi+2*pi/n_bins,
                          to = pi,
                          by = (2*pi/n_bins))) {
        # increment
        bin_count <- bin_count + 1
        # vectorized if sentence
        idx <- angles_sub < ang_lim
        #
        angles_sub[idx] <- 100
        H_sub[bin_count] <- H_sub[bin_count] + sum(magnit_sub[idx])
      }
      # normalize and return values
      H_sub <- H_sub / sqrt(sum(H_sub^2))
      H_sub[is.nan(H_sub)] <- 0

      H[((count-1)*n_bins+1):(count*n_bins)] <- H_sub
    }
  }
  # return HOG features
  H
}
