#' hog_descriptor
#'
#' Histogram of Orientated Gradients (HOG)..
#'
#'
#' @param img a gray-scale matrix.
#' @param cell the number of HOG windows per bound box.
#' @param n_bins the number of histogram bins.
#'
#' @return a feature vector of length `cell^2 * n_bins`.
#' @export
#'
hog_descriptor <- function(img, cell = 3, n_bins = 9) {

  # set the number of HOG windows per bound box
  nwin_y <- nwin_x <- cell

  # initial vector to hog features
  H <- numeric(nwin_y * nwin_x * n_bins)

  # image dimensions
  rows <- dim(img)[1]
  cols <- dim(img)[2]

  # calculate (approximations of) gradients
  grad_x <- matrix(0, nrow = rows, ncol = cols)
  grad_y <- matrix(0, nrow = rows, ncol = cols)
  for(i in 2:(cols-1)) {
    for(j in 2:(rows-1)) {
      # approx eq. 5.10 and 5.11
      grad_x[j,i] <- img[j + 1, i] - img[j - 1, i]
      grad_y[j,i] <- img[j, i + 1] - img[j, i - 1]
    }
  }

  # calculate angles and magnitudes
  angles <- atan2(grad_y, grad_x)
  magnit <- sqrt(grad_y^2 + grad_x^2) # eq. 5.8

  # ??
  step_x <- floor(cols / (nwin_x + 1))
  step_y <- floor(rows / (nwin_y + 1))
  count <- 0
  # loop
  for (n in 0:(nwin_y-1)) {
    for (m in 0:(nwin_x-1)) {
      # increment iterator
      count <- count + 1
      # subset
      angles_sub <- angles[(n*step_y+1):((n+2)*step_y), (m*step_x+1):((m+2)*step_x)]  %>%
        as.vector()
      magnit_sub <- magnit[(n*step_y+1):((n+2)*step_y), (m*step_x+1):((m+2)*step_x)]  %>%
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
        for (k in 1:length(angles_sub)) {
          if (angles_sub[k] < ang_lim) {
            angles_sub[k] <- 100
            H_sub[bin_count] <- H_sub[bin_count] + magnit_sub[k]
          }
        }
      }
      # normalize and return values
      H[((count-1)*n_bins+1):(count*n_bins)] <- H_sub / sqrt(sum(H_sub^2))
    }
  }
  # return HOG features
  H
}
