#' hough_transform
#'
#' @param img a gray_scale image "(,,1)"
#' @param TT strong thredshold used in canny edge detection
#' @param tt weak thredshold used in canny edge detection
#' @param degree vector with possible rotations
#' @param d_rho rho resolution
#'
#' @return an image rotation
#'
hough_transform <- function(img, TT, tt, degree, d_rho) {

  # image dimensions
  rows <- dim(img)[1]
  cols <- dim(img)[2]

  # calculate (approximations of) gradients
  grad_x <- matrix(0, nrow = rows, ncol = cols)
  grad_y <- matrix(0, nrow = rows, ncol = cols)

  # filters for edge
  sob_h <- matrix(c(-1,0,1,-2,0,2,1,0,1), nrow = 3, ncol = 3)
  sob_v <- matrix(c(-1,-2,-1,0,0,0,1,2,1), nrow = 3, ncol = 3)
  r <- (length(diag(sob_h)) - 1) / 2
  for(x in (1+r):(cols-r)) {
    for(y in (1+r):(rows-r)) {
      grad_x[y, x] <- sum(img[(y - 1):(y + 1), (x - 1):(x + 1),1] * sob_h)
      grad_y[y, x] <- sum(img[(y - 1):(y + 1), (x - 1):(x + 1),1] * sob_v)
    }
  }
  # calculate angles
  angles <- atan2(grad_y, grad_x)
  grad <- sqrt(grad_x^2 + grad_y^2)

  # subset
  angles <- angles[2:(rows-1), 2:(cols-1)]
  grad <- grad[2:(rows-1), 2:(cols-1)]

  # normalize
  grad <- grad / max(grad) * 255
  #
  Z <- canny_edge(grad, angles, TT = TT, tt = tt)
  # image_read(array(Z, dim(Z)) / 255)

  # return rotation
  hough_line_votes(Z, degree, d_rho)
}
