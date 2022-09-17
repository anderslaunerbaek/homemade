#' canny_edge
#'
#' edge detection algorithm
#'
#' @param grad magnitude of the gradients
#' @param angles angles of the gradients
#' @param TT strong thredshold used in canny edge detection
#' @param tt weak thredshold used in canny edge detection
#' @param strong strong value
#' @param weak weak value
#'
#' @return a binary gray-scale image
#'
canny_edge <- function(grad, angles, TT, tt, strong = 255, weak = 50) {

  if(is.null(tt)) { tt <- floor(TT * 0.4)}
  #
  rows <- dim(grad)[1]
  cols <- dim(grad)[2]
  r <- 1
  # Non-maximum suppression
  angle_round <- c(0, 90, 135, 45)
  Z <- array(0, dim = c(rows,cols,1))
  for(x in (1+r):(cols-r)) {
    for(y in (1+r):(rows-r)) {
      case <- which.min(abs(angles[y, x] - angle_round))
      if (case == 1 & (grad[y, x] >= grad[y, x - 1]) & (grad[y, x] >= grad[y, x + 1])) {
        Z[y, x,] <- grad[y, x]
      } else if (case == 2 & (grad[y, x] >= grad[y - 1, x]) & (grad[y, x] >= grad[y + 1, x])) {
        Z[y, x,] <- grad[y, x]
      } else if (case == 3 & (grad[y, x] >= grad[y - 1, x - 1]) & (grad[y, x] >= grad[y + 1, x + 1])) {
        Z[y, x,] <- grad[y, x]
      } else if (case == 4 & (grad[y, x] >= grad[y - 1, x + 1]) & (grad[y, x] >= grad[y + 1, x - 1])) {
        Z[y, x,] <- grad[y, x]
      }
    }
  }

  # Thresholding: define weak and strong pixel edges
  Z[Z >= TT] <- strong
  Z[Z >= tt & Z < TT] <- weak
  Z[Z < tt] <- 0

  # check local neighbour pixels
  for(x in (1+r):(cols-r)) {
    for(y in (1+r):(rows-r)) {
      #
      if (Z[y,x,] == weak & any(Z[(y-1):(y+1),(x-1):(x+1),] %in% strong)) {
        Z[y,x,] <- strong
      } else {
        Z[y,x,] <- 0
      }
    }
  }

  # return binary image
  Z
}
