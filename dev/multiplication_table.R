
#' multiplication_table
#'
#' @param x sequence with integers F5 = {0,1,...,5}
#'
#' @return matrix
#' @export
#'
multiplication_table <- function(x){
  #
  n <- length(x)

  # init matrix
  mat <- x %*% t(x)

  #
  idx <- mat > n - 1
  mat[idx] <- mat[idx] %% n

  # remane
  colnames(mat) <- x
  rownames(mat) <- x

  #
  return(mat)
}
