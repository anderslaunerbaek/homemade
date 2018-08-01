
#' addition_table
#'
#' @param x sequence with integers F5 = {0,1,...,5}
#'
#' @return matrix
#' @export
#'
addition_table <- function(x){
  #
  n <- length(x)

  # init matrix
  mat <- matrix(nrow = n, ncol = n)
  for(i in 1:n) { mat[i, ] <- x[i] + x }
  #
  idx <- mat > n - 1
  mat[idx] <- mat[idx] %% n

  # remane
  colnames(mat) <- x
  rownames(mat) <- x

  #
  return(mat)
}
