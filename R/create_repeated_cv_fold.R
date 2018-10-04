#' create_repeated_cv_fold
#'
#' @param idx indecies
#' @param pct_test percentage of indecies for test
#' @param n_rep number of CV repeats
#' @param n_fold number of CV folds
#' @param grid search grid
#'
#' @return a expanded list
#' @export
#'
#' @examples
#' \dontrun{
#' create_repeated_cv_fold(idx = 1:100,
#'                         pct_test = 0.2,
#'                         n_rep = 10,
#'                         n_fold = 5,
#'                         grid = expand.grid(mtry = 1:2, ntree = 2:5))
#'
#' }
create_repeated_cv_fold <- function(idx, pct_test, n_rep, n_fold, grid) {
  # init
  cv <- list()
  cv_grid <- list()
  iter <- 1
  for (i in seq(from=1, to = n_rep * n_fold, by = n_fold)) {
      tmp <- caret::createDataPartition(idx, times = n_fold, p = pct_test)
    for (k in 1:length(tmp)) {
      cv[[i+k-1]] <- list("test" = tmp[[k]],
                          "train" = which(!(idx %in% tmp[[k]])),
                          "fold" = k,
                          "rep" = iter)
    }
    iter <- iter + 1
  }
  #
  step <- seq(from=1, to = length(cv) * nrow(grid), by = nrow(grid))
  for (k in 1:length(step)) {
    for (i in 1:nrow(grid)) {
      cv_grid[[i+step[k]-1]] <- cv[[k]]
      cv_grid[[i+step[k]-1]]$grid <- grid[i, ]
    }
  }

  # return
  cv_grid
}


create_repeated_cv_fold(idx = 1:100,
                        pct_test = 0.2,
                        n_rep = 10,
                        n_fold = 5,
                        grid = expand.grid(mtry = 1:2, ntree = 2:5))
