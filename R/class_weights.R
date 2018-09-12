#' class_weights
#'
#' @param labels vector of labels.
#' @param case choose the weight function.
#' @param mu scalar to the log case.
#'
#' @return list with weigths to keras
#' @export
#'
#' @examples
#' \donotrun{
#' labels <- sample(1:7, 100, T)
#' class_weights(labels)
#' }
#'
class_weights <- function(labels, case = 1, mu = 0.15) {
  # do bin count
  weights <- table(labels)
  if (case == 1){
    # sklearn.utils.class_weight.compute_class_weight approach
    # http://scikit-learn.org/stable/modules/generated/sklearn.utils.class_weight.compute_class_weight.html
    weights <- sum(weights) / (length(weights) * weights)
  } else if (case == 2) {
    weights <- log(mu * sum(weights) / weights)
    weights <- ifelse(weights < 1, 1, weights)
  } else if (case == 3) {
    weights <- ceiling(max(weights) / weights)
  } else {
    weights <- weights / sum(weights)
  }
  # create and return list
  setNames(as.list(weights), names(weights))
}
