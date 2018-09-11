#' cross_entropy_loss
#'
#' Computes cross entropy loss.
#' @param predictions one-hot encoded proba matrix (with (n,k)).
#' @param targets one-hot encoded matrix (with (n,k)).
#' @param epsilon clipping offset.
#' @param log_epsilon log offset.
#'
#' @return numeric loss.
#' @export
#'
#' @examples
#' \dontrun{
#' cross_entropy_loss(matrix(c(0.25,0.01,0.25,0.01,0.25,0.01,0.25,0.96), nrow = 2),
#'                    matrix(c(0,0,0,0,0,0,1,1), nrow=2))
#' }
#'
cross_entropy_loss <- function(predictions, targets, epsilon = 1e-12, log_epsilon = 1e-10) {
  # clip values
  predictions <- ifelse(predictions > (1. - epsilon), 1. - epsilon, predictions)
  predictions <- ifelse(predictions < epsilon, epsilon, predictions)
  # return loss
  -sum(sum(targets * log(predictions + log_epsilon))) / dim(predictions)[1]
}
