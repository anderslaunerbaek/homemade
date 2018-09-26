
#' eval_kappa
#'
#' @param y_act true labels
#' @param y_pred actual labels
#' @param order_classes Sort class order?
#'
#' @return a list with ...
#' @export
#'
eval_kappa <- function(y_act = sample(1:5, 400, TRUE),
                       y_pred = sample(1:5, 400, TRUE),
                       order_classes = c(1, 2, 3, 4, 5)) {
  #
  n_classes <- length(order_classes)
  n <- length(y_pred)
  # create confusion matrix ----
  cm <- matrix(0, ncol = n_classes, nrow = n_classes)
  for (ii in 1:n) {
    cm[y_act[ii], y_pred[ii]] <- cm[y_act[ii], y_pred[ii]] + 1
  }
  # re-order
  cm <- cm[order_classes, order_classes]

  n <- sum(cm)
  diag <- diag(cm)
  rowsums <- apply(cm, 1, sum)
  colsums <- apply(cm, 2, sum)
  diag_sum <- sum(diag)
  rowsums_sum <- sum(rowsums)
  colsums_sum <- sum(colsums)
  p <- rowsums / n
  q <- colsums / n
  expAcc <- sum(p * q)

  # per class metric
  accuracy <- diag / (rowsums + colsums - diag)
  kappa <- (accuracy - expAcc) / (1 - expAcc)
  kappa <- ifelse(kappa < 0, 0, kappa)

  # micro (best for imblanaced data)
  accuracy_mic <- diag_sum /  (rowsums_sum + colsums_sum - diag_sum)
  kappa_mic <- (accuracy_mic - expAcc) / (1 - expAcc)

  # return
  list("kappa" = kappa_mic,
       "kappa_class" = kappa)
}
