
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
  TP <- diag(cm)
  rowsums <- apply(cm, 1, sum)
  colsums <- apply(cm, 2, sum)
  #
  p <- rowsums / n
  q <- colsums / n
  expAcc <- sum(p * q)
  FP <- rowsums - TP
  FN <- colsums - TP

  # per class metric
  accuracy <- TP / (FP +TP)
  accuracy[is.nan(accuracy)] <- 0
  kappa <- (accuracy - expAcc) / (1 - expAcc)
  kappa[is.nan(kappa)] <- 0

  # micro (best for imblanaced data)
  accuracy_mic <- sum(TP) / (sum(FP) +sum(TP))
  accuracy_mic[is.nan(accuracy_mic)] <- 0

  kappa_mic <- (accuracy_mic - expAcc) / (1 - expAcc)
  kappa_mic[is.nan(kappa_mic)] <- 0

  # return
  list("kappa" = kappa_mic,
       "kappa_class" = kappa)
}
