# function for loading regarding of the object name...
#' Title
#' This functions creates a new environment and load object to a different "new" object name.
#'
#' @param file_path
#'
#' @return New opbejct.
#' @export
#'
load_obj <- function(file_path) {
  env <- new.env()
  nm <- load(file_path, env)[1]
  env[[nm]]
}

#' update_avg_mean
#'
#' This function averages and updates the array (avg_mean) for w.r.t. the given `file_in` array.
#' This works for 1 and 3 channels
#'
#' @param file_in path to complete array. objects must be named: arr_img
#' @param file_out path to save directory. objects must be named: avg_mean
#'
#' @return `TRUE`
#' @export
#'
update_avg_mean <- function(file_in, file_out = "./data/avg_mean.Rda") {
  arr_img <- load_obj(file_in)
  #
  if (length(dim(arr_img)) == 4){
    # 3 channels
    avg_mean <- apply(arr_img, c(2,3,4),mean)
  } else {
    # 1 channels
    avg_mean <- apply(arr_img, c(2,3),mean)
  }
  save(avg_mean, file = file_out)
  #
  return(invisible(TRUE))
}

#' array_to_view
#'
#' @param arr an array of dim(h x w x channel) in range 0-255.
#' @importFrom magick image_read
#' @return `TRUE`
#' @export
#'
array_to_view <- function(arr = arr_img[1,,,]) {
  #
  image_read(arr / 255)
  #
  return(invisible(TRUE))
}

#' magick_to_array
#'
#' This functions converts a magick object to a multidimensionelt integer array.
#'
#' @param img magick obejct.
#' @param channels number of channels. fefaults is `3` else a gray-scale image will be returned.
#' @importFrom magick image_data
#' @importFrom tibble tibble
#'
#' @return an array.
#' @export
#'
magick_to_array <- function(img, channels = 3){
  img <- img %>%
    image_data(channels = ifelse(channels == 3, "rgb", "gray")) %>%
    # change dimensions
    aperm(., c(2,3,1))
  # return
  array(data=as.integer(img), dim = dim(img))
}

#' eval_performance
#'
#' @param y_act correct class
#' @param y_pred predicted class
#' @param order_classes index of class name
#' @param label_classes class labels (unique)
#' @param file_name name of tex table file
#' @param model_name name of model, default is `NULL`
#' @param norm_cm normalize the CM?, default is `TRUE`
#' @param performace_metric include performance metrics, default is `TRUE`
#'
#' @return a text table and a list with performance metrics.
#' @export
#'

eval_performance <- function(y_act = sample(1:5, 400, TRUE),
                             y_pred = c(sample(1:5, 200, TRUE), rep(5, 200)),
                             order_classes = c(1, 2, 3, 4, 5),
                             prior_classes = NULL,
                             label_classes = paste(1:5),
                             file_name = "test",
                             create_tex_table = TRUE,
                             model_name = NULL,
                             norm_cm = TRUE,
                             performace_metric = TRUE) {
  #
  if (is.null(prior_classes)){
    prior_classes <- as.vector((table(y_act) / length(y_act)))
  }
  n_classes <- length(order_classes)
  n <- length(y_pred)
  # create confusion matrix ----
  cm <- matrix(0, ncol = n_classes, nrow = n_classes)
  for (ii in 1:n) {
    cm[y_act[ii], y_pred[ii]] <- cm[y_act[ii], y_pred[ii]] + 1
  }
  # re-order
  cm <- cm[order_classes, order_classes]

  # normalize
  if (norm_cm) {
    cm <- cm / rowSums(cm)
    cm <- matrix(as.integer(round(cm * 100)), ncol = n_classes, nrow = n_classes)
  } else {
    cm <- matrix(as.integer(cm), ncol = n_classes, nrow = n_classes)
  }

  # performance calculations ----
  n <- sum(cm)
  nc <- dim(cm)[1]
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
  accuracy <- diag / (rowsums + colsums - diag) # TODO
  precision <- diag / colsums
  precision[is.nan(precision)] <- 0
  recall <- diag / rowsums
  recall[is.nan(recall)] <- 0
  F1 <- 2 * (precision * recall) / (precision + recall)
  F1[is.nan(F1)] <- 0
  kappa <- (accuracy - expAcc) / (1 - expAcc)
  kappa <- ifelse(kappa < 0, 0, kappa)


  # macro
  accuracy_mac <- mean(accuracy)
  precision_mac <- mean(precision)
  recall_mac <- mean(recall)
  F1_mac <- mean(F1)
  kappa_mac <- (accuracy_mac - expAcc) / (1 - expAcc)
  kappa_mac <- ifelse(kappa_mac < 0, 0, kappa_mac)

  # micro (best for imblanaced data)
  accuracy_mic <- diag_sum /  (rowsums_sum + colsums_sum - diag_sum) # TODO
  precision_mic <- diag_sum / colsums_sum
  precision_mic[is.nan(precision_mic)] <- 0
  recall_mic <- diag_sum / rowsums_sum
  recall_mic[is.nan(recall_mic)] <- 0
  F1_mic <- 2 * (precision_mic * recall_mic) / (precision_mic + recall_mic)
  F1_mic[is.nan(F1_mic)] <- 0
  kappa_mic <- (accuracy_mic - expAcc) / (1 - expAcc)


  perforamnce_metric <- tibble(metric = c("Acc.", "Pre.", "Sens.", "F1", "Kappa"),
         micro_avg = c(accuracy_mic, precision_mic, recall_mic, F1_mic, kappa_mic),
         macro_avg = c(accuracy_mac, precision_mac, recall_mac, F1_mac, kappa_mac))

  # create cm data frame ----
  cm <- data.frame(cm, row.names = label_classes)
  colnames(cm)[1:length(label_classes)] <- label_classes
  if (performace_metric) {
    cm$Pre <- as.integer(round(precision * 100))
    cm$Sen <- as.integer(round(recall * 100))
    cm$F1 <- as.integer(round(F1 * 100))
    cm$Acc <- as.integer(round(accuracy * 100))
  }

  # create table
  if(create_tex_table){
    df_to_table(
      df = cm,
      case = 3,
      file_name = file_name,
      norm_cm = norm_cm,
      model_name = model_name
    )
  }


  # return
  return(
    list(
      "accuracy" = accuracy,
      "precision" = precision,
      "recall" = recall,
      "F1" = F1,
      "kappa" = kappa,
      "perforamnce_metric" = perforamnce_metric,
      "cm" = cm
    )
  )
}


#' my_top
#'
#' @param df input tibble.. must be `glo_df`
#' @param split case specific
#'
#' @return an ordered tibble grouped on `country_code` and `country_name`
#' @export
#'
my_top <- function(df, split = NULL) {
  N <- nrow(df)
  df <- df %>%
    group_by(country_code, country_name) %>%
    summarise(
      n = n(),
      pct = n / N,
      `Percentage (\\%)` = as.integer(round(pct * 100))
    ) %>%
    ungroup() %>%
    mutate(`Country Name` = country_name) %>%
    arrange(desc(n)) %>%
    rename(N = n)

  if (!is.null(split)) {
    df <-
      df %>% select(-country_name  , -`Country Name`,-`Percentage (\\%)`,-N)
  } else {
    df <- df %>% select(-country_code)
  }

  # return
  df

}



#' get_train_and_test
#'
#' @param df input tibble.. must be `glo_df`
#' @param n_top an integer for groupen the most `n_top` classes and the rest to another signle class.
#' @param shuffle shuffle the rows before splitting
#' @param balanced balance the number of classes in train and test.
#' @param pct percentage to split into test.
#'
#' @return `sagsnr`, `test` and `label`.. must bed left joined to `glo_df`
#' @export
#'
get_train_and_test <- function(df,
                               n_top = 5,
                               shuffle = TRUE,
                               balanced = TRUE,
                               pct = 1 / 5) {
  #
  glo_df_TOP <- my_top(df, TRUE)

  #
  df_n_top <-
    dplyr::left_join(df %>% filter(EU_EEA) %>% select(country_code) %>% distinct(),
                     glo_df_TOP,
                     by = "country_code") %>%
    arrange(desc(pct)) %>% slice(1:n_top)
  df <- df %>%
    mutate(
      label = ifelse(country_code %in% df_n_top$country_code, country_code, "OUT"),
      label = ifelse(EU_EEA, label, "NON")
    )

  tmp <- df %>%
    group_by(label) %>%
    summarise(w = n() / nrow(df)) %>%
    arrange(desc(w)) %>% ungroup() %>%
    mutate(label_int = 1:n())


  df <- dplyr::left_join(df, tmp %>% select(-w), by = "label") %>%
    mutate(test = FALSE)

  # devide samples
  if (shuffle) {
    df <- df %>% slice(sample(1:nrow(df), size = nrow(df)))
  }

  if (balanced) {
    for (ii in 1:nrow(tmp)) {
      df_sub <- df %>% filter(label == tmp$label[ii])
      df$test[df$idx %in% sample(df_sub$idx, size = floor(tmp$w[ii] * nrow(df) * pct))] <-
        TRUE
    }
  } else {
    df$test[df$idx %in% sample(1:nrow(df), size = floor(nrow(df) * pct))] <-
      TRUE
  }
  # only return relevant columns...
  df <- df %>% select(sagsnr, test, label)

  # return
  return(df)
}


