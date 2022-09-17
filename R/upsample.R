up_sample <- function(X_train, Y_train) {
  #
    dist <- table(Y_train)
    n <- length(dist)
    max <- max(dist)

    X_out <- X_train %>% slice(0)
    Y_out <- Y_train %>% slice(0)

    for (i in 1:n) {
      idx <- which(Y_train$label_int == as.integer(names(dist[i])))
      if( i != 1) {
        idx <- sample(idx, max, TRUE)
      } else {
        idx <- sample(idx, max, FALSE)
      }

      X_out <- bind_rows(X_out, X_train %>% slice(idx))
      Y_out <- bind_rows(Y_out, Y_train %>% slice(idx))
    }

    # shuffle
    idx <- sample(1:dim(X_out)[1], dim(X_out)[1], FALSE)

    #
    list("X_out" = X_out %>% slice(idx),
         "Y_out" = Y_out %>% slice(idx))

  }
