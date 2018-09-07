#' bootstrap
#'
#' @param values vector of population values.
#' @param n_sim number of simulations.
#' @param p CI, `0.05 = 95%`.
#'
#' @return a list with mean and CI values.
#' @export
#'
#' @examples
#' \dontrun{
#' values <- runif(1000, min = 0, max = 1)
#' bootstrap(values, p = 0.05)
#' }
#'
bootstrap <- function(values, n_sim = 1000000, p = 0.05){
  #
  values <- sample(values, size = n_sim, replace = TRUE)
  values_std <- sd(values)
  values_mean <- mean(values)
    # # normal dist
  # error <- qnorm(p = c(p / 2, 1 - p / 2)) * values_std / sqrt(n_sim)
  # t dist
  error <- qt(p = c(p / 2, 1 - p / 2), df = n_sim-1) * values_std / sqrt(n_sim)
  #
  return(list("mu" = values_mean,
              "CI" = values_mean + error))
}
