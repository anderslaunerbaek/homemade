#' number_of_days
#'
#' @param date as.Date() input
#'
#' @return integer for no. of days.
#'
number_of_days <- function(date) {
  m <- format(date, format="%m")

  while (format(date, format="%m") == m) {
    date <- date + 1
  }

  return(as.integer(format(date - 1, format="%d")))
}
