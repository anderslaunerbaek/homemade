#' constants_to_tex
#'
#' @param my_list list with key and values
#' @param n_digits The number of digits in text. Default is 4 for numeric values.
#' @param path directory to the .tex file.
#' @param file_name name of the .tex file.
#' @param verbose default `FALSE`. Set to `TRUE` for printing the string.
#' @importFrom dplyr mutate
#' @return saved file/string with multiple `"\def\key{...}"`
#' @export
#'
#' @examples
#' \dontrun{
#'   #
#'   my_list <- list("keyone" ="test12456-12.2",
#'                   "keytwo"="10.1")
#' }
constants_to_tex <-
  function(my_list,
           n_digits = 4L,
           path = "~/MSc/Writing/Report/auto_tbl_fig/",
           file_name = "constants_to_tex",
           verbose = FALSE) {
    # initial
    tmp <- ""

    # loop keys
    for (key in names(my_list)){
      #
      value <- my_list[[key]]
      # make pretty
      if (is.numeric(value)) {
        value <- prettyNum(round(value, n_digits),
                           big.mark = ",",
                           decimal.mark = ".")
      } else if(is.integer(value)){
        value <- prettyNum(value,
                           big.mark = ",",
                           decimal.mark = ".")
      }
      tmp <- paste0(tmp, "\\def\\", gsub("_", "", key), "{", value, "}\n")
    }

    #
    write(
      tmp,
      file = paste0(path, "/", file_name, ".tex"),
      ncolumns = 1,
      append = FALSE,
      sep = ""
    )
    message(paste0("File > " , file_name, " < is saved ..."))
    if (verbose) {
      cat(tmp)
    }
  }
