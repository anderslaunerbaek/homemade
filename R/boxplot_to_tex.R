#' df_to_table
#'
#' @param df data frame or tibble
#' @param path directory to the .tex file.
#' @param file_name name of the .tex file.
#' @param v_lines vertical lines in table.
#' @param h_lines horizontial lines in table.
#' @param case correspond to different types of tables.
#' @param n_digits The number of digits in the table. Default is 4 for numeric columns.
#' @param verbose default `FALSE`. Set to `TRUE` for printing the string.
#' @param model_name Model name for CM.
#' @param norm_cm is CM normalized?
#'
#' @importFrom dplyr mutate_if
#' @importFrom dplyr funs
#' @importFrom magrittr "%>%"
#' @importFrom stringi stri_detect
#'
#' @return saved file/string with a default tabular table.
#' @export
#'
#' @examples
#' \dontrun{
#'   library(tidyverse)
#'   df <- iris %>% group_by(Species) %>% summarise_all(funs(mean))
#'   df_to_table(df, path = "~/Downloads/", file_name = "test", v_lines = NULL, h_lines = c(1), case = 1, verbose = FALSE)
#' }
boxplot_to_tex <-
  function(x,
           x_idx,
           path,
           file_name,
           verbose = FALSE) {



    tmp <- boxplot(x)
    str <- paste0("\\addplot+[black, mark options={fill opacity=0.1},",
               "boxplot prepared={",
               "lower whisker=",tmp$stats[1,1],",\n",
               "lower quartile=",tmp$stats[2,1],",\n",
               "median=",tmp$stats[3,1],",\n",
               "upper quartile=",tmp$stats[4,1],",\n",
               "upper whisker=",tmp$stats[5,1],"},\n",
               "mark=x]\n")

    if (length(tmp$out) > 0) {
      str <- paste0(str, "coordinates{",paste0("(",x_idx,",",tmp$out,")", collapse = "")," };")
    } else {
      str <- paste0(str, "coordinates{ };")
    }


    #
    if (!verbose) {
      write(
        str,
        file = paste0(path, "/", file_name, ".tex"),
        ncolumns = 1,
        append = FALSE,
        sep = ""
      )
    }
    message(paste0("File > " , file_name, " < is saved ..."))
    if (verbose) {
      cat(tmp)
    }
  }
