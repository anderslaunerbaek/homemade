#' df_to_table
#'
#' @param df data frame or tibble
#' @param path directory to the .tex file.
#' @param file_name name of the .tex file.
#' @param v_lines vertical lines in table.
#' @param h_lines horizontial lines in table.
#' @param case correspond to different types of tables.
#' @param verbose default `FALSE`. Set to `TRUE` for printing the string.
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
df_to_table <-
  function(df,
           path,
           file_name,
           v_lines = NULL,
           h_lines = c(1),
           case = 1,
           verbose = FALSE) {
    #
    tmp <- ""

    # switch case
    if (case == 1) {
      # if
      if (is.null(v_lines)) {
        v_lines <-
          paste0("r|", paste0(rep("r", ncol(df) - 1), collapse = ""))
      }
      #
      tmp <- paste0(tmp, "\\begin{tabular}{", v_lines, "}")
      # colnames
      tmp <-
        paste0(
          tmp,
          "\n",
          "\\multicolumn{1}{l|}{",
          colnames(df)[1],
          "}",
          " & ",
          paste0("\\multicolumn{1}{l}{", colnames(df)[-1], "}", collapse = " & "),
          "\\\\"
        )
      if (1 %in% h_lines) {
        tmp <- paste0(tmp, "\\hline \n")
      } else {
        tmp <- paste0(tmp, " \n")
      }

      # loop rows and cols
      for (i in 1:nrow(df)[1]) {
        for (j in 1:ncol(df)) {
          if (j == ncol(df)) {
            tmp <- paste0(tmp, df[i, j])
          } else {
            tmp <- paste0(tmp, df[i, j], " & ")
          }
        }
        # hline
        if ((i + 1) %in% h_lines) {
          tmp <- paste0(tmp, "\\\\ \\hline \n")
        } else {
          tmp <- paste0(tmp, "\\\\ \n")
        }
      }
      # end
      tmp <- paste0(tmp, "\\end{tabular}", "\n")
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
      print(tmp, quote = FALSE)
    }
  }
