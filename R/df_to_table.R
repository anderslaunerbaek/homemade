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
df_to_table <-
  function(df,
           path = "~/MSc/Writing/Report/auto_tbl_fig/",
           file_name,
           v_lines = NULL,
           h_lines = c(1),
           case = 1,
           n_digits = 4,
           verbose = FALSE,
           model_name = NULL,
           norm_cm = FALSE) {

    # if
    if (is.null(v_lines)) {

      if(ncol(df) > 1) {
        v_lines <- ifelse(is.character(df[[1]]),"l|","r|")
        for (col_idx in 2:ncol(df)){
          v_lines <- paste0(v_lines, ifelse(is.character(df[[col_idx]]),"l","r"))
        }
      } else {
        v_lines <- ifelse(is.character(df[[1]]),"l","r")
      }

      # v_lines <-
      #   paste0("r|", paste0(rep("r", ncol(df) - 1), collapse = ""))
    }


    # N digitgs and pretty
    df <- df %>%
      mutate_if(is.integer, funs(prettyNum(., big.mark = ","))) %>%
      mutate_if(is.numeric, funs(round(., n_digits))) %>%
      mutate_if(is.numeric, funs(prettyNum(., big.mark = ",", decimal.mark = ".")))

    #
    tmp <- ""


    # switch case
    if (case == 1) {
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
    else if (case == 2) {
      #
      tmp <- paste0(tmp, "\\begin{tabular}{", v_lines, "}")
      # colnames
      tmp <-
        paste0(
          tmp,
          "\n",
          paste0(colnames(df), collapse = " & "),
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
    else if (case == 3) {

      row_labels <- row.names(df)
      col_labels <- colnames(df)
      shape <- dim(df)

      cm_case <- ifelse(any(stri_detect(str = col_labels, regex = "Acc")), "p_metric", "non")
      model_name <- ifelse(is.null(model_name), "Class", model_name)
      text <- ifelse(norm_cm, "Normalized pred. (in $\\%$)", "Predicted")

      if (cm_case == "p_metric") {
        tmp <- paste0(tmp,"\\begin{tabular}{cc|",paste0(rep("r", shape[2]-5), collapse = ""),"|rrrrr}","\n")
        tmp <- paste0(tmp, "&& \\multicolumn{",shape[2]-5,"}{c|}{",text,"}&\\multicolumn{5}{c}{Per-class metric (in $\\%$)} \\\\ \n")
        tmp <- paste0(tmp,"\\multirow{",shape[2]-2,"}{*}{",model_name,"} &&", paste0(row_labels,collapse = "&"), "&Pre.&Sen.&F$_1$&Acc.&Kap.  \\\\\\hline \n")
      } else {
        tmp <- paste0(tmp,"\\begin{tabular}{cc|",paste0(rep("r", shape[2]), collapse = ""),"} \n")
        tmp <- paste0(tmp, "&& \\multicolumn{",shape[2],"}{c}{",text,"} \\\\ \n")
        tmp <- paste0(tmp,"\\multirow{",shape[2]+2,"}{*}{",model_name,"} &&", paste0(row_labels,collapse = "&"), " \\\\\\hline \n")
      }

      for (ii in 1:nrow(df)) {
        tmp <- paste0(tmp, "&", row_labels[ii], "&", paste0(df[ii, ], collapse = "&"), "\\\\ \n")

      }
      tmp <- paste0(tmp, "\\end{tabular} \n")
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
