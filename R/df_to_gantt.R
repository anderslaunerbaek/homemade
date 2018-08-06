#' df_to_gantt
#'
#' @param df data frame or tibble
#' @param path directory to the .tex file.
#' @param file_name name of the .tex file.
#' @param start_date start date of project,
#' @param end_date end date of project.
#' @param verbose default `FALSE`. Set to `TRUE` for printing the string.
#'
#' @return saved file/string with a default tabular table.
#' @export
#'
#' @examples
#' \dontrun{
#'   # hmm
#' }
df_to_gantt <-
  function(df,
           path,
           file_name,
           start_date = as.Date("2018-08-01", "%Y-%m-%d"),
           end_date = as.Date("2019-02-01", "%Y-%m-%d"),
           verbose = FALSE) {
    # correct start date.
    d_start <- as.Date("2018-08-01", "%Y-%m-%d")

    #  pre calculations.
    diff_start <- as.integer(start_date - d_start)
    diff_start <- ifelse(diff_start == 0, 1, diff_start)
    #
    diff_date <- as.integer(end_date - start_date)
    seq_month <-
      as.Date(seq(start_date, end_date, "month"), "%Y-%m-%d")
    no_day_in_month <- sapply(seq_month, number_of_days)


    # % Connections
    # \ganttlink{printing_report}{inside_dtu}
    # % milestone
    # \ganttmilestone[name=handin]{Handin}{16}


    # initial
    tmp <- ""
    # meta data

    tmp <- paste0(
      tmp,
      "\\begin{ganttchart}[",
      "\n",
      "x unit=0.1cm,",
      "\n",
      "y unit title=0.5cm,",
      "\n",
      "y unit chart=0.6cm,",
      "\n",
      "milestone label font=\\tiny,",
      "\n",
      "bar label font=\\tiny,",
      "\n",
      "group label font=\\tiny,",
      "\n",
      "title label font=\\tiny,",
      "\n",
      "group incomplete/.append style={draw=black, fill=none},",
      "\n",
      "bar incomplete/.append style={fill=black!25},",
      "\n",
      "bar/.append style={fill=green},",
      "\n",
      "progress label text={\\tiny \\pgfmathprintnumber[precision=0]{#1}\\%}]",
      "\n",
      "{",
      diff_start,
      "}{",
      diff_date,
      "}",
      "\n\n\n",
      collapse = "\n"
    )

    #
    tmp <- paste0(
      tmp,
      "% months",
      "\n",
      "\\gantttitle{August}{",
      no_day_in_month[1],
      "}",
      "\n",
      "\\gantttitle{September}{",
      no_day_in_month[2],
      "}",
      "\n",
      "\\gantttitle{October}{",
      no_day_in_month[3],
      "}",
      "\n",
      "\\gantttitle{November}{",
      no_day_in_month[4],
      "}",
      "\n",
      "\\gantttitle{December}{",
      no_day_in_month[5],
      "}",
      "\n",
      "\\gantttitle{January}{",
      no_day_in_month[6],
      "} \\\\",
      "\n",
      "% weeks",
      "\n",
      "\\gantttitlelist{.}{2}",
      "\n",
      "\\gantttitlelist{1,...,26}{7}\\\\",
      "\n\n\n",
      collapse = "\n"
    )

    df <- df %>%
      mutate(
        level_tmp = substr(level, 1, 1),
        id_name = gsub(" ", "", id_name),
        id_name = gsub("[.]", "_", id_name)
      ) %>%
      arrange(level)

    for (lev in unique(df$level_tmp)) {
      # subset
      df_sub <- df %>% filter(level_tmp == lev)
      for (n_idx in 1:nrow(df_sub)) {
        if (nchar(df_sub$level[n_idx]) == 1) {
          tmp <- paste0(
            tmp,
            "\n\\ganttgroup[progress=",
            df_sub$process[n_idx],
            "]{",
            df_sub$Task[n_idx],
            "}{",
            df_sub$date_start[n_idx],
            "}{",
            df_sub$date_end[n_idx],
            "} \\\\ \n"
          )
        } else {
          if (substr(df_sub$level[n_idx], 3, 3) != "m") {
            tmp <- paste0(
              tmp,
              "\\ganttbar[progress=",
              df_sub$process[n_idx],
              ", name=",
              df_sub$id_name[n_idx],
              "]{",
              df_sub$Task[n_idx],
              "}{",
              df_sub$date_start[n_idx],
              "}{",
              df_sub$date_end[n_idx],
              "} \\\\ \n"
            )
          } else {
            tmp <-
              paste0(
                tmp,
                "\\ganttmilestone[name=",
                df_sub$id_name[n_idx],
                "]{",
                df_sub$Task[n_idx],
                "}{",
                df_sub$date_start[n_idx],
                "}\\\\ \n"
              )
          }
        }
      }
    }

    # end
    tmp <- paste0(tmp, "\n\\end{ganttchart}")

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
