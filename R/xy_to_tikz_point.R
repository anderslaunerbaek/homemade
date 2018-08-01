#' xy_to_tikz_point
#'
#' @param x first coordinate.
#' @param y second coordinate.
#' @param color color of point.
#' @param legend name of the realization.
#' @param path directory to the .tex file.
#' @param file_name name of the .tex file.
#' @param round_n number of decimal digits.
#' @param mark marking style.
#' @param verbose default `FALSE`. Set to `TRUE` for printing the string.
#' @param fill_opa fill opacity of the mark.
#' @param sacle scaling the size of the mark.
#'
#' @return saved file/string with a default tikz picture.
#' @export
#'
#' @examples
#' \dontrun{
#'   library(homemade)
#'   library(tidyverse)
#'   df1 <- iris %>% filter(Species == "setosa") %>% mutate(x = Sepal.Length, y = Petal.Length)
#'   df2 <- iris %>% filter(Species == "versicolor") %>% mutate(x = Sepal.Length, y = Petal.Length)
#'
#'   xy_to_tikz_point(x = df1$x, y = df1$y, color = "red", legend = "setosa", path = "~/Downloads/", file_name = "setosa_2")
#'   xy_to_tikz_point(x = df2$x, y = df2$y, color = "blue", legend = "versicolor", path = "~/Downloads/", file_name = "versicolor_2")
#' }
#'
xy_to_tikz_point <-
  function(x,
           y,
           color,
           legend,
           path = "~/Downloads/",
           file_name = "setosa",
           round_n = 3,
           mark = "*",
           verbose = FALSE,
           fill_opa = 1,
           sacle = 1) {
    # meta
    tmp <-
      paste0(
        "\\addplot[",
        color,
        ", fill opacity=",
        fill_opa,
        ", only marks, mark = ",
        mark,
        ",mark options={scale=",
        sacle,
        "}]\ncoordinates{"
      )

    # loop
    for (i in 1:length(x)) {
      tmp = paste0(tmp, paste0("(", round(x[i], round_n), ",", round(y[i], round_n), ")"))
    }
    # end
    tmp = paste0(tmp, "};\n\\addlegendentry{", legend, "};")

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
