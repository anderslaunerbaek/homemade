#' xy_to_tikz_line
#'
#'
#' @param x first coordinate.
#' @param y second coordinate.
#' @param color color of point.
#' @param legend name of the realization.
#' @param path directory to the .tex file.
#' @param file_name name of the .tex file.
#' @param round_n number of decimal digits.
#' @param line_type default is normal. dashed, dotted is possible.
#' @param verbose default `FALSE`. Set to `TRUE` for printing the string.
#' @param fill_opa fill opacity of the line.
#' @param sacle scaling the size of the line.
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
#'   xy_to_tikz_line(x = df1$x, y = df1$y, color = "red", legend = "setosa", path = "~/Downloads/", file_name = "setosa")
#'   xy_to_tikz_line(x = df2$x, y = df2$y, color = "blue", legend = "versicolor", path = "~/Downloads/", file_name = "versicolor")
#' }
#'
xy_to_tikz_line <-
  function(x,
           y ,
           color,
           legend,
           path,
           file_name,
           round_n = 3,
           line_type = "",
           verbose = FALSE,
           fill_opa = 1,
           sacle = 2) {
    # meta
    tmp <-
      paste0(
        "\\addplot[",
        line_type,
        ",",
        color,
        ", fill opacity=",
        fill_opa,
        ", mark options={scale=",
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
