#' Title
#'
#' @param x
#' @param y
#' @param color
#' @param name
#' @param save_yikz
#'
#' @return
#' @export
#'
#' @examples
print_to_tikz_line <- function(x, y, color, name, save_yikz){
  round_n <- 6
  tmp <- paste0("\\addplot[",color,", fill opacity=1,mark options={scale=2}] coordinates {")
  for (i in 1:length(X)) {
    tmp = paste0(tmp,paste0("(",round(x[i], round_n), ",", round(y[i],round_n), ")"))
  }
  tmp = paste0(tmp,"};\\addlegendentry{",name,"};")
  write(tmp, file = paste0(saveTikz,".tex"),
        ncolumns = 1,
        append = FALSE, sep = "")
  print("File is saved ...")
}
