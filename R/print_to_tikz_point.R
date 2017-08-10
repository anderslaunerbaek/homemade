#' Title
#'
#' @param X
#' @param Y
#' @param color
#' @param name
#' @param save_tikz
#'
#' @return
#' @export
#'
#' @examples
print_to_tikz_point <- function(x, y, color, name, save_tikz){
  round_n <- 6
  tmp <- paste0("\\addplot[",color,", fill opacity=0.6,only marks, mark = *,mark options={scale=0.6}] coordinates {")
  for (i in 1:length(X)) {
    tmp = paste0(tmp,paste0("(",round(x[i],roundN), ",", round(y[i], round_n), ")"))
  }
  tmp = paste0(tmp,"};\\addlegendentry{",name,"};")
  write(tmp, file = paste0(save_tikz,".tex"),
        ncolumns = 1,
        append = FALSE, sep = "")
  print("File is saved ...")
}
