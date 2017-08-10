#' Title
#'
#' @param tb
#' @param save_tikz
#'
#' @return
#' @export
#'
#' @examples
print_to_table <- function(tb, save_tikz){
  tmp <- ""
  for (i in 1:dim(tb)[1]) {
    for (j in 1:dim(tb)[2]) {
      if (j == dim(tb)[2]) {
        tmp <- paste0(tmp, tb[i, j])
      } else {
        tmp <- paste0(tmp, tb[i, j], "&")
      }
    }
    tmp <- paste0(tmp, "\\\\")
    tmp <- paste0(tmp, "\n")
  }
  write(tmp, file = paste0(save_tikz, ".tex"),
        ncolumns = 1, append = FALSE, sep = "")
  print("File is saved ...")
  print(tmp, quote = FALSE)
}
