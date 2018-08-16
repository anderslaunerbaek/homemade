#' constants_to_tex
#'
#' @param df data frame or tibble
#' @param path directory to the .tex file.
#' @param file_name name of the .tex file.
#' @param verbose default `FALSE`. Set to `TRUE` for printing the string.
#' @importFrom dplyr mutate
#' @return saved file/string with multiple `\def\layerseph{...}`
#' @export
#'
#' @examples
#' \dontrun{
#'   #
#'   df <- dplyr::bind_rows(tibble::tibble(key="keyone",
#'                                         desc=NA,
#'                                         value="test12456-12.2"),
#'                          tibble::tibble(key="keytwo",
#'                                         desc=NA,
#'                                         value="10.1"))
#' }
constants_to_tex <-
  function(df,
           path,
           file_name = "constants_to_tex",
           verbose = FALSE) {
    # initial
    tmp <-
      (df %>% mutate(tex = paste0("\\def\\", key, "{", value, "}")))$tex %>%
      paste(., collapse = "\n")

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
