#' Lintr current package
#' @description Apply one or more linters to all of the R files in a package.
#'              This function can be used as an addin.
#'
lintr_package <- function() {
  lintr::lint_package()
}
