#' update_GIT_files
#'
#' @param commit_msg msg which describes the commit.
#' @param path directory to the GIT repo.
#' @param files files you want to update.
#'
#' @return NULL.
#' @export
#'
#' @examples
#' \dontrun{
#'   update_GIT_files(commit_msg = "test", path = "~/Downloads/test/", files = NULL)
#' }
#'
update_GIT_files <- function(commit_msg, path = "~/MSc/Writing/Report/auto_tbl_fig/", files = NULL) {
  #
  recent_wd <- getwd()
  setwd(path)

  if (is.null(files)) {
    files <- "-A"
  } else {
    files <- paste0(files, collapse = " ")
  }

  # git status
  system("git status")
  # git pull
  system("git pull")
  # git add
  system(paste("git add", files))
  # git commit
  system(paste("git commit -m'", commit_msg, "'"))
  # git push
  # system("git push")
  message("\n\nRemember to push!\n\n")

  # get back to the normal wd agian
  setwd(recent_wd)

  #
  return(invisible(NULL))
}
