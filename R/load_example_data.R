#' @title Load the example heart failure datasets
#' @description Loads example datasets included with the package into the user's environment.
#' @return Invisibly returns the names of the objects loaded.
#' @export
load_heart_failure <- function() {
  load(
    system.file("sample_data", "HeartFailure.RData", package = "caretMultimodal"),
    envir = parent.frame()
  )
}
