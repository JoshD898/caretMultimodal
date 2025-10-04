#' @title Load the example heart failure datasets
#' @description Loads example datasets included with the package into the user's environment.
#' @return Invisibly returns the names of the objects loaded.
#' @references Singh et al. Ensembling Electrical and Proteogenomics Biomarkers for Improved Prediction of Cardiac-Related 3-Month Hospitalizations: A Pilot Study. Can J Cardiol. 2019 Apr
#' @export
load_heart_failure <- function() {
  load(
    system.file("sample_data", "HeartFailure.RData", package = "caretMultimodal"),
    envir = parent.frame()
  )
}


#' @title Load the example gestational age datasets
#' @description Loads example datasets included with the package into the user's environment.
#' @return Invisibly returns the names of the objects loaded.
#' @export
load_gestational_age <- function() {
  load(
    system.file("sample_data", "GestationalAge.RData", package = "caretMultimodal"),
    envir = parent.frame()
  )
}
