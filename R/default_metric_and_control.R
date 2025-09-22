#' @title Construct a default metric for use with caret_list
#' @description Determines the default metric based on the type of target vector.
#' For classification, defaults to "ROC" if the target is binary and "Accuracy" otherwise.
#' For regression, defaults to "RMSE".
#' @param target Target parameter vector, either numeric for regression or a factor/character for classification.
#' @return A character string indicating the default metric
#' @noRd
.default_metric <- function(target) {
  is_class <- is.factor(target) || is.character(target)
  is_binary <- length(unique(target)) == 2L

  if (is_class) {
    if (is_binary) {
      "ROC"
    } else {
      "Accuracy"
    }
  } else {
    "RMSE"
  }
}

#' @title Construct a default train control for use with caret_list
#' @description Unlike `caret::trainControl()`, this function defaults to 5 fold CV.
#' CV is good for stacking, as every observation is in the test set exactly once.
#' 5 is used instead of 10 to save compute time, as caret_list is for fitting many
#' models. Explicit fold indexes are constructed, and predictions are saved for stacking.
#' For classification models, class probabilities are returned.
#' @param target Target parameter vector, either numeric for regression or a factor/character for classification.
#' @return A `caret::trainControl` object
#' @noRd
.default_control <- function(target) {
  is_class <- is.factor(target) || is.character(target)
  is_binary <- length(unique(target)) == 2L

  caret::trainControl(
    method = "cv",
    number = 5L,
    index = caret::createFolds(target, k = 5L, list = TRUE, returnTrain = TRUE),
    savePredictions = "final",
    classProbs = is_class,
    summaryFunction = ifelse(is_class && is_binary, caret::twoClassSummary, caret::defaultSummary),
    returnData = FALSE
  )
}
