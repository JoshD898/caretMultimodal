# What has changed from caretEnsemble:
#   caretList parameters
#   modelCheck takes in single argument instead of list
#   improved checkCustomModel



#' Create a caret train models for each object in dataList
#'
#'
#'
caretList <- function(
    target,
    dataList,
    method,
    trControl = NULL,
    metric = NULL
    ) {

  # Basic checks

  if (!is.vector(target)) {
    stop("The target must be a vector")
  }

  for (name in names(dataList)) {
    if (nrow(dataList[[name]]) != length(target)) {
      stop(
        "The rank of \"", name, "\" does not match the rank of the target."
      )
    }
  }

  methodCheck(method)

  # Set trControl and metric

  if (is.null(trControl)) {
    trControl <- defaultControl(target)
  }

  trControl[["classProbs"]] <- is.factor(target) || is.character(target)
  trControl[["savePredictions"]] <- "final"

  if (is.null(metric)) {
    metric <- defaultMetric(target)
  }

  # Make model list

  modelList <- lapply(
    dataList,
    function(data) caret::train(
      x = data,
      y = target,
      method = method,
      trControl = trControl,
      metric = metric
    )
  )
  names(modelList) <- names(dataList)

  class(modelList) <- c("caretList", "list")

  modelList
}

#' @title Check that the method supplied by the user is a valid caret method
#' @description Uses caret::modelLookup() to ensure the method supplied by the user
#' is a model caret can fit
#' @param method a user-supplied caret method or modelInfo list
#' @return NULL
#' @keywords internal
methodCheck <- function(method) {

  supported_models <- unique(caret::modelLookup()[["model"]])

  if (is.list(method)) {
    checkCustomModel(method)
  } else if (!(method %in% supported_models)) {
    stop(
      "Method \"", method, "\" is invalid. Method must either be a character name ",
      "supported by caret (e.g., \"gbm\") or a modelInfo list ",
      "(e.g., caret::getModelInfo(\"gbm\", regex = FALSE))",
      call. = FALSE
    )
  }
}

#' @title Check that a custom caret model has required elements
#' @description Checks for mandatory elements documented here:
#' https://topepo.github.io/caret/using-your-own-model-in-train.html
#' @param model the model to be validated
#' @return NULL
#' @keywords internal
checkCustomModel <- function(model) {

  required_elements <- list(
    library = "character",
    type = "character",
    parameters = "data.frame",
    grid = "function",
    fit = "function",
    predict = "function",
    prob = "function",
    sort = "function"
  )

  for (element in names(required_elements)) {
    if (!(elem %in% names(model))) {
      stop(
        "Custom model must be defined with a \"", elem, "\" component"
      )
    }
  }
}

#' @title Construct a default metric depending on classification or regression
#' @description Caret defaults to RMSE for classification and RMSE for regression.
#' For classification, I would rather use ROC.
#' @param target Target parameter vector
#' @export
defaultMetric <- function(target) {

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

#' @title Construct a default train control for use with caretList
#' @description Unlike caret::trainControl, this function defaults to 5 fold CV.
#' CV is good for stacking, as every observation is in the test set exactly once.
#' We use 5 instead of 10 to save compute time, as caretList is for fitting many
#' models. We also construct explicit fold indexes and return the stacked predictions,
#' which are needed for stacking. For classification models we return class probabilities.
#' @param target the target variable.
#' @param method the method to use for trainControl.
#' @param number the number of folds to use.
#' @param savePredictions the type of predictions to save.
#' @param index the fold indexes to use.
#' @param is_class logical, is this a classification or regression problem.
#' @param is_binary logical, is this binary classification.
#' @param ... other arguments to pass to \code{\link[caret]{trainControl}}
#' @export
defaultControl <- function(
    target,
    method = "cv",
    number = 5L,
    savePredictions = "final",
    index = caret::createFolds(target, k = number, list = TRUE, returnTrain = TRUE),
    ...) {

  is_class <- is.factor(target) || is.character(target)
  is_binary <- length(unique(target)) == 2L

  stopifnot(savePredictions %in% c("final", "all"))

  caret::trainControl(
    method = method,
    number = number,
    index = index,
    savePredictions = savePredictions,
    classProbs = is_class,
    summaryFunction = ifelse(is_class && is_binary, caret::twoClassSummary, caret::defaultSummary),
    returnData = FALSE,
    ...
  )
}
