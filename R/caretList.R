# What has changed from caretEnsemble:
#   caretList parameters
#   modelCheck takes in single argument instead of list
#   improved .CheckCustomModel



#' Create a caret train model for each object in dataList
#'
#'
#'
caretList <- function(
    target,
    dataList,
    method,
    trControl = NULL,
    metric = NULL,
    continueOnFail = FALSE,
    trim = TRUE,
    aggregateResamples = TRUE,
    ...
) {

  # Basic checks

  if (!is.vector(target)) {
    stop("The target must be a vector")
  }

  for (i in seq_along(dataList)) {
    if (nrow(dataList[[i]]) != length(target)) {
      stop(
        "The number of rows of dataList[[", i,  "]] does not match the length of the target vector."
      )
    }
  }

  .CheckMethod(method)

  # Set trControl and metric

  if (is.null(trControl)) {
    trControl <- .DefaultControl(target)
  }

  if (is.null(metric)) {
    metric <- .DefaultMetric(target)
  }

  # capture additional caret::train arguments as a list

  trArgs <- list(...)
  trArgs[["trControl"]] <- trControl
  trArgs[["metric"]] <- metric
  trArgs[["y"]] <- target
  trArgs[["method"]] <- method

  # Make model list

  modelList <- lapply(
    dataList,
    function(data) .CaretTrain(
      trArgs = trArgs,
      data = data,
      continueOnFail = FALSE,
      trim = TRUE,
      aggregateResamples = TRUE
    )
  )
  # TODO test this... what happens if training fails?
  if (length(modelList) != length(dataList)) {
    stop("caret::train failed for one or more models. Please inspect your data.", call. = FALSE)
  }



  if (is.null(names(dataList))) {
    # assign generic names if dataList is unnamed
    names(modelList) <- paste0("dataList[[", seq_along(dataList), "]]_model")
  } else {
    names(modelList) <- paste0(names(dataList), "_model")
  }


  class(modelList) <- c("caretList", "list")

  modelList
}

#' @title Wrapper to train caret models
#' @description This function is a wrapper around the `train` function from the `caret` package.
#' It allows for the passing of local and global arguments to the `train` function.
#' It also allows for the option to continue on fail, and to trim the output model.
#' Trimming the model removes components that are not needed for stacking, to save
#' memory and speed up the stacking process. It also converts preds to a data.table.
#' Its an internal function for use with caretList.
#' @param trArgs A list of arguments to pass to the `train` function.
#' @param continueOnFail A logical indicating whether to continue if the `train` function fails.
#'  If `TRUE`, the function will return `NULL` if the `train` function fails.
#' @param trim A logical indicating whether to trim the output model.
#' If `TRUE`, the function will remove some elements that are not needed from the output model.
#' @param aggregateResamples A logical indicating whether to aggregate stacked predictions Default is TRUE.
#' @return The output of the `caret::train` function.
#' @keywords internal
.CaretTrain <- function(
    trArgs,
    data,
    continueOnFail = FALSE,
    trim = TRUE,
    aggregateResamples = TRUE
) {

  trArgs[["x"]] <- data

  # Fit model
  if (continueOnFail) {
    model <- tryCatch(do.call(caret::train, trArgs), error = function(e) {
      warning(conditionMessage(e), call. = FALSE)
      NULL
    })
  } else {
    model <- do.call(caret::train, trArgs)
  }

  # Only save stacked predictions for the best model
  if ("pred" %in% names(model)) {
    model[["pred"]] <- .ExtractBestPreds(model, aggregateResamples = aggregateResamples)
  }

  if (trim) {
    # Remove some elements that are not needed from the final model
    if (!is.null(model[["modelInfo"]][["trim"]])) {
      model[["finalModel"]] <- model[["modelInfo"]][["trim"]](model[["finalModel"]])
    }

    # Remove some elements that are not needed from the train model
    # note that caret::trim will remove stuff we DO need, such as results, preds, besttune, etc.
    removals <- c("call", "dots", "trainingData", "resampledCM")
    for (i in removals) {
      if (i %in% names(model)) {
        model[[i]] <- NULL
      }
    }

    # Remove some elements that are not needed from the model control (within the train model)
    c_removals <- c("index", "indexOut", "indexFinal")
    for (i in c_removals) {
      if (i %in% names(model[["control"]])) {
        model[["control"]][[i]] <- NULL
      }
    }
  }

  model
}


#' @title Extract the best predictions from a train object
#' @description Extract the best predictions from a train object.
#' @param x a train object
#' @param aggregateResamples logical, whether to aggregate resamples by keys. Default is TRUE.
#' @return a data.table::data.table with predictions
#' @keywords internal
.ExtractBestPreds <- function(x, aggregateResamples = TRUE) {
  stopifnot(is.logical(aggregateResamples), length(aggregateResamples) == 1L, methods::is(x, "train"))
  if (is.null(x[["pred"]])) {
    stop("No predictions saved during training. Please set savePredictions = 'final' in trControl", call. = FALSE)
  }
  stopifnot(methods::is(x$pred, "data.frame"))

  # Extract the best tune
  keys <- names(x$bestTune)
  best_tune <- data.table::data.table(x$bestTune, key = keys)

  # Extract the best predictions
  pred <- data.table::data.table(x$pred, key = keys)

  # Subset pred data to the best tune only
  # Drop rows for other tunes
  pred <- pred[best_tune, ]

  # Set keys for sorting
  keys <- "rowIndex"
  data.table::setkeyv(pred, keys)

  # If aggregate_resamples is TRUE, aggregate by keys
  if (aggregateResamples) {
    pred <- pred[, lapply(.SD, .AggregateMeanOrFirst), by = keys]
  }

  # Order results consistently
  data.table::setorderv(pred, keys)

  # Return
  pred
}

#' @title Aggregate mean or first
#' @description For numeric data take the mean. For character data take the first value.
#' @param x a train object
#' @return a data.table::data.table with predictions
#' @keywords internal
.AggregateMeanOrFirst <- function(x) {
  if (is.numeric(x)) {
    mean(x)
  } else {
    x[[1L]]
  }
}


#' @title Check that the method supplied by the user is a valid caret method
#' @description Uses caret::modelLookup() to ensure the method supplied by the user
#' is a model caret can fit
#' @param method a user-supplied caret method or modelInfo list
#' @return NULL
#' @keywords internal
.CheckMethod <- function(method) {

  supported_models <- unique(caret::modelLookup()[["model"]])

  if (is.list(method)) {
    .CheckCustomModel(method)
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
.CheckCustomModel <- function(model) {

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

#' @title Construct a default metric for use with caretList
#' @description Caret defaults to RMSE for classification and RMSE for regression.
#' For classification, I would rather use ROC.
#' @param target Target parameter vector
#' @return TODO
#' @keywords internal
.DefaultMetric <- function(target) {

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
#' @param target Target parameter vector
#' @return TODO
#' @keywords internal
.DefaultControl <- function(target) {

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

