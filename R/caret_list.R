#' @title Create a list of `caret::train` objects
#' @description This function builds a list of `caret::train` objects, where each model corresponds to a data set in `data_list`.
#'   The resulting list is structured for use in ensembling workflows. Users can specify the training method, control parameters,
#'   and metrics, and the function supports error handling and model trimming for efficiency.
#' @param target Target parameter vector, either numeric for regression or a factor/character for classification.
#' @param data_list The data sets to train models on
#' @param method The method to train the models with. Can be a custom method or one found in `caret::modelLookup()`.
#' @param trControl Control for use with the `caret::train` function. A default control will be constructed depending on the target type.
#' @param metric Metric for use with `caret::train` function. A default metric will be constructed depending on the target type.
#' @param continue_on_fail Logical, whether to skip over a data set if the model fails to train.
#' @param trim Logical, whether the train models be trimmed to save memory and speed up stacking.
#' @param aggregate_resamples Logical, whether to aggregate stacked predictions.
#' @param ... Any additional arguments to pass to the `caret::train` function
#' @return A `caret_list` object, which is a list of `caret::train` model corresponding to `data_list`.
#'    If `continue_on_fail` is `TRUE`, the list may have fewer elements that `data_list`.
#' @examples
#' # TODO example code
#'
#' @export
caret_list <- function(
    target,
    data_list,
    method,
    trControl = NULL,
    metric = NULL,
    continue_on_fail = FALSE,
    trim = TRUE,
    aggregate_resamples = TRUE,
    ...) {
  if (!is.vector(target)) {
    stop("The target must be a vector.")
  }

  for (i in seq_along(data_list)) {
    if (nrow(data_list[[i]]) != length(target)) {
      stop(
        "The number of rows of data_list[[", i, "]] does not match the length of the target vector."
      )
    }
  }

  .check_method(method)


  if (is.null(trControl)) {
    trControl <- .default_control(target)
  }

  if (is.null(metric)) {
    metric <- .default_metric(target)
  }

  train_args <- list(...)
  train_args[["trControl"]] <- trControl
  train_args[["metric"]] <- metric
  train_args[["y"]] <- target
  train_args[["method"]] <- method

  model_list <- lapply(
    data_list,
    function(data) {
      .caret_train(
        train_args = train_args,
        data = data,
        continue_on_fail = FALSE,
        trim = TRUE,
        aggregate_resamples = TRUE
      )
    }
  )
  # TODO test this... what happens if training fails?
  if (length(model_list) != length(data_list)) {
    stop("caret::train failed for one or more models. Please inspect your data.", call. = FALSE)
  }

  if (is.null(names(data_list))) {
    names(model_list) <- paste0("data_list[[", seq_along(data_list), "]]_model")
  } else {
    names(model_list) <- paste0(names(data_list), "_model")
  }

  class(model_list) <- c("caret_list", "list")

  model_list
}


# caret_list methods ------------------------------------------------------------------------------------------

#' @title Create a matrix of predictions for each model in a caret_list
#'
#' @param
#' @param new_data_list A list of datasets to predict on, with each dataset matching the corresponding model in `caret_list`
#'
#'
#'
predict.caret_list <- function(
    caret_list,
    new_data_list = NULL,
    verbose = FALSE,
    excluded_class_id = 1L,
    aggregate_resamples = TRUE,
    ...) {
  stopifnot(methods::is(caret_list, "caret_list"))

  apply_fun <- if (verbose) pbapply::pblapply else lapply

  if (!is.null(new_data_list)) {
    if (length(new_data_list) != length(caret_list)) {
      stop("The length of new_data_list must be the same length as caret_list", .call = FALSE)
    }

    row_counts <- vapply(new_data_list, nrow, integer(1L))

    if (!all(row_counts == row_counts[1])) {
      stop("All matrices in new_data_list must have the same number of rows", call. = FALSE)
    }

    new_data_list <- lapply(new_data_list, data.table::as.data.table)
  }

    prediction_list <- apply_fun(seq_along(caret_list), function(i) {
      model <- caret_list[[i]]
      new_data <- if (!is.null(new_data_list)) new_data_list[[i]] else NULL

      .caret_predict(
        model = model,
        new_data = new_data,
        excluded_class_id = excluded_class_id,
        aggregate_resamples = aggregate_resamples,
        ...
      )
    })

  names(prediction_list) <- names(caret_list)

  prediction_matrix <- data.table::as.data.table(prediction_list)

  prediction_matrix
}


# Helper functions -----------------------------------------------------------------------------------------

#' @title Wrapper to train caret models
#' @description This function is a wrapper around the `caret::train`.
#'    It allows for the option to continue on fail, and to trim the output model.
#'    Trimming the model removes components that are not needed for stacking, to save
#'    memory and speed up the stacking process. It also converts predictionss to a data.table.
#' @param train_args A named list of arguments to pass to the `caret::train` function.
#' @param data A data set to use for model training.
#' @param continue_on_fail A logical indicating whether to continue if the `caret::train` function fails.
#'   If `TRUE`, the function will return `NULL` if the function fails.
#' @param trim A logical indicating whether to trim the output model.
#'   If `TRUE`, the function will remove some elements that are not needed from the output model.
#' @param aggregate_resamples A logical indicating whether to aggregate stacked predictions Default is `TRUE`.
#' @return A `caret::train` object or `NULL` if training fails
#' @noRd
.caret_train <- function(
    train_args,
    data,
    continue_on_fail = FALSE,
    trim = TRUE,
    aggregate_resamples = TRUE) {
  train_args[["x"]] <- data

  if (continue_on_fail) {
    model <- tryCatch(do.call(caret::train, train_args), error = function(e) {
      warning(conditionMessage(e), call. = FALSE)
      NULL
    })
  } else {
    model <- do.call(caret::train, train_args)
  }

  if ("pred" %in% names(model)) {
    model[["pred"]] <- .extract_best_preds(model, aggregate_resamples = aggregate_resamples)
  }

  if (trim) {
    if (!is.null(model[["modelInfo"]][["trim"]])) {
      model[["finalModel"]] <- model[["modelInfo"]][["trim"]](model[["finalModel"]])
    }

    # note that caret::trim will remove stuff we DO need, such as results, preds, besttune, etc.
    removals <- c("call", "dots", "trainingData", "resampledCM")
    for (i in removals) {
      if (i %in% names(model)) {
        model[[i]] <- NULL
      }
    }

    control_removals <- c("index", "indexOut", "indexFinal")
    for (i in control_removals) {
      if (i %in% names(model[["control"]])) {
        model[["control"]][[i]] <- NULL
      }
    }
  }

  model
}


#' @title Extract the best predictions from a train object
#' @description Extract the best predictions from a train object.
#' @param model A `caret::train` object
#' @param aggregate_resamples Logical, whether to aggregate resamples by keys. Default is TRUE.
#' @return A data.table::data.table with predictions
#' @noRd
.extract_best_preds <- function(model, aggregate_resamples = TRUE) {
  stopifnot(is.logical(aggregate_resamples), length(aggregate_resamples) == 1L, methods::is(model, "train"))

  if (is.null(model[["pred"]])) {
    stop("No predictions saved during training. Please set savePredictions = 'final' in trControl", call. = FALSE)
  }

  stopifnot(methods::is(model$pred, "data.frame"))

  keys <- names(model$bestTune)
  best_tune <- data.table::data.table(model$bestTune, key = keys)

  pred <- data.table::data.table(model$pred, key = keys)

  pred <- pred[best_tune, ]

  keys <- "rowIndex"
  data.table::setkeyv(pred, keys)

  if (aggregate_resamples) {
    pred <- pred[, lapply(.SD, .aggregate_mean_or_first), by = keys]
  }

  data.table::setorderv(pred, keys)

  pred
}

#' @title Aggregate mean or first
#' @description For numeric data take the mean. For character data take the first value.
#' @param x A vector to be aggregated.
#' @return The mean of numeric data or the first value of non-numeric data.
#' @noRd
.aggregate_mean_or_first <- function(x) {
  if (is.numeric(x)) {
    mean(x)
  } else {
    x[[1L]]
  }
}


#' @title Check that the method supplied by the user is a valid caret method
#' @description Uses `caret::modelLookup()` to ensure the method supplied by the user
#' is a model caret can fit
#' @param method A user-supplied caret method or modelInfo list
#' @return NULL
#' @noRd
.check_method <- function(method) {
  supported_models <- unique(caret::modelLookup()[["model"]])

  if (is.list(method)) {
    .check_custom_model(method)
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
#' @param model The model to be validated
#' @return NULL
#' @noRd
.check_custom_model <- function(model) {
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
