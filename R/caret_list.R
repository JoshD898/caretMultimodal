#' @title Create a `caret_list` object
#' @description Build a list of `caret::train` objects, where each model corresponds to a data set in `data_list`.
#'   The resulting list is structured for use in ensembling workflows. Users can specify the training method, control parameters,
#'   and metrics, and the function supports error handling and model trimming for efficiency.
#' @param target Target parameter vector, either numeric for regression or a factor/character for classification.
#' @param data_list The data sets to train models on
#' @param method The method to train the models with. Can be a custom method or one found in `caret::modelLookup()`.
#' @param trControl Control for use with the `caret::train` function. A default control will be constructed depending on the target type.
#' @param metric Metric for use with `caret::train` function. A default metric will be constructed depending on the target type.
#' @param continue_on_fail Logical, whether to skip over a data set if the model fails to train. Default is `FALSE`.
#' @param trim Logical, whether the train models be trimmed to save memory and speed up stacking.
#' @param aggregate_resamples Logical, whether to aggregate stacked predictions.
#' @param ... Any additional arguments to pass to the `caret::train` function
#' @return A `caret_list` object, which is a list of `caret::train` model corresponding to `data_list`.
#'    If `continue_on_fail` is `TRUE`, the list may have fewer elements that `data_list`.
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

  stopifnot(is.vector(target))

  for (i in seq_along(data_list)) {
    if (nrow(data_list[[i]]) != length(target)) {
      stop("The number of rows of data_list[[", i, "]] does not match the length of the target vector.")
    }
  }

  .check_method(method)

  trControl <- if (is.null(trControl)) .default_control(target) else trControl

  metric <- if (is.null(metric)) .default_metric(target) else metric

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


  if (is.null(names(data_list))) {
    names(model_list) <- paste0("data_list[[", seq_along(data_list), "]]_model")
  } else {
    names(model_list) <- paste0(names(data_list), "_model")
  }

  class(model_list) <- c("caret_list", "list")

  model_list
}


# Methods ------------------------------------------------------------------------------------------

#' @title Create a matrix of predictions for each model in a caret_list
#' @param caret_list A `caret_list` object
#' @param new_data_list A list of datasets to predict on, with each dataset matching the corresponding model in `caret_list`
#' @param verbose A boolean that controls the verbosity of error messages
#' @param excluded_class_id An integer indicating the class to exclude from predictions. If 0L, no class is excluded. Default is 1L.
#' @param aggregate_resamples A boolean that controls whether to aggregate resamples by keys. Default is `TRUE`.
#' @export
predict.caret_list <- function(
    caret_list,
    new_data_list = NULL,
    verbose = FALSE,
    excluded_class_id = 1L,
    aggregate_resamples = TRUE,
    ...) {

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

extract_metric <- function(x, ...) UseMethod("extract_metric")

#' @title Extract accuracy metrics from a `caret_list` object
#' @description Extract the cross-validated accuracy metrics from each model in a `caret_list`.
#' @param x a `caret_list` object
#' @param metric a character string representing the metric to extract from each model.
#' If NULL or if the provided metric is not found, default model metrics will be extracted
#' @return A data.table with metrics from each model.
#' @export
extract_metric.caret_list <- function (caret_list, metric = NULL) {
  metrics <- lapply(names(caret_list), function(model_name) {
    df <- .extract_train_metric(caret_list[[model_name]], metric = metric)
    df[, model := model_name]
    return(df)
  })
  metrics <- data.table::rbindlist(metrics, use.names = TRUE, fill = TRUE)
  metrics <- metrics[, c("model", setdiff(names(metrics), "model")), with = FALSE]
  metrics
}

#' @title Summarize a caret_list
#' @description This function summarizes the performance of each model in a caret_list object.
#' @param caret_list a caret_list object
#' @param metric The metric to show. If NULL will use the metric used to train each model
#' @return A data.table with metrics from each model.
#' @method summary caret_list
#' @export
summary.caret_list <- function(caret_list, metric = NULL) {
  out <- list(
    models = toString(names(caret_list)),
    metrics = extract_metric(caret_list, metric = metric)
  )
  class(out) <- "summary.caret_list"

  out
}

#' @export
print.summary.caret_list <- function(summary) {
  cat("The following models were trained:", summary[["models"]], "\n")
  cat("\nModel metrics:\n")
  print(summary[["metrics"]])
}

#' @title Plot a caret_list object
#' @description This function plots the performance of each model in a caret_list object.
#' @param x a caret_list object
#' @param metric which metric to plot
#' @return A ggplot2 object
#' @export
plot.caret_list <- function (caret_list, metric = NULL) {
  dat <- extract_metric(caret_list, metric = metric)
  plt <- ggplot2::ggplot(
    dat, ggplot2::aes(
      x = .data[["model"]],
      y = .data[["value"]],
      ymin = .data[["value"]] - .data[["sd"]],
      ymax = .data[["value"]] + .data[["sd"]],
      color = .data[["metric"]]
    )
  ) +
    ggplot2::geom_pointrange() +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Model", y = "Metric Value")

  plt
}



# Helper functions -----------------------------------------------------------------------------------------

#' @title Wrapper to train caret models
#' @description This function is a wrapper around the `caret::train` function.
#'    It allows for the option to continue on fail, and to trim the output model.
#'    Trimming the model removes components that are not needed for stacking, to save
#'    memory and speed up the stacking process. It also converts predictions to a data.table.
#' @param train_args A named list of arguments to pass to the `caret::train` function.
#' @param data A data set to use for model training.
#' @param continue_on_fail A logical indicating whether to continue if the `caret::train` function fails.
#'   If `TRUE`, the function will return `NULL` if the function fails. Default is `FALSE`.
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


#' @title Extract the best predictions from a `caret::train` object
#' @description Extract the best predictions from a `caret::train` object.
#' @param model A `caret::train` object
#' @param aggregate_resamples Logical, whether to aggregate resamples by keys. Default is TRUE.
#' @return A `data.table::data.table` with predictions
#' @noRd
.extract_best_preds <- function(model, aggregate_resamples = TRUE) {
  stopifnot(is.logical(aggregate_resamples), length(aggregate_resamples) == 1L, inherits(model, "train"))

  if (is.null(model[["pred"]])) {
    stop("No predictions saved during training. Please set savePredictions = 'final' in trControl", call. = FALSE)
  }

  stopifnot(inherits(model$pred, "data.frame"))

  if (nrow(model$bestTune) > 0) {
    keys <- names(model$bestTune)
    best_tune <- data.table::data.table(model$bestTune, key = keys)

    pred <- data.table::data.table(model$pred, key = keys)

    pred <- pred[best_tune, ]
  } else {
    pred <- data.table::data.table(model$pred)
  }

  keys <- "rowIndex"
  data.table::setkeyv(pred, keys)

  if (aggregate_resamples) {
    pred <- pred[, lapply(.SD, .aggregate_vector), by = keys]
  }

  data.table::setorderv(pred, keys)

  pred
}

#' @title Aggregate vector
#' @description Aggregate a vector depending on its type. For numeric data return the mean. For factor data return the first value.
#' @param x A vector to be aggregated.
#' @return The mean of numeric data or the first value of non-numeric data.
#' @noRd
.aggregate_vector <- function(x) {
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
    .check_custom_method(method)
  } else if (!(method %in% supported_models)) {
    stop(
      "Method \"", method, "\" is invalid. Method must either be a character name ",
      "supported by caret (e.g., \"gbm\") or a modelInfo list ",
      "(e.g., caret::getModelInfo(\"gbm\", regex = FALSE))",
      call. = FALSE
    )
  }
}

#' @title Check that a custom caret method has required elements
#' @description Checks for mandatory elements documented here:
#' https://topepo.github.io/caret/using-your-own-model-in-train.html
#' @param method The method to be validated
#' @return NULL
#' @noRd
.check_custom_method <- function(method) {
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
    if (!(element %in% names(method))) {
      stop(sprintf('Custom method must be defined with a "%s" component.', element))
    }

    if (!inherits(method[[element]], required_elements[[element]])) {
      stop(sprintf('Component "%s" of the custom method must be of type %s.', element, required_elements[[element]]))
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

#' @title Extract accuracy metrics from a `caret::train` model
#' @description Extract the cross-validated accuracy metrics and their standard deviations.
#' @param x a `caret::train` object
#' @param metric a character string representing the metric to extract. If NULL, uses the metric that was used to train the model.
#' @return A `data.table::data.table` with the name, value and standard deviation of the metric
#' @noRd
.extract_train_metric <- function(x, metric = NULL) {
  if (is.null(metric) || !metric %in% names(x$results)) {
    metric <- x$metric
  }

  results <- data.table::data.table(x$results, key = names(x$bestTune))
  best_tune <- data.table::data.table(x$bestTune, key = names(x$bestTune))

  best_results <- results[best_tune, ]
  value <- best_results[[metric]]
  stdev <- best_results[[paste0(metric, "SD")]]
  if (is.null(stdev)) stdev <- NA_real_

  out <- data.table::data.table(
    method = x$method,
    metric = metric,
    value = value,
    sd = stdev
  )
  out
}
