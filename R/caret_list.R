#' @title Create a `caret_list` object
#' @description Build a list of `caret::train` objects, where each model corresponds to a data set in `data_list`.
#'   The resulting list is structured for use in ensembling workflows. Users can specify the training method, control parameters,
#'   and metrics, and the function supports error handling and model trimming for efficiency.
#' @param target Target parameter vector, either numeric for regression or a factor/character for classification.
#' @param data_list A list of data sets to train models on
#' @param method The method to train the models with. Can be a custom method or one found in `caret::modelLookup()`.
#' @param identifier_column_name The name of a column that connects the rows in the dataset (ex. a participant ID).
#' If provided, this column must be present in all datasets within the `data_list` for proper matching.
#' @param trControl Control for use with the `caret::train` function. A default control will be constructed depending on the target type.
#' @param metric Metric for use with `caret::train` function. A default metric will be constructed depending on the target type.
#' @param continue_on_fail Logical, whether to skip over a data set if the model fails to train. Default is `FALSE`.
#' @param trim Logical, whether the train models be trimmed to save memory.
#' @param aggregate_resamples Logical, whether to aggregate stacked predictions.
#' @param ... Any additional arguments to pass to the `caret::train` function
#' @return A `caret_list` object, which is a list of `caret::train` model corresponding to `data_list`.
#' @export
caret_list <- function(
    target,
    data_list,
    method,
    identifier_column_name = NULL,
    trControl = NULL,
    metric = NULL,
    continue_on_fail = FALSE,
    trim = TRUE,
    aggregate_resamples = TRUE,
    ...) {

  if (is.null(identifier_column_name)) {
    if (!(is.vector(target) || is.factor(target))) {
      stop("Target must be a vector when no identifier column name is provided.")
    }

    for (i in seq_along(data_list)) {
      if (nrow(data_list[[i]]) != length(target)) {
        stop("The number of rows of data_list[[", i, "]] does not match the length of the target vector, and no identifier column name was provided.")
      }
    }

  } else {
    for (i in seq_along(data_list)) {
      if (!(identifier_column_name %in% colnames(data_list[[i]]))) {
        stop("The identifier column '", identifier_column_name, "' is missing in data_list[[", i, "]].")
      }
    }

    if (!(identifier_column_name %in% colnames(target))) {
      stop("The identifier column '", identifier_column_name, "' is missing in the target dataframe.")
    }

    if(ncol(target) != 2) {
      stop("Target must have exactly two columns: one serving as an identifier and the other containing values for training.")
    }
  }

  .check_method(method)

  raw_target <- if (is.null(identifier_column_name)) {
    target
  } else {
    .match_identifiers(target, data_list[[1]], identifier_column_name)
  }

  trControl <- if (is.null(trControl)) .default_control(raw_target) else trControl
  metric <- if (is.null(metric)) .default_metric(raw_target) else metric

  train_args <- list(...)
  train_args[["trControl"]] <- trControl
  train_args[["metric"]] <- metric
  train_args[["method"]] <- method


  model_list <- lapply(
    data_list,
    function(data) {

      if (is.null(identifier_column_name)) {
        train_args[["y"]] <- target
      } else {
        train_args[["y"]] <- .match_identifiers(target, data, identifier_column_name)
      }

      .caret_train(
        train_args = train_args,
        data = data,
        continue_on_fail = continue_on_fail,
        trim = trim,
        aggregate_resamples = aggregate_resamples
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
#' @param new_data_list A list of datasets to predict on, with each dataset matching the corresponding model in `caret_list`. If `NULL`, predictions from the training data are returned.
#' @param verbose A boolean that controls the verbosity of error messages
#' @param excluded_class_id An integer indicating the class to exclude from predictions. If 0L, no class is excluded. Default is 1L.
#' @param aggregate_resamples A boolean that controls whether to aggregate resamples by keys. Default is `TRUE`.
#' @return A `data.table::data.table` of predictions
#' @export
predict.caret_list <- function(
    caret_list,
    new_data_list = NULL,
    verbose = FALSE,
    excluded_class_id = 1L,
    aggregate_resamples = TRUE) {

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
        aggregate_resamples = aggregate_resamples
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

#' @title Plot a `caret_list` object
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
    model <- tryCatch({
      do.call(caret::train, train_args)
    }, error = function(e) {
      stop("A model failed to train")
    })
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


#' @title Matches the identifiers of a target column with the rows of a data set
#' @param target The target data table (two columns: one for the variable to train on one for identifier)
#' @param data A data set to use for model training that contains an identifier column.
#' @param identifier_column_name The name of the identifier column
#' @return A vector of target variables to train on
#' @noRd
.match_identifiers <- function(
    target,
    data,
    identifier_column_name
){
  data <- data.table::as.data.table(data)
  target <- data.table::as.data.table(target)

  target <- target[target[[identifier_column_name]] %in% data[[identifier_column_name]]]
  target <- target[match(data[[identifier_column_name]], target[[identifier_column_name]])]
  target <- target[[setdiff(colnames(target), identifier_column_name)]]
  target
}


