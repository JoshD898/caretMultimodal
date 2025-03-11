#' @title Ensemble the models of a `caret_list` object
#' @description Stack several `caret::train` models from a `caret_list` object using a `caret::train` model.
#' @param caret_list a `caret_list` object
#' @param method The method to train the ensemble model. Can be a custom method or one found in `caret::modelLookup()`.
#' @param target Target parameter vector. Defaults to `NULL`, in which case the training data from the first model is `caret_list` will be used.
#' @param new_data_list A list of datasets to predict on, with each dataset matching the corresponding model in `caret_list`. Defaults to `NULL`, in which case the training data for each model is used.
#' @param metric Metric for use with `caret::train` function. A default metric will be constructed depending on the target type.
#' @param trControl Control for use with the `caret::train` function. A default control will be constructed depending on the target type.
#' @param excluded_class_id An integer indicating the class to exclude from predictions. If 0L, no class is excluded. Default is 1L.
#' @param aggregate_resamples Boolean, whether to aggregate resamples by keys. Default is `TRUE`.
#' @return A `caret_stack` object.
#' @export
caret_stack <- function(
    caret_list,
    method,
    target = NULL,
    new_data_list = NULL,
    metric = NULL,
    trControl = NULL,
    excluded_class_id = 1L,
    aggregate_resamples = TRUE
    ) {

  stopifnot(inherits(caret_list, "caret_list"))

  if (is.null(target) != is.null(new_data_list)) {
    stop("target and data_list must both be NULL, or neither", call. = FALSE)
  }

  .check_method(method)

  predictions <- predict.caret_list(
    caret_list,
    new_data_list = new_data_list,
    excluded_class_id = excluded_class_id,
    aggregate_resamples = aggregate_resamples
  )

  obs <- target
  if (is.null(obs)) {
    obs <- data.table::data.table(caret_list[[1L]]$pred)
    data.table::setorderv(obs, "rowIndex")
    obs <- obs[, list(obs = obs[1L]), by = "rowIndex"]
    obs <- obs[["obs"]]
  }

  trControl <- if (is.null(trControl)) .default_control(obs) else trControl
  metric <- if (is.null(metric)) .default_metric(obs) else metric

  ensemble_model <- caret::train(predictions, obs, method=method, metric = metric, trControl = trControl)

  caret_stack <- list(
    individual_models = caret_list,
    ensemble_model = ensemble_model,
    error = ensemble_model$results,
    excluded_class_id = excluded_class_id
  )

  class(caret_stack) <- "caret_stack"

  caret_stack
}


# Methods ----------------------------------------------------------------------

#' @title Create a matrix of predictions for a `caret_stack` object.
#' @param caret_stack A `caret_stack` object
#' @param new_data_list New data to predict on. Default is `NULL`, in which case the training data is predicted on.
#' @param verbose Boolean that controls the verbosity of error messages
#' @param excluded_class_id An integer indicating the class to exclude from predictions. If 0L, no class is excluded. Default is 1L.
#' @param aggregate_resamples Boolean, controls whether to aggregate resamples by keys. Default is `TRUE`.
#' @return A `data.table::data.table` of predictions
#' @export
predict.caret_stack <- function(
    caret_stack,
    new_data_list = NULL,
    verbose = FALSE,
    excluded_class_id = 1L,
    aggregate_resamples = TRUE
    ) {

  if (!is.null(new_data_list)) {

    new_pred_dataset = predict.caret_list(caret_list = caret_stack$individual_models,
                                          new_data_list = new_data_list,
                                          verbose = verbose,
                                          excluded_class_id = excluded_class_id,
                                          aggregate_resamples = aggregate_resamples)

    pred <- .caret_predict(model = caret_stack$ensemble_model,
                           new_data = new_pred_dataset,
                           excluded_class_id = excluded_class_id,
                           aggregate_resamples = aggregate_resamples)
  } else {
    pred <- .caret_predict(model = caret_stack$ensemble_model,
                           excluded_class_id = excluded_class_id,
                           aggregate_resamples = aggregate_resamples)
  }

  pred
}

#' @title Print details of a `caret_stack` object.
#' @export
print.caret_stack <- function(caret_stack) {
  cat("The following models were ensembled:", toString(names(caret_stack$individual_models)), " \n")
  cat("\ncaret::train model:\n")
  print(caret_stack$ensemble_model)
  cat("\nFinal model:\n")
  print(caret_stack$ensemble_model$finalModel)
}

#' @title Get a summary of a `caret_stack` object
#' @export
summary.caret_stack <- function(caret_stack) {
  metric <- caret_stack$ensemble_model$metric
  imp <- caret::varImp(caret_stack$ensemble_model$finalModel, scale = TRUE)
  imp$Overall <- imp$Overall / sum(imp$Overall) * 100

  out <- list(
    models = toString(names(caret_stack$individual_models)),
    imp = imp,
    metric = metric,
    results = extract_metric(caret_stack, metric = metric)
  )
  class(out) <- "summary.caret_stack"
  out
}

#' @title Print a summary of a `caret_stack` object
#' @export
print.summary.caret_stack <- function(summary) {
  cat("The following models were ensembled:", summary$models, " \n")
  cat("\nRelative importance:\n")
  print(summary$imp)
  cat("\nModel accuracy:\n")
  print(summary$results)
}

#' @title Extract metrics from a `caret_stack` object
#' @return A `data.table::data.table` of metrics of the individual and emsembled model
#' @export
extract_metric.caret_stack <- function(caret_stack, metric= NULL) {
  ensemble_metrics <- .extract_train_metric(caret_stack$ensemble_model, metric)
  individual_metrics <- extract_metric.caret_list(caret_stack$individual_models, metric)

  data.table::set(ensemble_metrics, j = "model", value = "ensemble")
  data.table::setcolorder(ensemble_metrics, c("model", setdiff(names(ensemble_metrics), "model")))

  all_metrics <- rbind(ensemble_metrics, individual_metrics)
  all_metrics
}

#' @title Plot metrics of a `caret_stack` object
#' @return A `ggplot2` object
#' @export
plot.caret_stack <- function(caret_stack, metric = NULL) {
  dat <- extract_metric(caret_stack, metric = metric)
  model_order <- unique(dat[["model"]])
  dat[["model"]] <- factor(dat[["model"]], levels = model_order)

  plt <- ggplot2::ggplot(
    dat,
    ggplot2::aes(
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


