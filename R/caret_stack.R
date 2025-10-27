#' @title Ensemble the models of a `caret_list` object
#' @description Train an ensemble (stacked) model from the base learners in a
#'   `caret_list`. The ensemble is itself a `caret::train` model that learns to
#'   combine the predictions of the base models. By default, the meta-learner is
#'   trained on out-of-fold predictions from the resampling process, ensuring that
#'   the ensemble does not overfit to in-sample predictions. Alternatively, new
#'   datasets can be supplied via `data_list` and `target` for transfer-learning
#'   style ensembling.
#'
#' @param caret_list a `caret_list` object
#' @param method The method to train the ensemble model. Can be a custom method or one found in `caret::modelLookup()`.
#' @param data_list A list of datasets to predict on, with each dataset matching the corresponding model in `caret_list`.
#' If `NULL`, the out-of-fold predictions from the base models will be used.
#' @param target Target parameter vector that must be provided if predicting on a new data list.
#' If `NULL`, the target vector used to train the base models will be used.
#' @param metric Metric for use with `caret::train` function.
#' If `NULL`, default metric will be constructed depending on the target type.
#' @param trControl Control for use with the `caret::train` function.
#' If `NULL`, a default control will be constructed depending on the target type.
#' @param ... Additional arguments to pass to `caret::train`
#' @return A `caret_stack` object.
#' @export
caret_stack <- function(
    caret_list,
    method,
    data_list = NULL,
    target = NULL,
    metric = NULL,
    trControl = NULL,
    ...) {

  stopifnot(inherits(caret_list, "caret_list"))

  if ((is.null(data_list) && !is.null(target)) | (!is.null(data_list) && is.null(target))) {
    stop("Both `data_list` and `target` must be provided, or neither", call. = FALSE)
  }

  if (!is.null(data_list) && !is.null(target)) {
    for (i in seq_along(data_list)) {
      if (nrow(data_list[[i]]) != length(target)) {
        stop(
          sprintf(
            "Mismatch detected: `data_list[[%d]]` has %d rows, but `target` has length %d.",
            i, nrow(data_list[[i]]), length(target)
          ),
          call. = FALSE
        )
      }
    }

    predictions <- predict.caret_list(caret_list, data_list)
  } else {
    predictions <- oof_predictions.caret_list(caret_list)

    obs <- data.table::data.table(caret_list[[1L]]$pred)
    data.table::setorderv(obs, "rowIndex")
    obs <- obs[, list(obs = obs[1L]), by = "rowIndex"]
    target <- obs[["obs"]]

  }

  .check_method(method)

  metric <- if (is.null(metric)) .default_metric(target) else metric
  trControl <- if (is.null(trControl)) .default_control(target) else trControl


  ensemble_model <- caret::train(x = predictions,
                                 y = target,
                                 method=method,
                                 metric = metric,
                                 trControl = trControl,
                                 ...)

  caret_stack <- list(
    caret_list = caret_list,
    ensemble = ensemble_model
  )

  class(caret_stack) <- "caret_stack"

  caret_stack
}


# Methods ----------------------------------------------------------------------

#' @title Create a matrix of predictions for a `caret_stack` object.
#' @param object A `caret_stack` object
#' @param data_list A list of datasets to predict on, with each dataset matching the corresponding model in `caret_list`.
#' @param excluded_class_id An integer indicating the class index to exclude from prediction output.
#' If `NULL`, no class is excluded. Default is 1L.
#' @param ... Additional arguments
#' @return A `data.table::data.table` of predictions for base and ensemble models.
#' @export
predict.caret_stack <- function(
    object,
    data_list,
    excluded_class_id = 1L,
    ...
  ) {

  base_predictions <- predict.caret_list(object$caret_list, data_list)

  ensemble <- object$ensemble

  is_classifier <- ensemble$modelType == "Classification"

  if (is_classifier && !is.function(ensemble$modelInfo$prob)) {
    stop("No probability function found. Re-fit with a the ensemble model with a method that supports prob.", call. = FALSE)
  }

  pred <- caret::predict.train(
    ensemble,
    type = if (is_classifier) "prob" else "raw",
    newdata = base_predictions,
    ...
  )

  if (!is.null(excluded_class_id)) {
    pred <- .drop_excluded_class(pred, all_classes = ensemble$levels, excluded_class_id)
  }

  if (ncol(pred) > 1) {
    names(pred) <- paste0("ensemble.", names(pred))
  } else {
    names(pred) <- "ensemble"
  }

  combined_preds <- cbind(base_predictions, pred)

  combined_preds
}

#' @title Out-of-fold predictions from a caret_stack
#' @description Retrieve the out-of-fold predictions corresponding to the best
#'   hyperparameter setting of a trained ensemble model. These predictions come from
#'   the resampling process (not the final refit) and can optionally be aggregated
#'   across resamples to produce a single prediction per training instance.
#'
#'   The base model predictions returned here are the training data for the ensemble;
#'   depending on model setup, these may be true out-of-fold predictions or simply
#'   fitted values. For classification models, the predictions always exclude the
#'   first class index.
#'
#' @param object A `caret_stack` object
#' @param excluded_class_id An integer indicating the class index to exclude from ensemble prediction output.
#' If `NONE`, no class is excluded. Default is 1L.
#' @param aggregate_resamples Logical, whether to aggregate resamples across folds.
#' @param ... Additional arguments
#' @return A `data.table::data.table` of OOF predictions
#' @export
oof_predictions.caret_stack <- function(
    object,
    excluded_class_id = 1L,
    aggregate_resamples = TRUE,
    ...
  ) {

  caret_stack <- object

  model <- caret_stack$ensemble
  is_classifier <- model$modelType == "Classification"

  if (is.null(model$control$savePredictions) | !model$control$savePredictions %in% c("all", "final", TRUE)) {
    stop("Must have savePredictions = 'all', 'final', or TRUE in trainControl for the ensemble model.", call. = FALSE)
  }

  if (is_classifier && !model$control$classProbs) {
    stop("classProbs = FALSE. Re-fit with classProbs = TRUE in trainControl for the ensemble model.", call. = FALSE)
  }

  if (is.null(model$pred) || nrow(model$pred) == 0) {
    stop("No out-of-fold predictions were generated. Check resampling setup for the ensemble model.", call. = FALSE)
  }

  pred <- .get_oof_preds(model, aggregate_resamples)

  if (!is.null(excluded_class_id)) {
    pred <- .drop_excluded_class(pred, all_classes = model$levels, excluded_class_id)
  }

  # nicely name the ensemble predictions
  if (ncol(pred) > 1) {
    names(pred) <- paste0("ensemble.", names(pred))
  } else {
    names(pred) <- "ensemble"
  }

  training_data <- subset(model$trainingData, subset = TRUE, select = -c(.outcome))

  combined_preds <- cbind(training_data, pred)

  combined_preds
}

#' @title Get a summary of a `caret_stack` object
#' @param object A `caret_stack` object
#' @param ... Additional arguments
#' @return A `data.table` of methods, tuning parameters and performance metrics for the base and ensemble model
#' @export
summary.caret_stack <- function(object, ...) {

  summary_dt <- summary.caret_list(object$caret_list)
  model <- object$ensemble
  results <- data.table::as.data.table(model$results)

  if (is.null(model$bestTune) || nrow(model$bestTune) == 0) {
    best_results <- results
  } else {
    best_tune <- as.list(model$bestTune)
    best_results <- results[best_tune, on = names(best_tune)]
  }

  best_results[, method := model$method[1]]
  best_results[, model := "ensemble"]

  summary_dt <- rbind(summary_dt, best_results, fill = TRUE)
  summary_dt
}

#' @title Plot ROC curves for individual and ensemble models in a caret_stack
#' @description This function calculates ROC curves for all base models and the ensemble model
#' using the out-of-fold predictions from a `caret_stack` object.
#' The `pROC` package is used to compute the ROC curves. ROC curves can only be constructed for binary calssifiers.
#'
#' @param object A `caret_stack` object
#' @param include_auc Whether to include AUC values in the legend. Default is `True`.
#' @param ... Additional arguments
#' @return A `ggplot2` object
#' @export
plot_roc.caret_stack <- function(
    object,
    include_auc = TRUE,
    ...
  ) {

  caret_stack <- object

  if (!(caret_stack$ensemble$modelType == "Classification" && length(caret_stack$ensemble$levels) == 2)) {
    stop("ROC curves are only available for binary classifiers.")
  }

  predictions <- oof_predictions.caret_stack(caret_stack)
  target <- caret_stack$ensemble$trainingData$.outcome

  rocs <- lapply(predictions, function(col) {
    pROC::roc(response = target, predictor = col, quiet = TRUE)
  })

  roc_data <- data.table::rbindlist(lapply(names(rocs), function(model_name) {
    roc <- rocs[[model_name]]

    results <- data.table::data.table(
      FPR = 1 - roc$specificities,
      raw_TPR = roc$sensitivities,
      Model = model_name
    )

    results <- results[, .(TPR = mean(raw_TPR)), by = .(FPR, Model)]
  }))

  aucs <- sort(sapply(rocs, pROC::auc), decreasing = TRUE)
  auc_labels <- paste0(names(aucs), " (AUC = ", round(aucs, 3), ")")

  roc_data$Model <- factor(
    roc_data$Model,
    levels = names(aucs),
    labels = if (include_auc) auc_labels else names(aucs)
  )

  ggplot2::ggplot(roc_data,
    ggplot2::aes(x = .data$FPR, y = .data$TPR, color = .data$Model, linetype = .data$Model)) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    ggplot2::labs(title = "ROC Curves", x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)") +
    ggplot2::theme_bw(base_size = 14)
}


#' @title Compute metrics with a provided metric function
#' @description The metric_function is applied to the out-of-fold predictions for the caret_stack.
#' @param object A `caret_stack` object
#' @param metric_function A function that takes two arguments `(predictions, target)`
#' and returns a single numeric value representing the metric to compute (e.g., RMSE, accuracy, AUC).
#' @param metric_name The name of the metric
#' @param descending Whether to sort in descending order. If `FALSE`, the output is sorted in ascending order. Default is `TRUE`.
#' @param ... Additional arguments
#' @return A `data.table` of metrics
#' @export
compute_metric.caret_stack <- function(
    object,
    metric_function,
    metric_name,
    descending = TRUE,
    ...
  ) {

  caret_stack <- object

  predictions <- oof_predictions.caret_stack(caret_stack)
  target <- caret_stack$ensemble$trainingData$.outcome

  metric_vals <- sapply(predictions, function(pred) {
    metric <- suppressWarnings(metric_function(pred, target))

    if (is.list(metric) | length(metric) > 1) {
      stop("metric_function must return a single value.")
    }

    metric
  })

  metric_dt <- data.table::data.table(
    Model = names(metric_vals),
    temp = as.numeric(metric_vals)
  )

  if (descending) {
    data.table::setorder(metric_dt, "temp")
  } else {
    data.table::setorder(metric_dt, -"temp")
  }

  data.table::setnames(metric_dt, "temp", metric_name)

  metric_dt
}


#' @title Plot metrics computed with a provided metric function
#' @description This function constructs a bar plot with the output of the compute metric method.
#' The bars are ordered by increasing value.
#' @param object A `caret_stack` object
#' @param metric_function A function that takes two arguments `(predictions, target)`
#' and returns a single numeric value representing the metric to compute (e.g., RMSE, accuracy, AUC).
#' @param metric_name The name of the metric
#' @param descending Whether to sort in descending order. If `FALSE`, the output is sorted in ascending order. Default is `TRUE`.
#' @param ... Additional arguments
#' @return A `ggplot2` bar chart
#' @export
plot_metric.caret_stack <- function(
    object,
    metric_function,
    metric_name,
    descending = TRUE,
    ...
  ) {

  caret_stack <- object

  metrics <- compute_metric.caret_stack(caret_stack, metric_function, metric_name, descending)
  metrics[["Model"]] <- factor(metrics[["Model"]], levels = metrics[["Model"]])

  ggplot2::ggplot(metrics,
    ggplot2::aes(x = metrics[["Model"]], y = metrics[[metric_name]], fill = metrics[["Model"]])) +
    ggplot2::geom_col() +
    ggplot2::labs(title = paste0(metric_name, " by Model"), x = NULL, y = metric_name) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(hjust = 0.5),
      legend.position = "none",
      axis.text.x  = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1, size = 14)
    )
}


#' @title Compute the relative contributions of each of the base models in the ensemble model
#' @description The relative contributions are calculated using the `caret::varImp` function on the ensemble model.
#' A scaling factor is applied to make the contributions sum to 100%.
#' @param object A `caret_stack` object
#' @param descending Whether to sort in descending order. If `FALSE`, the output is sorted in ascending order. Default is `TRUE`.
#' @param ... Additional arguments
#' @return A `ggplot2` bar chart
#' @export
compute_model_contributions.caret_stack <- function(
    object,
    descending = TRUE,
    ...
  ) {
  scaled_varImp(caret_stack$ensemble, descending = descending)
}

#' @title Plot the relative contributions of each of the base models in the ensemble model
#' @description The relative contributions are calculated using the `caret::varImp` function on the ensemble model.
#' A scaling factor is applied to make the contributions sum to 100%.
#' @param object A `caret_stack` object
#' @param descending Whether to sort in descending order. If `FALSE`, the output is sorted in ascending order. Default is `TRUE`.
#' @param ... Additional arguments
#' @return A `ggplot2` bar chart
#' @export
plot_model_contributions.caret_stack <- function(
    object,
    descending = TRUE,
    ...
  ) {

  caret_stack <- object

  importance_dt <- compute_model_contributions.caret_stack(caret_stack, descending)
  importance_dt[, Model := factor(Model, levels = Model)]

  ggplot2::ggplot(importance_dt,
    ggplot2::aes(x = Model, y = .data[["Relative Contribution"]], fill = Model)) +
    ggplot2::geom_col() +
    ggplot2::labs(title = "Relative Importance of Base Models", x = NULL, y = "Relative Contribution (%)") +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(hjust = 0.5),
      legend.position = "none",
      axis.text.x  = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1, size = 14)
    )
}


#' @description The method, tuneGrid, and trControl is identical to that of the original caret_stack.
#' @param metric_function A function that takes two arguments `(predictions, target)`
#' and returns a single numeric value representing the metric to compute (e.g., RMSE, accuracy, AUC).
compute_ablation.caret_stack <- function(
    object,
    metric_function,
    reverse = FALSE,
    ...) {
  caret_stack <- object
  ensemble <- caret_stack$ensemble

  method <- ensemble$method
  tuneGrid <- ensemble$results[, names(ensemble$bestTune), drop = FALSE]
  metric <- ensemble$metric
  trControl <- ensemble$control

  target <- ensemble$trainingData$.outcome
  training_data <- subset(oof_predictions.caret_stack(caret_stack), select = -ensemble)

  results <- data.table::data.table(Row = c(names(training_data), "metric"))


  while (ncol(training_data) > 1) {
    ensemble_model <- caret::train(x = training_data,
                                   y = target,
                                   method = method,
                                   metric = metric,
                                   trControl = trControl,
                                   tuneGrid = tuneGrid)

    imp <- scaled_varImp(ensemble_model)
    remove_model <- if (reverse) imp[which.max(`Relative Contribution`), Model] else imp[which.min(`Relative Contribution`), Model]
    training_data[[remove_model]] <- NULL

    oof_pred <- .get_oof_preds(ensemble_model, aggregate_resamples = TRUE)
    oof_pred <- .drop_excluded_class(oof_pred, all_classes = ensemble_model$levels, excluded_class_id = 1L)
    oof_pred <- as.numeric(oof_pred[[1]])


    metric_val <- metric_function(oof_pred, target)

    new_col <- c(imp[match(results$Row, imp$Model), `Relative Contribution`])
    new_col[length(new_col)] <- metric_val
    results[[paste0("Ablation_", ncol(results))]] <- new_col
  }

  results
}


plot_ablation.caret_stack <- function(
    object,
    metric_function,
    metric_name,
    reverse = FALSE,
    ...) {

  data <- t(compute_ablation.caret_stack(object, metric_function, reverse))

  print(data)


}
