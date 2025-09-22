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
#' @param caret_stack A `caret_stack` object
#' @param data_list A list of datasets to predict on, with each dataset matching the corresponding model in `caret_list`.
#' @param excluded_class_id An integer indicating the class index to exclude from prediction output.
#' If `NULL`, no class is excluded. Default is 1L.
#' @param include_base Default is FALSE. Whether to include the predictions of the base models in the final data table. The base model predictions will always have an excluded_class_id of 1L.
#' @return A `data.table::data.table` of predictions
#' @export
predict.caret_stack <- function(
    caret_stack,
    data_list,
    excluded_class_id = 1L,
    include_base = TRUE,
    ...) {

  base_predictions <- predict.caret_list(caret_stack$caret_list, data_list)

  ensemble <- caret_stack$ensemble

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



  if (include_base) {

  } else {
    out <- data.table::as.data.table()
  }



}

# TODO WRITE DESCRIPTION
oof_predictions.caret_stack <- function(
    caret_stack,
    excluded_class_id = 1L,
    aggregate_resamples = TRUE
    ) {

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

  base_oof_predictions <- oof_predictions.caret_list(caret_stack$caret_list, excluded_class_id, aggregate_resamples)

  combined_preds <- cbind(base_oof_predictions, pred)

  combined_preds
}

#' @title Get a summary of a `caret_stack` object
#' @param object A `caret_stack` object
#' @param ... Additional arguments
#' @return A `summary.caret_stack` object
#' @export
#' TODO MAKE THIS ONE TABLE WITH NA FILLING
summary.caret_stack <- function(object, ...) {

  print(summary.caret_list(object$caret_list))


  model <- object$ensemble
  results <- data.table::as.data.table(model$results)

  if (is.null(model$bestTune) || nrow(model$bestTune) == 0) {
    best_results <- results
  } else {
    best_tune <- as.list(model$bestTune)
    best_results <- results[best_tune, on = names(best_tune)]
  }

  print(best_results)
}

# Helper functions -------------------------------------------------------------


#' @title Plot ROC curves for individual and ensemble models in a caret_stack
#' @description This function calculates an ROC curve for all the base models and ensemble model based on their out of fold predictions.
#' Optionally, you can provide new data and a target variable
#' @importFrom rlang .data
#' @param caret_stack The caret_stack to plot
#' @return A `ggplot2` object
#' @noRd
.plot_roc <- function(caret_stack) {
  predictions <- predict.caret_list(
    caret_stack$individual_models,
    new_data_list = caret_stack$training_data,
    excluded_class_id = caret_stack$excluded_class_id,
    aggregate_resamples = FALSE
  )

  target <- caret_stack$training_target

  get_roc_data <- function(model_name, preds) {
    roc <- pROC::roc(target ~ preds, quiet = TRUE)
    tpr <- roc$sensitivities
    fpr <- 1 - roc$specificities

    roc_data <- data.frame(
      FPR = fpr,
      TPR = tpr,
      Model = model_name
    )

    roc_data_grouped <- dplyr::summarise(
      dplyr::group_by(roc_data, .data$FPR),
      mean_TPR = mean(.data$TPR),
      .groups = "drop"
    )

    roc_data_grouped$Model <- model_name
    roc_data_grouped
  }

  roc_data_list <- lapply(names(predictions), function(model_name) {
    preds <- predictions[[model_name]]
    get_roc_data(model_name, preds)
  })

  ensemble_preds <- stats::predict(caret_stack)[[1]]
  ensemble_roc_data <- get_roc_data("ensemble", ensemble_preds)

  roc_data <- do.call(rbind, roc_data_list)
  roc_data <- rbind(ensemble_roc_data, roc_data)
  roc_data$Model <- factor(roc_data$Model, levels = c("ensemble", names(predictions)))

  ggplot2::ggplot(roc_data, ggplot2::aes(x = .data$FPR, y = .data$mean_TPR, color = .data$Model, linetype = .data$Model)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    ggplot2::labs(title = "ROC Curves", x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)") +
    ggplot2::theme_bw() +
    ggplot2::scale_linetype_manual(values = c("solid", "dashed", "dotdash", "twodash", "solid"))
}


