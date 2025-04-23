#' @title Ensemble the models of a `caret_list` object
#' @description Stack several `caret::train` models from a `caret_list` object using a `caret::train` model.
#' @param caret_list a `caret_list` object
#' @param data_list A list of datasets to predict on, with each dataset matching the corresponding model in `caret_list`.
#' @param target Target parameter vector.
#' @param method The method to train the ensemble model. Can be a custom method or one found in `caret::modelLookup()`.
#' @param metric Metric for use with `caret::train` function. If `NULL`, default metric will be constructed depending on the target type.
#' @param trControl Control for use with the `caret::train` function. A default control will be constructed depending on the target type.
#' @param excluded_class_id An integer indicating the class to exclude from predictions. If 0L, no class is excluded. Default is 1L.
#' @param aggregate_resamples Boolean, whether to aggregate resamples by keys. Default is `TRUE`.
#' @return A `caret_stack` object.
#' @export
caret_stack <- function(
    caret_list,
    data_list,
    target,
    method,
    metric = NULL,
    trControl = NULL,
    excluded_class_id = 1L,
    aggregate_resamples = TRUE
    ) {

  stopifnot(inherits(caret_list, "caret_list"))

  if (length(unique(target)) == 1) {
    stop("Target vector must contain two or more classes.")
  }


  for (i in seq_along(data_list)) {
    if (nrow(data_list[[i]]) != length(target)) {
      stop("The number of rows of data_list[[", i, "]] does not match the length of the target vector.")
    }
  }

  .check_method(method)

  predictions <- predict.caret_list(
    caret_list,
    new_data_list = data_list,
    excluded_class_id = excluded_class_id,
    aggregate_resamples = aggregate_resamples
  )

  metric <- if (is.null(metric)) .default_metric(target) else metric
  trControl <- if (is.null(trControl)) .default_control(target) else trControl


  caret_list_metrics <- lapply(names(predictions), function(model_name) {
    preds <- predictions[[model_name]]
    individual_metric = .default_metric(target) # Default metric from making caret_list

    if (individual_metric == "ROC") {
      roc <- pROC::roc(target ~ preds, quiet = TRUE)
      auc <- roc$auc
      auc_ci <- pROC::ci(roc)
      auc_sd <- (auc_ci[2] - auc_ci[1]) / 2
      data.table::data.table(model = model_name, method = caret_list[[1]]$method, metric = "ROC", value = auc, sd = auc_sd)

    } else if (individual_metric == "Accuracy") {
      common_levels <- union(levels(target), levels(preds))
      cm <- caret::confusionMatrix(factor(preds, levels = common_levels), factor(target, levels = common_levels))
      acc <- cm$overall["Accuracy"]
      acc_sd <- cm$byClass["AccuracySD"]
      data.table::data.table(model = model_name, method = caret_list[[1]]$method, metric = "Accuracy", value = acc, sd = acc_sd)

    } else if (individual_metric == "RMSE") {
      res <- caret::postResample(preds, target)
      rmse <- res[1]
      rmse_sd <- res[2]
      data.table::data.table(model = model_name, method = caret_list[[1]]$method, metric = "RMSE", value = rmse, sd = rmse_sd)
    }
  })

  caret_list_metrics <- data.table::rbindlist(caret_list_metrics, use.names = TRUE, fill = TRUE)


  ensemble_model <- caret::train(x = predictions,
                                 y = target,
                                 method=method,
                                 metric = metric,
                                 trControl = trControl)

  caret_stack <- list(
    individual_models = caret_list,
    individual_metrics = caret_list_metrics,
    training_data = data_list,
    training_target = target,
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
#' @param x A `caret_stack` object
#' @param ... Additional arguments
#' @export
print.caret_stack <- function(x, ...) {
  cat("The following models were ensembled:", toString(names(x$individual_models)), " \n")
  cat("\ncaret::train model:\n")
  print(x$ensemble_model)
  cat("\nFinal model:\n")
  print(x$ensemble_model$finalModel)
}

#' @title Get a summary of a `caret_stack` object
#' @param object A `caret_stack` object
#' @param ... Additional arguments
#' @return A `summary.caret_stack` object
#' @export
summary.caret_stack <- function(object, ...) {
  metric <- object$ensemble_model$metric
  imp <- caret::varImp(object$ensemble_model$finalModel, scale = TRUE)
  imp$Overall <- imp$Overall / sum(imp$Overall) * 100

  out <- list(
    models = toString(names(object$individual_models)),
    imp = imp,
    metric = metric,
    results = extract_metric(object)
  )
  class(out) <- "summary.caret_stack"
  out
}

#' @title Print a summary of a `caret_stack` object
#' @param x A `summary.caret_stack` object
#' @param ... Additional arguments
#' @export
print.summary.caret_stack <- function(x, ...) {
  cat("The following models were ensembled:", x$models, " \n")
  cat("\nRelative importance:\n")
  print(x$imp)
  cat("\nModel metrics (based on caret_stack training data):\n")
  print(x$results)
}

#' @title Extract metrics from a `caret_stack` object
#' @description Produce metrics for how the caret_list models and ensemble model perform on the training data for the caret stack
#' @param x A `caret_stack` object
#' @param ... Additional arguments
#' @return A `data.table::data.table` of metrics
#' @export
extract_metric.caret_stack <- function(x, ...) {
  ensemble_metrics <- .extract_train_metric(x$ensemble_model)
  individual_metrics <- x$individual_metrics

  data.table::set(ensemble_metrics, j = "model", value = "ensemble")
  data.table::setcolorder(ensemble_metrics, c("model", setdiff(names(ensemble_metrics), "model")))

  all_metrics <- rbind(ensemble_metrics, individual_metrics, ignore.attr = TRUE)
  all_metrics
}

#' @title Plot various metrics of a `caret_stack` object
#' @param x The `caret_stack` object to plot metrics for
#' @param ... Additional arguments
#' @return A `ggplot` object
#' @export
plot.caret_stack <- function(x, ...) {
  dat <- extract_metric(x)
  summary <- summary(x)

  model_order <- unique(dat[["model"]])
  dat[["model"]] <- factor(dat[["model"]], levels = model_order)

  metric_plot <- ggplot2::ggplot(
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
    ggplot2::labs(title = "Model Metrics", x = "", y = "Metric Value")

  imp <- summary$imp
  importance_plot <- ggplot2::ggplot(
    imp,
    ggplot2::aes(
      x = reorder(rownames(imp), Overall),
      y = Overall
    )
  ) +
    ggplot2::geom_bar(stat = "identity", fill = "skyblue") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Relative Importance of Individual Models", x = "", y = "Relative Importance (%)")

  if (x$ensemble_model$metric == "ROC") {
    out <- patchwork::wrap_plots(metric_plot, importance_plot, .plot_roc(x), ncol = 1)
  } else {
    out <- patchwork::wrap_plots(metric_plot, importance_plot, ncol = 1)
  }

  out

}

# Helper functions -------------------------------------------------------------


#' @title Plot ROC curves for individual and ensemble models in a caret_stack
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
      dplyr::group_by(roc_data, FPR),
      mean_TPR = mean(TPR),
      .groups = 'drop'
    )

    roc_data_grouped$Model <- model_name
    roc_data_grouped
  }

    roc_data_list <- lapply(names(predictions), function(model_name) {
    preds <- predictions[[model_name]]
    get_roc_data(model_name, preds)
  })

  ensemble_preds <- predict(caret_stack)[[1]]
  ensemble_roc_data <- get_roc_data("ensemble", ensemble_preds)

  roc_data <- do.call(rbind, roc_data_list)
  roc_data <- rbind(ensemble_roc_data, roc_data)

  roc_data$Model <- factor(roc_data$Model, levels = c("ensemble", names(predictions)))

  ggplot(roc_data, aes(x = FPR, y = mean_TPR, color = Model, linetype = Model)) +
    geom_line(size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(title = "ROC Curves",
         x = "False Positive Rate (FPR)",
         y = "True Positive Rate (TPR)") +
    theme_bw() +
    scale_linetype_manual(values = c("solid", "dashed", "dotdash", "twodash", "solid"))
}


