

#'
#' This seems to almost work, but getting error with the names of the list
#'
#'
#'
caret_stack <- function(
    caret_list,
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

  trControl <- if (is.null(trControl)) .default_control(target) else trControl
  metric <- if (is.null(metric)) .default_metric(target) else metric

  ensemble_model <- caret::train(predictions, obs, metric = metric, trControl = trControl)

  caret_stack <- list(
    individual_models = caret_list,
    ensemble_model = ensemble_model,
    error = ensemble_model$results,
    excluded_class_id = excluded_class_id,
    original_features = original_features
  )

  class(caret_stack) <- "caret_stack"

  caret_stack
}


# Methods ----------------------------------------------------------------------

summary.caret_stack <- function() {

}

print.caret_stack <- function() {

}


print.summary.caret_stack <- function() {

}

extract_metric.caret_stack <- function() {

}

dotplot.caret_stack <- function() {

}

autoplot.caret_stack <- function() {

}

plot.caret_stack <- function() {

}









# Helper functions -------------------------------------------------------------
