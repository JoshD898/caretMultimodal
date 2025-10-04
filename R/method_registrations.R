# This file is where all the S3 methods for the caretMultimodal package are registered

oof_predictions <- function(object, ...) {
  UseMethod("oof_predictions")
}

plot_roc <- function(object, ...) {
  UseMethod("plot_roc")
}

compute_metric <- function(object, ...) {
  UseMethod("compute_metric")
}

plot_metric <- function(object, ...) {
  UseMethod("plot_metric")
}

compute_varimp <- function(object, ...) {
  UseMethod("compute_varimp")
}

plot_varimp <- function(object, ...) {
  UseMethod("plot_varimp")
}
