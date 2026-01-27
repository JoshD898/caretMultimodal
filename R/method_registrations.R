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

compute_model_contributions <- function(object, ...) {
  UseMethod("compute_model_contributions")
}

plot_model_contributions <- function(object, ...) {
  UseMethod("plot_model_contributions")
}

compute_ablation <- function(object, ...) {
  UseMethod("compute_ablation")
}

plot_ablation <- function(object, ...) {
  UseMethod("plot_ablation")
}

compute_feature_contributions <- function(object, ...) {
  UseMethod("compute_feature_contributions")
}

plot_feature_contributions <- function(object, ...) {
  UseMethod("plot_feature_contributions")
}

