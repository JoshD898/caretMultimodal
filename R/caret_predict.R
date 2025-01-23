#' @title Prediction wrapper for \code{\link[caret]{train}}
#' @description This is a prediction wrapper for \code{\link[caret]{train}} with several features:
#' - If new_data is null, return stacked predictions from the training job, rather than in-sample predictions.
#' - Always returns probabilities for classification models.
#' - Optionally drops one predicted class for classification models.
#' - Always returns a \code{\link[data.table]{data.table}}
#' @param model a \code{\link[caret]{train}} object
#' @param new_data New data to use for predictions. If NULL, stacked predictions from the training data are returned.
#' @param excluded_class_id an integer indicating the class to exclude. If 0L, no class is excluded
#' @param aggregate_resamples logical, whether to aggregate resamples by keys. Default is TRUE.
#' @param ... additional arguments to pass to \code{\link[caret]{predict.train}}, if new_data is not NULL
#' @return A vector of predictions
#' @keywords internal
.caret_predict <- function(model,
                          new_data = NULL,
                          excluded_class_id = 1L,
                          aggregate_resamples = TRUE,
                          ...) {
  stopifnot(
    is.logical(aggregate_resamples),
    length(aggregate_resamples) == 1L,
    methods::is(model, "train")
  )

  # Extract the model type
  is_class <- .is_classifer_and_validate(model, validate_for_stacking = is.null(new_data))

  # If new_data is NULL, return the stacked predictions
  if (is.null(new_data)) {
    pred <- .extract_best_preds(model, aggregate_resamples = aggregate_resamples)
    keep_cols <- if (is_class) levels(model) else "pred"
    pred <- pred[, keep_cols, with = FALSE]

    # Otherwise, predict on new_data
  } else {
    if (any(model[["modelInfo"]][["library"]] %in% c("neuralnet", "klaR"))) {
      new_data <- as.matrix(new_data)
    }
    if (is_class) {
      pred <- stats::predict(model, type = "prob", new_data = new_data, ...)
      stopifnot(is.data.frame(pred))
    } else {
      pred <- stats::predict(model, type = "raw", new_data = new_data, ...)
      stopifnot(is.numeric(pred))
      if (!is.vector(pred)) {
        pred <- as.vector(pred) # Backwards compatability with older earth and caret::train models
      }
      stopifnot(
        is.vector(pred),
        is.numeric(pred),
        is.null(dim(pred))
      )
      pred <- unname(pred)
    }
    pred <- data.table::data.table(pred)
  }

  # In both cases (stacked predictions and new predictions), drop the excluded class
  # Make sure in both cases we have consitent column names and column order
  # Drop the excluded class for classificaiton
  stopifnot(nrow(pred) == nrow(new_data))
  if (is_class) {
    stopifnot(
      ncol(pred) == nlevels(model),
      names(pred) == levels(model)
    )
    pred <- .drop_excluded_class(pred, all_classes = levels(model), excluded_class_id)
  } else {
    stopifnot(
      ncol(pred) == 1L,
      names(pred) == "pred"
    )
  }

  pred
}


#' @title Validate a model type
#' @description Validate the model type from a \code{\link[caret]{train}} object.
#' For classification, validates that the model can predict probabilities, and,
#'  if stacked predictions are requested, that classProbs = TRUE.
#' @param model a \code{\link[caret]{train}} object
#' @param validate_for_stacking a logical indicating whether to validate the model for stacked predictions
#' @return a logical. TRUE if classifier, otherwise FALSE.
#' @keywords internal
.is_classifer_and_validate <- function(model, validate_for_stacking = TRUE) {
  stopifnot(methods::is(model, "train"))

  is_class <- .is_classifier(model)

  # Validate for predictions
  if (is_class && !is.function(model$modelInfo$prob)) {
    stop("No probability function found. Re-fit with a method that supports prob.", call. = FALSE)
  }
  # Validate for stacked predictions
  if (validate_for_stacking) {
    err <- "Must have savePredictions = 'all', 'final', or TRUE in trainControl to do stacked predictions."
    if (is.null(model$control$savePredictions)) {
      stop(err, call. = FALSE)
    }
    if (!model$control$savePredictions %in% c("all", "final", TRUE)) {
      stop(err, call. = FALSE)
    }
    if (is_class && !model$control$classProbs) {
      stop("classProbs = FALSE. Re-fit with classProbs = TRUE in trainControl.", call. = FALSE)
    }
  }

  is_class
}

#' @title Is Classifier
#' @description Check if a model is a classifier.
#' @param model A train object from the caret package.
#' @return A logical indicating whether the model is a classifier.
#' @keywords internal
.is_classifier <- function(model) {
  stopifnot(methods::is(model, "train") || methods::is(model, "caretStack"))
  if (methods::is(model, "train")) {
    out <- model$modelType == "Classification"
  } else {
    out <- model$ens_model$modelType == "Classification"
  }
  out
}


#' @title Drop Excluded Class
#' @description Drop the excluded class from a prediction data.table
#' @param x a data.table of predictions
#' @param all_classes a character vector of all classes
#' @param excluded_class_id an integer indicating the class to exclude
#' @keywords internal
.drop_excluded_class <- function(x, all_classes, excluded_class_id) {
  stopifnot(methods::is(x, "data.table"), is.character(all_classes))
  excluded_class_id <- .validate_excluded_class(excluded_class_id)
  if (length(all_classes) > 1L) {
    excluded_class <- all_classes[excluded_class_id] # Note that if excluded_class_id is 0, no class will be excluded
    classes_included <- setdiff(all_classes, excluded_class)
    x <- x[, classes_included, drop = FALSE, with = FALSE]
  }
  x
}

#' @title Validate the excluded class
#' @description Helper function to ensure that the excluded level for classification is an integer.
#' Set to 0L to exclude no class.
#' @param arg The value to check
#' @return integer
#' @keywords internal
.validate_excluded_class <- function(arg) {
  # Handle the null case (usually old object where the missing level was not defined)
  if (is.null(arg)) {
    arg <- 1L
    warning("No excluded_class_id set. Setting to 1L.", call. = FALSE)
  }
  # Check the input
  if (!is.numeric(arg)) {
    stop("classification excluded level must be numeric: ", arg, call. = FALSE)
  }
  if (length(arg) != 1L) {
    stop("classification excluded level must have a length of 1: length=", length(arg), call. = FALSE)
  }

  # Convert to integer if possible
  if (is.integer(arg)) {
    out <- arg
  } else {
    warning("classification excluded level is not an integer: ", arg, call. = FALSE)
    if (is.numeric(arg)) {
      out <- floor(arg)
    }
    out <- suppressWarnings(as.integer(out))
  }
  # Check the output
  if (!is.finite(out)) {
    stop("classification excluded level must be finite: ", arg, call. = FALSE)
  }
  if (out < 0L) {
    stop("classification excluded level must be >= 0: ", arg, call. = FALSE)
  }

  out
}
