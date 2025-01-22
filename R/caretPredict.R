#' @title Prediction wrapper for \code{\link[caret]{train}}
#' @description This is a prediction wrapper for \code{\link[caret]{train}} with several features:
#' - If newdata is null, return stacked predictions from the training job, rather than in-sample predictions.
#' - Always returns probabilities for classification models.
#' - Optionally drops one predicted class for classification models.
#' - Always returns a \code{\link[data.table]{data.table}}
#' @param object a \code{\link[caret]{train}} object
#' @param newdata New data to use for predictions. If NULL, stacked predictions from the training data are returned.
#' @param excluded_class_id an integer indicating the class to exclude. If 0L, no class is excluded
#' @param aggregate_resamples logical, whether to aggregate resamples by keys. Default is TRUE.
#' @param ... additional arguments to pass to \code{\link[caret]{predict.train}}, if newdata is not NULL
#' @return a data.table
#' @keywords internal
.CaretPredict <- function(object,
                         newdata = NULL,
                         excluded_class_id = 1L,
                         aggregate_resamples = TRUE,
                         ...) {

  #TODO make type checks concistent accross project
  stopifnot(is.logical(aggregate_resamples),
            length(aggregate_resamples) == 1L,
            methods::is(object, "train"))

  # Extract the model type
  is_class <- isClassifierAndValidate(object, validate_for_stacking = is.null(newdata))

  # If newdata is NULL, return the stacked predictions
  if (is.null(newdata)) {
    pred <- extractBestPreds(object, aggregate_resamples = aggregate_resamples)
    keep_cols <- if (is_class) levels(object) else "pred"
    pred <- pred[, keep_cols, with = FALSE]

    # Otherwise, predict on newdata
  } else {
    if (any(object[["modelInfo"]][["library"]] %in% c("neuralnet", "klaR"))) {
      newdata <- as.matrix(newdata)
    }
    if (is_class) {
      pred <- stats::predict(object, type = "prob", newdata = newdata, ...)
      stopifnot(is.data.frame(pred))
    } else {
      pred <- stats::predict(object, type = "raw", newdata = newdata, ...)
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
  stopifnot(nrow(pred) == nrow(newdata))
  if (is_class) {
    stopifnot(
      ncol(pred) == nlevels(object),
      names(pred) == levels(object)
    )
    pred <- dropExcludedClass(pred, all_classes = levels(object), excluded_class_id)
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
#' @param object a \code{\link[caret]{train}} object
#' @param validate_for_stacking a logical indicating whether to validate the object for stacked predictions
#' @return a logical. TRUE if classifier, otherwise FALSE.
#' @keywords internal
.IsClassifierAndValidate <- function(object, validate_for_stacking = TRUE) {
  stopifnot(methods::is(object, "train"))

  is_class <- .IsClassifier(object)

  # Validate for predictions
  if (is_class && !is.function(object$modelInfo$prob)) {
    stop("No probability function found. Re-fit with a method that supports prob.", call. = FALSE)
  }
  # Validate for stacked predictions
  if (validate_for_stacking) {
    err <- "Must have savePredictions = 'all', 'final', or TRUE in trainControl to do stacked predictions."
    if (is.null(object$control$savePredictions)) {
      stop(err, call. = FALSE)
    }
    if (!object$control$savePredictions %in% c("all", "final", TRUE)) {
      stop(err, call. = FALSE)
    }
    if (is_class && !object$control$classProbs) {
      stop("classProbs = FALSE. Re-fit with classProbs = TRUE in trainControl.", call. = FALSE)
    }
  }

  # Return
  is_class
}

#' @title Is Classifier
#' @description Check if a model is a classifier.
#' @param model A train object from the caret package.
#' @return A logical indicating whether the model is a classifier.
#' @keywords internal
.IsClassifier <- function(model) {
  stopifnot(methods::is(model, "train") || methods::is(model, "caretStack"))
  if (methods::is(model, "train")) {
    out <- model$modelType == "Classification"
  } else {
    out <- model$ens_model$modelType == "Classification"
  }
  out
}
