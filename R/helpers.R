#' @title Prediction wrapper for `caret::train`
#' @description This is a prediction wrapper for `caret::train` with several features:
#' - If new_data is null, return stacked predictions from the training job, rather than in-sample predictions.
#' - Always returns probabilities for classification models.
#' - Optionally drops one predicted class for classification models.
#' - Always returns a `data.table::data.table`
#' @param model a `caret::train` object
#' @param new_data New data to use for predictions. If `NULL`, stacked predictions from the training data are returned.
#' @param excluded_class_id an integer indicating the class to exclude. If 0L, no class is excluded
#' @param aggregate_resamples logical, whether to aggregate resamples by keys. Default is TRUE.
#' @param ... additional arguments to pass to \code{\link[caret]{predict.train}}, if new_data is not NULL
#' @return A vector of predictions
#' @noRd
.caret_predict <- function(model,
                          new_data = NULL,
                          excluded_class_id = 1L,
                          aggregate_resamples = TRUE,
                          ...) {
  stopifnot(
    is.logical(aggregate_resamples),
    length(aggregate_resamples) == 1L,
    inherits(model, "train")
  )

  is_class <- .is_classifier_and_validate(model, validate_for_stacking = is.null(new_data))

  if (is.null(new_data)) {
    pred <- .extract_best_preds(model, aggregate_resamples = aggregate_resamples)
    keep_cols <- if (is_class) levels(model) else "pred"
    pred <- pred[, keep_cols, with = FALSE]

  } else {
    if (any(model[["modelInfo"]][["library"]] %in% c("neuralnet", "klaR"))) {
      new_data <- as.matrix(new_data)
    }
    if (is_class) {
      pred <- caret::predict.train(model, type = "prob", newdata = new_data, ...)

    } else {
      pred <- caret::predict.train(model, type = "raw", newdata = new_data, ...)

      pred <- as.vector(pred)

      stopifnot(
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

# Validating models ------------------------------------------------------------

#' @title Validate a model type
#' @description Validate the model type from a `caret::train` object.
#' For classification, validates that the model can predict probabilities, and,
#'  if stacked predictions are requested, that classProbs = TRUE.
#' @param model a `caret::train` object
#' @param validate_for_stacking a logical indicating whether to validate the model for stacked predictions
#' @return a logical. TRUE if classifier, otherwise FALSE.
#' @noRd
.is_classifier_and_validate <- function(model, validate_for_stacking = TRUE) {
  stopifnot(inherits(model, "train"))

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
#' @noRd
.is_classifier <- function(model) {
  stopifnot(inherits(model, "train"))

  out <- model$modelType == "Classification"

  out
}

# Dropping excluded classes ----------------------------------------------------

#' @title Drop Excluded Class
#' @description Drop the excluded class from a prediction data.table
#' @param pred a data.table of predictions
#' @param all_classes a character vector of all classes
#' @param excluded_class_id an integer indicating the class to exclude
#' @return pred with the excluded_class_id column dropped
#' @noRd
.drop_excluded_class <- function(pred, all_classes, excluded_class_id) {
  stopifnot(inherits(pred, "data.table"), is.character(all_classes))
  excluded_class_id <- .validate_excluded_class(excluded_class_id)
  if (length(all_classes) > 1L) {
    excluded_class <- all_classes[excluded_class_id]
    classes_included <- setdiff(all_classes, excluded_class)
    pred <- pred[, classes_included, drop = FALSE, with = FALSE]
  }
  pred
}

#' @title Validate the excluded class
#' @description Helper function to ensure that the excluded level for classification is an integer.
#' Set to 0L to exclude no class.
#' @param excluded_class_id The value to check
#' @return integer
#' @noRd
.validate_excluded_class <- function(excluded_class_id) {

  if (is.null(excluded_class_id)) {
    excluded_class_id <- 1L
    warning("No excluded_class_id set. Setting to 1L.", call. = FALSE)
  }

  if (!is.numeric(excluded_class_id)) {
    stop("classification excluded level must be numeric: ", excluded_class_id, call. = FALSE)
  }
  if (length(excluded_class_id) != 1L) {
    stop("classification excluded level must have a length of 1: length=", length(excluded_class_id), call. = FALSE)
  }

  if (is.integer(excluded_class_id)) {
    out <- excluded_class_id
  } else {
    warning("classification excluded level is not an integer: ", excluded_class_id, call. = FALSE)
    if (is.numeric(excluded_class_id)) {
      out <- floor(excluded_class_id)
    }
    out <- suppressWarnings(as.integer(out))
  }

  if (!is.finite(out)) {
    stop("classification excluded level must be finite: ", excluded_class_id, call. = FALSE)
  }
  if (out < 0L) {
    stop("classification excluded level must be >= 0: ", excluded_class_id, call. = FALSE)
  }

  out
}

# Default metrics and controls -------------------------------------------------

#' @title Construct a default metric for use with caret_list
#' @description Determines the default metric based on the type of target vector.
#' For classification, defaults to "ROC" if the target is binary and "Accuracy" otherwise.
#' For regression, defaults to "RMSE".
#' @param target Target parameter vector, either numeric for regression or a factor/character for classification.
#' @return A character string indicating the default metric
#' @noRd
.default_metric <- function(target) {
  is_class <- is.factor(target) || is.character(target)
  is_binary <- length(unique(target)) == 2L

  if (is_class) {
    if (is_binary) {
      "ROC"
    } else {
      "Accuracy"
    }
  } else {
    "RMSE"
  }
}

#' @title Construct a default train control for use with caret_list
#' @description Unlike `caret::trainControl()`, this function defaults to 5 fold CV.
#' CV is good for stacking, as every observation is in the test set exactly once.
#' 5 is used instead of 10 to save compute time, as caret_list is for fitting many
#' models. Explicit fold indexes are constructed, and predictions are saved for stacking.
#' For classification models, class probabilities are returned.
#' @param target Target parameter vector, either numeric for regression or a factor/character for classification.
#' @return A `caret::trainControl` object
#' @noRd
.default_control <- function(target) {
  is_class <- is.factor(target) || is.character(target)
  is_binary <- length(unique(target)) == 2L

  caret::trainControl(
    method = "cv",
    number = 5L,
    index = caret::createFolds(target, k = 5L, list = TRUE, returnTrain = TRUE),
    savePredictions = "final",
    classProbs = is_class,
    summaryFunction = ifelse(is_class && is_binary, caret::twoClassSummary, caret::defaultSummary),
    returnData = FALSE
  )
}



# Method validation ------------------------------------------------------------

#' @title Check that the method supplied by the user is a valid caret method
#' @description Uses `caret::modelLookup()` to ensure the method supplied by the user
#' is a model caret can fit
#' @param method A user-supplied caret method or modelInfo list
#' @return NULL
#' @noRd
.check_method <- function(method) {
  supported_models <- unique(caret::modelLookup()[["model"]])

  if (is.list(method)) {
    .check_custom_method(method)
  } else if (!(method %in% supported_models)) {
    stop(
      "Method \"", method, "\" is invalid. Method must either be a character name ",
      "supported by caret (e.g., \"gbm\") or a modelInfo list ",
      "(e.g., caret::getModelInfo(\"gbm\", regex = FALSE))",
      call. = FALSE
    )
  }
}

#' @title Check that a custom caret method has required elements
#' @description Checks for mandatory elements documented here:
#' https://topepo.github.io/caret/using-your-own-model-in-train.html
#' @param method The method to be validated
#' @return NULL
#' @noRd
.check_custom_method <- function(method) {
  required_elements <- list(
    library = "character",
    type = "character",
    parameters = "data.frame",
    grid = "function",
    fit = "function",
    predict = "function",
    prob = "function",
    sort = "function"
  )

  for (element in names(required_elements)) {
    if (!(element %in% names(method))) {
      stop(sprintf('Custom method must be defined with a "%s" component.', element))
    }

    if (!inherits(method[[element]], required_elements[[element]])) {
      stop(sprintf('Component "%s" of the custom method must be of type %s.', element, required_elements[[element]]))
    }
  }
}



# Extracting from `caret::train` objects` --------------------------------------

#' @title Extract accuracy metrics from a `caret::train` model
#' @description Extract the cross-validated accuracy metrics and their standard deviations.
#' @param x a `caret::train` object
#' @param metric a character string representing the metric to extract. If NULL, uses the metric that was used to train the model.
#' @return A `data.table::data.table` with the name, value and standard deviation of the metric
#' @noRd
.extract_train_metric <- function(model, metric = NULL) {
  if (is.null(metric) || !metric %in% names(model$results)) {
    metric <- model$metric
  }

  results <- data.table::data.table(model$results, key = names(model$bestTune))
  best_tune <- data.table::data.table(model$bestTune, key = names(model$bestTune))

  best_results <- results[best_tune, ]

  value <- best_results[[metric]]
  stdev <- best_results[[paste0(metric, "SD")]]
  if (is.null(stdev)) stdev <- NA_real_

  out <- data.table::data.table(
    method = model$method,
    metric = metric,
    value = value,
    sd = stdev
  )
  out
}

# import data.table is necessary, if not included some data.table methods are not recognized

#' @title Extract the best predictions from a `caret::train` object
#' @description Extract the best predictions from a `caret::train` object.
#' @import data.table
#' @param model A `caret::train` object
#' @param aggregate_resamples Logical, whether to aggregate resamples by keys. Default is TRUE.
#' @return A `data.table::data.table` with predictions
#' @noRd
.extract_best_preds <- function(model, aggregate_resamples = TRUE) {
  stopifnot(is.logical(aggregate_resamples), length(aggregate_resamples) == 1L, inherits(model, "train"))

  if (is.null(model[["pred"]])) {
    stop("No predictions saved during training. Please set savePredictions = 'final' in trControl", call. = FALSE)
  }

  stopifnot(inherits(model$pred, "data.frame"))

  if (!is.null(model$bestTune) && nrow(model$bestTune) > 0 && !(ncol(model$bestTune) == 1 && model$bestTune[[1]] == "none")) {
    keys <- names(model$bestTune)
    best_tune <- data.table::data.table(model$bestTune, key = keys)

    pred <- data.table::data.table(model$pred, key = keys)
    pred <- pred[best_tune, ]
  } else {
    pred <- data.table::data.table(model$pred)
  }

  keys <- "rowIndex"
  data.table::setkeyv(pred, keys)

  if (aggregate_resamples) {
    pred <- pred[, lapply(.SD, .aggregate_vector), by = keys]
  }

  data.table::setorderv(pred, keys)

  pred
}

#' @title Aggregate vector
#' @description Aggregate a vector depending on its type. For numeric data return the mean. For factor data return the first value.
#' @param x A vector to be aggregated.
#' @return The mean of numeric data or the first value of non-numeric data.
#' @noRd
.aggregate_vector <- function(x) {
  if (is.numeric(x)) {
    mean(x)
  } else {
    x[[1L]]
  }
}

