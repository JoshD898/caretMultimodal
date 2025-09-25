#' @title Trim a caret model to reduce memory usage
#' @description Removes unnecessary elements from a `caret::train` model to save memory.
#' @param model A `caret::train` object.
#' @param remove_training A boolean that controls whether or not to remove trainingData. Default is `TRUE`.
#' @return A trimmed `caret::train` object.
#' @noRd
.trim_model <- function(
    model,
    remove_training = TRUE
    ) {

  if (!is.null(model[["modelInfo"]][["trim"]])) {
    model[["finalModel"]] <- model[["modelInfo"]][["trim"]](model[["finalModel"]])
  }

  removals <- c("call", "dots", "resampledCM")
  for (i in removals) {
    if (i %in% names(model)) {
      model[[i]] <- NULL
    }
  }

  control_removals <- c("index", "indexOut", "indexFinal")
  for (i in control_removals) {
    if (!is.null(model[["control"]]) && i %in% names(model[["control"]])) {
      model[["control"]][[i]] <- NULL
    }
  }

  if (remove_training) {
    model[["trainingData"]] <- NULL
  }

  model
}
