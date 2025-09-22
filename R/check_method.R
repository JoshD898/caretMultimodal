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
