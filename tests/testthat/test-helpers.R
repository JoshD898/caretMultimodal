# Setup ------------------------------------------------------------------------
library(testthat)

set.seed(192L)

numeric_vector <- runif(30)
binary_factor_vector <- factor(sample(c("Type1", "Type2"), 30, replace = TRUE))
three_factor_vector <- factor(sample(c("Type1", "Type2", "Type3"), 30, replace = TRUE))

numeric_table <- data.table::data.table(
  Var1 = runif(30),
  Var2 = rnorm(30),
  Var3 = runif(30, 10, 50),
  Var4 = rpois(30, lambda = 5),
  Var5 = rnorm(30, mean = 100, sd = 15)
)

factor_table <- data.table::data.table(
  Var1 = factor(sample(c("A", "B", "C"), 30, replace = TRUE)),
  Var2 = factor(sample(c("X", "Y", "Z"), 30, replace = TRUE)),
  Var3 = factor(sample(c("Red", "Blue", "Green"), 30, replace = TRUE)),
  Var4 = factor(sample(c("Small", "Medium", "Large"), 30, replace = TRUE)),
  Var5 = factor(sample(c("Yes", "No"), 30, replace = TRUE))
)

mixed_table <- data.table::data.table(
  Num1 = runif(30),
  Num2 = rnorm(30),
  Num3 = rpois(30, lambda = 5),
  Factor1 = factor(sample(c("Apple", "Banana", "Cherry"), 30, replace = TRUE)),
  Factor2 = factor(sample(c("Hot", "Cold"), 30, replace = TRUE))
)

unnamed_data_list = list(numeric_table,
                         factor_table,
                         mixed_table)

named_data_list = list(numeric = numeric_table,
                       factor = factor_table,
                       mixed = mixed_table)

numeric_model <- suppressWarnings(caret::train(x = numeric_table, y = numeric_vector, method = "rf", trControl = .default_control(numeric_vector)))
factor_model <- suppressWarnings(caret::train(x = numeric_table, y = binary_factor_vector, method = "glm", trControl = .default_control(binary_factor_vector)))
nnet <- suppressWarnings(caret::train(x = numeric_table, y = numeric_vector, method = "neuralnet", trControl = .default_control(numeric_vector)))
no_sd_model <- suppressWarnings(caret::train(x = numeric_table, y = binary_factor_vector, method = "glm", trControl = caret::trainControl(method = "none", savePredictions = TRUE, classProbs = TRUE)))
# Test caret prediction wrapper ------------------------------------------------
test_that(".caret_predict", {
  numeric_new_data <- numeric_table[1:5, ]
  numeric_pred <- .caret_predict(numeric_model, new_data = numeric_new_data)

  expect_equal(ncol(numeric_pred), 1L)
  expect_equal(nrow(numeric_pred), 5L)
  expect_equal(names(numeric_pred), "pred")
  expect_true(is.numeric(numeric_pred$pred))

  factor_new_data <- numeric_table[1:5, ]
  factor_pred_no_exclude <- .caret_predict(factor_model, new_data = factor_new_data, excluded_class_id = 0L)
  factor_pred_with_exclude <- .caret_predict(factor_model, new_data = factor_new_data, excluded_class_id = 1L)

  expect_equal(ncol(factor_pred_no_exclude), 2L)
  expect_equal(nrow(factor_pred_no_exclude), 5L)
  expect_equal(names(factor_pred_no_exclude), c("Type1", "Type2"))
  expect_true(is.numeric(factor_pred_no_exclude$Type1))
  expect_true(is.numeric(factor_pred_no_exclude$Type2))

  expect_equal(ncol(factor_pred_with_exclude), 1L)
  expect_equal(nrow(factor_pred_with_exclude), 5L)
  expect_equal(names(factor_pred_with_exclude), "Type2")
  expect_true(is.numeric(factor_pred_with_exclude$Type2))

  expect_silent(nnet_pred <- .caret_predict(nnet, new_data = numeric_table))
})

# Test validating models -------------------------------------------------------
test_that(".is_classifer_and_validate", {
  sample_model <- factor_model

  sample_model$modelInfo$prob <- NULL

  sample_model_2 <- factor_model
  sample_model_2$control$savePredictions <- NULL

  sample_model_3 <- factor_model
  sample_model_3$control$savePredictions <- FALSE

  sample_model_4 <- factor_model
  sample_model_4$control$classProbs <- FALSE

  expect_true(.is_classifier_and_validate(factor_model))
  expect_error(.is_classifier_and_validate(sample_model), "No probability function found. Re-fit with a method that supports prob.")
  expect_error(.is_classifier_and_validate(sample_model_2), "Must have savePredictions = 'all', 'final', or TRUE in trainControl to do stacked predictions.")
  expect_error(.is_classifier_and_validate(sample_model_3), "Must have savePredictions = 'all', 'final', or TRUE in trainControl to do stacked predictions.")
  expect_error(.is_classifier_and_validate(sample_model_4), "classProbs = FALSE. Re-fit with classProbs = TRUE in trainControl.")
  expect_false(.is_classifier_and_validate(numeric_model))
})

test_that(".is_classifier", {
  expect_true(.is_classifier(factor_model))
  expect_false(.is_classifier(numeric_model))
})

# Test dropping excluded classes -----------------------------------------------

test_that(".drop_excluded_class", {
  expect_equal(.drop_excluded_class(numeric_table, names(numeric_table), 1L), numeric_table[, -1, with = FALSE])
  expect_equal(.drop_excluded_class(numeric_table, names(numeric_table), 0L), numeric_table)
})

test_that(".validate_excluded_class", {
  expect_error(.validate_excluded_class("a"), "classification excluded level must be numeric")
  expect_error(.validate_excluded_class(c(1L, 2L)), "classification excluded level must have a length of 1")
  expect_error(.validate_excluded_class(NA), "classification excluded level must be numeric: NA")
  expect_error(.validate_excluded_class(-1L), "classification excluded level must be >= 0")
  suppressWarnings(expect_error(.validate_excluded_class(Inf), "classification excluded level must be finite: Inf"))

  expect_warning(test <- .validate_excluded_class(3.5), "classification excluded level is not an integer")
  expect_equal(test, 3L)

  expect_equal(.validate_excluded_class(1L), 1L)
  expect_equal(.validate_excluded_class(0L), 0L)


  expect_warning(test <- .validate_excluded_class(NULL), "No excluded_class_id set. Setting to 1L.")
  expect_equal(test, 1L)
})

# Test default metrics and controls --------------------------------------------

test_that("defaul_metric", {
  expect_equal(.default_metric(numeric_vector), "RMSE")
  expect_equal(.default_metric(binary_factor_vector), "ROC")
  expect_equal(.default_metric(three_factor_vector), "Accuracy")
})

test_that("default_control", {

  ctrl_numeric <- .default_control(numeric_vector)
  expect_equal(ctrl_numeric$method, "cv")
  expect_equal(ctrl_numeric$number, 5L)
  expect_false(ctrl_numeric$classProbs)
  expect_identical(ctrl_numeric$summaryFunction, caret::defaultSummary)
  expect_false(ctrl_numeric$returnData)
  expect_length(ctrl_numeric$index, 5L)


  ctrl_binary <- .default_control(binary_factor_vector)
  expect_identical(ctrl_binary$summaryFunction, caret::twoClassSummary)
  expect_true(ctrl_binary$classProbs)


  ctrl_multi <- .default_control(three_factor_vector)
  expect_identical(ctrl_multi$summaryFunction, caret::defaultSummary)
  expect_true(ctrl_multi$classProbs)
})

# Test method validation -------------------------------------------------------

test_that("check_method", {
  valid_custom_method <- list(
    library = "customLib",
    type = "classification",
    parameters = data.frame(),
    grid = function() NULL,
    fit = function() NULL,
    predict = function() NULL,
    prob = function() NULL,
    sort = function() NULL
  )

  invalid_custom_method <- list(
    library = 123,  # Should be character
    type = "classification",
    parameters = data.frame(),
    grid = function() NULL,
    fit = function() NULL,
    predict = function() NULL,
    prob = function() NULL,
    sort = function() NULL
  )

  incomplete_custom_method <- list(
    library = "customLib",
    type = "classification",
    parameters = data.frame(),
    fit = function() NULL
  )

  expect_silent(.check_method(valid_custom_method))
  expect_error(
    .check_method(incomplete_custom_method),
    'Custom method must be defined with a "grid" component.'
  )
  expect_error(
    .check_method(invalid_custom_method),
    'Component "library" of the custom method must be of type character.'
  )
  expect_silent(.check_method("rf"))
  expect_error(.check_method("INVALID_METHOD"))
})

# Test extracting from `caret::train` objects` ---------------------------------

test_that("exctract_train_metric", {

  result <- .extract_train_metric(numeric_model, metric = "RMSE")
  expect_equal(result$metric[1], "RMSE")
  expect_true(is.numeric(result$value))
  expect_true(is.numeric(result$sd))
  expect_false(any(is.na(result$value)))
  expect_false(any(is.na(result$sd)))

  result <- .extract_train_metric(numeric_model, metric = "InvalidMetric")
  expect_equal(result$metric[1], "RMSE")
  expect_true(is.numeric(result$value))
  expect_true(is.numeric(result$sd))

  expect_equal(.extract_train_metric(no_sd_model)$sd, NA_real_)
})

test_that("extract_best_preds", {
  expect_error(.extract_best_preds(model = "NOT_A_MODEL"))
  expect_error(.extract_best_preds(model = numeric_model, aggregate_resamples = "NOT_A_BOOLEAN"))

  model_no_pred <- numeric_model
  model_no_pred$pred <- NULL

  expect_error(.extract_best_preds(model = model_no_pred),
                         "No predictions saved during training. Please set savePredictions = 'final' in trControl")

  expect_false((!is.null(factor_model$bestTune) && nrow(factor_model$bestTune) > 0 && !(ncol(factor_model$bestTune) == 1 && factor_model$bestTune[[1]] == "none")))

  preds_no_bestTune <- .extract_best_preds(model = factor_model)
  preds_with_bestTune <- .extract_best_preds(model = numeric_model)

  expect_true(inherits(preds_no_bestTune, "data.table"))
  expect_true(inherits(preds_with_bestTune, "data.table"))

  expect_equal(nrow(preds_no_bestTune), 30)
  expect_equal(nrow(preds_with_bestTune), 30)
})

test_that("aggregate_vector", {
  expect_equal(.aggregate_vector(c(1, 2, 3, 4)), 2.5)
  expect_equal(.aggregate_vector(c("a", "b", "c")), "a")
})
