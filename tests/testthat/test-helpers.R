# Setup ------------------------------------------------------------------------

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
testthat::test_that(".caret_predict", {
  numeric_new_data <- numeric_table[1:5, ]
  numeric_pred <- .caret_predict(numeric_model, new_data = numeric_new_data)

  testthat::expect_equal(ncol(numeric_pred), 1L)
  testthat::expect_equal(nrow(numeric_pred), 5L)
  testthat::expect_equal(names(numeric_pred), "pred")
  testthat::expect_true(is.numeric(numeric_pred$pred))

  factor_new_data <- numeric_table[1:5, ]
  factor_pred_no_exclude <- .caret_predict(factor_model, new_data = factor_new_data, excluded_class_id = 0L)
  factor_pred_with_exclude <- .caret_predict(factor_model, new_data = factor_new_data, excluded_class_id = 1L)

  testthat::expect_equal(ncol(factor_pred_no_exclude), 2L)
  testthat::expect_equal(nrow(factor_pred_no_exclude), 5L)
  testthat::expect_equal(names(factor_pred_no_exclude), c("Type1", "Type2"))
  testthat::expect_true(is.numeric(factor_pred_no_exclude$Type1))
  testthat::expect_true(is.numeric(factor_pred_no_exclude$Type2))

  testthat::expect_equal(ncol(factor_pred_with_exclude), 1L)
  testthat::expect_equal(nrow(factor_pred_with_exclude), 5L)
  testthat::expect_equal(names(factor_pred_with_exclude), "Type2")
  testthat::expect_true(is.numeric(factor_pred_with_exclude$Type2))

  testthat::expect_silent(nnet_pred <- .caret_predict(nnet, new_data = numeric_table))
})

# Test validating models -------------------------------------------------------
testthat::test_that(".is_classifer_and_validate", {
  sample_model <- factor_model

  sample_model$modelInfo$prob <- NULL

  sample_model_2 <- factor_model
  sample_model_2$control$savePredictions <- NULL

  sample_model_3 <- factor_model
  sample_model_3$control$savePredictions <- FALSE

  sample_model_4 <- factor_model
  sample_model_4$control$classProbs <- FALSE

  testthat::expect_true(.is_classifier_and_validate(factor_model))
  testthat::expect_error(.is_classifier_and_validate(sample_model), "No probability function found. Re-fit with a method that supports prob.")
  testthat::expect_error(.is_classifier_and_validate(sample_model_2), "Must have savePredictions = 'all', 'final', or TRUE in trainControl to do stacked predictions.")
  testthat::expect_error(.is_classifier_and_validate(sample_model_3), "Must have savePredictions = 'all', 'final', or TRUE in trainControl to do stacked predictions.")
  testthat::expect_error(.is_classifier_and_validate(sample_model_4), "classProbs = FALSE. Re-fit with classProbs = TRUE in trainControl.")
  testthat::expect_false(.is_classifier_and_validate(numeric_model))
})

testthat::test_that(".is_classifier", {
  testthat::expect_true(.is_classifier(factor_model))
  testthat::expect_false(.is_classifier(numeric_model))
})

# Test dropping excluded classes -----------------------------------------------

testthat::test_that(".drop_excluded_class", {
  testthat::expect_equal(.drop_excluded_class(numeric_table, names(numeric_table), 1L), numeric_table[, -1, with = FALSE])
  testthat::expect_equal(.drop_excluded_class(numeric_table, names(numeric_table), 0L), numeric_table)
})

testthat::test_that(".validate_excluded_class", {
  testthat::expect_error(.validate_excluded_class("a"), "classification excluded level must be numeric")
  testthat::expect_error(.validate_excluded_class(c(1L, 2L)), "classification excluded level must have a length of 1")
  testthat::expect_error(.validate_excluded_class(NA), "classification excluded level must be numeric: NA")
  testthat::expect_error(.validate_excluded_class(-1L), "classification excluded level must be >= 0")
  testthat::expect_error(.validate_excluded_class(Inf), "classification excluded level must be finite: Inf")

  testthat::expect_warning(test <- .validate_excluded_class(3.5), "classification excluded level is not an integer")
  testthat::expect_equal(test, 3L)

  testthat::expect_equal(.validate_excluded_class(1L), 1L)
  testthat::expect_equal(.validate_excluded_class(0L), 0L)


  testthat::expect_warning(test <- .validate_excluded_class(NULL), "No excluded_class_id set. Setting to 1L.")
  testthat::expect_equal(test, 1L)
})

# Test default metrics and controls --------------------------------------------

testthat::test_that("defaul_metric", {
  testthat::expect_equal(.default_metric(numeric_vector), "RMSE")
  testthat::expect_equal(.default_metric(binary_factor_vector), "ROC")
  testthat::expect_equal(.default_metric(three_factor_vector), "Accuracy")
})

testthat::test_that("default_control", {

  ctrl_numeric <- .default_control(numeric_vector)
  testthat::expect_equal(ctrl_numeric$method, "cv")
  testthat::expect_equal(ctrl_numeric$number, 5L)
  testthat::expect_false(ctrl_numeric$classProbs)
  testthat::expect_identical(ctrl_numeric$summaryFunction, caret::defaultSummary)
  testthat::expect_false(ctrl_numeric$returnData)
  testthat::expect_length(ctrl_numeric$index, 5L)


  ctrl_binary <- .default_control(binary_factor_vector)
  testthat::expect_identical(ctrl_binary$summaryFunction, caret::twoClassSummary)
  testthat::expect_true(ctrl_binary$classProbs)


  ctrl_multi <- .default_control(three_factor_vector)
  testthat::expect_identical(ctrl_multi$summaryFunction, caret::defaultSummary)
  testthat::expect_true(ctrl_multi$classProbs)
})

# Test method validation -------------------------------------------------------

testthat::test_that("check_method", {
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

  testthat::expect_silent(.check_method(valid_custom_method))
  testthat::expect_error(
    .check_method(incomplete_custom_method),
    'Custom method must be defined with a "grid" component.'
  )
  testthat::expect_error(
    .check_method(invalid_custom_method),
    'Component "library" of the custom method must be of type character.'
  )
  testthat::expect_silent(.check_method("rf"))
  testthat::expect_error(.check_method("INVALID_METHOD"))
})

# Test extracting from `caret::train` objects` ---------------------------------

testthat::test_that("exctract_train_metric", {

  result <- .extract_train_metric(numeric_model, metric = "RMSE")
  testthat::expect_equal(result$metric[1], "RMSE")
  testthat::expect_true(is.numeric(result$value))
  testthat::expect_true(is.numeric(result$sd))
  testthat::expect_false(any(is.na(result$value)))
  testthat::expect_false(any(is.na(result$sd)))

  result <- .extract_train_metric(numeric_model, metric = "InvalidMetric")
  testthat::expect_equal(result$metric[1], "RMSE")
  testthat::expect_true(is.numeric(result$value))
  testthat::expect_true(is.numeric(result$sd))

  testthat::expect_equal(.extract_train_metric(no_sd_model)$sd, NA_real_)
})

testthat::test_that("extract_best_preds", {
  testthat::expect_error(.extract_best_preds(model = "NOT_A_MODEL"))
  testthat::expect_error(.extract_best_preds(model = numeric_model, aggregate_resamples = "NOT_A_BOOLEAN"))

  model_no_pred <- numeric_model
  model_no_pred$pred <- NULL

  testthat::expect_error(.extract_best_preds(model = model_no_pred),
                         "No predictions saved during training. Please set savePredictions = 'final' in trControl")

  testthat::expect_false((!is.null(factor_model$bestTune) && nrow(factor_model$bestTune) > 0 && !(ncol(factor_model$bestTune) == 1 && factor_model$bestTune[[1]] == "none")))

  preds_no_bestTune <- .extract_best_preds(model = factor_model)
  preds_with_bestTune <- .extract_best_preds(model = numeric_model)

  testthat::expect_true(inherits(preds_no_bestTune, "data.table"))
  testthat::expect_true(inherits(preds_with_bestTune, "data.table"))

  testthat::expect_equal(nrow(preds_no_bestTune), 30)
  testthat::expect_equal(nrow(preds_with_bestTune), 30)
})

testthat::test_that("aggregate_vector", {
  testthat::expect_equal(.aggregate_vector(c(1, 2, 3, 4)), 2.5)
  testthat::expect_equal(.aggregate_vector(c("a", "b", "c")), "a")
})
