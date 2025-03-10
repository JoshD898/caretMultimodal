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

named_models <- caret_list(target = numeric_vector, data_list = named_data_list, method = "rf")
unnamed_models <- caret_list(target = numeric_vector, data_list = unnamed_data_list, method = "rf")

# Constructor tests ------------------------------------------------------------

testthat::test_that("caret_list", {
  testthat::expect_error(caret_list(target = numeric_vector, data_list = list(head(numeric_table, 29)), method = "glm"),
               "The number of rows of data_list\\[\\[1\\]\\] does not match the length of the target vector.")

  testthat::expect_equal(names(named_models), c("numeric_model", "factor_model", "mixed_model"))
  testthat::expect_equal(names(unnamed_models), c("data_list[[1]]_model", "data_list[[2]]_model", "data_list[[3]]_model"))

  testthat::expect_true(inherits(named_models, "caret_list"))
})
# Method tests -----------------------------------------------------------------

testthat::test_that("predict.caret_list", {
  testthat::expect_error(predict(named_models, new_data_list = list(mixed_table)),
               "The length of new_data_list must be the same length as caret_list")

  testthat::expect_error(predict(named_models, new_data_list = list(mixed_table, head(factor_table, 29), numeric_table)),
               "All matrices in new_data_list must have the same number of rows")

  preds <- predict(named_models)

  testthat::expect_true(inherits(preds, "data.table"))

  testthat::expect_equal(nrow(preds), 30)
})

testthat::test_that("predict.caret_list", {
  testthat::expect_error(predict(named_models, new_data_list = list(mixed_table)),
                         "The length of new_data_list must be the same length as caret_list")

  testthat::expect_error(predict(named_models, new_data_list = list(mixed_table, head(factor_table, 29), numeric_table)),
                         "All matrices in new_data_list must have the same number of rows")

  preds <- predict(named_models)

  testthat::expect_true(inherits(preds, "data.table"))

  testthat::expect_equal(nrow(preds), 30)
})

testthat::test_that("extract_metric, summary and print methods", {
  metrics <- extract_metric(named_models)
  testthat::expect_equal(nrow(metrics), 3)
  testthat::expect_equal(colnames(metrics), c("model", "method", "metric", "value", "sd"))
})

testthat::test_that("print.summary.caret_list", {
  summary <- summary(named_models)
  testthat::expect_true(inherits(summary, "summary.caret_list"))
  testthat::expect_output(print(summary))
})

testthat::test_that("plot.caret_list", {
  testthat::expect_silent(plt <- plot(named_models))
  testthat::expect_true(inherits(plt, "ggplot"))
})
# Helper function tests --------------------------------------------------------
testthat::test_that("caret_train wrapper", {

  bad_args <- list(method = "NOT_A_METHOD",
                     trControl = .default_control(numeric_vector),
                     y = numeric_vector,
                     metric = .default_metric(numeric_vector))

  testthat::expect_error(.caret_train(bad_args, mixed_table, continue_on_fail = FALSE))
  testthat::expect_warning(model_fail <- .caret_train(bad_args, mixed_table, continue_on_fail = TRUE))
  testthat::expect_null(model_fail)

  train_args <- list(method = "glm",
                     trControl = .default_control(numeric_vector),
                     y = numeric_vector,
                     metric = .default_metric(numeric_vector))
  untrimmed_model <- .caret_train(train_args, mixed_table, trim = FALSE)
  trimmed_model <- .caret_train(train_args, mixed_table, trim = TRUE)

  testthat::expect_false("call" %in% names(trimmed_model))
  testthat::expect_false("trainingData" %in% names(trimmed_model))
  testthat::expect_true("call" %in% names(untrimmed_model))
  testthat::expect_true("trainingData" %in% names(untrimmed_model))

})

testthat::test_that("extract_best_preds", {
  testthat::expect_error(.extract_best_preds(model = "NOT_A_MODEL"))
  testthat::expect_error(.extract_best_preds(model = numeric_model, aggregate_resamples = "NOT_A_BOOLEAN"))

  model_no_pred <- numeric_model
  model_no_pred$pred <- NULL

  testthat::expect_error(.extract_best_preds(model = model_no_pred),
               "No predictions saved during training. Please set savePredictions = 'final' in trControl")

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
})
