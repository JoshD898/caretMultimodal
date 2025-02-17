# TODO load data here
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

numeric_model <- caret::train(x = numeric_table, y = numeric_vector, method = "glm", trControl = .default_control(numeric_vector))
factor_model <- caret::train(x = numeric_table, y = binary_factor_vector, method = "glm", trControl = .default_control(binary_factor_vector))
# Constructor tests ------------------------------------------------------------


# Method tests -----------------------------------------------------------------

# Helper function tests --------------------------------------------------------
testthat::test_that("extract_best_preds", {
  expect_error(.extract_best_preds(model = "NOT_A_MODEL"))
  expect_error(.extract_best_preds(model = numeric_model, aggregate_resamples = "NOT_A_BOOLEAN"))

  model_no_pred <- numeric_model
  model_no_pred$pred <- NULL

  expect_error(.extract_best_preds(model = model_no_pred),
               "No predictions saved during training. Please set savePredictions = 'final' in trControl")

  preds_no_aggregate <- .extract_best_preds(model = numeric_model, aggregate_resamples = FALSE)
})

testthat::test_that("aggregate_vector", {
  expect_equal(.aggregate_vector(c(1, 2, 3, 4)), 2.5)
  expect_equal(.aggregate_vector(c("a", "b", "c")), "a")
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

testthat::test_that("defaul_metric", {
  expect_equal(.default_metric(numeric_vector), "RMSE")
  expect_equal(.default_metric(binary_factor_vector), "ROC")
  expect_equal(.default_metric(three_factor_vector), "Accuracy")
})

testthat::test_that("default_control", {

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
