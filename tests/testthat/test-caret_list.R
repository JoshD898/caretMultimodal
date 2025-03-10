# Setup ------------------------------------------------------------------------
# TODO reorganize helper tests
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
